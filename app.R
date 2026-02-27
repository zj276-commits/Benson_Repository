# app.R
# Trading Insight Hub
# Simplified interface: Market Data, News, AI Forecast

if (getRversion() < "4.1") {
  stop("This app requires R >= 4.1 (for native pipe |>).")
}

for (pkg in c("shiny", "httr2", "DT", "dplyr", "plotly", "jsonlite")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Missing package: ", pkg,
      ". Run: install.packages(c('shiny', 'httr2', 'DT', 'dplyr', 'plotly', 'jsonlite'))"
    )
  }
}

library(shiny)
library(httr2)
library(DT)
library(dplyr)
library(plotly)
library(jsonlite)

# ----------------------------
# basic settings
# ----------------------------

TICKERS <- c("AAPL", "TSLA", "META", "NVDA", "GOOGL", "AMZN", "MSFT")
TICKER_LABELS <- c(
  AAPL = "Apple", TSLA = "Tesla", META = "Meta", NVDA = "NVIDIA",
  GOOGL = "Google", AMZN = "Amazon", MSFT = "Microsoft"
)
MARKET_TICKER <- "SPY"
SECTOR_MAP <- c(
  AAPL = "Information Technology", TSLA = "Consumer Discretionary",
  META = "Communication Services", NVDA = "Information Technology",
  GOOGL = "Communication Services", AMZN = "Consumer Discretionary",
  MSFT = "Information Technology"
)
RISK_FREE_RATE <- 0.043
DIVIDEND_YIELD_MAP <- c(
  AAPL = 0.004, TSLA = 0, META = 0.003, NVDA = 0.0003,
  GOOGL = 0.004, AMZN = 0, MSFT = 0.007
)

WINDOW_MAP <- c("7D" = 7L, "1M" = 22L, "1Y" = 252L)

CACHE_DIR <- "data"
CACHE_MAX_AGE_HOURS <- 24L

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
clamp <- function(x, lo, hi) pmax(lo, pmin(hi, x))

# Add N trading days (Mon-Fri) to a Date.
add_trading_days <- function(start_date, n_days) {
  d <- as.Date(start_date)
  if (is.na(d)) return(rep(as.Date(NA), n_days))
  out <- as.Date(rep(NA, n_days))
  i <- 0L
  while (i < n_days) {
    d <- d + 1
    # Locale-independent weekend check: wday 0=Sunday ... 6=Saturday.
    wd <- as.POSIXlt(d)$wday
    if (wd %in% c(0, 6)) next
    i <- i + 1L
    out[[i]] <- d
  }
  out
}

# ----------------------------
# key loading
# ----------------------------

load_api_keys <- function() {
  # Use the directory where app.R lives as the base path
  app_dir <- if (nzchar(Sys.getenv("SHINY_APP_DIR"))) {
    Sys.getenv("SHINY_APP_DIR")
  } else {
    getwd()
  }
  candidate_files <- c(
    file.path(app_dir, ".env"),
    file.path(app_dir, "all_api"),
    file.path(".", ".env"),
    file.path(".", "all_api")
  )
  for (f in candidate_files) {
    if (!file.exists(f)) next
    lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) character(0))
    for (ln in lines) {
      ln <- trimws(ln)
      if (ln == "" || startsWith(ln, "#")) next
      parts <- strsplit(ln, "=", fixed = TRUE)[[1]]
      if (length(parts) < 2) next
      key <- trimws(parts[[1]])
      value <- paste(parts[-1], collapse = "=")
      value <- trimws(value)
      if (nzchar(key) && nzchar(value) && nchar(Sys.getenv(key)) == 0) {
        do.call(Sys.setenv, setNames(list(value), key))
      }
    }
  }
}

load_api_keys()

FINNHUB_API_KEY <- Sys.getenv("FINNHUB_API_KEY")
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")
OLLAMA_API_KEY <- Sys.getenv("OLLAMA_API_KEY")

# Startup diagnostics
message("--- API Key Check ---")
message("Working dir: ", getwd())
message("FINNHUB_API_KEY: ", ifelse(nzchar(FINNHUB_API_KEY), paste0(substr(FINNHUB_API_KEY, 1, 6), "...OK"), "MISSING"))
message("OPENAI_API_KEY: ", ifelse(nzchar(OPENAI_API_KEY), paste0(substr(OPENAI_API_KEY, 1, 6), "...OK"), "MISSING"))
message("OLLAMA_API_KEY: ", ifelse(nzchar(OLLAMA_API_KEY), paste0(substr(OLLAMA_API_KEY, 1, 6), "...OK"), "MISSING"))
message("all_api exists in cwd: ", file.exists("all_api"))
message("---------------------")

# ----------------------------
# utility
# ----------------------------

cache_path <- function(name) file.path(CACHE_DIR, name)

fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), "N/A", format(round(x, digits), nsmall = digits, big.mark = ","))
}

fmt_price <- function(x) {
  ifelse(is.na(x), "N/A", paste0("$", fmt_num(x, 2)))
}

fmt_pct <- function(x) {
  if (is.na(x)) return("N/A")
  txt <- sprintf("%.2f%%", x)
  if (x >= 0) paste0("+", txt) else txt
}

to_num <- function(x) {
  as.numeric(gsub(",", "", trimws(as.character(x))))
}

safe_now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# ----------------------------
# API calls
# ----------------------------

to_iso_date <- function(days_back) {
  if (days_back <= 5L) {
    "5d"
  } else if (days_back <= 31L) {
    "1mo"
  } else if (days_back <= 95L) {
    "3mo"
  } else if (days_back <= 190L) {
    "6mo"
  } else if (days_back <= 400L) {
    "1y"
  } else {
    "2y"
  }
}

fetch_yahoo_candles <- function(symbol, days_back = 365) {
  range <- to_iso_date(days_back)
  symbol <- trimws(symbol)
  if (!nzchar(symbol)) return(NULL)

  resp <- tryCatch({
    request("https://query1.finance.yahoo.com/v8/finance/chart/") |>
      req_url_path_append(symbol) |>
      req_url_query(
        range = range,
        interval = "1d",
        events = "history",
        includeAdjustedClose = "true"
      ) |>
      req_headers(`User-Agent` = "Mozilla/5.0") |>
      req_timeout(20) |>
      req_retry(max_tries = 2) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
  }, error = function(e) {
    message("Yahoo fallback candle failed for ", symbol, ": ", conditionMessage(e))
    NULL
  })

  if (is.null(resp)) return(NULL)
  if (resp_status(resp) != 200) {
    message("Yahoo fallback candle HTTP ", resp_status(resp), " for ", symbol)
    return(NULL)
  }

  body <- tryCatch(resp_body_json(resp), error = function(e) NULL)
  result <- body$chart$result[[1]]
  if (is.null(result)) {
    message("Yahoo fallback no result for ", symbol)
    return(NULL)
  }

  timestamps <- as.numeric(unlist(result$timestamp))
  quotes <- result$indicators$quote[[1]]
  if (is.null(timestamps) || is.null(quotes)) {
    message("Yahoo fallback payload incomplete for ", symbol)
    return(NULL)
  }

  open <- as.numeric(unlist(quotes$open))
  high <- as.numeric(unlist(quotes$high))
  low <- as.numeric(unlist(quotes$low))
  close <- as.numeric(unlist(quotes$close))
  volume <- as.numeric(unlist(quotes$volume))

  valid <- which(!is.na(close))
  if (length(valid) == 0) return(NULL)

  out <- data.frame(
    Symbol = rep(symbol, length(valid)),
    Date = as.Date(as.POSIXct(timestamps[valid], origin = "1970-01-01", tz = "UTC")),
    Open = open[valid],
    High = high[valid],
    Low = low[valid],
    Close = close[valid],
    Volume = volume[valid],
    stringsAsFactors = FALSE
  )
  out <- out[order(out$Date), ]
  if (nrow(out) == 0) return(NULL)
  tail(out, min(nrow(out), as.integer(days_back)))
}

fetch_finnhub_candles <- function(symbol, api_key, days_back = 365) {
  to_ts <- as.integer(Sys.time())
  from_ts <- to_ts - as.integer(days_back) * 86400L

  resp <- tryCatch({
    request("https://finnhub.io/api/v1/stock/candle") |>
      req_url_query(
        symbol = symbol,
        resolution = "D",
        from = from_ts,
        to = to_ts,
        token = api_key
      ) |>
      req_timeout(20) |>
      req_retry(max_tries = 2) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
  }, error = function(e) {
    message("Finnhub candle failed for ", symbol, ": ", conditionMessage(e))
    NULL
  })

  if (is.null(resp)) {
    message("Retrying with Yahoo fallback for ", symbol)
    return(fetch_yahoo_candles(symbol, days_back))
  }
  if (resp_status(resp) != 200) {
    msg <- tryCatch({
      body <- resp_body_json(resp)
      body$error %||% body$message %||% resp_body_string(resp)
    }, error = function(e) {
      resp_body_string(resp)
    })
    msg_txt <- tolower(as.character(msg))
    status <- resp_status(resp)

    # Some API keys only have quote/news permissions and block candle history.
    # Fall back to Yahoo Finance historical data so charting still works.
    if (status %in% c(401L, 403L, 404L, 429L)) {
      message("Finnhub candle HTTP ", status, " for ", symbol, "; using Yahoo fallback. ", substr(gsub("\\s+", " ", msg_txt), 1, 140))
      return(fetch_yahoo_candles(symbol, days_back))
    }

    message("Finnhub candle HTTP ", status, " for ", symbol, ": ", substr(gsub("\\s+", " ", as.character(msg)), 1, 180))
    return(NULL)
  }

  data <- resp_body_json(resp)
  if (is.null(data$s) || data$s != "ok") {
    msg <- data$error %||% data$message %||% "empty payload"
    message("Finnhub candle not ok for ", symbol, ": ", as.character(msg))
    return(fetch_yahoo_candles(symbol, days_back))
  }

  n <- length(data$t)
  if (n == 0) {
    return(fetch_yahoo_candles(symbol, days_back))
  }

  out <- data.frame(
    Symbol = rep(symbol, n),
    Date = as.Date(as.POSIXct(unlist(data$t), origin = "1970-01-01", tz = "UTC")),
    Open = as.numeric(unlist(data$o)),
    High = as.numeric(unlist(data$h)),
    Low = as.numeric(unlist(data$l)),
    Close = as.numeric(unlist(data$c)),
    Volume = as.numeric(unlist(data$v)),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$Date), ]
  rownames(out) <- NULL
  out
}

fetch_finnhub_news <- function(symbol, api_key, days_back = 7) {
  to_date <- format(Sys.Date(), "%Y-%m-%d")
  from_date <- format(Sys.Date() - as.integer(days_back), "%Y-%m-%d")

  resp <- tryCatch({
    request("https://finnhub.io/api/v1/company-news") |>
      req_url_query(symbol = symbol, from = from_date, to = to_date, token = api_key) |>
      req_timeout(20) |>
      req_perform()
  }, error = function(e) {
    message("Finnhub error for ", symbol, ": ", conditionMessage(e))
    NULL
  })

  if (is.null(resp) || resp_status(resp) != 200) return(NULL)

  arts <- resp_body_json(resp)
  if (is.null(arts) || length(arts) == 0) return(NULL)

  lapply(arts, function(a) {
    data.frame(
      Symbol = symbol,
      Title = a[["headline"]][1] %||% "",
      Source = a[["source"]][1] %||% "",
      Published = a[["datetime"]][1] %||% NA,
      Summary = a[["summary"]][1] %||% "",
      Url = a[["url"]][1] %||% "",
      stringsAsFactors = FALSE
    )
  })
}

fetch_finnhub_quote <- function(symbol, api_key) {
  resp <- tryCatch({
    request("https://finnhub.io/api/v1/quote") |>
      req_url_query(symbol = symbol, token = api_key) |>
      req_timeout(10) |> req_perform()
  }, error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) != 200) return(NULL)
  data <- resp_body_json(resp)
  if (is.null(data$c) || data$c == 0) return(NULL)
  list(symbol = symbol, price = data$c, change = data$d,
       change_pct = data$dp, high = data$h, low = data$l,
       open = data$o, prev_close = data$pc)
}

fetch_all_quotes <- function(api_key, tickers = TICKERS) {
  quotes <- list()
  for (sym in tickers) {
    q <- fetch_finnhub_quote(sym, api_key)
    if (!is.null(q)) quotes[[sym]] <- q
    Sys.sleep(0.12)
  }
  quotes
}

fetch_openai_forecast <- function(prompt_text, api_key) {
  if (!nzchar(api_key)) {
    return(list(ok = FALSE, error = "OPENAI_API_KEY not set."))
  }

  body <- list(
    model = "gpt-4o-mini",
    temperature = 0.2,
    messages = list(
      list(role = "system", content = "You are a concise, finance-focused stock forecaster.") ,
      list(role = "user", content = prompt_text)
    ),
    response_format = list(type = "json_object"),
    max_tokens = 700
  )

  resp <- tryCatch({
    request("https://api.openai.com/v1/chat/completions") |>
      req_headers(
        Authorization = paste0("Bearer ", api_key),
        "Content-Type" = "application/json"
      ) |>
      req_body_json(body) |>
      req_perform()
  }, error = function(e) {
    list(ok = FALSE, error = conditionMessage(e))
  })

  if (!inherits(resp, "httr2_response")) return(resp)
  if (resp_status(resp) != 200) return(list(ok = FALSE, error = resp_body_string(resp)))

  raw <- resp_body_json(resp)
  text <- raw$choices[[1]]$message$content
  if (is.null(text) || !nzchar(text)) return(list(ok = FALSE, error = "Empty response from OpenAI."))

  text <- gsub("^```json|```$", "", trimws(text))
  parsed <- tryCatch(jsonlite::fromJSON(text), error = function(e) NULL)
  if (is.null(parsed)) return(list(ok = FALSE, error = "Invalid JSON returned by OpenAI.", raw = text))

  list(ok = TRUE, payload = parsed, raw = text)
}

fetch_ollama_forecast <- function(prompt_text, api_key) {
  use_local <- !nzchar(api_key) || trimws(tolower(api_key)) == "local"
  model <- Sys.getenv("OLLAMA_MODEL", "llama3.1:8b")

  req <- if (use_local) {
    tryCatch({
      request("http://localhost:11434/v1/chat/completions") |>
        req_headers(`Content-Type` = "application/json") |>
        req_body_json(list(
          model = model,
          messages = list(
            list(role = "system", content = "You are a concise, finance-focused stock forecaster."),
            list(role = "user", content = prompt_text)
          ),
          temperature = 0.2,
          max_tokens = 700
        )) |>
        req_perform()
    }, error = function(e) NULL)
  } else {
    tryCatch({
      request("https://ollama.com/v1/chat/completions") |>
        req_headers(
          Authorization = paste0("Bearer ", api_key),
          `Content-Type` = "application/json"
        ) |>
        req_body_json(list(
          model = "gpt-oss:20b-cloud",
          messages = list(
            list(role = "system", content = "You are a concise, finance-focused stock forecaster."),
            list(role = "user", content = prompt_text)
          ),
          temperature = 0.2,
          max_tokens = 700
        )) |>
        req_perform()
    }, error = function(e) NULL)
  }

  if (!inherits(req, "httr2_response")) return(list(ok = FALSE, error = "Ollama request failed."))
  if (resp_status(req) != 200) return(list(ok = FALSE, error = resp_body_string(req)))

  raw <- tryCatch(resp_body_json(req), error = function(e) NULL)
  text <- NULL
  if (!is.null(raw)) {
    if (!is.null(raw$choices) && length(raw$choices) > 0) {
      text <- raw$choices[[1]]$message$content
    } else if (!is.null(raw$message) && !is.null(raw$message$content)) {
      text <- raw$message$content
    }
  }
  if (is.null(text) || !nzchar(text)) return(list(ok = FALSE, error = "Empty response from Ollama."))

  text <- gsub("^```json|```$", "", trimws(text))
  parsed <- tryCatch(jsonlite::fromJSON(text), error = function(e) NULL)
  if (is.null(parsed)) return(list(ok = FALSE, error = "Invalid JSON returned by Ollama.", raw = text))
  list(ok = TRUE, payload = parsed, raw = text, via = "ollama")
}

# ----------------------------
# local cache
# ----------------------------

load_cached_data <- function() {
  daily_file <- cache_path("daily_prices.csv")
  news_file <- cache_path("news.csv")
  meta_file <- cache_path("cache_updated.txt")

  if (!file.exists(daily_file)) return(NULL)

  daily_df <- tryCatch(
    {
      d <- read.csv(daily_file, stringsAsFactors = FALSE)
      d$Date <- as.Date(d$Date)
      d
    },
    error = function(e) NULL
  )
  if (is.null(daily_df) || nrow(daily_df) == 0) return(NULL)

  news_df <- tryCatch(
    {
      if (!file.exists(news_file)) return(NULL)
      n <- read.csv(news_file, stringsAsFactors = FALSE)
      n$Published <- as.POSIXct(n$Published, origin = "1970-01-01", tz = "UTC")
      n
    },
    error = function(e) NULL
  )

  updated <- if (file.exists(meta_file)) trimws(readLines(meta_file, n = 1L, warn = FALSE)) else ""
  list(daily = daily_df, news = news_df, updated_at = updated)
}

save_cached_data <- function(daily_df, news_df = NULL) {
  if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)

  write.csv(daily_df, cache_path("daily_prices.csv"), row.names = FALSE)
  if (!is.null(news_df) && nrow(news_df) > 0) {
    save_news <- news_df
    save_news$Published <- as.numeric(save_news$Published)
    write.csv(save_news, cache_path("news.csv"), row.names = FALSE)
  }
  writeLines(safe_now(), cache_path("cache_updated.txt"))
}

get_export_dir <- function() {
  if (nzchar(Sys.getenv("SHINY_APP_DIR"))) Sys.getenv("SHINY_APP_DIR") else getwd()
}

export_csv_files <- function(daily_df, rag_df) {
  app_dir <- get_export_dir()
  if (!is.null(daily_df) && nrow(daily_df) > 0) {
    tryCatch({
      prices <- daily_df
      prices$Date <- as.character(prices$Date)
      prices <- prices[order(prices$Symbol, prices$Date), ]
      write.csv(prices, file.path(app_dir, "historical_prices.csv"), row.names = FALSE)
      message("[Export] historical_prices.csv: ", nrow(prices), " rows")
    }, error = function(e) message("Could not export historical_prices.csv: ", e$message))
  }
  if (!is.null(rag_df) && nrow(rag_df) > 0) {
    tryCatch({
      params <- rag_df[order(rag_df$snapshot_date, rag_df$ticker), ]
      write.csv(params, file.path(app_dir, "model_parameters.csv"), row.names = FALSE)
      message("[Export] model_parameters.csv: ", nrow(params), " rows")
    }, error = function(e) message("Could not export model_parameters.csv: ", e$message))
  }
}

export_news_csv <- function(news_df) {
  if (is.null(news_df) || nrow(news_df) == 0) return()
  app_dir <- get_export_dir()
  news_file <- file.path(app_dir, "news_archive.csv")
  tryCatch({
    export <- news_df
    export$Published <- as.numeric(export$Published)
    export$published_et <- format(
      as.POSIXct(export$Published, origin = "1970-01-01", tz = "UTC"),
      "%Y-%m-%d %H:%M", tz = "America/New_York"
    )
    existing <- if (file.exists(news_file)) {
      tryCatch(read.csv(news_file, stringsAsFactors = FALSE), error = function(e) NULL)
    } else NULL
    if (!is.null(existing) && nrow(existing) > 0) {
      combined <- dplyr::bind_rows(existing, export)
      combined <- combined[!duplicated(combined$Title), ]
    } else {
      combined <- export
    }
    combined <- combined[order(combined$Published, decreasing = TRUE), ]
    write.csv(combined, news_file, row.names = FALSE)
    message("[Export] news_archive.csv: ", nrow(combined), " rows")
  }, error = function(e) message("Could not export news_archive.csv: ", e$message))
}

# ----------------------------
# feature helpers
# ----------------------------

compute_ohlc_metrics <- function(df, lookback = 252) {
  if (is.null(df) || nrow(df) < 2) return(NULL)
  df <- df |> arrange(Date)

  n <- nrow(df)
  latest <- tail(df, 1)
  idx_1d <- max(1, n - 1)
  idx_7d <- max(1, n - 6)
  idx_30d <- max(1, n - 29)
  idx_n <- max(1, n - lookback)

  p1d <- latest$Close - df$Close[idx_1d]
  p1d_pct <- ifelse(df$Close[idx_1d] > 0, p1d / df$Close[idx_1d] * 100, NA_real_)
  p7d <- latest$Close - df$Close[idx_7d]
  p7d_pct <- ifelse(df$Close[idx_7d] > 0, p7d / df$Close[idx_7d] * 100, NA_real_)
  p30d <- latest$Close - df$Close[idx_30d]
  p30d_pct <- ifelse(df$Close[idx_30d] > 0, p30d / df$Close[idx_30d] * 100, NA_real_)
  pn <- latest$Close - df$Close[idx_n]
  pn_pct <- ifelse(df$Close[idx_n] > 0, pn / df$Close[idx_n] * 100, NA_real_)

  list(
    latest = latest$Close,
    latest_date = latest$Date,
    change1d = p1d,
    change1d_pct = p1d_pct,
    change7d_pct = p7d_pct,
    change30d_pct = p30d_pct,
    changeN_pct = pn_pct,
    mean_20d_volatility = {
      if (n < 21) NA_real_ else {
        lr <- diff(log(df$Close))
        round(sd(tail(lr, 20), na.rm = TRUE) * sqrt(252) * 100, 2)
      }
    }
  )
}

build_news_snapshot <- function(symbol, news_df, max_rows = 5) {
  if (is.null(news_df) || nrow(news_df) == 0) return("No recent news.")
  sub <- news_df[news_df$Symbol == symbol, ]
  if (nrow(sub) == 0) return("No recent news.")

  sub <- sub[order(as.POSIXct(sub$Published, origin = "1970-01-01", tz = "UTC"), decreasing = TRUE), ]
  top <- head(sub, max_rows)
  items <- paste0(
    seq_len(nrow(top)), ". ",
    top$Symbol, ": ", top$Title,
    " (", format(as.POSIXct(top$Published, origin = "1970-01-01", tz = "UTC"), "%m-%d %H:%M", tz = "America/New_York"), " ET)"
  )
  paste(items, collapse = "\n")
}

heuristic_news_tone <- function(news_df) {
  if (is.null(news_df) || nrow(news_df) == 0) return(0)
  pos <- c("beat", "beats", "upgrade", "growth", "profit", "surge", "strong", "outperform", "positive", "up", "rally", "raise")
  neg <- c("miss", "lower", "drop", "down", "weak", "risk", "lawsuit", "delay", "loss", "concern", "warning", "fall")

  corpus <- tolower(paste(paste(news_df$Title, news_df$Summary), collapse = " "))
  pos_score <- sum(vapply(pos, function(w) sum(grepl(w, corpus, fixed = TRUE)), integer(1)))
  neg_score <- sum(vapply(neg, function(w) sum(grepl(w, corpus, fixed = TRUE)), integer(1)))
  pos_score - neg_score
}

compute_news_bias <- function(news_df, max_items = 20) {
  # Lightweight, grounded bias: keyword + recency weighting.
  if (is.null(news_df) || nrow(news_df) == 0) {
    return(list(
      ok = TRUE,
      bias_score = 0,
      mu_shift_annual = 0,
      sigma_mult = 1,
      details = data.frame()
    ))
  }

  sub <- news_df[order(news_df$Published, decreasing = TRUE), ]
  sub <- head(sub, max_items)

  pos <- c("beat", "beats", "upgrade", "raised", "raise", "growth", "profit", "surge", "strong", "outperform", "record", "rally", "buy", "bullish")
  neg <- c("miss", "misses", "downgrade", "cut", "lower", "drop", "down", "weak", "risk", "lawsuit", "probe", "delay", "loss", "concern", "warning", "sell", "bearish")

  cat_map <- list(
    earnings = c("earnings", "eps", "revenue", "guidance", "quarter", "q1", "q2", "q3", "q4"),
    analyst = c("upgrade", "downgrade", "price target", "pt", "rating"),
    product = c("launch", "product", "release", "chip", "ai", "model"),
    legal_reg = c("lawsuit", "sec", "doj", "antitrust", "ban", "regulator", "probe"),
    macro = c("rates", "fed", "inflation", "recession", "macro")
  )

  now_utc <- as.POSIXct(Sys.time(), tz = "UTC")
  rows <- lapply(seq_len(nrow(sub)), function(i) {
    title <- sub$Title[[i]] %||% ""
    summary <- sub$Summary[[i]] %||% ""
    corpus <- tolower(paste(title, summary))

    pos_hits <- sum(vapply(pos, function(w) grepl(w, corpus, fixed = TRUE), logical(1)))
    neg_hits <- sum(vapply(neg, function(w) grepl(w, corpus, fixed = TRUE), logical(1)))
    polarity <- if (pos_hits > neg_hits) 1 else if (neg_hits > pos_hits) -1 else 0

    # Simple category assignment (first match by priority).
    category <- "other"
    for (nm in names(cat_map)) {
      kws <- cat_map[[nm]]
      if (any(vapply(kws, function(w) grepl(w, corpus, fixed = TRUE), logical(1)))) {
        category <- nm
        break
      }
    }

    published <- as.POSIXct(sub$Published[[i]], tz = "UTC", origin = "1970-01-01")
    age_days <- as.numeric(difftime(now_utc, published, units = "days"))
    age_days <- ifelse(is.na(age_days) || age_days < 0, 0, age_days)

    # Recency weight: ~1.0 today, ~0.37 after 7 days.
    w <- exp(-age_days / 7)
    contrib <- polarity * w

    pub_hm <- format(published, "%H:%M", tz = "America/New_York")
    pub_str <- if (pub_hm == "00:00") format(published, "%Y-%m-%d", tz = "America/New_York") else paste0(format(published, "%Y-%m-%d %H:%M", tz = "America/New_York"), " ET")
    data.frame(
      Published = pub_str,
      Source = sub$Source[[i]] %||% "",
      Category = category,
      Polarity = polarity,
      Weight = round(w, 3),
      Contribution = round(contrib, 3),
      Title = title,
      stringsAsFactors = FALSE
    )
  })

  details <- dplyr::bind_rows(rows)
  bias_score <- sum(details$Contribution, na.rm = TRUE)

  # Map bias score -> small drift adjustment, capped to avoid extreme moves.
  # Interpretation: annual drift shift in [-5%, +5%].
  mu_shift_annual <- clamp(bias_score * 0.01, -0.05, 0.05)
  # Slight vol expansion when news is highly skewed.
  sigma_mult <- clamp(1 + abs(bias_score) * 0.02, 1, 1.15)

  list(
    ok = TRUE,
    bias_score = round(bias_score, 3),
    mu_shift_annual = round(mu_shift_annual, 4),
    sigma_mult = round(sigma_mult, 3),
    details = details
  )
}

estimate_gbm_params <- function(hist_df, max_lookback = 252) {
  if (is.null(hist_df) || nrow(hist_df) < 5) return(NULL)
  hist_df <- hist_df |> arrange(Date)
  h <- tail(hist_df, min(nrow(hist_df), as.integer(max_lookback)))
  close <- as.numeric(h$Close)
  close <- close[!is.na(close)]
  if (length(close) < 5) return(NULL)

  r <- diff(log(close))
  r <- r[is.finite(r)]
  # Need enough returns to make volatility estimation meaningful.
  if (length(r) < 21) return(NULL)

  mu_daily <- mean(r)
  sig_daily <- sd(r)
  list(
    s0 = tail(close, 1),
    last_date = tail(h$Date, 1),
    n_obs = length(r),
    mu_annual = as.numeric(mu_daily) * 252,
    sigma_annual = as.numeric(sig_daily) * sqrt(252),
    mu_daily = as.numeric(mu_daily),
    sigma_daily = as.numeric(sig_daily)
  )
}

gbm_forecast_path <- function(s0, mu_annual, sigma_annual, n_days, start_date, p_lo = 0.05, p_hi = 0.95) {
  if (!is.finite(s0) || !is.finite(mu_annual) || !is.finite(sigma_annual) || n_days < 1) return(NULL)
  t <- (0:n_days) / 252
  z_lo <- qnorm(p_lo)
  z_hi <- qnorm(p_hi)

  # Black-Scholes / GBM dynamics: log-price normal.
  drift_term <- (mu_annual - 0.5 * sigma_annual^2) * t
  vol_term <- sigma_annual * sqrt(t)

  expected <- s0 * exp(mu_annual * t)
  median <- s0 * exp(drift_term)
  qlo <- s0 * exp(drift_term + vol_term * z_lo)
  qhi <- s0 * exp(drift_term + vol_term * z_hi)

  dates <- c(as.Date(start_date), add_trading_days(as.Date(start_date), n_days))
  data.frame(
    Date = dates,
    Expected = as.numeric(expected),
    Median = as.numeric(median),
    Lo = as.numeric(qlo),
    Hi = as.numeric(qhi),
    stringsAsFactors = FALSE
  )
}

estimate_lattice_params <- function(hist_df, T_years = 1/12, N_steps = 22) {
  if (is.null(hist_df) || nrow(hist_df) < 22) return(NULL)
  hist_df <- hist_df |> arrange(Date)
  cl <- as.numeric(hist_df$Close); cl <- cl[!is.na(cl)]
  if (length(cl) < 22) return(NULL)
  r <- diff(log(cl)); r <- r[is.finite(r)]
  if (length(r) < 21) return(NULL)
  mu_a <- mean(r) * 252; sig_a <- sd(r) * sqrt(252)
  dt <- T_years / N_steps
  u <- exp(sig_a * sqrt(dt)); d <- 1 / u
  p_real <- clamp((exp(mu_a * dt) - d) / (u - d), 0.001, 0.999)
  p_rn <- clamp((exp(RISK_FREE_RATE * dt) - d) / (u - d), 0.001, 0.999)
  list(s0 = tail(cl, 1), last_date = tail(hist_df$Date, 1),
       T_years = T_years, N_steps = as.integer(N_steps), dt = dt,
       u = u, d = d, p_real = p_real, p_rn = p_rn,
       mu_annual = mu_a, sigma_annual = sig_a, method = "CRR", n_obs = length(r))
}

lattice_forecast_path <- function(s0, u, d, p, N_steps, start_date, p_lo = 0.05, p_hi = 0.95) {
  if (!is.finite(s0) || !is.finite(u) || !is.finite(d) || N_steps < 1) return(NULL)
  dates <- c(as.Date(start_date), add_trading_days(as.Date(start_date), N_steps))
  exp_v <- lo <- hi <- numeric(N_steps + 1)
  exp_v[1] <- lo[1] <- hi[1] <- s0
  for (k in seq_len(N_steps)) {
    exp_v[k + 1] <- s0 * (p * u + (1 - p) * d)^k
    j_lo <- qbinom(p_lo, k, p); j_hi <- qbinom(p_hi, k, p)
    lo[k + 1] <- s0 * u^j_lo * d^(k - j_lo)
    hi[k + 1] <- s0 * u^j_hi * d^(k - j_hi)
  }
  data.frame(Date = dates, Expected = exp_v, Lo = lo, Hi = hi, stringsAsFactors = FALSE)
}

compute_sim_params <- function(stock_df, market_df) {
  if (is.null(stock_df) || is.null(market_df)) return(NULL)
  stock_df <- stock_df |> arrange(Date)
  market_df <- market_df |> arrange(Date)
  mg <- merge(data.frame(Date = stock_df$Date, SC = stock_df$Close),
              data.frame(Date = market_df$Date, MC = market_df$Close), by = "Date")
  if (nrow(mg) < 22) return(NULL)
  mg <- mg[order(mg$Date), ]
  sr <- diff(log(mg$SC)); mr <- diff(log(mg$MC))
  ok <- is.finite(sr) & is.finite(mr); sr <- sr[ok]; mr <- mr[ok]
  if (length(sr) < 21) return(NULL)
  fit <- lm(sr ~ mr); s <- summary(fit); co <- s$coefficients
  tc <- qt(0.975, df = s$df[2])
  list(alpha_daily = co[1,1], alpha_annual = co[1,1] * 252,
       beta = co[2,1], alpha_se = co[1,2], beta_se = co[2,2],
       alpha_ci95_low = (co[1,1] - tc * co[1,2]) * 252,
       alpha_ci95_high = (co[1,1] + tc * co[1,2]) * 252,
       beta_ci95_low = co[2,1] - tc * co[2,2],
       beta_ci95_high = co[2,1] + tc * co[2,2],
       r_squared = s$r.squared, resid_std = s$sigma, n_obs = length(sr))
}

compute_rag_table <- function(daily_df) {
  if (is.null(daily_df) || nrow(daily_df) < 5) return(NULL)
  mkt <- daily_df[daily_df$Symbol == MARKET_TICKER, ]
  rows <- lapply(TICKERS, function(sym) {
    sdf <- daily_df[daily_df$Symbol == sym, ]
    if (nrow(sdf) < 22) return(NULL)
    sdf <- sdf[order(sdf$Date), ]
    cl <- as.numeric(sdf$Close); cl <- cl[!is.na(cl)]
    if (length(cl) < 22) return(NULL)
    r <- diff(log(cl)); r <- r[is.finite(r)]
    if (length(r) < 21) return(NULL)
    n <- length(r); mu <- mean(r); sg <- sd(r)
    vol_a <- sg * sqrt(252)
    skew <- if (n > 2) mean((r - mu)^3) / sg^3 else NA_real_
    kurt <- if (n > 3) mean((r - mu)^4) / sg^4 else NA_real_
    dd <- (cummax(cl) - cl) / cummax(cl)
    td <- as.numeric(diff(range(sdf$Date)))
    ret_a <- if (td > 0) (tail(cl, 1) / head(cl, 1))^(365.25 / td) - 1 else NA_real_
    r_test <- if (n <= 5000) r else sample(r, 5000)
    n_test <- length(r_test)
    nt <- tryCatch({
      st <- shapiro.test(r_test)
      list(nm = "Shapiro-Wilk", pv = st$p.value)
    }, error = function(e) list(nm = "N/A", pv = NA_real_))
    log10_pv <- if (!is.na(nt$pv) && nt$pv > 0) log10(nt$pv) else NA_real_
    outl <- sum(abs(r - mu) > 3 * sg) / n
    r_below <- pmin(r, 0)
    vol_down <- sqrt(mean(r_below^2)) * sqrt(252)
    vol_20 <- if (n >= 20) sd(tail(r, 20)) * sqrt(252) else vol_a
    avg_vol_20 <- if (nrow(sdf) >= 20) round(mean(tail(sdf$Volume, 20), na.rm = TRUE)) else NA_real_
    gbm <- estimate_gbm_params(sdf, 252)
    mu_se <- sg * 252 / sqrt(n); sig_se <- vol_a / sqrt(2 * n)
    tc_v <- qt(0.975, df = n - 1)
    lat <- estimate_lattice_params(sdf, 1/12, 22)
    sim <- compute_sim_params(sdf, mkt)
    div_y <- DIVIDEND_YIELD_MAP[[sym]] %||% 0
    is_normal <- !is.na(nt$pv) && nt$pv >= 0.05
    beta_val <- sim$beta %||% NA_real_
    tags_v <- paste(c(
      if (!is.na(beta_val) && beta_val > 1.3) "high_beta" else if (!is.na(beta_val) && beta_val < 0.7) "low_beta",
      if (!is_normal) "non_normal",
      if (!is.na(vol_a) && vol_a > 0.4) "high_vol" else if (!is.na(vol_a) && vol_a < 0.15) "low_vol",
      if (!is.na(kurt) && kurt > 4) "heavy_tails",
      if (div_y == 0) "no_dividend"
    ), collapse = ";")
    mfn <- paste0(
      if (!is_normal) "Shapiro-Wilk rejects normality; " else "Normality not rejected; ",
      if (!is.na(kurt) && kurt > 4) "heavy tails (consider t-innovations/GARCH); " else "",
      sprintf("R2_vs_SPY=%.2f%%", (sim$r_squared %||% 0) * 100)
    )
    data.frame(
      ticker = sym, company_name = TICKER_LABELS[[sym]],
      asof_date = as.character(max(sdf$Date)),
      est_window_start = as.character(min(sdf$Date)),
      n_obs = n,
      spot_price = round(tail(cl, 1), 4),
      log_return_mean = round(mu, 8), log_return_std = round(sg, 8),
      vol_annual_realized = round(vol_a, 6),
      vol_annual_downside = round(vol_down, 6),
      vol_annual_20d = round(vol_20, 6),
      skewness = round(skew, 4), kurtosis = round(kurt, 4),
      max_drawdown = round(max(dd, na.rm = TRUE), 6),
      return_annual = round(ret_a, 6),
      avg_daily_volume_20d = avg_vol_20,
      normality_n = n_test,
      normality_pvalue = round(nt$pv, 6),
      normality_log10_pvalue = round(log10_pv, 4),
      outlier_fraction = round(outl, 6),
      gbm_mu_annual = round(gbm$mu_annual %||% NA_real_, 6),
      gbm_sigma_annual = round(gbm$sigma_annual %||% NA_real_, 6),
      gbm_mu_se = round(mu_se, 6), gbm_sigma_se = round(sig_se, 6),
      gbm_mu_ci95_low = round((gbm$mu_annual %||% NA_real_) - tc_v * mu_se, 6),
      gbm_mu_ci95_high = round((gbm$mu_annual %||% NA_real_) + tc_v * mu_se, 6),
      gbm_sigma_ci95_low = round((gbm$sigma_annual %||% NA_real_) - tc_v * sig_se, 6),
      gbm_sigma_ci95_high = round((gbm$sigma_annual %||% NA_real_) + tc_v * sig_se, 6),
      dividend_yield = div_y,
      carry_r_minus_q = RISK_FREE_RATE - div_y,
      lattice_u = round(lat$u %||% NA_real_, 8),
      lattice_d = round(lat$d %||% NA_real_, 8),
      lattice_p_real = round(lat$p_real %||% NA_real_, 8),
      lattice_p_rn = round(lat$p_rn %||% NA_real_, 8),
      lattice_implied_sigma = round(lat$sigma_annual %||% NA_real_, 6),
      sim_alpha_annual = round(sim$alpha_annual %||% NA_real_, 8),
      sim_beta = round(sim$beta %||% NA_real_, 6),
      sim_alpha_se = round(sim$alpha_se %||% NA_real_, 8),
      sim_beta_se = round(sim$beta_se %||% NA_real_, 6),
      sim_alpha_ci95_low = round(sim$alpha_ci95_low %||% NA_real_, 8),
      sim_alpha_ci95_high = round(sim$alpha_ci95_high %||% NA_real_, 8),
      sim_beta_ci95_low = round(sim$beta_ci95_low %||% NA_real_, 6),
      sim_beta_ci95_high = round(sim$beta_ci95_high %||% NA_real_, 6),
      sim_r_squared = round(sim$r_squared %||% NA_real_, 6),
      sim_resid_std = round(sim$resid_std %||% NA_real_, 8),
      model_fit_notes = mfn,
      tags = tags_v,
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(rows)
}

update_rag_history <- function(daily_df, start_date = "2026-02-01") {
  if (is.null(daily_df) || nrow(daily_df) < 5) return(NULL)
  history_file <- cache_path("rag_history.csv")
  expected_cols <- c("normality_n", "normality_log10_pvalue",
                     "vol_annual_realized", "vol_annual_downside", "tags")
  existing <- if (file.exists(history_file)) {
    tryCatch(read.csv(history_file, stringsAsFactors = FALSE), error = function(e) NULL)
  } else NULL
  if (!is.null(existing) && !all(expected_cols %in% names(existing))) {
    message("[RAG] Schema changed — rebuilding rag_history.csv from scratch")
    existing <- NULL
  }
  existing_dates <- if (!is.null(existing)) unique(existing$snapshot_date) else character(0)
  all_dates <- sort(unique(daily_df$Date))
  dates_needed <- all_dates[all_dates >= as.Date(start_date) & all_dates <= Sys.Date()]
  dates_to_compute <- as.character(dates_needed[!as.character(dates_needed) %in% existing_dates])
  if (length(dates_to_compute) == 0) return(existing)
  new_rows <- list()
  for (d_str in dates_to_compute) {
    d <- as.Date(d_str)
    sub_df <- daily_df[daily_df$Date <= d, ]
    rag <- compute_rag_table(sub_df)
    if (!is.null(rag)) {
      rag$snapshot_date <- d_str
      new_rows[[length(new_rows) + 1]] <- rag
    }
  }
  if (length(new_rows) == 0) return(existing)
  new_df <- dplyr::bind_rows(new_rows)
  result <- if (!is.null(existing)) dplyr::bind_rows(existing, new_df) else new_df
  result <- result[order(result$snapshot_date, result$ticker), ]
  if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)
  write.csv(result, history_file, row.names = FALSE)
  result
}

build_forecast_prompt <- function(symbol, window_days, trend, news_text, tone_score) {
  list(
    direction = paste(
      "Forecast:", symbol,
      "time horizon", window_days, "trading days."
    ),
    summary = paste(
      "Price metrics:",
      "Latest close", fmt_price(trend$latest),
      "1d", fmt_pct(trend$change1d_pct),
      "7d", fmt_pct(trend$change7d_pct),
      "30d", fmt_pct(trend$change30d_pct),
      "1y(last", min(window_days, 252), "days approx)", fmt_pct(trend$changeN_pct),
      "Volatility(annualized 20d)", ifelse(is.na(trend$mean_20d_volatility), "N/A", paste0(trend$mean_20d_volatility, "%")),
      "."
    ),
    news = paste(
      "Latest company news text:", news_text, "."
    ),
    task = paste(
      "Return JSON exactly with keys:",
      "trend (UP/DOWN/NEUTRAL), confidence (0-100),",
      "forecast_signal (UP / DOWN / FLAT),",
      "news_adjustment (one word: POSITIVE/NEGATIVE/NEUTRAL),",
      "reasoning (3 bullets max in one JSON array),",
      "expected_price_level (short text),",
      "risk_note (short text)."
    ),
    hint = sprintf("Use sentiment and metrics; a negative news tone score is %.2f.", tone_score)
  ) |>
    paste(collapse = "\n")
}

# ----------------------------
# ui
# ----------------------------

ui <- navbarPage(
  title = "Market Insight Studio",
  id = "main_tabs",
  inverse = TRUE,
  collapsible = TRUE,
  position = "fixed-top",
  header = tags$head(tags$style(HTML("\n    body { background: radial-gradient(circle at top, #111827, #0b1220) !important; color: #e5e7eb; }\n    .navbar { background: linear-gradient(90deg, #111827, #0f172a); border-bottom: 1px solid #334155; }\n    .navbar-brand { color: #f8fafc !important; font-weight: 700; letter-spacing: .4px; }\n    .tab-content { padding: 70px 14px 20px; }\n    .metric-card { border: 1px solid #334155; background: linear-gradient(150deg, rgba(30,41,59,.75), rgba(15,23,42,.85)); border-radius: 14px; padding: 14px; margin-bottom: 12px; box-shadow: 0 8px 28px rgba(2,6,23,.35); }\n    .metric-title { font-size: 13px; color: #94a3b8; letter-spacing: .2px; }\n    .metric-value { font-size: 28px; font-weight: 700; color: #f8fafc; margin-top: 6px; }\n    .metric-sub { margin-top: 4px; color: #cbd5e1; font-size: 13px; }\n    .trend-up { color: #34d399; font-weight: 700; }\n    .trend-down { color: #f87171; font-weight: 700; }\n    .trend-flat { color: #cbd5e1; font-weight: 700; }\n    .panel-card { border: 1px solid #334155; border-radius: 14px; background: #0b1220; padding: 14px; margin-top: 14px; }\n    .section-title { margin: 6px 0 12px; color: #f8fafc; font-size: 22px; letter-spacing: .2px; }\n    .btn { border-radius: 10px; }\n    .btn-primary { background: #2563eb; border: none; }\n    .btn-success { background: #16a34a; border: none; }\n    .btn-warning { background: #ca8a04; border: none; }\n    table.dataTable thead th { background: #1e293b !important; color: #e2e8f0 !important; }\n    .dt-container .dataTables_wrapper { color: #e2e8f0; }\n    table.dataTable tbody td { color: #e5e7eb !important; padding: 6px 10px !important; font-size: 13px; white-space: nowrap; }\n    table.dataTable thead th { padding: 8px 10px !important; font-size: 13px; white-space: nowrap; }\n    table.dataTable tbody tr { background: #111827 !important; }\n    table.dataTable tbody tr:hover { background: #1e293b !important; }\n    .dataTables_info, .dataTables_paginate { color: #cbd5e1 !important; }\n    .dataTables_paginate .paginate_button { color: #cbd5e1 !important; }\n    .forecast-box { border: 1px solid #334155; border-left: 5px solid #2563eb; background: #111827; border-radius: 12px; padding: 16px; margin-top: 10px; }\n    .forecast-title { font-size: 22px; font-weight: 700; margin-bottom: 6px; }\n    .chip { display: inline-block; padding: 3px 10px; border-radius: 999px; font-size: 12px; margin-right: 8px; margin-bottom: 4px; }\n    .chip-up { background: rgba(34,197,94,.2); color: #bbf7d0; border: 1px solid rgba(34,197,94,.5); }\n    .chip-down { background: rgba(248,113,113,.2); color: #fecaca; border: 1px solid rgba(248,113,113,.45); }\n    .chip-flat { background: rgba(148,163,184,.2); color: #cbd5e1; border: 1px solid rgba(148,163,184,.5); }\n  "))),
  tabPanel(
    "Data",
    fluidPage(
      uiOutput("data_page_content")
    )
  ),
  tabPanel(
    "News",
    fluidPage(
      fluidRow(
        column(3,
          selectInput("news_ticker", "Filter", choices = c("ALL", names(TICKER_LABELS)), selected = "ALL")
        ),
        column(3,
          radioButtons(
            "news_window", "Date Range",
            choices = c("7D" = 7L, "30D" = 30L, "90D" = 90L),
            selected = 7L,
            inline = TRUE
          )
        ),
        column(3,
          br(),
          actionButton("refresh_news", "Refresh News", class = "btn-warning")
        ),
        column(3,
          br(),
          helpText("Source: Finnhub", style = "color: #94a3b8;")
        )
      ),
      fluidRow(column(12, DTOutput("news_table"))),
      fluidRow(
        column(12,
          uiOutput("news_error")
        )
      )
    )
  ),
  tabPanel(
    "AI Prediction",
    fluidPage(
      fluidRow(
        column(3,
          selectInput("ai_ticker", "Select Stock", choices = TICKERS, selected = "AAPL")
        ),
        column(3,
          radioButtons(
            "forecast_window", "Forecast Window",
            choices = c("7D" = 7L, "30D" = 30L, "90D" = 90L),
            selected = 30L,
            inline = TRUE
          )
        ),
        column(3,
          actionButton("run_prediction", "Run Prediction", class = "btn-success"),
          br(),
          actionButton("toggle_bias", "News Bias / Adjustment", class = "btn-primary"),
          style = "margin-top: 24px;"
        ),
        column(3,
          tags$div(
            style = "margin-top: 28px; color: #94a3b8;",
            "Model: OpenAI gpt-4o-mini preferred (fast + lower cost), fallback Ollama"
          )
        )
      ),
      fluidRow(column(12, uiOutput("ai_error"))),
      fluidRow(column(12, plotlyOutput("ai_forecast_chart", height = "480px"))),
      fluidRow(column(12, plotlyOutput("lattice_chart", height = "480px"))),
      fluidRow(column(12, uiOutput("news_bias_panel"))),
      fluidRow(column(12, uiOutput("ai_output")))
    )
  )
)

# ----------------------------
# server
# ----------------------------

server <- function(input, output, session) {
  stock_data <- reactiveVal(NULL)
  news_data <- reactiveVal(NULL)
  ai_payload <- reactiveVal(NULL)
  last_updated <- reactiveVal("")
  show_bias <- reactiveVal(FALSE)
  rag_history <- reactiveVal(NULL)

  # init
  cached <- load_cached_data()
  if (!is.null(cached)) {
    stock_data(list(daily = cached$daily, news = cached$news))
    news_data(cached$news)
    last_updated(cached$updated_at)
  }
  rag_file <- cache_path("rag_history.csv")
  if (file.exists(rag_file)) {
    existing_rag <- tryCatch(read.csv(rag_file, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(existing_rag) && nrow(existing_rag) > 0) rag_history(existing_rag)
  }

  live_quotes <- reactiveVal(list())
  quote_time <- reactiveVal("")

  do_fetch_quotes <- function() {
    key <- Sys.getenv("FINNHUB_API_KEY")
    if (!nzchar(key)) return()
    qs <- fetch_all_quotes(key)
    if (length(qs) > 0) {
      live_quotes(qs)
      quote_time(format(Sys.time(), "%H:%M:%S"))
    }
  }

  observe({
    invalidateLater(15000)
    do_fetch_quotes()
  })

  output$live_quotes_ui <- renderUI({
    qs <- live_quotes()
    if (length(qs) == 0) return(tags$div(style = "color:#94a3b8", "Live Stock Price"))
    cards <- lapply(names(qs), function(sym) {
      q <- qs[[sym]]
      chg_col <- if (q$change >= 0) "#34d399" else "#f87171"
      chg_sign <- if (q$change >= 0) "+" else ""
      tags$div(
        style = "flex:1 1 130px; border:1px solid #334155; background:linear-gradient(150deg,rgba(30,41,59,.75),rgba(15,23,42,.85)); border-radius:12px; padding:10px 14px; text-align:center;",
        tags$div(style = "font-weight:700; color:#f8fafc; font-size:13px;", paste0(sym, " ", TICKER_LABELS[[sym]])),
        tags$div(style = "font-size:22px; font-weight:700; color:#f8fafc; margin:4px 0;", fmt_price(q$price)),
        tags$div(style = paste0("color:", chg_col, "; font-size:13px;"),
                 sprintf("%s%s (%s%s%%)", chg_sign, fmt_num(q$change), chg_sign, fmt_num(q$change_pct)))
      )
    })
    tags$div(style = "display:flex; flex-wrap:wrap; gap:10px; margin-bottom:14px;", cards)
  })

  output$quote_timestamp <- renderUI({
    qt <- quote_time()
    if (!nzchar(qt)) return(NULL)
    tags$div(style = "color:#64748b; font-size:12px; text-align:right;",
             paste("Last quote:", qt, "| Auto-refresh: ON (15s)"))
  })

  output$last_updated <- renderText({
    lu <- last_updated()
    if (nzchar(lu)) lu else "No cached data. Click Refresh Market Data."
  })

  gather_all_news <- function(news_days = 7L) {
    key <- Sys.getenv("FINNHUB_API_KEY")
    if (!nzchar(key)) return(NULL)
    out <- list()
    for (sym in TICKERS) {
      news <- fetch_finnhub_news(sym, key, days_back = as.integer(news_days))
      if (is.null(news)) next
      out <- c(out, news)
      Sys.sleep(0.15)
    }
    if (length(out) == 0) return(NULL)
    df <- dplyr::bind_rows(out)
    df <- df[!duplicated(df$Title), ]
    df$Published <- as.POSIXct(df$Published, origin = "1970-01-01", tz = "UTC")
    df <- df[order(df$Published, decreasing = TRUE), ]
    head(df, 400)
  }

  fetch_all_histories <- function() {
    key <- Sys.getenv("FINNHUB_API_KEY")
    if (!nzchar(key)) {
      showNotification("FINNHUB_API_KEY missing.", type = "error")
      return(NULL)
    }
    all_rows <- list()
    all_tickers <- c(TICKERS, MARKET_TICKER)
    withProgress(message = "Fetching stock history", value = 0, {
      for (i in seq_along(all_tickers)) {
        sym <- all_tickers[[i]]
        setProgress((i - 1) / length(all_tickers), detail = paste("Fetching", sym))
        df <- fetch_finnhub_candles(sym, key, days_back = 365)
        if (!is.null(df)) all_rows[[length(all_rows) + 1]] <- df
        if (i < length(all_tickers)) Sys.sleep(0.2)
      }
      setProgress(0.95, detail = "Finalizing")
    })
    if (length(all_rows) == 0) return(NULL)
    dplyr::bind_rows(all_rows)
  }

  observeEvent(input$refresh_data, {
    req(input$refresh_data)

    shiny::withProgress(message = "Refreshing market data", value = 0, {
      hist_df <- fetch_all_histories()
      if (is.null(hist_df)) {
        showNotification("No stock data fetched. Check FINNHUB key / rate limit.", type = "error")
        return(NULL)
      }
      setProgress(0.75, detail = "Fetching news")
      news_df <- gather_all_news(90)
      stock_data(list(daily = hist_df, news = news_df))
      news_data(news_df)
      save_cached_data(hist_df, news_df)
      setProgress(0.85, detail = "Computing RAG parameters...")
      rag_result <- update_rag_history(hist_df)
      rag_history(rag_result)
      setProgress(0.95, detail = "Exporting CSVs...")
      export_csv_files(hist_df, rag_result)
      export_news_csv(news_df)
      last_updated(safe_now())
      setProgress(1)
      showNotification("Data refreshed.", type = "message")
    })
  })

  observeEvent(input$refresh_news, {
    if (!nzchar(Sys.getenv("FINNHUB_API_KEY"))) {
      output$news_error <- renderUI(div(class = "alert alert-danger", "FINNHUB_API_KEY missing."))
      return()
    }
    days <- as.integer(input$news_window)
    news_df <- gather_all_news(days)
    if (is.null(news_df) || nrow(news_df) == 0) {
      output$news_error <- renderUI(div(class = "alert alert-warning", "No news returned. Try again later."))
      return()
    }
    news_data(news_df)
    d <- stock_data()
    if (!is.null(d) && is.data.frame(d$daily)) {
      d$news <- news_df
      stock_data(d)
    }
    if (!is.null(d) && !is.null(d$daily)) {
      save_cached_data(d$daily, news_df)
    }
    export_news_csv(news_df)
    output$news_error <- renderUI(NULL)
  }, ignoreInit = TRUE)

  # refresh news on load if empty
  observe({
    if (is.null(news_data())) {
      output$news_error <- renderUI(div(class = "alert alert-warning", "News not loaded. Click Refresh News."))
    } else {
      output$news_error <- renderUI(NULL)
    }
  })

  data_view <- reactiveVal("prices")

  output$lattice_chart <- renderPlotly({
    payload <- ai_payload()
    if (is.null(payload) || is.null(payload$lattice_path)) return(NULL)
    sym <- payload$symbol
    lpath <- payload$lattice_path
    lat <- payload$lattice

    d <- stock_data()
    hist <- NULL
    if (!is.null(d) && !is.null(d$daily)) {
      h <- d$daily[d$daily$Symbol == sym, ]
      if (nrow(h) > 0) { h <- h[order(h$Date), ]; hist <- tail(h, min(nrow(h), 90)) }
    }

    p <- plot_ly()
    if (!is.null(hist) && nrow(hist) > 1) {
      p <- p |> add_trace(data = hist, x = ~Date, y = ~Close, type = "scatter", mode = "lines",
                           name = "History", line = list(color = "rgba(148,163,184,0.75)", width = 2))
    }
    p <- p |>
      add_trace(data = lpath, x = ~Date, y = ~Lo, type = "scatter", mode = "lines",
                name = "5-95% band", line = list(color = "rgba(251,191,36,0)"), showlegend = FALSE) |>
      add_trace(data = lpath, x = ~Date, y = ~Hi, type = "scatter", mode = "lines",
                name = "5-95% band", fill = "tonexty", fillcolor = "rgba(251,191,36,0.15)",
                line = list(color = "rgba(251,191,36,0)"), hoverinfo = "skip") |>
      add_trace(data = lpath, x = ~Date, y = ~Expected, type = "scatter", mode = "lines",
                name = "Lattice E[S]", line = list(color = "#fbbf24", width = 3)) |>
      layout(
        title = list(text = sprintf("Binomial Lattice (CRR) - %s  (u=%.4f, d=%.4f, p=%.4f)",
                                    sym, lat$u, lat$d, lat$p_real),
                     font = list(color = "#e5e7eb")),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        xaxis = list(title = "", gridcolor = "#334155", color = "#e2e8f0", tickformat = "%Y-%m-%d"),
        yaxis = list(title = "Price (USD)", gridcolor = "#334155", color = "#e2e8f0", tickprefix = "$"),
        legend = list(orientation = "h", font = list(color = "#e2e8f0")),
        font = list(color = "#e2e8f0")
      )
    p
  })

  observeEvent(input$goto_analytics, {
    d <- stock_data()
    if (!is.null(d) && !is.null(d$daily) && is.null(rag_history())) {
      withProgress(message = "Computing model parameters...", value = 0.5, {
        rag_result <- update_rag_history(d$daily)
        rag_history(rag_result)
        export_csv_files(d$daily, rag_result)
      })
    }
    data_view("analytics")
  })

  observeEvent(input$back_to_prices, {
    data_view("prices")
  })

  output$data_page_content <- renderUI({
    if (data_view() == "analytics") {
      tagList(
        fluidRow(
          column(6,
            actionButton("back_to_prices", "Back to Live Prices",
                         class = "btn-primary", icon = icon("arrow-left"))
          ),
          column(6,
            downloadButton("download_rag_csv", "Download CSV",
                           class = "btn-success", style = "margin-top:8px; float:right;")
          )
        ),
        fluidRow(
          column(12,
            h3("Model Parameters", class = "section-title"),
            DTOutput("rag_table")
          )
        ),
        fluidRow(
          column(12,
            h3("Historical Prices", class = "section-title"),
            DTOutput("analytics_price_table")
          )
        )
      )
    } else {
      tagList(
        fluidRow(
          column(12,
            h3("Live Stock Price", class = "section-title"),
            uiOutput("live_quotes_ui"),
            uiOutput("quote_timestamp")
          )
        ),
        fluidRow(
          column(3,
            selectInput("data_ticker", "Select Stock",
                        choices = TICKERS, selected = "AAPL", width = "100%")
          ),
          column(3,
            radioButtons("data_window", "Time Range",
                         choices = c("7D", "1M", "1Y"), selected = "1M", inline = TRUE)
          ),
          column(3, br(),
            actionButton("refresh_data", "Refresh Market Data", class = "btn-primary")
          ),
          column(3, br(),
            tags$div(style = "padding-top: 4px; color: #cbd5e1;",
                     strong("Last update: "), textOutput("last_updated", inline = TRUE))
          )
        ),
        fluidRow(column(12, h3("Price Trend", class = "section-title"))),
        fluidRow(column(12, plotlyOutput("price_chart", height = "460px"))),
        fluidRow(
          column(12, br(),
            actionButton("goto_analytics", "View Analytics & Download CSV",
                         class = "btn-primary", icon = icon("table")),
            helpText("Model parameters (daily snapshots), RAG-ready CSV, and historical prices",
                     style = "color:#94a3b8; margin-top:6px;")
          )
        )
      )
    }
  })

  selected_history <- reactive({
    d <- stock_data()
    if (is.null(d) || is.null(d$daily)) return(NULL)
    sub <- d$daily[d$daily$Symbol == input$data_ticker, ]
    if (nrow(sub) == 0) return(NULL)
    sub <- sub[order(sub$Date), ]
    n <- WINDOW_MAP[[input$data_window]]
    if (is.null(n)) n <- 22L
    tail(sub, min(n, nrow(sub)))
  })

  output$price_chart <- renderPlotly({
    sub <- selected_history()
    if (is.null(sub) || nrow(sub) < 2) return(NULL)

    x <- sub$Date
    y <- sub$Close
    first <- head(y, 1)
    last <- tail(y, 1)

    p <- plot_ly(sub, x = x, y = y, type = "scatter", mode = "lines", name = "Close", line = list(color = "#60a5fa", width = 3)) |>
      add_trace(x = x, y = y, type = "scatter", mode = "markers", marker = list(color = "#93c5fd", size = 4), showlegend = FALSE, name = "Points") |>
      add_trace(x = x[1], y = first, type = "scatter", mode = "markers", marker = list(color = "#34d399", size = 8), name = "Start") |>
      add_trace(x = x[length(x)], y = last, type = "scatter", mode = "markers", marker = list(color = "#f97316", size = 8), name = "End") |>
      layout(
        title = list(
          text = paste("Price Trend -", TICKER_LABELS[[input$data_ticker]], "(", input$data_window, ")", sep = " "),
          font = list(color = "#e5e7eb")
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        xaxis = list(title = "", gridcolor = "#334155", color = "#e2e8f0", tickformat = "%Y-%m-%d"),
        yaxis = list(title = "Price (USD)", gridcolor = "#334155", color = "#e2e8f0", tickprefix = "$"),
        font = list(color = "#e2e8f0"),
        legend = list(orientation = "h", font = list(color = "#e2e8f0"))
      )

    p
  })

  output$recent_price_table <- renderDT({
    d <- stock_data()
    if (is.null(d) || is.null(d$daily)) return(NULL)
    sub <- d$daily[d$daily$Symbol == input$data_ticker, ]
    if (nrow(sub) == 0) return(NULL)

    sub <- sub[order(sub$Date, decreasing = TRUE), ]
    sub <- head(sub, 20)

    datatable(
      sub[, c("Date", "Open", "High", "Low", "Close", "Volume")],
      rownames = FALSE,
      options = list(dom = "t", pageLength = 20, ordering = TRUE)
    )
  })

  output$news_table <- renderDT({
    news_df <- news_data()
    if (is.null(news_df) || nrow(news_df) == 0) return(NULL)

    selected <- input$news_ticker
    if (!is.null(selected) && selected != "ALL") {
      news_df <- news_df[news_df$Symbol == selected, ]
    }

    if (nrow(news_df) == 0) return(NULL)

    shown <- news_df[order(news_df$Published, decreasing = TRUE), ]
    ts <- shown$Published
    local_ts <- format(ts, "%Y-%m-%d %H:%M", tz = "America/New_York")
    local_hm <- format(ts, "%H:%M", tz = "America/New_York")
    shown$Published <- ifelse(local_hm == "00:00", format(ts, "%Y-%m-%d", tz = "America/New_York"), paste0(local_ts, " ET"))
    # Some Finnhub plans return a non-public URL; fall back to a News search link.
    url <- as.character(shown$Url)
    needs_search <- grepl("^https?://finnhub\\.io/api/news\\?id=", url)
    q <- utils::URLencode(paste0(shown$Title, " ", shown$Source), reserved = TRUE)
    url[needs_search] <- paste0("https://news.google.com/search?q=", q[needs_search])
    shown$Link <- sprintf('<a href="%s" target="_blank" rel="noopener noreferrer" style="color:#38bdf8">Open</a>', url)

    datatable(
      shown[, c("Symbol", "Published", "Title", "Source", "Link")],
      rownames = FALSE,
      escape = FALSE,
      options = list(dom = "tip", pageLength = 20, autoWidth = TRUE, order = list(list(1, 'desc'))),
      selection = "none"
    ) |>
      formatStyle("Symbol", `font-weight` = "bold", color = "#e2e8f0")
  })

  observeEvent(input$toggle_bias, {
    show_bias(!isTRUE(show_bias()))
  }, ignoreInit = TRUE)

  observeEvent(input$run_prediction, {
    d <- stock_data()
    req(d)

    if (is.null(d$daily) || nrow(d$daily) < 5) {
      output$ai_error <- renderUI(div(class = "alert alert-danger", "No market data. Refresh Data first."))
      return()
    }

    sym <- input$ai_ticker
    hist <- d$daily[d$daily$Symbol == sym, ]
    if (nrow(hist) < 5) {
      output$ai_error <- renderUI(div(class = "alert alert-danger", "Not enough history for selected ticker."))
      return()
    }

    n_window <- as.integer(input$forecast_window)
    hist <- hist[order(hist$Date), ]
    trend <- compute_ohlc_metrics(hist, lookback = max(n_window, nrow(hist)))

    all_news <- if (is.null(d$news)) NULL else {
      nd <- d$news[d$news$Symbol == sym, ]
      if (nrow(nd) == 0) NULL else head(nd, 20)
    }

    news_text <- build_news_snapshot(sym, all_news, max_rows = 8)
    tone_score <- if (is.null(all_news)) 0 else heuristic_news_tone(all_news)
    prompt <- build_forecast_prompt(sym, n_window, trend, news_text, tone_score)
    bias <- compute_news_bias(all_news, max_items = 20)

    gbm <- estimate_gbm_params(hist, max_lookback = 252)
    gbm_base <- NULL
    gbm_adj <- NULL
    lattice_T <- n_window / 252
    lattice <- estimate_lattice_params(hist, T_years = lattice_T, N_steps = n_window)
    lattice_path <- NULL
    if (!is.null(lattice)) {
      lattice_path <- lattice_forecast_path(lattice$s0, lattice$u, lattice$d,
                                            lattice$p_real, lattice$N_steps, lattice$last_date)
    }
    if (!is.null(gbm) && isTRUE(bias$ok)) {
      mu0 <- gbm$mu_annual
      sig0 <- gbm$sigma_annual
      mu1 <- mu0 + bias$mu_shift_annual
      sig1 <- sig0 * bias$sigma_mult

      gbm_base <- list(mu_annual = mu0, sigma_annual = sig0)
      gbm_adj <- list(mu_annual = mu1, sigma_annual = sig1)
      gbm$path_base <- gbm_forecast_path(gbm$s0, mu0, sig0, n_window, gbm$last_date)
      gbm$path_adj <- gbm_forecast_path(gbm$s0, mu1, sig1, n_window, gbm$last_date)
    }

    output$ai_error <- renderUI(NULL)
    ai_payload(NULL)

    withProgress(message = "Generating AI forecast", value = 0, {
      setProgress(0.2, detail = "Summarizing market context")
      setProgress(0.6, detail = "Calling AI model")
      forecast <- NULL
      if (nzchar(Sys.getenv("OPENAI_API_KEY"))) {
        forecast <- fetch_openai_forecast(prompt, Sys.getenv("OPENAI_API_KEY"))
        if (!forecast$ok && nzchar(Sys.getenv("OLLAMA_API_KEY"))) {
          forecast <- fetch_ollama_forecast(prompt, Sys.getenv("OLLAMA_API_KEY"))
        }
      } else if (nzchar(Sys.getenv("OLLAMA_API_KEY"))) {
        forecast <- fetch_ollama_forecast(prompt, Sys.getenv("OLLAMA_API_KEY"))
      } else {
        forecast <- list(ok = FALSE, error = "No OPENAI_API_KEY and no OLLAMA_API_KEY configured.")
      }
      if (!forecast$ok) {
        output$ai_error <- renderUI(div(class = "alert alert-danger", forecast$error))
        ai_payload(NULL)
        return()
      }

      ai_payload(list(
        symbol = sym,
        window = n_window,
        payload = forecast$payload,
        raw = forecast$raw,
        trend = trend,
        news_snippet = news_text,
        tone = tone_score,
        news_bias = bias,
        gbm = gbm,
        gbm_base = gbm_base,
        gbm_adj = gbm_adj,
        lattice = lattice,
        lattice_path = lattice_path
      ))
      setProgress(1)
    })
  }, ignoreInit = TRUE)

  output$ai_forecast_chart <- renderPlotly({
    payload <- ai_payload()
    if (is.null(payload) || is.null(payload$gbm) || is.null(payload$gbm$path_adj)) return(NULL)

    sym <- payload$symbol
    gbm <- payload$gbm
    path0 <- gbm$path_base
    path1 <- gbm$path_adj

    # Show a short recent history context on the left.
    d <- stock_data()
    hist <- NULL
    if (!is.null(d) && !is.null(d$daily)) {
      h <- d$daily[d$daily$Symbol == sym, ]
      if (nrow(h) > 0) {
        h <- h[order(h$Date), ]
        hist <- tail(h, min(nrow(h), 90))
      }
    }

    p <- plot_ly()
    if (!is.null(hist) && nrow(hist) > 1) {
      p <- p |>
        add_trace(
          data = hist,
          x = ~Date, y = ~Close,
          type = "scatter", mode = "lines",
          name = "History",
          line = list(color = "rgba(148,163,184,0.75)", width = 2)
        )
    }

    # Ribbon (Lo-Hi) for adjusted forecast.
    p <- p |>
      add_trace(
        data = path1,
        x = ~Date, y = ~Lo,
        type = "scatter", mode = "lines",
        name = "Adj band (5-95%)",
        line = list(color = "rgba(96,165,250,0)"),
        showlegend = FALSE
      ) |>
      add_trace(
        data = path1,
        x = ~Date, y = ~Hi,
        type = "scatter", mode = "lines",
        name = "Adj band (5-95%)",
        fill = "tonexty",
        fillcolor = "rgba(96,165,250,0.18)",
        line = list(color = "rgba(96,165,250,0)"),
        hoverinfo = "skip"
      )

    # Base expected (dashed) vs adjusted expected (solid).
    if (!is.null(path0)) {
      p <- p |>
        add_trace(
          data = path0,
          x = ~Date, y = ~Expected,
          type = "scatter", mode = "lines",
          name = "GBM base (E[S])",
          line = list(color = "rgba(203,213,225,0.7)", width = 2, dash = "dash")
        )
    }
    p <- p |>
      add_trace(
        data = path1,
        x = ~Date, y = ~Expected,
        type = "scatter", mode = "lines",
        name = "GBM adj (E[S])",
        line = list(color = "#60a5fa", width = 3)
      ) |>
      layout(
        title = list(text = paste0("GBM (Black-Scholes) Forecast - ", sym), font = list(color = "#e5e7eb")),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        xaxis = list(title = "", gridcolor = "#334155", color = "#e2e8f0", tickformat = "%Y-%m-%d"),
        yaxis = list(title = "Price (USD)", gridcolor = "#334155", color = "#e2e8f0", tickprefix = "$"),
        legend = list(orientation = "h", font = list(color = "#e2e8f0")),
        font = list(color = "#e2e8f0")
      )

    p
  })

  output$news_bias_panel <- renderUI({
    payload <- ai_payload()
    if (is.null(payload) || !isTRUE(show_bias())) return(NULL)
    bias <- payload$news_bias
    gbm <- payload$gbm
    if (is.null(bias) || !isTRUE(bias$ok) || is.null(gbm)) {
      return(tags$div(class = "panel-card", "No news bias details available."))
    }

    mu0 <- payload$gbm_base$mu_annual %||% NA_real_
    sig0 <- payload$gbm_base$sigma_annual %||% NA_real_
    mu1 <- payload$gbm_adj$mu_annual %||% NA_real_
    sig1 <- payload$gbm_adj$sigma_annual %||% NA_real_

    tags$div(
      class = "panel-card",
      tags$h4("News Bias & Adjustment (Heuristic, Grounded)", class = "section-title"),
      tags$p(style = "color:#94a3b8; margin-top:-6px;",
             "Method: keyword polarity + recency weighting; converts bias to a capped drift shift and mild vol multiplier."),
      tags$p(sprintf("Bias score: %s | Drift shift (annual): %s | Vol multiplier: %s",
                     bias$bias_score, bias$mu_shift_annual, bias$sigma_mult)),
      tags$p(sprintf("LLM news impact (from AI response): %s", payload$payload$news_adjustment %||% "NEUTRAL")),
      tags$p(sprintf("GBM base params (annual): mu=%.3f, sigma=%.3f | adjusted: mu=%.3f, sigma=%.3f",
                     mu0, sig0, mu1, sig1)),
      if (is.null(bias$details) || nrow(bias$details) == 0) {
        tags$p(style = "color:#94a3b8;", "No news items loaded for this ticker (bias defaults to neutral).")
      } else {
        tagList(
          tags$h5("Top contributing news (most impactful items)", style = "color:#cbd5e1;"),
          DT::DTOutput("bias_table")
        )
      }
    )
  })

  output$bias_table <- renderDT({
    payload <- ai_payload()
    if (is.null(payload) || !isTRUE(show_bias())) return(NULL)
    bias <- payload$news_bias
    if (is.null(bias) || is.null(bias$details) || nrow(bias$details) == 0) return(NULL)
    dt <- bias$details
    dt <- dt[order(abs(dt$Contribution), decreasing = TRUE), ]
    dt <- head(dt, 12)
    datatable(
      dt[, c("Published", "Source", "Category", "Polarity", "Weight", "Contribution", "Title")],
      rownames = FALSE,
      options = list(dom = "t", pageLength = 12, ordering = FALSE, autoWidth = TRUE)
    )
  })

  output$ai_output <- renderUI({
    payload <- ai_payload()
    if (is.null(payload)) return(NULL)

    p <- payload$payload
    direction <- toupper(as.character(p$trend %||% "FLAT"))
    conf <- as.numeric(p$confidence %||% 50)
    conf <- ifelse(is.na(conf), 50, conf)

    chip_cls <- if (direction == "UP") {
      "chip chip-up"
    } else if (direction == "DOWN") {
      "chip chip-down"
    } else {
      "chip chip-flat"
    }

    tags$div(
      class = "forecast-box",
      tags$div(class = "forecast-title", sprintf("%s AI Forecast (%s)", payload$symbol, payload$window), style = "color:#e2e8f0;"),
      tags$div(class = chip_cls, paste("Trend:", direction)),
      tags$div(class = chip_cls, paste("Confidence:", sprintf("%s%%", conf))),
      tags$div(class = chip_cls, paste("News impact:", p$news_adjustment %||% "NEUTRAL")),
      tags$hr(style = "border-color:#334155;"),
      tags$h4("Key reason (AI)", style = "color:#cbd5e1;"),
      tags$ul(
        lapply(p$reasoning %||% c("No reasoning returned"), function(x) {
          tags$li(x)
        })
      ),
      tags$h4("Output summary", style = "color:#cbd5e1;"),
      tags$p(tags$strong("Expected price level: "), p$expected_price_level %||% "-"),
      tags$p(tags$strong("Risk note: "), p$risk_note %||% "-"),
      tags$h4("Quant forecast (GBM / Black-Scholes dynamics)", style = "color:#cbd5e1;"),
      {
        gbm <- payload$gbm
        if (is.null(gbm) || is.null(gbm$path_adj)) {
          tags$p(style = "color:#94a3b8;", "Not enough price history to estimate GBM parameters (need sufficient daily closes).")
        } else {
          end_row <- tail(gbm$path_adj, 1)
          tags$p(sprintf("Horizon %s trading days: E[S]=%s, 5-95%% range [%s, %s].",
                         payload$window, fmt_price(end_row$Expected), fmt_price(end_row$Lo), fmt_price(end_row$Hi)))
        }
      },
      tags$h4("Trend support", style = "color:#cbd5e1;"),
      tags$p(sprintf("Latest close: %s | 1d %s | 7d %s | 30d %s | Annualized vol(20d): %s", fmt_price(payload$trend$latest), fmt_pct(payload$trend$change1d_pct), fmt_pct(payload$trend$change7d_pct), fmt_pct(payload$trend$change30d_pct), ifelse(is.na(payload$trend$mean_20d_volatility), "N/A", paste0(payload$trend$mean_20d_volatility, "%")))),
      tags$p(tags$strong("News sample:"), tags$pre(style = "white-space: pre-wrap; color:#94a3b8;", payload$news_snippet)),
      tags$h5("Prompt raw output (for traceability)", style = "color:#94a3b8;"),
      tags$pre(style = "white-space: pre-wrap; background:#0f172a; border:1px solid #334155; padding:10px; border-radius:8px; color:#cbd5e1;", payload$raw)
    )
  })

  output$rag_table <- renderDT({
    rag <- rag_history()
    if (is.null(rag) || nrow(rag) == 0) return(NULL)
    display <- rag[order(rag$snapshot_date, rag$ticker, decreasing = TRUE), ]
    datatable(display, rownames = FALSE,
              options = list(dom = "tip", pageLength = 20, scrollX = TRUE, autoWidth = FALSE,
                             columnDefs = list(list(className = "dt-center dt-nowrap", targets = "_all"))),
              selection = "none")
  })

  output$analytics_price_table <- renderDT({
    d <- stock_data()
    if (is.null(d) || is.null(d$daily)) return(NULL)
    display <- d$daily[order(d$daily$Date, decreasing = TRUE), ]
    datatable(display, rownames = FALSE,
              options = list(dom = "tip", pageLength = 20, scrollX = TRUE, autoWidth = FALSE,
                             columnDefs = list(list(className = "dt-center dt-nowrap", targets = "_all"))),
              selection = "none")
  })

  output$download_rag_csv <- downloadHandler(
    filename = function() paste0("rag_data_", Sys.Date(), ".csv"),
    content = function(file) {
      rag <- rag_history()
      if (!is.null(rag) && nrow(rag) > 0) {
        write.csv(rag, file, row.names = FALSE)
      }
    }
  )
}

shinyApp(ui = ui, server = server)
