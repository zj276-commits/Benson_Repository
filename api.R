# api.R
# API interaction functions: Finnhub, Yahoo Finance, OpenAI, Ollama

# ----------------------------
# Yahoo Finance
# ----------------------------

to_iso_date <- function(days_back) {
  if (days_back <= 5L) "5d"
  else if (days_back <= 31L) "1mo"
  else if (days_back <= 95L) "3mo"
  else if (days_back <= 190L) "6mo"
  else if (days_back <= 400L) "1y"
  else "2y"
}

fetch_yahoo_candles <- function(symbol, days_back = 365) {
  range <- to_iso_date(days_back)
  symbol <- trimws(symbol)
  if (!nzchar(symbol)) return(NULL)

  resp <- tryCatch({
    request("https://query1.finance.yahoo.com/v8/finance/chart/") |>
      req_url_path_append(symbol) |>
      req_url_query(range = range, interval = "1d", events = "history", includeAdjustedClose = "true") |>
      req_headers(`User-Agent` = "Mozilla/5.0") |>
      req_timeout(20) |> req_retry(max_tries = 2) |>
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
  if (is.null(result)) return(NULL)

  timestamps <- as.numeric(unlist(result$timestamp))
  quotes <- result$indicators$quote[[1]]
  if (is.null(timestamps) || is.null(quotes)) return(NULL)

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
    Open = open[valid], High = high[valid], Low = low[valid],
    Close = close[valid], Volume = volume[valid],
    stringsAsFactors = FALSE
  )
  out <- out[order(out$Date), ]
  if (nrow(out) == 0) return(NULL)
  tail(out, min(nrow(out), as.integer(days_back)))
}

fetch_yahoo_fundamentals <- function(symbol) {
  qf <- tryCatch({
    quantmod::getQuote(symbol, what = quantmod::yahooQF(c(
      "Market Capitalization", "Book Value", "Earnings/Share",
      "P/E Ratio", "Price/Book", "Dividend Yield",
      "52-week High", "52-week Low", "Average Daily Volume",
      "Price/EPS Estimate Next Year", "Shares Outstanding"
    )))
  }, error = function(e) { message("quantmod getQuote failed for ", symbol, ": ", e$message); NULL })

  if (is.null(qf) || nrow(qf) == 0) return(NULL)

  grab <- function(col) {
    if (col %in% names(qf)) {
      v <- as.numeric(qf[[col]][1])
      if (is.na(v)) NA_real_ else v
    } else NA_real_
  }

  mc  <- grab("Market Capitalization")
  so  <- grab("Shares Outstanding")
  bv  <- grab("Book Value")
  eps <- grab("Earnings/Share")
  tpe <- grab("P/E Ratio")
  ptb <- grab("Price/Book")
  dy  <- grab("Dividend Yield")
  h52 <- grab("52-week High")
  l52 <- grab("52-week Low")
  fpe <- grab("Price/EPS Estimate Next Year")
  avgv <- grab("Ave. Daily Volume")

  cp <- if (!is.na(mc) && !is.na(so) && so > 0) mc / so else NA_real_

  list(
    market_cap          = mc,
    forward_pe          = fpe,
    trailing_pe         = tpe,
    price_to_book       = ptb,
    roe                 = if (!is.na(eps) && !is.na(bv) && bv > 0) eps / bv else NA_real_,
    roa                 = NA_real_,
    debt_to_equity      = NA_real_,
    free_cashflow       = NA_real_,
    target_mean_price   = NA_real_,
    float_shares        = NA_real_,
    shares_outstanding  = so,
    current_price       = cp,
    fifty_two_week_high = h52,
    fifty_two_week_low  = l52,
    beta                = NA_real_,
    avg_volume          = avgv,
    book_value          = bv,
    trailing_eps        = eps,
    dividend_yield      = dy,
    payout_ratio        = NA_real_,
    total_revenue       = NA_real_,
    current_ratio       = NA_real_
  )
}

fetch_yahoo_extra <- function(symbol, base_data) {
  ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"
  sc <- tryCatch({
    r <- request("https://fc.yahoo.com/") |>
      req_headers(`User-Agent` = ua) |>
      req_error(is_error = function(r) FALSE) |> req_timeout(10) |> req_perform()
    resp_header(r, "Set-Cookie")
  }, error = function(e) NULL)
  if (is.null(sc) || !nzchar(sc)) return(base_data)

  cr <- tryCatch({
    r <- request("https://query2.finance.yahoo.com/v1/test/getcrumb") |>
      req_headers(`User-Agent` = ua, Cookie = sc) |>
      req_error(is_error = function(r) FALSE) |> req_timeout(10) |> req_perform()
    if (resp_status(r) == 200) resp_body_string(r) else NULL
  }, error = function(e) NULL)
  if (is.null(cr) || !nzchar(cr)) return(base_data)

  resp <- tryCatch({
    request(paste0("https://query2.finance.yahoo.com/v10/finance/quoteSummary/", symbol)) |>
      req_url_query(modules = "defaultKeyStatistics,financialData", crumb = cr) |>
      req_headers(`User-Agent` = ua, Cookie = sc) |>
      req_timeout(15) |> req_error(is_error = function(r) FALSE) |> req_perform()
  }, error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) != 200) return(base_data)

  body <- tryCatch(resp_body_json(resp), error = function(e) NULL)
  result <- body$quoteSummary$result[[1]]
  if (is.null(result)) return(base_data)

  ks <- result$defaultKeyStatistics %||% list()
  fd <- result$financialData %||% list()
  xr <- function(obj, field) { v <- obj[[field]]; if (is.list(v)) v$raw %||% NA_real_ else NA_real_ }

  fill <- function(field, val) { if (is.na(base_data[[field]])) base_data[[field]] <<- val }
  fill("beta",             xr(ks, "beta"))
  fill("float_shares",     xr(ks, "floatShares"))
  fill("payout_ratio",     xr(ks, "payoutRatio"))
  fill("roe",              xr(fd, "returnOnEquity"))
  fill("roa",              xr(fd, "returnOnAssets"))
  fill("debt_to_equity",   xr(fd, "debtToEquity"))
  fill("free_cashflow",    xr(fd, "freeCashflow"))
  fill("target_mean_price", xr(fd, "targetMeanPrice"))
  fill("total_revenue",    xr(fd, "totalRevenue"))
  fill("current_ratio",    xr(fd, "currentRatio"))
  fill("current_price",    xr(fd, "currentPrice"))
  base_data
}

# ----------------------------
# Finnhub
# ----------------------------

fetch_finnhub_candles <- function(symbol, api_key, days_back = 365) {
  to_ts <- as.integer(Sys.time())
  from_ts <- to_ts - as.integer(days_back) * 86400L

  resp <- tryCatch({
    request("https://finnhub.io/api/v1/stock/candle") |>
      req_url_query(symbol = symbol, resolution = "D", from = from_ts, to = to_ts, token = api_key) |>
      req_timeout(20) |> req_retry(max_tries = 2) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
  }, error = function(e) {
    message("Finnhub candle failed for ", symbol, ": ", conditionMessage(e))
    NULL
  })

  if (is.null(resp)) return(fetch_yahoo_candles(symbol, days_back))
  if (resp_status(resp) != 200) {
    status <- resp_status(resp)
    if (status %in% c(401L, 403L, 404L, 429L)) {
      message("Finnhub candle HTTP ", status, " for ", symbol, "; using Yahoo fallback.")
      return(fetch_yahoo_candles(symbol, days_back))
    }
    return(NULL)
  }

  data <- resp_body_json(resp)
  if (is.null(data$s) || data$s != "ok") return(fetch_yahoo_candles(symbol, days_back))

  n <- length(data$t)
  if (n == 0) return(fetch_yahoo_candles(symbol, days_back))

  out <- data.frame(
    Symbol = rep(symbol, n),
    Date = as.Date(as.POSIXct(unlist(data$t), origin = "1970-01-01", tz = "UTC")),
    Open = as.numeric(unlist(data$o)), High = as.numeric(unlist(data$h)),
    Low = as.numeric(unlist(data$l)), Close = as.numeric(unlist(data$c)),
    Volume = as.numeric(unlist(data$v)),
    stringsAsFactors = FALSE
  )
  out[order(out$Date), ]
}

fetch_finnhub_news <- function(symbol, api_key, days_back = 7) {
  to_date <- format(Sys.Date(), "%Y-%m-%d")
  from_date <- format(Sys.Date() - as.integer(days_back), "%Y-%m-%d")

  resp <- tryCatch({
    request("https://finnhub.io/api/v1/company-news") |>
      req_url_query(symbol = symbol, from = from_date, to = to_date, token = api_key) |>
      req_timeout(20) |> req_perform()
  }, error = function(e) NULL)

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

# ----------------------------
# LLM (OpenAI / Ollama)
# ----------------------------

fetch_openai_forecast <- function(prompt_text, api_key) {
  if (!nzchar(api_key)) return(list(ok = FALSE, error = "OPENAI_API_KEY not set."))

  body <- list(
    model = "gpt-4o-mini", temperature = 0.2,
    messages = list(
      list(role = "system", content = "You are a concise, finance-focused stock forecaster."),
      list(role = "user", content = prompt_text)
    ),
    response_format = list(type = "json_object"),
    max_tokens = 700
  )

  resp <- tryCatch({
    request("https://api.openai.com/v1/chat/completions") |>
      req_headers(Authorization = paste0("Bearer ", api_key), "Content-Type" = "application/json") |>
      req_body_json(body) |> req_perform()
  }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))

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
          ), temperature = 0.2, max_tokens = 700
        )) |> req_perform()
    }, error = function(e) NULL)
  } else {
    tryCatch({
      request("https://ollama.com/v1/chat/completions") |>
        req_headers(Authorization = paste0("Bearer ", api_key), `Content-Type` = "application/json") |>
        req_body_json(list(
          model = "gpt-oss:20b-cloud",
          messages = list(
            list(role = "system", content = "You are a concise, finance-focused stock forecaster."),
            list(role = "user", content = prompt_text)
          ), temperature = 0.2, max_tokens = 700
        )) |> req_perform()
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

call_openai_text <- function(system_prompt, user_prompt, api_key, max_tokens = 1500) {
  if (!nzchar(api_key)) return(NULL)
  body <- list(
    model = "gpt-4o-mini", temperature = 0.3,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt)
    ),
    response_format = list(type = "json_object"),
    max_tokens = max_tokens
  )
  resp <- tryCatch({
    request("https://api.openai.com/v1/chat/completions") |>
      req_headers(Authorization = paste0("Bearer ", api_key), "Content-Type" = "application/json") |>
      req_body_json(body) |> req_timeout(60) |> req_perform()
  }, error = function(e) NULL)

  if (!inherits(resp, "httr2_response") || resp_status(resp) != 200) return(NULL)
  raw <- resp_body_json(resp)
  text <- raw$choices[[1]]$message$content
  if (is.null(text) || !nzchar(text)) return(NULL)
  text <- gsub("^```json|```$", "", trimws(text))
  tryCatch(jsonlite::fromJSON(text), error = function(e) NULL)
}
