# api.R
# API interaction functions: Finnhub, Yahoo Finance, OpenAI

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

api_request_cache_key <- function(...) {
  parts <- list(...)
  paste(vapply(parts, function(x) paste(as.character(x), collapse = "-"), character(1)), collapse = "__")
}

fetch_yahoo_candles <- function(symbol, days_back = 365) {
  range <- to_iso_date(days_back)
  symbol <- trimws(symbol)
  if (!nzchar(symbol)) return(NULL)

  with_disk_cache(
    namespace = "candles",
    key = api_request_cache_key("yahoo", symbol, days_back, range),
    max_age_secs = CANDLE_CACHE_TTL_SECS,
    validate = function(x) is.data.frame(x) && nrow(x) > 0,
    loader = function() {
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
      out <- tail(out, min(nrow(out), as.integer(days_back)))
      attr(out, "data_source") <- "yahoo"
      out
    }
  )
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
# Tiingo
# ----------------------------

fetch_tiingo_candles <- function(symbol,
                                 token = Sys.getenv("TIINGO_API_TOKEN"),
                                 days_back = 365) {
  symbol <- trimws(symbol)
  if (!nzchar(symbol) || !nzchar(token)) return(NULL)

  start_date <- format(Sys.Date() - as.integer(days_back), "%Y-%m-%d")
  end_date <- format(Sys.Date(), "%Y-%m-%d")

  with_disk_cache(
    namespace = "candles",
    key = api_request_cache_key("tiingo", symbol, days_back, start_date, end_date),
    max_age_secs = CANDLE_CACHE_TTL_SECS,
    validate = function(x) is.data.frame(x) && nrow(x) > 0,
    loader = function() {
      resp <- tryCatch({
        request(paste0("https://api.tiingo.com/tiingo/daily/", symbol, "/prices")) |>
          req_url_query(startDate = start_date, endDate = end_date, token = token) |>
          req_timeout(20) |>
          req_retry(max_tries = 2) |>
          req_error(is_error = function(resp) FALSE) |>
          req_perform()
      }, error = function(e) {
        message("Tiingo candle failed for ", symbol, ": ", conditionMessage(e))
        NULL
      })

      if (is.null(resp) || resp_status(resp) != 200) return(NULL)
      body <- tryCatch(resp_body_json(resp), error = function(e) NULL)
      if (!is.list(body) || length(body) == 0) return(NULL)

      out <- dplyr::bind_rows(lapply(body, function(row) {
        data.frame(
          Symbol = symbol,
          Date = as.Date(substr(row$date %||% "", 1, 10)),
          Open = as.numeric(row$open %||% NA_real_),
          High = as.numeric(row$high %||% NA_real_),
          Low = as.numeric(row$low %||% NA_real_),
          Close = as.numeric(row$close %||% NA_real_),
          Volume = as.numeric(row$volume %||% NA_real_),
          stringsAsFactors = FALSE
        )
      }))
      out <- out[is.finite(out$Close), , drop = FALSE]
      if (nrow(out) == 0) return(NULL)
      out <- out[order(out$Date), , drop = FALSE]
      attr(out, "data_source") <- "tiingo"
      out
    }
  )
}

# ----------------------------
# Finnhub
# ----------------------------

fetch_finnhub_candles <- function(symbol, api_key, days_back = 365) {
  with_source <- function(df, source) {
    if (is.null(df)) return(NULL)
    attr(df, "data_source") <- source
    df
  }

  symbol <- trimws(symbol)
  if (!nzchar(symbol)) return(NULL)
  preferred_source <- preferred_price_source()

  fallback_candles <- function() {
    if (preferred_source != "tiingo") {
      tiingo_df <- fetch_tiingo_candles(symbol, days_back = days_back)
      if (!is.null(tiingo_df) && nrow(tiingo_df) > 0) return(with_source(tiingo_df, "tiingo_fallback"))
    }
    with_source(fetch_yahoo_candles(symbol, days_back), "yahoo_fallback")
  }

  if (preferred_source == "tiingo") {
    tiingo_df <- fetch_tiingo_candles(symbol, days_back = days_back)
    if (!is.null(tiingo_df) && nrow(tiingo_df) > 0) return(with_source(tiingo_df, "tiingo_primary"))
  }

  if (preferred_source == "yahoo") {
    yahoo_df <- fetch_yahoo_candles(symbol, days_back)
    if (!is.null(yahoo_df) && nrow(yahoo_df) > 0) return(with_source(yahoo_df, "yahoo_primary"))
  }

  if (!nzchar(api_key)) return(fallback_candles())

  to_ts <- as.integer(Sys.time())
  from_ts <- to_ts - as.integer(days_back) * 86400L

  with_disk_cache(
    namespace = "candles",
    key = api_request_cache_key("finnhub", symbol, days_back, Sys.Date()),
    max_age_secs = CANDLE_CACHE_TTL_SECS,
    validate = function(x) is.data.frame(x) && nrow(x) > 0,
    loader = function() {
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

      if (is.null(resp)) return(fallback_candles())
      if (resp_status(resp) != 200) {
        status <- resp_status(resp)
        if (status %in% c(401L, 403L, 404L, 429L)) {
          message("Finnhub candle HTTP ", status, " for ", symbol, "; using fallback source.")
          return(fallback_candles())
        }
        return(NULL)
      }

      data <- resp_body_json(resp)
      if (is.null(data$s) || data$s != "ok") return(fallback_candles())

      n <- length(data$t)
      if (n == 0) return(fallback_candles())

      out <- data.frame(
        Symbol = rep(symbol, n),
        Date = as.Date(as.POSIXct(unlist(data$t), origin = "1970-01-01", tz = "UTC")),
        Open = as.numeric(unlist(data$o)), High = as.numeric(unlist(data$h)),
        Low = as.numeric(unlist(data$l)), Close = as.numeric(unlist(data$c)),
        Volume = as.numeric(unlist(data$v)),
        stringsAsFactors = FALSE
      )
      out <- out[order(out$Date), ]
      attr(out, "data_source") <- "finnhub"
      out
    }
  )
}

fetch_finnhub_news <- function(symbol, api_key, days_back = 7) {
  symbol <- trimws(symbol)
  if (!nzchar(symbol) || !nzchar(api_key)) return(NULL)
  to_date <- format(Sys.Date(), "%Y-%m-%d")
  from_date <- format(Sys.Date() - as.integer(days_back), "%Y-%m-%d")

  with_disk_cache(
    namespace = "news",
    key = api_request_cache_key("finnhub_company", symbol, days_back, from_date, to_date),
    max_age_secs = NEWS_CACHE_TTL_SECS,
    validate = function(x) is.list(x) && length(x) > 0,
    loader = function() {
      resp <- tryCatch({
        request("https://finnhub.io/api/v1/company-news") |>
          req_url_query(symbol = symbol, from = from_date, to = to_date, token = api_key) |>
          req_timeout(20) |> req_retry(max_tries = 2) |>
          req_error(is_error = function(resp) FALSE) |>
          req_perform()
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
  )
}

fetch_finnhub_general_news <- function(api_key, days_back = 7) {
  if (!nzchar(api_key)) return(NULL)
  with_disk_cache(
    namespace = "news",
    key = api_request_cache_key("finnhub_macro", days_back, Sys.Date()),
    max_age_secs = NEWS_CACHE_TTL_SECS,
    validate = function(x) is.list(x) && length(x) > 0,
    loader = function() {
      resp <- tryCatch({
        request("https://finnhub.io/api/v1/news") |>
          req_url_query(category = "general", token = api_key) |>
          req_timeout(20) |> req_retry(max_tries = 2) |>
          req_error(is_error = function(resp) FALSE) |>
          req_perform()
      }, error = function(e) NULL)

      if (is.null(resp) || resp_status(resp) != 200) return(NULL)
      arts <- resp_body_json(resp)
      if (is.null(arts) || length(arts) == 0) return(NULL)

      cutoff <- as.numeric(Sys.time()) - as.integer(days_back) * 86400L
      arts <- Filter(function(a) (a[["datetime"]][1] %||% 0) >= cutoff, arts)
      if (length(arts) == 0) return(NULL)

      lapply(arts, function(a) {
        data.frame(
          Symbol = "MACRO",
          Title = a[["headline"]][1] %||% "",
          Source = a[["source"]][1] %||% "",
          Published = a[["datetime"]][1] %||% NA,
          Summary = a[["summary"]][1] %||% "",
          Url = a[["url"]][1] %||% "",
          stringsAsFactors = FALSE
        )
      })
    }
  )
}

fetch_marketaux_news <- function(symbol,
                                 api_token = Sys.getenv("MARKETAUX_API_TOKEN"),
                                 days_back = 7,
                                 limit = 20L) {
  symbol <- trimws(symbol)
  if (!nzchar(symbol) || !nzchar(api_token)) return(NULL)
  published_after <- paste0(format(Sys.Date() - as.integer(days_back), "%Y-%m-%d"), "T00:00:00")

  with_disk_cache(
    namespace = "news",
    key = api_request_cache_key("marketaux", symbol, days_back, limit, published_after),
    max_age_secs = NEWS_CACHE_TTL_SECS,
    validate = function(x) is.data.frame(x) && nrow(x) > 0,
    loader = function() {
      resp <- tryCatch({
        request("https://api.marketaux.com/v1/news/all") |>
          req_url_query(
            symbols = symbol,
            filter_entities = "true",
            published_after = published_after,
            limit = as.integer(limit),
            api_token = api_token
          ) |>
          req_timeout(20) |>
          req_retry(max_tries = 2) |>
          req_error(is_error = function(resp) FALSE) |>
          req_perform()
      }, error = function(e) {
        message("MarketAux news failed for ", symbol, ": ", conditionMessage(e))
        NULL
      })

      if (is.null(resp) || resp_status(resp) != 200) return(NULL)
      body <- tryCatch(resp_body_json(resp), error = function(e) NULL)
      items <- body$data %||% list()
      if (!is.list(items) || length(items) == 0) return(NULL)

      rows <- lapply(items, function(item) {
        entity1 <- if (is.list(item$entities) && length(item$entities) > 0) item$entities[[1]] else list()
        sentiment <- entity1$sentiment_score %||% entity1$sentiment %||% NA_real_
        extra <- if (is.finite(as.numeric(sentiment))) paste0(" MarketAux sentiment=", round(as.numeric(sentiment), 3), ".") else ""
        data.frame(
          Symbol = symbol,
          Title = item$title %||% "",
          Source = item$source %||% "MarketAux",
          Published = as.numeric(as.POSIXct(item$published_at %||% NA_character_, tz = "UTC")),
          Summary = paste0(item$description %||% "", extra),
          Url = item$url %||% "",
          stringsAsFactors = FALSE
        )
      })
      dplyr::bind_rows(rows)
    }
  )
}

fetch_alpha_vantage_news_sentiment <- function(symbol,
                                               api_key = Sys.getenv("ALPHA_VANTAGE_API_KEY"),
                                               limit = 10L) {
  symbol <- trimws(symbol)
  if (!nzchar(symbol) || !nzchar(api_key)) return(NULL)

  with_disk_cache(
    namespace = "news",
    key = api_request_cache_key("alpha_vantage", symbol, limit, Sys.Date()),
    max_age_secs = NEWS_CACHE_TTL_SECS,
    validate = function(x) is.data.frame(x) && nrow(x) > 0,
    loader = function() {
      resp <- tryCatch({
        request(paste0(
          "https://www.alphavantage.co/query?function=NEWS_SENTIMENT",
          "&tickers=", symbol,
          "&limit=", as.integer(limit),
          "&apikey=", api_key
        )) |>
          req_timeout(20) |>
          req_retry(max_tries = 2) |>
          req_error(is_error = function(resp) FALSE) |>
          req_perform()
      }, error = function(e) {
        message("Alpha Vantage news failed for ", symbol, ": ", conditionMessage(e))
        NULL
      })

      if (is.null(resp) || resp_status(resp) != 200) return(NULL)
      body <- tryCatch(resp_body_json(resp), error = function(e) NULL)
      items <- body$feed %||% list()
      if (!is.list(items) || length(items) == 0) return(NULL)

      rows <- lapply(items, function(item) {
        published <- item$time_published %||% ""
        published_ts <- suppressWarnings(as.POSIXct(published, format = "%Y%m%dT%H%M%S", tz = "UTC"))
        sentiment <- item$overall_sentiment_score %||% NA_real_
        sentiment_label <- item$overall_sentiment_label %||% ""
        extra <- if (nzchar(sentiment_label) || is.finite(as.numeric(sentiment))) {
          paste0(
            " Alpha Vantage sentiment=",
            sentiment_label,
            if (is.finite(as.numeric(sentiment))) paste0(" (", round(as.numeric(sentiment), 3), ")") else "",
            "."
          )
        } else {
          ""
        }
        data.frame(
          Symbol = symbol,
          Title = item$title %||% "",
          Source = "Alpha Vantage",
          Published = as.numeric(published_ts),
          Summary = paste0(item$summary %||% "", extra),
          Url = item$url %||% "",
          stringsAsFactors = FALSE
        )
      })
      dplyr::bind_rows(rows)
    }
  )
}

fetch_finnhub_quote <- function(symbol, api_key) {
  symbol <- trimws(symbol)
  if (!nzchar(symbol) || !nzchar(api_key)) return(NULL)
  with_disk_cache(
    namespace = "quotes",
    key = api_request_cache_key("finnhub_quote", symbol),
    max_age_secs = QUOTE_CACHE_TTL_SECS,
    validate = function(x) is.list(x) && is.finite(as.numeric(x$price %||% NA_real_)),
    loader = function() {
      resp <- tryCatch({
        request("https://finnhub.io/api/v1/quote") |>
          req_url_query(symbol = symbol, token = api_key) |>
          req_timeout(10) |>
          req_retry(max_tries = 2) |>
          req_error(is_error = function(resp) FALSE) |>
          req_perform()
      }, error = function(e) NULL)
      if (is.null(resp) || resp_status(resp) != 200) return(NULL)
      data <- resp_body_json(resp)
      if (is.null(data$c) || data$c == 0) return(NULL)
      list(symbol = symbol, price = data$c, change = data$d,
           change_pct = data$dp, high = data$h, low = data$l,
           open = data$o, prev_close = data$pc)
    }
  )
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
# LLM (OpenAI)
# ----------------------------

call_openai_text <- function(system_prompt, user_prompt, api_key, max_tokens = 1500, model = "gpt-4o-mini") {
  if (!nzchar(api_key)) return(NULL)
  body <- list(
    model = model, temperature = 0, seed = 42L,
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
