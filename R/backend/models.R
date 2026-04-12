# models.R
# Financial models (GBM, Lattice, SIM), RAG computation, caching, export

# ----------------------------
# local cache
# ----------------------------

load_cached_data <- function() {
  daily_file <- cache_path("daily_prices.csv")
  news_file <- cache_path("news.csv")
  meta_file <- cache_path("cache_updated.txt")

  if (!file.exists(daily_file)) return(NULL)

  daily_df <- tryCatch({
    d <- read.csv(daily_file, stringsAsFactors = FALSE)
    d$Date <- as.Date(d$Date)
    d
  }, error = function(e) NULL)
  if (is.null(daily_df) || nrow(daily_df) == 0) return(NULL)

  news_df <- tryCatch({
    if (!file.exists(news_file)) return(NULL)
    n <- read.csv(news_file, stringsAsFactors = FALSE)
    normalize_news_archive(n)
  }, error = function(e) NULL)

  updated <- if (file.exists(meta_file)) trimws(readLines(meta_file, n = 1L, warn = FALSE)) else ""
  list(daily = daily_df, news = news_df, updated_at = updated)
}

load_exported_data <- function() {
  app_dir <- get_export_dir()
  daily_file <- file.path(app_dir, "historical_prices.csv")
  news_file <- file.path(app_dir, "news_archive.csv")

  if (!file.exists(daily_file)) return(NULL)

  daily_df <- tryCatch({
    d <- read.csv(daily_file, stringsAsFactors = FALSE)
    d$Date <- as.Date(d$Date)
    d
  }, error = function(e) NULL)
  if (is.null(daily_df) || nrow(daily_df) == 0) return(NULL)

  news_df <- tryCatch({
    if (!file.exists(news_file)) return(NULL)
    n <- read.csv(news_file, stringsAsFactors = FALSE)
    normalize_news_archive(n)
  }, error = function(e) NULL)

  updated <- paste(
    "Loaded from exported CSVs at",
    format(file.info(daily_file)$mtime, "%Y-%m-%d %H:%M:%S")
  )

  list(daily = daily_df, news = news_df, updated_at = updated)
}

save_cached_data <- function(daily_df, news_df = NULL) {
  if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)
  write.csv(daily_df, cache_path("daily_prices.csv"), row.names = FALSE)
  if (!is.null(news_df) && nrow(news_df) > 0) {
    save_news <- normalize_news_archive(news_df)
    save_news$Published <- as.numeric(save_news$Published)
    write.csv(save_news, cache_path("news.csv"), row.names = FALSE)
  }
  writeLines(safe_now(), cache_path("cache_updated.txt"))
}

get_export_dir <- function() {
  if (nzchar(Sys.getenv("SHINY_APP_DIR"))) Sys.getenv("SHINY_APP_DIR") else PROJECT_ROOT
}

get_refresh_lock_path <- function() {
  if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)
  cache_path("market_refresh.lock")
}

get_refresh_status_path <- function() {
  results_dir <- file.path(get_export_dir(), "results")
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
  file.path(results_dir, "latest_refresh_status.json")
}

load_latest_refresh_status <- function() {
  path <- get_refresh_status_path()
  if (!file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
}

save_latest_refresh_status <- function(payload) {
  path <- get_refresh_status_path()
  jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE, na = "null")
  payload
}

acquire_refresh_lock <- function(ttl_minutes = 30) {
  lock_path <- get_refresh_lock_path()
  if (file.exists(lock_path)) {
    age_minutes <- tryCatch(
      as.numeric(difftime(Sys.time(), file.info(lock_path)$mtime, units = "mins")),
      error = function(e) Inf
    )
    if (is.finite(age_minutes) && age_minutes > ttl_minutes) {
      unlink(lock_path, force = TRUE)
    }
  }

  if (file.exists(lock_path)) return(FALSE)
  created <- file.create(lock_path)
  if (isTRUE(created)) {
    writeLines(paste("pid=", Sys.getpid(), " started_at=", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
               lock_path)
  }
  isTRUE(created)
}

release_refresh_lock <- function() {
  lock_path <- get_refresh_lock_path()
  if (file.exists(lock_path)) unlink(lock_path, force = TRUE)
  invisible(TRUE)
}

recent_refresh_age_minutes <- function() {
  status <- load_latest_refresh_status()
  if (is.null(status) || is.null(status$ended_at)) return(Inf)
  ended_at <- suppressWarnings(as.POSIXct(status$ended_at, tz = "UTC"))
  if (is.na(ended_at)) return(Inf)
  as.numeric(difftime(Sys.time(), ended_at, units = "mins"))
}

previous_weekday <- function(x) {
  d <- as.Date(x)
  repeat {
    wd <- as.POSIXlt(d)$wday
    if (!wd %in% c(0, 6)) return(d)
    d <- d - 1
  }
}

latest_expected_market_date <- function(now = Sys.time(), tz = "America/New_York", close_hour = 18L) {
  ts <- as.POSIXlt(as.POSIXct(now, tz = tz), tz = tz)
  d <- as.Date(ts)
  if (ts$wday %in% c(0, 6)) return(previous_weekday(d))
  if (ts$hour < close_hour) return(previous_weekday(d - 1))
  d
}

run_financial_refresh_script <- function() {
  app_dir <- get_export_dir()
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(app_dir)
  source_project_file("R", "backend", "fetch_financials.R", local = TRUE)
  TRUE
}

fetch_all_histories_data <- function(api_key = Sys.getenv("FINNHUB_API_KEY"),
                                     tickers = c(TICKERS, MARKET_TICKER),
                                     days_back = 365,
                                     sleep_sec = 0.2,
                                     on_progress = NULL) {
  rows <- list()
  source_counts <- list()
  for (i in seq_along(tickers)) {
    sym <- tickers[[i]]
    if (is.function(on_progress)) {
      on_progress(0.05 + 0.45 * (i / length(tickers)), paste("Fetching history for", sym))
    }
    df <- fetch_finnhub_candles(sym, api_key, days_back = days_back)
    if (!is.null(df) && nrow(df) > 0) {
      rows[[length(rows) + 1L]] <- df
      source <- attr(df, "data_source") %||% "unknown"
      source_counts[[source]] <- (source_counts[[source]] %||% 0L) + 1L
    }
    if (sleep_sec > 0 && i < length(tickers)) Sys.sleep(sleep_sec)
  }
  if (length(rows) == 0) return(NULL)
  out <- dplyr::bind_rows(rows)
  attr(out, "source_counts") <- source_counts
  out
}

gather_all_news_data <- function(news_days = 90L,
                                 api_key = Sys.getenv("FINNHUB_API_KEY"),
                                 sleep_sec = 0.15,
                                 on_progress = NULL) {
  has_any_news_provider <- nzchar(api_key) ||
    nzchar(Sys.getenv("MARKETAUX_API_TOKEN")) ||
    nzchar(Sys.getenv("ALPHA_VANTAGE_API_KEY"))
  if (!has_any_news_provider) return(NULL)
  rows <- list()
  source_counts <- list()
  for (i in seq_along(TICKERS)) {
    sym <- TICKERS[[i]]
    if (is.function(on_progress)) {
      on_progress(0.50 + 0.25 * (i / length(TICKERS)), paste("Fetching news for", sym))
    }
    if (nzchar(api_key)) {
      news <- fetch_finnhub_news(sym, api_key, days_back = as.integer(news_days))
      if (!is.null(news)) {
        rows <- c(rows, news)
        source_counts$finnhub_company <- (source_counts$finnhub_company %||% 0L) + length(news)
      }
    }

    marketaux <- fetch_marketaux_news(sym, days_back = as.integer(news_days), limit = 15L)
    if (!is.null(marketaux) && nrow(marketaux) > 0) {
      rows[[length(rows) + 1L]] <- marketaux
      source_counts$marketaux <- (source_counts$marketaux %||% 0L) + nrow(marketaux)
    }

    alpha <- fetch_alpha_vantage_news_sentiment(sym, limit = 10L)
    if (!is.null(alpha) && nrow(alpha) > 0) {
      rows[[length(rows) + 1L]] <- alpha
      source_counts$alpha_vantage <- (source_counts$alpha_vantage %||% 0L) + nrow(alpha)
    }

  if (sleep_sec > 0 && i < length(TICKERS)) Sys.sleep(sleep_sec)
  }
  if (nzchar(api_key)) {
    macro <- fetch_finnhub_general_news(api_key, days_back = as.integer(news_days))
    if (!is.null(macro)) {
      rows <- c(rows, macro)
      source_counts$finnhub_macro <- (source_counts$finnhub_macro %||% 0L) + length(macro)
    }
  }
  if (length(rows) == 0) return(NULL)
  out <- dedupe_news_archive(dplyr::bind_rows(rows))
  attr(out, "source_counts") <- source_counts
  out
}

market_data_status <- function(app_dir = get_export_dir(), now = Sys.time()) {
  prices_path <- file.path(app_dir, "historical_prices.csv")
  news_path <- file.path(app_dir, "news_archive.csv")
  expected_date <- latest_expected_market_date(now)

  latest_price_date <- as.Date(NA)
  if (file.exists(prices_path)) {
    latest_price_date <- tryCatch({
      x <- read.csv(prices_path, stringsAsFactors = FALSE)
      if (!"Date" %in% names(x) || nrow(x) == 0) as.Date(NA) else max(as.Date(x$Date), na.rm = TRUE)
    }, error = function(e) as.Date(NA))
  }

  latest_news_time <- as.POSIXct(NA)
  if (file.exists(news_path)) {
    latest_news_time <- tryCatch({
      x <- normalize_news_archive(read.csv(news_path, stringsAsFactors = FALSE))
      if (is.null(x) || nrow(x) == 0) as.POSIXct(NA) else {
        as.POSIXct(max(as.numeric(x$Published), na.rm = TRUE), origin = "1970-01-01", tz = "UTC")
      }
    }, error = function(e) as.POSIXct(NA))
  }

  news_age_hours <- if (is.na(latest_news_time)) Inf else {
    as.numeric(difftime(as.POSIXct(now, tz = "UTC"), latest_news_time, units = "hours"))
  }

  prices_stale <- is.na(latest_price_date) || latest_price_date < expected_date
  news_stale <- !is.finite(news_age_hours) || news_age_hours > 24
  refresh_status <- load_latest_refresh_status()

  list(
    stale = prices_stale || news_stale,
    prices_stale = prices_stale,
    news_stale = news_stale,
    preferred_price_source = preferred_price_source(),
    expected_price_date = as.character(expected_date),
    latest_price_date = as.character(latest_price_date),
    latest_news_time = if (is.na(latest_news_time)) NA_character_ else format(latest_news_time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    news_age_hours = round(news_age_hours, 1),
    last_refresh_status = refresh_status$status %||% NULL,
    last_refresh_ended_at = refresh_status$ended_at %||% NULL
  )
}

refresh_market_data_exports <- function(news_days = 90L,
                                        days_back = 365,
                                        include_financials = FALSE,
                                        on_progress = NULL,
                                        force = FALSE,
                                        min_refresh_interval_minutes = 15) {
  api_key <- Sys.getenv("FINNHUB_API_KEY")

  if (!isTRUE(force) && recent_refresh_age_minutes() < min_refresh_interval_minutes) {
    status <- load_latest_refresh_status()
    exported <- load_exported_data()
    rag_file <- cache_path("rag_history.csv")
    rag_result <- if (file.exists(rag_file)) {
      tryCatch(read.csv(rag_file, stringsAsFactors = FALSE), error = function(e) NULL)
    } else NULL
    return(list(
      ok = TRUE,
      skipped = TRUE,
      daily = exported$daily,
      news = exported$news,
      rag = rag_result,
      updated_at = status$ended_at %||% safe_now(),
      status = market_data_status(),
      refresh_summary = status
    ))
  }

  if (!acquire_refresh_lock()) {
    return(list(ok = FALSE, error = "A market data refresh is already in progress."))
  }
  on.exit(release_refresh_lock(), add = TRUE)

  started_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  if (is.function(on_progress)) on_progress(0.02, "Fetching historical prices...")
  hist_df <- fetch_all_histories_data(api_key = api_key, days_back = days_back, on_progress = on_progress)
  if (is.null(hist_df) || nrow(hist_df) == 0) {
    failure <- list(
      status = "failed",
      started_at = started_at,
      ended_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      error = "No historical prices were fetched."
    )
    save_latest_refresh_status(failure)
    return(list(ok = FALSE, error = "No historical prices were fetched."))
  }

  if (is.function(on_progress)) on_progress(0.52, "Fetching news archive...")
  news_df <- gather_all_news_data(news_days = news_days, api_key = api_key, on_progress = on_progress)

  if (is.function(on_progress)) on_progress(0.78, "Computing RAG history and exporting files...")
  save_cached_data(hist_df, news_df)
  rag_result <- update_rag_history(hist_df)
  export_csv_files(hist_df, rag_result)
  if (!is.null(news_df)) export_news_csv(news_df)

  financial_ok <- TRUE
  financial_error <- NULL
  if (isTRUE(include_financials)) {
    if (is.function(on_progress)) on_progress(0.90, "Refreshing financial statement CSVs...")
    financial_ok <- tryCatch(run_financial_refresh_script(), error = function(e) {
      financial_error <<- conditionMessage(e)
      FALSE
    })
  }

  source_counts <- attr(hist_df, "source_counts") %||% list()
  news_source_counts <- attr(news_df, "source_counts") %||% list()
  refresh_summary <- list(
    status = "success",
    preferred_price_source = preferred_price_source(),
    started_at = started_at,
    ended_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    historical_price_rows = nrow(hist_df),
    historical_latest_date = as.character(max(as.Date(hist_df$Date), na.rm = TRUE)),
    news_rows = if (is.null(news_df)) 0L else nrow(news_df),
    rag_rows = if (is.null(rag_result)) 0L else nrow(rag_result),
    price_source_counts = source_counts,
    news_source_counts = news_source_counts,
    financial_refresh_ok = financial_ok,
    financial_refresh_error = financial_error
  )
  save_latest_refresh_status(refresh_summary)

  if (is.function(on_progress)) on_progress(1, "Refresh complete")
  list(
    ok = TRUE,
    daily = hist_df,
    news = news_df,
    rag = rag_result,
    updated_at = safe_now(),
    financial_refresh_ok = financial_ok,
    financial_refresh_error = financial_error,
    status = market_data_status(),
    refresh_summary = refresh_summary
  )
}

refresh_market_data_if_stale <- function(include_financials = FALSE, verbose = TRUE) {
  status <- market_data_status()
  if (!isTRUE(status$stale)) {
    if (is.null(load_latest_refresh_status())) {
      save_latest_refresh_status(list(
        status = "snapshot",
        started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        ended_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        historical_latest_date = status$latest_price_date,
        latest_news_time = status$latest_news_time
      ))
    }
    if (isTRUE(verbose)) {
      message("[Startup] Market data fresh - latest price date ", status$latest_price_date,
              ", expected ", status$expected_price_date)
    }
    return(invisible(list(ok = TRUE, skipped = TRUE, status = status)))
  }

  if (isTRUE(verbose)) {
    message("[Startup] Market data stale. latest_price_date=", status$latest_price_date,
            " expected=", status$expected_price_date,
            " news_age_hours=", status$news_age_hours)
  }

  result <- tryCatch(
    refresh_market_data_exports(include_financials = include_financials),
    error = function(e) list(ok = FALSE, error = conditionMessage(e))
  )

  if (isTRUE(verbose)) {
    if (isTRUE(result$ok)) message("[Startup] Market data refresh completed.")
    else message("[Startup] Market data refresh failed: ", result$error %||% "unknown error")
  }
  invisible(result)
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
    export <- normalize_news_archive(news_df)
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
    } else {
      combined <- export
    }
    combined <- dedupe_news_archive(combined)
    write.csv(combined, news_file, row.names = FALSE)
    save_latest_news_summary(combined)
    message("[Export] news_archive.csv: ", nrow(combined), " rows")
  }, error = function(e) message("Could not export news_archive.csv: ", e$message))
}

# ----------------------------
# OHLC metrics
# ----------------------------

compute_ohlc_metrics <- function(df, lookback = 252) {
  if (is.null(df) || nrow(df) < 2) return(NULL)
  df <- df |> arrange(Date)
  n <- nrow(df)
  latest <- tail(df, 1)
  idx_1d <- max(1, n - 1); idx_7d <- max(1, n - 6)
  idx_30d <- max(1, n - 29); idx_n <- max(1, n - lookback)

  p1d <- latest$Close - df$Close[idx_1d]
  p1d_pct <- ifelse(df$Close[idx_1d] > 0, p1d / df$Close[idx_1d] * 100, NA_real_)
  p7d <- latest$Close - df$Close[idx_7d]
  p7d_pct <- ifelse(df$Close[idx_7d] > 0, p7d / df$Close[idx_7d] * 100, NA_real_)
  p30d <- latest$Close - df$Close[idx_30d]
  p30d_pct <- ifelse(df$Close[idx_30d] > 0, p30d / df$Close[idx_30d] * 100, NA_real_)
  pn <- latest$Close - df$Close[idx_n]
  pn_pct <- ifelse(df$Close[idx_n] > 0, pn / df$Close[idx_n] * 100, NA_real_)

  list(
    latest = latest$Close, latest_date = latest$Date,
    change1d = p1d, change1d_pct = p1d_pct,
    change7d_pct = p7d_pct, change30d_pct = p30d_pct,
    changeN_pct = pn_pct,
    mean_20d_volatility = {
      if (n < 21) NA_real_ else {
        lr <- diff(log(df$Close))
        round(sd(tail(lr, 20), na.rm = TRUE) * sqrt(252) * 100, 2)
      }
    }
  )
}

# ----------------------------
# news helpers
# ----------------------------

NEWS_ENTITY_ALIASES <- list(
  AAPL = c("apple", "iphone", "ipad", "mac", "ios", "app store", "cupertino"),
  TSLA = c("tesla", "elon musk", "cybertruck", "model 3", "model y", "robotaxi", "fsd", "autopilot"),
  META = c("meta", "facebook", "instagram", "whatsapp", "threads", "reels", "llama"),
  NVDA = c("nvidia", "nvda", "geforce", "cuda", "blackwell", "hopper", "h100", "h200"),
  GOOGL = c("google", "alphabet", "youtube", "android", "gemini", "waymo", "pixel"),
  AMZN = c("amazon", "aws", "prime", "alexa", "kindle", "whole foods"),
  MSFT = c("microsoft", "azure", "windows", "copilot", "xbox", "linkedin", "openai")
)

news_symbol_terms <- function(symbol) {
  company_name <- if (symbol %in% names(TICKER_LABELS)) TICKER_LABELS[[symbol]] else symbol
  aliases <- NEWS_ENTITY_ALIASES[[symbol]] %||% character(0)
  terms <- unique(trimws(tolower(c(symbol, company_name, aliases))))
  terms[nzchar(terms)]
}

score_company_mentions <- function(news_df, symbol) {
  if (is.null(news_df) || nrow(news_df) == 0) {
    return(data.frame(
      title_hits = integer(0),
      summary_hits = integer(0),
      mention_score = integer(0),
      company_mentioned = logical(0),
      stringsAsFactors = FALSE
    ))
  }
  if (!all(c("Title", "Summary") %in% names(news_df))) {
    return(data.frame(
      title_hits = integer(0),
      summary_hits = integer(0),
      mention_score = integer(0),
      company_mentioned = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  terms <- news_symbol_terms(symbol)
  title_blob <- tolower(ifelse(is.na(news_df$Title), "", news_df$Title))
  summary_blob <- tolower(ifelse(is.na(news_df$Summary), "", news_df$Summary))

  title_hits <- integer(nrow(news_df))
  summary_hits <- integer(nrow(news_df))
  for (term in terms) {
    title_hits <- title_hits + as.integer(grepl(term, title_blob, fixed = TRUE))
    summary_hits <- summary_hits + as.integer(grepl(term, summary_blob, fixed = TRUE))
  }

  mention_score <- 2L * title_hits + summary_hits
  data.frame(
    title_hits = title_hits,
    summary_hits = summary_hits,
    mention_score = mention_score,
    company_mentioned = mention_score > 0L,
    stringsAsFactors = FALSE
  )
}

normalize_news_archive <- function(news_df) {
  if (is.null(news_df) || nrow(news_df) == 0) return(NULL)
  out <- news_df
  required_cols <- c("Symbol", "Title", "Source", "Published", "Summary", "Url")
  for (nm in required_cols) {
    if (!nm %in% names(out)) out[[nm]] <- NA
  }

  published_num <- suppressWarnings(as.numeric(out$Published))
  if (!all(is.na(published_num))) {
    out$Published <- published_num
  } else {
    published_ts <- suppressWarnings(as.POSIXct(out$Published, tz = "UTC"))
    out$Published <- as.numeric(published_ts)
  }

  out$Symbol <- trimws(as.character(out$Symbol))
  out$Title <- trimws(as.character(out$Title))
  out$Source <- trimws(as.character(out$Source))
  out$Summary <- trimws(as.character(out$Summary))
  out$Url <- trimws(as.character(out$Url))
  out <- out[!is.na(out$Published) & nzchar(out$Title), required_cols, drop = FALSE]
  if (nrow(out) == 0) return(NULL)
  out[order(out$Published, decreasing = TRUE), , drop = FALSE]
}

dedupe_news_archive <- function(news_df) {
  news_df <- normalize_news_archive(news_df)
  if (is.null(news_df) || nrow(news_df) == 0) return(news_df)

  dedupe_key <- ifelse(
    nzchar(news_df$Url),
    tolower(news_df$Url),
    paste(
      tolower(news_df$Symbol),
      tolower(news_df$Source),
      tolower(news_df$Title),
      news_df$Published,
      sep = "|"
    )
  )
  news_df$.__dedupe_key <- dedupe_key
  news_df <- news_df[!duplicated(news_df$.__dedupe_key), , drop = FALSE]
  news_df$.__dedupe_key <- NULL
  news_df[order(news_df$Published, decreasing = TRUE), , drop = FALSE]
}

score_news_relevance <- function(news_df, symbol, keywords = character(0), as_of = NULL) {
  news_df <- normalize_news_archive(news_df)
  if (is.null(news_df) || nrow(news_df) == 0) return(numeric(0))

  score <- rep(0, nrow(news_df))
  text_blob <- tolower(paste(news_df$Title, news_df$Summary))
  mention_info <- score_company_mentions(news_df, symbol)
  score <- score + ifelse(news_df$Symbol == symbol, 8, ifelse(news_df$Symbol == "MACRO", 3, 0))
  if (nrow(mention_info) == nrow(news_df)) {
    score <- score + pmin(8, 4 * mention_info$title_hits + 2 * mention_info$summary_hits)
    score <- score - ifelse(news_df$Symbol == symbol & !mention_info$company_mentioned, 6, 0)
  }

  keyword_terms <- unique(tolower(keywords))
  for (term in keyword_terms[nzchar(keyword_terms)]) {
    score <- score + as.integer(grepl(term, text_blob, fixed = TRUE))
  }

  if (!is.null(as_of)) {
    age_days <- pmax(0, as.numeric(as.Date(as_of) - as.Date(as.POSIXct(news_df$Published, origin = "1970-01-01", tz = "UTC"))))
    score <- score + pmax(0, 4 - age_days / 2)
  }

  score
}

select_relevant_news <- function(news_df, symbol, as_of = NULL,
                                 n_company = 10L, n_macro = 8L,
                                 keywords = character(0)) {
  news_df <- dedupe_news_archive(news_df)
  if (is.null(news_df) || nrow(news_df) == 0) return(NULL)

  if (!is.null(as_of)) {
    cutoff <- as.numeric(as.POSIXct(paste0(as.Date(as_of), " 23:59:59"), tz = "UTC"))
    news_df <- news_df[news_df$Published <= cutoff, , drop = FALSE]
    if (nrow(news_df) == 0) return(NULL)
  }

  news_df$relevance_score <- score_news_relevance(news_df, symbol, keywords = keywords, as_of = as_of)

  company <- news_df[news_df$Symbol == symbol, , drop = FALSE]
  macro <- news_df[news_df$Symbol == "MACRO", , drop = FALSE]

  if (nrow(company) > 0) {
    company <- company[order(company$relevance_score, company$Published, decreasing = TRUE), , drop = FALSE]
    company <- head(company, n_company)
  }
  if (nrow(macro) > 0) {
    macro <- macro[order(macro$relevance_score, macro$Published, decreasing = TRUE), , drop = FALSE]
    macro <- head(macro, n_macro)
  }

  combined <- dplyr::bind_rows(company, macro)
  if (nrow(combined) == 0) return(NULL)
  combined <- combined[order(combined$relevance_score, combined$Published, decreasing = TRUE), , drop = FALSE]
  combined$relevance_score <- NULL
  combined
}

build_news_summary_payload <- function(news_df, max_per_symbol = 3L) {
  news_df <- dedupe_news_archive(news_df)
  if (is.null(news_df) || nrow(news_df) == 0) return(NULL)

  symbols <- unique(news_df$Symbol)
  symbols <- symbols[order(symbols)]
  counts <- lapply(symbols, function(sym) {
    sub <- news_df[news_df$Symbol == sym, , drop = FALSE]
    list(symbol = sym, count = nrow(sub))
  })

  top_headlines <- lapply(symbols, function(sym) {
    sub <- news_df[news_df$Symbol == sym, , drop = FALSE]
    sub <- head(sub[order(sub$Published, decreasing = TRUE), , drop = FALSE], max_per_symbol)
    list(
      symbol = sym,
      headlines = lapply(seq_len(nrow(sub)), function(i) {
        list(
          title = sub$Title[i],
          source = sub$Source[i],
          published = format(as.POSIXct(sub$Published[i], origin = "1970-01-01", tz = "UTC"),
                             "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
          url = sub$Url[i]
        )
      })
    )
  })

  list(
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    total_articles = nrow(news_df),
    counts_by_symbol = counts,
    top_headlines = top_headlines
  )
}

save_latest_news_summary <- function(news_df) {
  payload <- build_news_summary_payload(news_df)
  if (is.null(payload)) return(NULL)
  write_latest_result_json("latest_news_summary.json", payload)
  payload
}

load_latest_news_summary <- function() {
  read_latest_result_json("latest_news_summary.json")
}

build_news_snapshot <- function(symbol, news_df, max_rows = 5) {
  sub <- select_relevant_news(news_df, symbol, n_company = max_rows, n_macro = 0L)
  if (is.null(sub)) return("No recent news.")
  if (nrow(sub) == 0) return("No recent news.")
  top <- head(sub, max_rows)
  items <- paste0(
    seq_len(nrow(top)), ". ", top$Symbol, ": ", top$Title,
    " (", format(as.POSIXct(top$Published, origin = "1970-01-01", tz = "UTC"), "%m-%d %H:%M", tz = "America/New_York"), " ET)"
  )
  paste(items, collapse = "\n")
}

# ----------------------------
# GBM (Geometric Brownian Motion)
# ----------------------------

estimate_gbm_params <- function(hist_df, max_lookback = 252) {
  if (is.null(hist_df) || nrow(hist_df) < 5) return(NULL)
  hist_df <- hist_df |> arrange(Date)
  h <- tail(hist_df, min(nrow(hist_df), as.integer(max_lookback)))
  close <- as.numeric(h$Close)
  close <- close[!is.na(close)]
  if (length(close) < 5) return(NULL)

  r <- diff(log(close))
  r <- r[is.finite(r)]
  if (length(r) < 21) return(NULL)

  mu_daily <- mean(r); sig_daily <- sd(r)
  list(
    s0 = tail(close, 1), last_date = tail(h$Date, 1),
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
  z_lo <- qnorm(p_lo); z_hi <- qnorm(p_hi)
  drift_term <- (mu_annual - 0.5 * sigma_annual^2) * t
  vol_term <- sigma_annual * sqrt(t)
  expected <- s0 * exp(mu_annual * t)
  median <- s0 * exp(drift_term)
  qlo <- s0 * exp(drift_term + vol_term * z_lo)
  qhi <- s0 * exp(drift_term + vol_term * z_hi)
  dates <- c(as.Date(start_date), add_trading_days(as.Date(start_date), n_days))
  data.frame(Date = dates, Expected = as.numeric(expected), Median = as.numeric(median),
             Lo = as.numeric(qlo), Hi = as.numeric(qhi), stringsAsFactors = FALSE)
}

# ----------------------------
# Binomial Lattice (RWPM - Real-World Probability Measure)
# Matches Julia VLQuantitativeFinancePackage approach:
#   u = mean of daily price ratios on up days
#   d = mean of daily price ratios on down days
#   p = empirical fraction of up days
# ----------------------------

estimate_lattice_params <- function(hist_df, T_years = 1/12, N_steps = 22) {
  if (is.null(hist_df) || nrow(hist_df) < 22) return(NULL)
  hist_df <- hist_df |> arrange(Date)
  cl <- as.numeric(hist_df$Close); cl <- cl[!is.na(cl)]
  if (length(cl) < 22) return(NULL)
  r <- diff(log(cl)); r <- r[is.finite(r)]
  if (length(r) < 21) return(NULL)

  ratios <- cl[-1] / cl[-length(cl)]
  up_idx <- which(r > 0)
  dn_idx <- which(r <= 0)
  if (length(up_idx) < 3 || length(dn_idx) < 3) return(NULL)

  u <- mean(ratios[up_idx])
  d <- mean(ratios[dn_idx])
  p_real <- length(up_idx) / length(r)

  if (u <= d) return(NULL)

  p_rn <- clamp((exp(RISK_FREE_RATE / 252) - d) / (u - d), 0.001, 0.999)
  sig_a <- sd(r) * sqrt(252)

  list(s0 = tail(cl, 1), last_date = tail(hist_df$Date, 1),
       T_years = T_years, N_steps = as.integer(N_steps), dt = T_years / N_steps,
       u = u, d = d, p_real = p_real, p_rn = p_rn,
       mu_annual = mean(r) * 252, sigma_annual = sig_a, method = "RWPM", n_obs = length(r))
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

# ----------------------------
# Single Index Model (SIM)
# ----------------------------

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

# ----------------------------
# RAG computation and history
# ----------------------------

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
      n_obs = n, spot_price = round(tail(cl, 1), 4),
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
    message("[RAG] Schema changed - rebuilding rag_history.csv from scratch")
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
