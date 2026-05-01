# global.R
# Constants, libraries, utility functions, and API key loading

if (getRversion() < "4.1") {
  stop("This app requires R >= 4.1 (for native pipe |>).")
}

for (pkg in c("shiny", "httr2", "DT", "dplyr", "plotly", "jsonlite", "quantmod", "digest",
              "shinycssloaders", "shinyjs")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Missing package: ", pkg,
      ". Run: install.packages(c('shiny', 'httr2', 'DT', 'dplyr', 'plotly', 'jsonlite', 'quantmod', 'digest', 'shinycssloaders', 'shinyjs'))"
    )
  }
}

library(shiny)
library(httr2)
library(DT)
library(dplyr)
library(quantmod)
library(plotly)
library(jsonlite)
library(shinycssloaders)
library(shinyjs)

PROJECT_ROOT <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

project_path <- function(...) {
  file.path(PROJECT_ROOT, ...)
}

source_project_file <- function(..., local = FALSE) {
  source(project_path(...), local = local)
}

# ----------------------------
# constants
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
BACKTEST_LOOKBACK_DAYS <- 90L
BACKTEST_MODEL_CHOICES <- c(
  "GPT-4o mini (fast)" = "gpt-4o-mini",
  "GPT-4o (stronger)" = "gpt-4o"
)
BACKTEST_STRATEGY_MODES <- c(
  "Auto (Agent chooses)" = "auto",
  "Trend only" = "trend_only",
  "Mean reversion only" = "mean_reversion_only",
  "Event momentum only" = "event_momentum_only",
  "Defensive only" = "defensive_only"
)

CACHE_DIR <- "data"
CACHE_MAX_AGE_HOURS <- 24L
API_CACHE_SUBDIR <- "api"
QUOTE_CACHE_TTL_SECS <- 10L
NEWS_CACHE_TTL_SECS <- 20L * 60L
CANDLE_CACHE_TTL_SECS <- 6L * 3600L
PREFERRED_PRICE_SOURCE_OPTIONS <- c("auto", "finnhub", "tiingo", "yahoo")

# ----------------------------
# utility operators / helpers
# ----------------------------

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
clamp <- function(x, lo, hi) pmax(lo, pmin(hi, x))

add_trading_days <- function(start_date, n_days) {
  d <- as.Date(start_date)
  if (is.na(d)) return(rep(as.Date(NA), n_days))
  out <- as.Date(rep(NA, n_days))
  i <- 0L
  while (i < n_days) {
    d <- d + 1
    wd <- as.POSIXlt(d)$wday
    if (wd %in% c(0, 6)) next
    i <- i + 1L
    out[[i]] <- d
  }
  out
}

cache_path <- function(name) file.path(CACHE_DIR, name)

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

api_cache_dir <- function(namespace = NULL) {
  base_dir <- ensure_dir(cache_path(API_CACHE_SUBDIR))
  if (is.null(namespace) || !nzchar(namespace)) return(base_dir)
  ensure_dir(file.path(base_dir, namespace))
}

sanitize_cache_key <- function(x) {
  x <- tolower(trimws(as.character(x %||% "")))
  x <- gsub("[^a-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  if (!nzchar(x)) "cache_item" else x
}

api_cache_file <- function(namespace, key) {
  file.path(api_cache_dir(namespace), paste0(sanitize_cache_key(key), ".rds"))
}

read_disk_cache <- function(namespace, key, max_age_secs = Inf) {
  path <- api_cache_file(namespace, key)
  if (!file.exists(path)) return(NULL)

  if (is.finite(max_age_secs)) {
    age_secs <- tryCatch(
      as.numeric(difftime(Sys.time(), file.info(path)$mtime, units = "secs")),
      error = function(e) Inf
    )
    if (!is.finite(age_secs) || age_secs > max_age_secs) return(NULL)
  }

  tryCatch(readRDS(path), error = function(e) NULL)
}

write_disk_cache <- function(namespace, key, value) {
  path <- api_cache_file(namespace, key)
  saveRDS(value, path)
  value
}

with_disk_cache <- function(namespace,
                            key,
                            max_age_secs,
                            loader,
                            validate = NULL,
                            cache_null = FALSE) {
  cached <- read_disk_cache(namespace, key, max_age_secs = max_age_secs)
  if (!is.null(cached)) {
    if (is.list(cached) && identical(cached$.__cached_null, TRUE)) return(NULL)
    return(cached)
  }

  value <- loader()
  is_valid <- if (is.function(validate)) {
    isTRUE(validate(value))
  } else {
    !is.null(value)
  }

  if (is_valid) {
    write_disk_cache(namespace, key, value)
  } else if (isTRUE(cache_null)) {
    write_disk_cache(namespace, key, list(.__cached_null = TRUE))
  }

  value
}

preferred_price_source <- function() {
  configured <- tolower(trimws(Sys.getenv("PREFERRED_PRICE_SOURCE", unset = "auto")))
  if (!configured %in% PREFERRED_PRICE_SOURCE_OPTIONS) configured <- "auto"
  if (configured != "auto") return(configured)
  if (nzchar(Sys.getenv("TIINGO_API_TOKEN"))) return("tiingo")
  if (nzchar(Sys.getenv("FINNHUB_API_KEY"))) return("finnhub")
  "yahoo"
}

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
# API key loading
# ----------------------------

load_api_keys <- function() {
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

message("--- API Key Check ---")
message("Working dir: ", getwd())
message("FINNHUB_API_KEY: ", ifelse(nzchar(FINNHUB_API_KEY), paste0(substr(FINNHUB_API_KEY, 1, 6), "...OK"), "MISSING"))
message("OPENAI_API_KEY: ", ifelse(nzchar(OPENAI_API_KEY), paste0(substr(OPENAI_API_KEY, 1, 6), "...OK"), "MISSING"))
message("OLLAMA_API_KEY: ", ifelse(nzchar(OLLAMA_API_KEY), paste0(substr(OLLAMA_API_KEY, 1, 6), "...OK"), "MISSING"))
message("all_api exists in cwd: ", file.exists("all_api"))
message("---------------------")

# Source dependent runtime modules.
# Order matters: api -> models -> rag -> agents -> report -> backtester
source_project_file("R", "backend", "api.R", local = FALSE)
source_project_file("R", "backend", "models.R", local = FALSE)
source_project_file("R", "backend", "rag.R", local = FALSE)
source_project_file("R", "backend", "agents.R", local = FALSE)
source_project_file("R", "backend", "report.R", local = FALSE)
source_project_file("R", "backend", "backtester.R", local = FALSE)

# ----------------------------
# Startup auto-refresh of financial CSVs
# ----------------------------

refresh_financials_if_stale <- function(max_age_hours = 24) {
  export_dir <- if (exists("get_export_dir", mode = "function")) get_export_dir() else PROJECT_ROOT
  kf_path <- file.path(export_dir, "key_financials.csv")
  needs_refresh <- FALSE

  if (!file.exists(kf_path)) {
    message("[Startup] key_financials.csv not found - will fetch")
    needs_refresh <- TRUE
  } else {
    kf <- tryCatch(read.csv(kf_path, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(kf) && "fetch_date" %in% names(kf) && nrow(kf) > 0) {
      fetch_date <- tryCatch(as.Date(kf$fetch_date[1]), error = function(e) NA)
      if (!is.na(fetch_date)) {
        age_hours <- as.numeric(difftime(Sys.time(), as.POSIXct(fetch_date), units = "hours"))
        message("[Startup] key_financials.csv age: ", round(age_hours, 1), " hours")
        if (age_hours > max_age_hours) needs_refresh <- TRUE
      } else {
        needs_refresh <- TRUE
      }
    } else {
      needs_refresh <- TRUE
    }
  }

  if (needs_refresh) {
    message("[Startup] Refreshing financial CSVs from Yahoo Finance...")
    tryCatch({
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(export_dir)
      source_project_file("R", "backend", "fetch_financials.R", local = TRUE)
      message("[Startup] Financial CSVs refreshed at ", Sys.time())
    }, error = function(e) {
      message("[Startup] Could not refresh CSVs: ", e$message)
      message("[Startup] Using existing CSV files as fallback")
    })
  } else {
    message("[Startup] Financial CSVs are fresh - skipping refresh")
  }
}

if (!identical(Sys.getenv("SKIP_STARTUP_REFRESH"), "1")) {
  refresh_financials_if_stale(max_age_hours = 24)
  refresh_market_data_if_stale(include_financials = FALSE, verbose = TRUE)
} else {
  message("[Startup] SKIP_STARTUP_REFRESH=1 - skipping automatic startup refresh")
}
