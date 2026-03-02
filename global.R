# global.R
# Constants, libraries, utility functions, and API key loading

if (getRversion() < "4.1") {
  stop("This app requires R >= 4.1 (for native pipe |>).")
}

for (pkg in c("shiny", "httr2", "DT", "dplyr", "plotly", "jsonlite", "quantmod")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Missing package: ", pkg,
      ". Run: install.packages(c('shiny', 'httr2', 'DT', 'dplyr', 'plotly', 'jsonlite', 'quantmod'))"
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

CACHE_DIR <- "data"
CACHE_MAX_AGE_HOURS <- 24L

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

# Source dependent modules (Shiny auto-sources global.R before ui.R/server.R)
source("api.R", local = FALSE)
source("models.R", local = FALSE)
source("report.R", local = FALSE)

# ----------------------------
# Startup auto-refresh of financial CSVs
# ----------------------------

refresh_financials_if_stale <- function(max_age_hours = 24) {
  kf_path <- file.path(getwd(), "key_financials.csv")
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
      source("fetch_financials.R", local = TRUE)
      message("[Startup] Financial CSVs refreshed at ", Sys.time())
    }, error = function(e) {
      message("[Startup] Could not refresh CSVs: ", e$message)
      message("[Startup] Using existing CSV files as fallback")
    })
  } else {
    message("[Startup] Financial CSVs are fresh - skipping refresh")
  }
}

refresh_financials_if_stale(max_age_hours = 24)
