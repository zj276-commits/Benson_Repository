# tests/test_suite.R
# Deterministic eval suite for Market Insight Studio

if (file.exists("tests/test_rag_eval.R")) {
  source("tests/test_rag_eval.R", local = FALSE)
}

test_agent_schema <- function() {
  cat("=== Agent Schema Definition Tests ===\n")
  required_keys <- list(
    fundamentals = c("fundamentals_summary", "valuation_assessment", "financial_health_score"),
    news = c("news_sentiment", "key_events", "macro_impact"),
    technical = c("technical_signal", "model_interpretation", "momentum_summary"),
    bull = c("bull_thesis", "catalysts", "upside_target"),
    bear = c("bear_thesis", "risks", "downside_target"),
    risk = c("risk_level", "risk_factors", "risk_score")
  )
  missing_defs <- setdiff(names(AGENT_ROLES), names(required_keys))
  ok <- length(missing_defs) == 0L
  if (!ok) cat("  FAIL: Missing schema definitions for:", paste(missing_defs, collapse = ", "), "\n")
  cat(sprintf("Schema Definitions: %s\n\n", if (ok) "PASS" else "FAIL"))
  list(name = "agent_schema_definitions", passed = as.integer(ok), total = 1L)
}

test_backtest_mask <- function() {
  cat("=== Backtest Mask Tests ===\n")

  dates <- seq(as.Date("2026-03-02"), as.Date("2026-03-20"), by = "day")
  dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  prices <- data.frame(
    Symbol = "TEST",
    Date = dates,
    Open = 100 + seq_along(dates) - 1,
    High = 101 + seq_along(dates) - 1,
    Low = 99 + seq_along(dates) - 1,
    Close = 100 + seq_along(dates) - 1,
    Volume = 1000000 + seq_along(dates) * 10,
    stringsAsFactors = FALSE
  )
  news <- data.frame(
    Symbol = c("TEST", "MACRO", "TEST"),
    Title = c("Positive product update", "Fed holds rates", "Supply chain risk monitored"),
    Source = c("UnitTest", "UnitTest", "UnitTest"),
    Published = c(
      as.numeric(as.POSIXct("2026-03-05 14:00:00", tz = "UTC")),
      as.numeric(as.POSIXct("2026-03-10 15:00:00", tz = "UTC")),
      as.numeric(as.POSIXct("2026-03-18 16:00:00", tz = "UTC"))
    ),
    Summary = c("Product momentum remains healthy", "Macro unchanged", "Risk remains manageable"),
    Url = c("https://example.com/1", "https://example.com/2", "https://example.com/3"),
    stringsAsFactors = FALSE
  )

  stub_agent <- function(context) {
    lookback <- context$lookback_prices
    if (!is.null(lookback) && nrow(lookback) >= 3 && tail(lookback$Close, 1) > head(lookback$Close, 1)) {
      list(action = "BUY", shares = 5, confidence = 70, reasoning = "Momentum in the visible window is positive.")
    } else {
      list(action = "HOLD", shares = 0, confidence = 40, reasoning = "No clear edge from the visible window.")
    }
  }

  result <- run_backtest(
    symbol = "TEST",
    start_date = as.Date("2026-03-05"),
    end_date = as.Date("2026-03-20"),
    initial_capital = 10000,
    lookback_days = 10,
    price_data = prices,
    news_data = news,
    decision_fn = stub_agent,
    sleep_sec = 0,
    persist = FALSE
  )

  checks <- c(
    isTRUE(result$ok),
    isTRUE(validate_no_lookahead(result)$passed),
    isTRUE(validate_trade_constraints(result)$passed),
    length(result$equity_curve) == result$trading_days + 1L
  )

  passed <- sum(checks)
  total <- length(checks)
  if (!all(checks)) {
    if (!checks[[1]]) cat("  FAIL: Backtest run failed\n")
    if (!checks[[2]]) cat("  FAIL: Look-ahead validation failed\n")
    if (!checks[[3]]) cat("  FAIL: Trade constraint validation failed\n")
    if (!checks[[4]]) cat("  FAIL: Equity curve length mismatch\n")
  }

  cat(sprintf("Backtest Mask: %d/%d passed (%.0f%%)\n\n", passed, total, passed / total * 100))
  list(name = "backtest_mask", passed = passed, total = total)
}

test_backtest_activity <- function() {
  cat("=== Backtest Activity Tests ===\n")

  dates <- seq(as.Date("2026-03-02"), as.Date("2026-03-31"), by = "day")
  dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  close <- c(100, 102, 104, 103, 101, 98, 96, 99, 101, 103, 105, 104, 102, 100, 97, 99, 101, 103, 102, 104, 106, 105)
  prices <- data.frame(
    Symbol = "TEST",
    Date = dates[seq_along(close)],
    Open = close - 0.5,
    High = close + 1,
    Low = close - 1,
    Close = close,
    Volume = 1000000 + seq_along(close) * 1000,
    stringsAsFactors = FALSE
  )
  news <- data.frame(
    Symbol = c("TEST", "TEST", "MACRO", "TEST"),
    Title = c("Strong partnership announced", "Tariff concerns grow", "Fed signals caution", "Product demand rebounds"),
    Source = c("UnitTest", "UnitTest", "UnitTest", "UnitTest"),
    Published = c(
      as.numeric(as.POSIXct("2026-03-05 14:00:00", tz = "UTC")),
      as.numeric(as.POSIXct("2026-03-11 14:00:00", tz = "UTC")),
      as.numeric(as.POSIXct("2026-03-18 14:00:00", tz = "UTC")),
      as.numeric(as.POSIXct("2026-03-24 14:00:00", tz = "UTC"))
    ),
    Summary = c("Positive event with strong growth language", "Negative event with risk language", "Macro uncertainty remains elevated", "Bullish demand recovery signal"),
    Url = c("https://example.com/1", "https://example.com/2", "https://example.com/3", "https://example.com/4"),
    stringsAsFactors = FALSE
  )

  stub_agent <- function(context) {
    day_num <- as.integer(format(as.Date(context$current_date), "%d"))
    if (day_num %% 3 == 0) {
      list(
        selected_strategy = "defensive_risk_off",
        target_position_pct = 0,
        confidence = 68,
        rebalance_reason = "Risk is elevated, so the strategy moves to cash.",
        signal_scores = list(trend_following = 0.15, mean_reversion = 0.20, event_momentum = 0.25, defensive_risk_off = 0.90),
        risk_flags = c("macro_risk"),
        holding_horizon = "1-2 trading days"
      )
    } else if (day_num %% 2 == 0) {
      list(
        selected_strategy = "trend_following",
        target_position_pct = 75,
        confidence = 74,
        rebalance_reason = "Momentum is positive, so the strategy leans into trend-following exposure.",
        signal_scores = list(trend_following = 0.88, mean_reversion = 0.22, event_momentum = 0.35, defensive_risk_off = 0.18),
        risk_flags = c("volatility_watch"),
        holding_horizon = "2-5 trading days"
      )
    } else {
      list(
        selected_strategy = "mean_reversion",
        target_position_pct = 30,
        confidence = 64,
        rebalance_reason = "Short-term weakness supports a smaller mean-reversion position.",
        signal_scores = list(trend_following = 0.30, mean_reversion = 0.82, event_momentum = 0.28, defensive_risk_off = 0.25),
        risk_flags = c("oversold"),
        holding_horizon = "1-3 trading days"
      )
    }
  }

  result <- run_backtest(
    symbol = "TEST",
    start_date = min(prices$Date) + 2,
    end_date = max(prices$Date),
    initial_capital = 10000,
    lookback_days = 10,
    price_data = prices,
    news_data = news,
    decision_fn = stub_agent,
    sleep_sec = 0,
    persist = FALSE,
    strategy_mode = "auto"
  )

  checks <- c(
    isTRUE(result$ok),
    isTRUE(result$metrics$trade_count >= 3),
    isTRUE(result$metrics$strategy_diversity >= 2),
    isTRUE(all(vapply(result$decision_log, function(x) !is.null(x$target_position_pct), logical(1)))),
    isTRUE(all(vapply(result$decision_log, function(x) nzchar(x$selected_strategy %||% ""), logical(1))))
  )

  passed <- sum(checks)
  total <- length(checks)
  if (!all(checks)) {
    if (!checks[[1]]) cat("  FAIL: Activity backtest run failed\n")
    if (!checks[[2]]) cat("  FAIL: Trade count stayed too low\n")
    if (!checks[[3]]) cat("  FAIL: Strategy diversity stayed below target\n")
    if (!checks[[4]]) cat("  FAIL: Target position percentage missing from decision log\n")
    if (!checks[[5]]) cat("  FAIL: Selected strategy missing from decision log\n")
  }

  cat(sprintf("Backtest Activity: %d/%d passed (%.0f%%)\n\n", passed, total, passed / total * 100))
  list(name = "backtest_activity", passed = passed, total = total)
}

test_trade_execution <- function() {
  cat("=== Trade Execution Tests ===\n")
  passed <- 0L
  total <- 0L

  total <- total + 1L
  portfolio <- execute_trade(list(cash = 1000, shares = 0L), list(action = "BUY", shares = 100L), price = 50)
  if (portfolio$cash >= 0) passed <- passed + 1L else cat("  FAIL: Cash went negative after buy\n")

  total <- total + 1L
  portfolio <- execute_trade(list(cash = 1000, shares = 5L), list(action = "SELL", shares = 10L), price = 50)
  if (portfolio$shares >= 0) passed <- passed + 1L else cat("  FAIL: Shares went negative after sell\n")

  total <- total + 1L
  portfolio <- execute_trade(list(cash = 1000, shares = 5L), list(action = "HOLD", shares = 0L), price = 50)
  if (identical(portfolio, list(cash = 1000, shares = 5L))) passed <- passed + 1L else cat("  FAIL: HOLD modified the portfolio\n")

  total <- total + 1L
  portfolio <- execute_trade(list(cash = 10000, shares = 0L), list(action = "BUY", shares = 10L), price = 100, fee_rate = 0.001)
  expected_cash <- 10000 - 10 * 100 * 1.001
  if (abs(portfolio$cash - expected_cash) < 0.01) passed <- passed + 1L else cat("  FAIL: Fee handling is incorrect\n")

  cat(sprintf("Trade Execution: %d/%d passed (%.0f%%)\n\n", passed, total, passed / total * 100))
  list(name = "trade_execution", passed = passed, total = total)
}

test_metrics <- function() {
  cat("=== Metrics Tests ===\n")
  passed <- 0L
  total <- 0L

  total <- total + 1L
  flat_curve <- list(list(date = "d1", equity = 10000), list(date = "d2", equity = 10000))
  metrics <- compute_backtest_metrics(flat_curve, 10000)
  if (metrics$total_return_pct == 0) passed <- passed + 1L else cat("  FAIL: Flat curve should return 0%\n")

  total <- total + 1L
  up_curve <- list(list(date = "d1", equity = 10000), list(date = "d2", equity = 10500))
  metrics <- compute_backtest_metrics(up_curve, 10000)
  if (metrics$total_return_pct == 5) passed <- passed + 1L else cat("  FAIL: Positive return computation is incorrect\n")

  total <- total + 1L
  dd_curve <- list(
    list(date = "d1", equity = 10000),
    list(date = "d2", equity = 11000),
    list(date = "d3", equity = 9000),
    list(date = "d4", equity = 9500)
  )
  metrics <- compute_backtest_metrics(dd_curve, 10000)
  if (abs(metrics$max_drawdown_pct - ((9000 - 11000) / 11000 * 100)) < 0.1) {
    passed <- passed + 1L
  } else {
    cat("  FAIL: Max drawdown computation is incorrect\n")
  }

  cat(sprintf("Metrics: %d/%d passed (%.0f%%)\n\n", passed, total, passed / total * 100))
  list(name = "metrics", passed = passed, total = total)
}

test_result_persistence <- function() {
  cat("=== Persistence Tests ===\n")
  passed <- 0L
  total <- 0L

  old_dir <- Sys.getenv("SHINY_APP_DIR", "")
  on.exit({
    if (nzchar(old_dir)) Sys.setenv(SHINY_APP_DIR = old_dir) else Sys.unsetenv("SHINY_APP_DIR")
  }, add = TRUE)
  Sys.setenv(SHINY_APP_DIR = tempdir())

  payload <- list(ok = TRUE, ticker = "TEST", generated_at = "2026-04-11T00:00:00Z")
  write_latest_result_json("latest_backtest_TEST.json", payload)
  restored <- load_latest_backtest("TEST")
  total <- total + 1L
  if (!is.null(restored) && identical(restored$ticker, "TEST")) passed <- passed + 1L else cat("  FAIL: Backtest JSON persistence failed\n")

  summary_payload <- save_latest_news_summary(data.frame(
    Symbol = "TEST",
    Title = "Headline",
    Source = "UnitTest",
    Published = as.numeric(as.POSIXct("2026-04-11 12:00:00", tz = "UTC")),
    Summary = "Summary",
    Url = "https://example.com",
    stringsAsFactors = FALSE
  ))
  restored_summary <- load_latest_news_summary()
  total <- total + 1L
  if (!is.null(summary_payload) && !is.null(restored_summary)) passed <- passed + 1L else cat("  FAIL: News summary persistence failed\n")

  refresh_payload <- save_latest_refresh_status(list(
    status = "success",
    started_at = "2026-04-11T12:00:00Z",
    ended_at = "2026-04-11T12:05:00Z",
    price_source_counts = list(finnhub = 3, yahoo_fallback = 5)
  ))
  restored_refresh <- load_latest_refresh_status()
  total <- total + 1L
  if (!is.null(refresh_payload) && !is.null(restored_refresh) && identical(restored_refresh$status, "success")) {
    passed <- passed + 1L
  } else {
    cat("  FAIL: Refresh status persistence failed\n")
  }

  total <- total + 1L
  first_lock <- acquire_refresh_lock()
  second_lock <- acquire_refresh_lock()
  release_refresh_lock()
  if (isTRUE(first_lock) && !isTRUE(second_lock)) passed <- passed + 1L else cat("  FAIL: Refresh lock behavior is incorrect\n")

  cat(sprintf("Persistence: %d/%d passed (%.0f%%)\n\n", passed, total, passed / total * 100))
  list(name = "persistence", passed = passed, total = total)
}

test_cache_helpers <- function() {
  cat("=== Cache Helper Tests ===\n")
  passed <- 0L
  total <- 0L

  cache_key <- paste0("unit_cache_", Sys.getpid(), "_", format(Sys.time(), "%H%M%S"))
  cache_file <- api_cache_file("tests", cache_key)
  if (file.exists(cache_file)) unlink(cache_file, force = TRUE)
  on.exit({
    if (file.exists(cache_file)) unlink(cache_file, force = TRUE)
  }, add = TRUE)

  loader_calls <- 0L
  first <- with_disk_cache(
    namespace = "tests",
    key = cache_key,
    max_age_secs = 3600,
    loader = function() {
      loader_calls <<- loader_calls + 1L
      list(value = 42L)
    },
    validate = function(x) is.list(x) && identical(x$value, 42L)
  )
  second <- with_disk_cache(
    namespace = "tests",
    key = cache_key,
    max_age_secs = 3600,
    loader = function() {
      loader_calls <<- loader_calls + 1L
      list(value = 99L)
    },
    validate = function(x) is.list(x) && !is.null(x$value)
  )

  total <- total + 1L
  if (identical(first$value, 42L) && identical(second$value, 42L) && identical(loader_calls, 1L)) {
    passed <- passed + 1L
  } else {
    cat("  FAIL: Disk cache did not return the cached payload\n")
  }

  old_pref <- Sys.getenv("PREFERRED_PRICE_SOURCE", unset = NA_character_)
  old_tiingo <- Sys.getenv("TIINGO_API_TOKEN", unset = NA_character_)
  old_finnhub <- Sys.getenv("FINNHUB_API_KEY", unset = NA_character_)
  on.exit({
    if (is.na(old_pref)) Sys.unsetenv("PREFERRED_PRICE_SOURCE") else Sys.setenv(PREFERRED_PRICE_SOURCE = old_pref)
    if (is.na(old_tiingo)) Sys.unsetenv("TIINGO_API_TOKEN") else Sys.setenv(TIINGO_API_TOKEN = old_tiingo)
    if (is.na(old_finnhub)) Sys.unsetenv("FINNHUB_API_KEY") else Sys.setenv(FINNHUB_API_KEY = old_finnhub)
  }, add = TRUE)

  Sys.setenv(PREFERRED_PRICE_SOURCE = "finnhub", TIINGO_API_TOKEN = "tiingo-test", FINNHUB_API_KEY = "finnhub-test")
  total <- total + 1L
  if (identical(preferred_price_source(), "finnhub")) {
    passed <- passed + 1L
  } else {
    cat("  FAIL: Explicit preferred price source override was ignored\n")
  }

  Sys.setenv(PREFERRED_PRICE_SOURCE = "auto", TIINGO_API_TOKEN = "tiingo-test")
  Sys.unsetenv("FINNHUB_API_KEY")
  total <- total + 1L
  if (identical(preferred_price_source(), "tiingo")) {
    passed <- passed + 1L
  } else {
    cat("  FAIL: Auto preferred price source did not favor Tiingo\n")
  }

  Sys.unsetenv("TIINGO_API_TOKEN")
  total <- total + 1L
  if (identical(preferred_price_source(), "yahoo")) {
    passed <- passed + 1L
  } else {
    cat("  FAIL: Auto preferred price source did not fall back to Yahoo\n")
  }

  cat(sprintf("Cache Helpers: %d/%d passed (%.0f%%)\n\n", passed, total, passed / total * 100))
  list(name = "cache_helpers", passed = passed, total = total)
}

run_all_tests <- function() {
  cat("╔══════════════════════════════════════╗\n")
  cat("║  Market Insight Studio - Test Suite  ║\n")
  cat("╚══════════════════════════════════════╝\n\n")

  results <- list(
    test_agent_schema(),
    test_trade_execution(),
    test_metrics(),
    test_backtest_mask(),
    test_backtest_activity(),
    test_result_persistence(),
    test_cache_helpers()
  )

  if (file.exists("key_financials.csv") && file.exists("news_archive.csv")) {
    results[[length(results) + 1L]] <- test_rag_dataset_freshness()
    results[[length(results) + 1L]] <- test_rag_retrieval()
  } else {
    cat("Skipping RAG retrieval tests because the exported CSV files are unavailable.\n\n")
  }

  total_passed <- sum(vapply(results, function(x) x$passed, numeric(1)))
  total_tests <- sum(vapply(results, function(x) x$total, numeric(1)))

  cat("╔══════════════════════════════════════╗\n")
  cat("║               SUMMARY                ║\n")
  cat("╚══════════════════════════════════════╝\n")
  cat(sprintf("\nOverall: %d/%d passed (%.0f%%)\n", total_passed, total_tests,
              if (total_tests > 0) total_passed / total_tests * 100 else 0))

  invisible(results)
}
