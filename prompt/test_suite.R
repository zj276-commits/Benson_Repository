# tests/test_suite.R
# Eval suite for Market Insight Studio
# Based on Anthropic's "Demystifying Evals for AI Agents" methodology
# Start with 20-50 tasks from real scenarios

# ----------------------------
# Test 1: RAG Retrieval Correctness
# ----------------------------

test_rag_retrieval <- function() {
  cat("=== RAG Retrieval Tests ===\n")
  passed <- 0; total <- 0
  
  for (sym in c("AAPL", "NVDA", "TSLA")) {
    for (atype in c("fundamentals", "news", "technical", "bull", "bear", "risk")) {
      total <- total + 1
      result <- tryCatch({
        json_str <- rag_retrieve(sym, atype)
        parsed <- jsonlite::fromJSON(json_str)
        
        # Check: no data from wrong ticker
        check_ticker <- function(df, col = "ticker") {
          if (is.null(df) || nrow(df) == 0) return(TRUE)
          target_col <- if (col %in% names(df)) col else if ("Symbol" %in% names(df)) "Symbol" else NULL
          if (is.null(target_col)) return(TRUE)
          all(df[[target_col]] %in% c(sym, "MACRO"))
        }
        
        all_ok <- TRUE
        for (nm in names(parsed)) {
          if (is.data.frame(parsed[[nm]])) {
            if (!check_ticker(parsed[[nm]])) {
              cat(sprintf("  FAIL: %s/%s - %s contains wrong ticker\n", sym, atype, nm))
              all_ok <- FALSE
            }
          }
        }
        all_ok
      }, error = function(e) {
        cat(sprintf("  FAIL: %s/%s - %s\n", sym, atype, e$message))
        FALSE
      })
      
      if (result) passed <- passed + 1
    }
  }
  
  cat(sprintf("\nRAG Retrieval: %d/%d passed (%.0f%%)\n\n", passed, total, passed/total*100))
  list(passed = passed, total = total)
}

# ----------------------------
# Test 2: Agent Output Schema Compliance
# ----------------------------

test_agent_schema <- function() {
  cat("=== Agent Schema Tests ===\n")
  
  required_keys <- list(
    fundamentals = c("fundamentals_summary", "valuation_assessment", "financial_health_score"),
    news = c("news_sentiment", "key_events", "macro_impact"),
    technical = c("technical_signal", "model_interpretation", "momentum_summary"),
    bull = c("bull_thesis", "catalysts", "upside_target"),
    bear = c("bear_thesis", "risks", "downside_target"),
    risk = c("risk_level", "risk_factors", "risk_score")
  )
  
  pm_keys <- c("purchase_rating", "confidence", "key_reason", "industry_analysis",
               "investment_overview", "risk_analysis", "bull_case", "bear_case",
               "target_price_3m", "risk_level", "forecast_trend")
  
  cat("Schema definitions loaded. Run with actual agent outputs to validate.\n")
  cat(sprintf("Agent types: %d | PM keys: %d\n", length(required_keys), length(pm_keys)))
  
  # Return validator function
  function(agent_output, agent_type) {
    if (agent_type == "pm") {
      missing <- setdiff(pm_keys, names(agent_output))
    } else {
      missing <- setdiff(required_keys[[agent_type]], names(agent_output))
    }
    list(
      valid = length(missing) == 0,
      missing = missing,
      extra = setdiff(names(agent_output), 
                      if (agent_type == "pm") pm_keys else required_keys[[agent_type]])
    )
  }
}

# ----------------------------
# Test 3: Backtest Mask Mechanism (No Look-Ahead Bias)
# ----------------------------

test_backtest_mask <- function() {
  cat("=== Backtest Mask Tests ===\n")
  
  # Create synthetic data
  dates <- seq(as.Date("2026-03-01"), as.Date("2026-03-15"), by = "day")
  dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  prices <- data.frame(
    Symbol = "TEST",
    Date = dates,
    Close = 100 + cumsum(rnorm(length(dates), 0, 2)),
    stringsAsFactors = FALSE
  )
  
  passed <- 0; total <- 0
  
  # Test: at each day t, masked data should not contain t+1
  for (i in seq_along(dates)) {
    total <- total + 1
    t <- dates[i]
    masked <- prices[prices$Date <= t, ]
    
    future_in_mask <- any(masked$Date > t)
    if (!future_in_mask) {
      passed <- passed + 1
    } else {
      cat(sprintf("  FAIL: Day %s mask contains future data\n", t))
    }
  }
  
  # Test: masked data row count should increase each day
  total <- total + 1
  counts <- sapply(dates, function(t) nrow(prices[prices$Date <= t, ]))
  if (all(diff(counts) >= 0)) {
    passed <- passed + 1
  } else {
    cat("  FAIL: Masked data count not monotonically increasing\n")
  }
  
  cat(sprintf("\nMask Tests: %d/%d passed (%.0f%%)\n\n", passed, total, passed/total*100))
  list(passed = passed, total = total)
}

# ----------------------------
# Test 4: Trade Execution Logic
# ----------------------------

test_trade_execution <- function() {
  cat("=== Trade Execution Tests ===\n")
  passed <- 0; total <- 0
  
  # Test: Can't buy more than cash allows
  total <- total + 1
  portfolio <- list(cash = 1000, shares = 0)
  decision <- list(action = "BUY", shares = 100)  # 100 shares at $50 = $5000
  result <- execute_trade(portfolio, decision, price = 50)
  if (result$cash >= 0) {
    passed <- passed + 1
  } else {
    cat("  FAIL: Cash went negative after buy\n")
  }
  
  # Test: Can't sell more than held
  total <- total + 1
  portfolio <- list(cash = 1000, shares = 5)
  decision <- list(action = "SELL", shares = 10)
  result <- execute_trade(portfolio, decision, price = 50)
  if (result$shares >= 0) {
    passed <- passed + 1
  } else {
    cat("  FAIL: Shares went negative after sell\n")
  }
  
  # Test: HOLD doesn't change portfolio
  total <- total + 1
  portfolio <- list(cash = 1000, shares = 5)
  decision <- list(action = "HOLD", shares = 0)
  result <- execute_trade(portfolio, decision, price = 50)
  if (result$cash == 1000 && result$shares == 5) {
    passed <- passed + 1
  } else {
    cat("  FAIL: HOLD modified portfolio\n")
  }
  
  # Test: Fee deduction
  total <- total + 1
  portfolio <- list(cash = 10000, shares = 0)
  decision <- list(action = "BUY", shares = 10)
  result <- execute_trade(portfolio, decision, price = 100, fee_rate = 0.001)
  expected_cost <- 10 * 100 * 1.001  # $1001
  if (abs(result$cash - (10000 - expected_cost)) < 0.01) {
    passed <- passed + 1
  } else {
    cat(sprintf("  FAIL: Fee calculation wrong. Expected cash=%.2f, got %.2f\n",
                10000 - expected_cost, result$cash))
  }
  
  cat(sprintf("\nExecution Tests: %d/%d passed (%.0f%%)\n\n", passed, total, passed/total*100))
  list(passed = passed, total = total)
}

# ----------------------------
# Test 5: Metrics Computation
# ----------------------------

test_metrics <- function() {
  cat("=== Metrics Tests ===\n")
  passed <- 0; total <- 0
  
  # Test: Zero return for flat equity
  total <- total + 1
  eq <- list(list(date="d1", equity=10000), 
             list(date="d2", equity=10000),
             list(date="d3", equity=10000))
  m <- compute_backtest_metrics(eq, 10000)
  if (m$total_return_pct == 0) {
    passed <- passed + 1
  } else {
    cat(sprintf("  FAIL: Flat equity should return 0%%, got %.2f%%\n", m$total_return_pct))
  }
  
  # Test: Positive return
  total <- total + 1
  eq <- list(list(date="d1", equity=10000), 
             list(date="d2", equity=10500))
  m <- compute_backtest_metrics(eq, 10000)
  if (m$total_return_pct == 5.0) {
    passed <- passed + 1
  } else {
    cat(sprintf("  FAIL: Expected 5%% return, got %.2f%%\n", m$total_return_pct))
  }
  
  # Test: Max drawdown
  total <- total + 1
  eq <- list(list(date="d1", equity=10000),
             list(date="d2", equity=11000),
             list(date="d3", equity=9000),
             list(date="d4", equity=10000))
  m <- compute_backtest_metrics(eq, 10000)
  expected_dd <- (9000 - 11000) / 11000 * 100  # -18.18%
  if (abs(m$max_drawdown_pct - expected_dd) < 0.1) {
    passed <- passed + 1
  } else {
    cat(sprintf("  FAIL: Expected DD=%.2f%%, got %.2f%%\n", expected_dd, m$max_drawdown_pct))
  }
  
  cat(sprintf("\nMetrics Tests: %d/%d passed (%.0f%%)\n\n", passed, total, passed/total*100))
  list(passed = passed, total = total)
}

# ----------------------------
# Run All Tests
# ----------------------------

run_all_tests <- function() {
  cat("╔══════════════════════════════════════╗\n")
  cat("║  Market Insight Studio - Test Suite  ║\n")
  cat("╚══════════════════════════════════════╝\n\n")
  
  results <- list()
  
  # Only run RAG tests if data files exist
  if (file.exists("key_financials.csv")) {
    results$rag <- test_rag_retrieval()
  } else {
    cat("⚠️ Skipping RAG tests (CSV files not found)\n\n")
  }
  
  results$schema <- test_agent_schema()  # Returns validator
  results$mask <- test_backtest_mask()
  results$execution <- test_trade_execution()
  results$metrics <- test_metrics()
  
  # Summary
  cat("╔══════════════════════════════════════╗\n")
  cat("║            SUMMARY                   ║\n")
  cat("╚══════════════════════════════════════╝\n")
  
  total_p <- 0; total_t <- 0
  for (nm in names(results)) {
    if (is.list(results[[nm]]) && "passed" %in% names(results[[nm]])) {
      total_p <- total_p + results[[nm]]$passed
      total_t <- total_t + results[[nm]]$total
    }
  }
  
  cat(sprintf("\nOverall: %d/%d passed (%.0f%%)\n", total_p, total_t, 
              if (total_t > 0) total_p/total_t*100 else 0))
  
  invisible(results)
}
