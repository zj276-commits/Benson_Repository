# backtester.R
# AI Trading Agent Backtesting Engine with Mask Mechanism
# 
# Core principle: At each decision point t, the agent can ONLY see data up to t.
# No future data leakage (look-ahead bias = 0).

# ----------------------------
# Trading Agent Decision Call
# ----------------------------

build_trading_decision_prompt <- function(symbol, current_date, current_price, 
                                           portfolio, trend, gbm_params, 
                                           news_headlines, lookback_prices) {
  system_prompt <- paste0(
    "You are an AI trading agent. Today is ", format(current_date, "%B %d, %Y"), ".\n",
    "You manage a portfolio and must decide: BUY, SELL, or HOLD.\n\n",
    "STRICT RULES:\n",
    "- You can ONLY go long (no short selling).\n",
    "- Transaction cost: 0.1% per trade.\n",
    "- You cannot buy more shares than your cash allows.\n",
    "- You cannot sell more shares than you hold.\n",
    "- Base your decision ONLY on the data provided. Do NOT invent information.\n\n",
    "Return JSON with EXACTLY these keys:\n",
    "- action: one of 'BUY', 'SELL', 'HOLD'\n",
    "- shares: integer (0 if HOLD)\n",
    "- confidence: integer 0-100\n",
    "- reasoning: 2-3 sentences explaining your decision"
  )
  
  # Format recent price changes
  price_summary <- ""
  if (!is.null(lookback_prices) && nrow(lookback_prices) >= 2) {
    recent <- tail(lookback_prices, 10)
    changes <- diff(recent$Close) / head(recent$Close, -1) * 100
    price_summary <- paste0(
      "Recent price history (last ", nrow(recent), " days):\n",
      paste(sprintf("  %s: $%.2f", recent$Date, recent$Close), collapse = "\n"),
      "\nDaily changes: ", paste(sprintf("%.2f%%", changes), collapse = ", ")
    )
  }
  
  # Format GBM info
  gbm_text <- if (!is.null(gbm_params)) {
    sprintf("GBM Model (annualized): drift=%.4f, volatility=%.4f", 
            gbm_params$mu_annual, gbm_params$sigma_annual)
  } else "Quantitative model: insufficient data."
  
  # Format trend
  trend_text <- if (!is.null(trend)) {
    sprintf("1d change: %s | 7d change: %s | 20d vol: %s",
            fmt_pct(trend$change1d_pct), 
            fmt_pct(trend$change7d_pct),
            ifelse(is.na(trend$mean_20d_volatility), "N/A", 
                   paste0(trend$mean_20d_volatility, "%")))
  } else "Trend data unavailable."
  
  # Format news (only headlines, compressed)
  news_text <- if (!is.null(news_headlines) && nrow(news_headlines) > 0) {
    top <- head(news_headlines, 8)
    paste(sprintf("- [%s] %s", top$Symbol, top$Title), collapse = "\n")
  } else "No recent news available."
  
  user_prompt <- paste0(
    "=== PORTFOLIO STATUS ===\n",
    sprintf("Cash: $%.2f | Shares held: %d | Position value: $%.2f | Total equity: $%.2f\n\n",
            portfolio$cash, portfolio$shares, 
            portfolio$shares * current_price,
            portfolio$cash + portfolio$shares * current_price),
    "=== CURRENT MARKET ===\n",
    sprintf("Symbol: %s | Price: $%.2f | Date: %s\n", 
            symbol, current_price, format(current_date, "%Y-%m-%d")),
    trend_text, "\n\n",
    "=== PRICE HISTORY ===\n", price_summary, "\n\n",
    "=== QUANTITATIVE SIGNALS ===\n", gbm_text, "\n\n",
    "=== NEWS (published before today) ===\n", news_text, "\n\n",
    "What is your trading decision?"
  )
  
  list(system = system_prompt, user = user_prompt)
}

call_trading_agent <- function(symbol, current_date, current_price, portfolio,
                                trend, gbm_params, news_headlines, lookback_prices,
                                api_key = Sys.getenv("OPENAI_API_KEY")) {
  if (!nzchar(api_key)) {
    return(list(action = "HOLD", shares = 0, confidence = 0, 
                reasoning = "No API key available."))
  }
  
  prompts <- build_trading_decision_prompt(
    symbol, current_date, current_price, portfolio,
    trend, gbm_params, news_headlines, lookback_prices
  )
  
  result <- call_openai_text(prompts$system, prompts$user, api_key, max_tokens = 500)
  
  if (is.null(result)) {
    return(list(action = "HOLD", shares = 0, confidence = 0,
                reasoning = "API call failed, defaulting to HOLD."))
  }
  
  # Validate and sanitize
  action <- toupper(result$action %||% "HOLD")
  if (!action %in% c("BUY", "SELL", "HOLD")) action <- "HOLD"
  
  shares <- as.integer(result$shares %||% 0)
  if (is.na(shares) || shares < 0) shares <- 0
  
  # Enforce portfolio constraints
  if (action == "BUY") {
    max_affordable <- floor(portfolio$cash * 0.999 / current_price)  # 0.1% fee
    shares <- min(shares, max_affordable)
  } else if (action == "SELL") {
    shares <- min(shares, portfolio$shares)
  }
  
  if (shares == 0 && action != "HOLD") action <- "HOLD"
  
  list(
    action = action,
    shares = shares,
    confidence = as.integer(result$confidence %||% 50),
    reasoning = result$reasoning %||% ""
  )
}

# ----------------------------
# Trade Execution (Simulated)
# ----------------------------

execute_trade <- function(portfolio, decision, price, fee_rate = 0.001) {
  action <- decision$action
  shares <- decision$shares
  
  if (action == "BUY" && shares > 0) {
    cost <- shares * price * (1 + fee_rate)
    if (cost > portfolio$cash) {
      # Adjust down
      shares <- floor(portfolio$cash / (price * (1 + fee_rate)))
      cost <- shares * price * (1 + fee_rate)
    }
    if (shares > 0) {
      portfolio$cash <- portfolio$cash - cost
      portfolio$shares <- portfolio$shares + shares
    }
  } else if (action == "SELL" && shares > 0) {
    shares <- min(shares, portfolio$shares)
    revenue <- shares * price * (1 - fee_rate)
    portfolio$cash <- portfolio$cash + revenue
    portfolio$shares <- portfolio$shares - shares
  }
  
  portfolio
}

# ----------------------------
# Backtest Metrics
# ----------------------------

compute_backtest_metrics <- function(equity_curve, initial_capital) {
  equities <- sapply(equity_curve, function(x) x$equity)
  n <- length(equities)
  if (n < 2) return(list(total_return_pct = 0, sharpe_ratio = 0, 
                          max_drawdown_pct = 0, win_rate = 0))
  
  total_return <- (tail(equities, 1) - initial_capital) / initial_capital * 100
  
  daily_returns <- diff(equities) / head(equities, -1)
  sharpe <- if (sd(daily_returns) > 0) {
    mean(daily_returns) / sd(daily_returns) * sqrt(252)
  } else 0
  
  cummax_eq <- cummax(equities)
  drawdowns <- (equities - cummax_eq) / cummax_eq * 100
  max_dd <- min(drawdowns)
  
  wins <- sum(daily_returns > 0)
  win_rate <- wins / length(daily_returns)
  
  list(
    total_return_pct = round(total_return, 2),
    sharpe_ratio = round(sharpe, 2),
    max_drawdown_pct = round(max_dd, 2),
    win_rate = round(win_rate, 2)
  )
}

# ----------------------------
# Main Backtest Runner
# ----------------------------

run_backtest <- function(symbol, start_date, end_date, 
                          initial_capital = 10000,
                          lookback_days = 90,
                          on_progress = NULL) {
  
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Fetch all data (will be masked per day)
  api_key <- Sys.getenv("FINNHUB_API_KEY")
  data_start <- start_date - lookback_days
  
  if (is.function(on_progress)) on_progress(0.05, "Fetching historical data...")
  
  all_prices <- fetch_finnhub_candles(symbol, api_key, 
                                       days_back = as.integer(end_date - data_start))
  if (is.null(all_prices) || nrow(all_prices) < 5) {
    return(list(ok = FALSE, error = "Insufficient price data for backtest."))
  }
  
  # Also fetch SPY for SIM
  spy_prices <- fetch_finnhub_candles("SPY", api_key, 
                                       days_back = as.integer(end_date - data_start))
  
  # Load news archive
  app_dir <- get_export_dir()
  all_news <- tryCatch({
    nf <- file.path(app_dir, "news_archive.csv")
    if (file.exists(nf)) read.csv(nf, stringsAsFactors = FALSE) else NULL
  }, error = function(e) NULL)
  
  # Get trading days in window
  all_prices <- all_prices[order(all_prices$Date), ]
  trading_days <- all_prices$Date[all_prices$Date >= start_date & 
                                   all_prices$Date <= end_date]
  
  if (length(trading_days) < 2) {
    return(list(ok = FALSE, error = "Not enough trading days in window."))
  }
  
  # Initialize
  portfolio <- list(cash = initial_capital, shares = 0L)
  equity_curve <- list()
  trades <- list()
  
  # Day 0: record initial equity
  first_price <- all_prices$Close[all_prices$Date == trading_days[1]]
  equity_curve[[1]] <- list(
    date = as.character(trading_days[1]),
    equity = initial_capital
  )
  
  if (is.function(on_progress)) on_progress(0.10, "Starting backtest loop...")
  
  for (i in seq_along(trading_days)) {
    t <- trading_days[i]
    
    # === MASK: only data up to t ===
    masked_prices <- all_prices[all_prices$Date <= t, ]
    current_price <- tail(masked_prices$Close, 1)
    
    # Masked news
    masked_news <- NULL
    if (!is.null(all_news) && "Published" %in% names(all_news)) {
      t_unix <- as.numeric(as.POSIXct(paste0(t, " 23:59:59"), tz = "UTC"))
      masked_news <- all_news[!is.na(all_news$Published) & 
                               all_news$Published <= t_unix, ]
      # Filter by symbol + MACRO
      if (nrow(masked_news) > 0) {
        masked_news <- masked_news[masked_news$Symbol %in% c(symbol, "MACRO"), ]
        masked_news <- masked_news[order(masked_news$Published, decreasing = TRUE), ]
        masked_news <- head(masked_news, 15)
      }
    }
    
    # Compute models on masked data only
    gbm_params <- estimate_gbm_params(masked_prices, max_lookback = 252)
    trend <- compute_ohlc_metrics(masked_prices, lookback = nrow(masked_prices))
    
    # Agent decision
    if (is.function(on_progress)) {
      on_progress(0.10 + 0.80 * (i / length(trading_days)),
                  sprintf("Day %d/%d: Agent deciding for %s...", 
                          i, length(trading_days), format(t, "%m-%d")))
    }
    
    decision <- call_trading_agent(
      symbol = symbol,
      current_date = t,
      current_price = current_price,
      portfolio = portfolio,
      trend = trend,
      gbm_params = gbm_params,
      news_headlines = masked_news,
      lookback_prices = tail(masked_prices, 20)
    )
    
    # Execute
    portfolio <- execute_trade(portfolio, decision, current_price)
    
    # Record
    current_equity <- portfolio$cash + portfolio$shares * current_price
    equity_curve[[length(equity_curve) + 1]] <- list(
      date = as.character(t),
      equity = round(current_equity, 2)
    )
    
    if (decision$action != "HOLD") {
      trades[[length(trades) + 1]] <- list(
        date = as.character(t),
        action = decision$action,
        shares = decision$shares,
        price = round(current_price, 2),
        reasoning = decision$reasoning,
        confidence = decision$confidence,
        portfolio_after = list(
          cash = round(portfolio$cash, 2),
          shares = portfolio$shares,
          equity = round(current_equity, 2)
        )
      )
    }
    
    # Rate limit
    Sys.sleep(0.5)
  }
  
  # Final metrics
  metrics <- compute_backtest_metrics(equity_curve, initial_capital)
  
  # Build result
  result <- list(
    ok = TRUE,
    ticker = symbol,
    start_date = as.character(start_date),
    end_date = as.character(end_date),
    initial_capital = initial_capital,
    trading_days = length(trading_days),
    trades = trades,
    equity_curve = equity_curve,
    metrics = metrics,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  
  # Save (overwrite previous)
  results_dir <- file.path(get_export_dir(), "results")
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
  
  out_path <- file.path(results_dir, paste0("latest_backtest_", symbol, ".json"))
  jsonlite::write_json(result, out_path, auto_unbox = TRUE, pretty = TRUE)
  message("[Backtest] Saved to ", out_path)
  
  result
}

# ----------------------------
# Load latest backtest for display
# ----------------------------

load_latest_backtest <- function(symbol) {
  results_dir <- file.path(get_export_dir(), "results")
  path <- file.path(results_dir, paste0("latest_backtest_", symbol, ".json"))
  if (!file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path), error = function(e) NULL)
}

# ----------------------------
# Look-Ahead Bias Validator (Eval)
# ----------------------------

validate_no_lookahead <- function(backtest_result, all_prices) {
  if (!isTRUE(backtest_result$ok)) return(list(passed = FALSE, reason = "Backtest failed"))
  
  violations <- 0
  for (trade in backtest_result$trades) {
    trade_date <- as.Date(trade$date)
    # Check: does reasoning mention dates after trade_date?
    future_dates <- all_prices$Date[all_prices$Date > trade_date]
    for (fd in as.character(future_dates)) {
      if (grepl(fd, trade$reasoning, fixed = TRUE)) {
        violations <- violations + 1
        message("[VIOLATION] Trade on ", trade$date, " references future date ", fd)
      }
    }
  }
  
  list(passed = violations == 0, violations = violations)
}
