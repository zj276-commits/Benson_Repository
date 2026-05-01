# backtester.R
# Mask-safe AI trading agent backtesting engine

get_results_dir <- function() {
  results_dir <- file.path(get_export_dir(), "results")
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
  results_dir
}

write_latest_result_json <- function(filename, payload) {
  path <- file.path(get_results_dir(), filename)
  jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE, na = "null")
  path
}

read_latest_result_json <- function(filename) {
  path <- file.path(get_results_dir(), filename)
  if (!file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
}

coerce_record_df <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) return(x)
  if (is.list(x) && length(x) == 0) return(data.frame())
  tryCatch(dplyr::bind_rows(x), error = function(e) NULL)
}

normalize_strategy_mode <- function(x) {
  mode <- tolower(trimws(as.character(x %||% "auto")))
  valid <- c("auto", "trend_only", "mean_reversion_only", "event_momentum_only", "defensive_only")
  if (!mode %in% valid) "auto" else mode
}

backtest_strategy_catalog <- function() {
  list(
    trend_following = "Ride persistent upside momentum with controlled add-ons.",
    mean_reversion = "Buy oversold weakness and fade short-term drawdowns.",
    event_momentum = "React to strong company-specific news and sentiment shifts.",
    defensive_risk_off = "Reduce exposure when volatility, drawdown, or news risk rises."
  )
}

clamp_pct_value <- function(x, lo = 0, hi = 100) {
  x <- suppressWarnings(as.numeric(x))
  if (!is.finite(x)) return(lo)
  max(lo, min(hi, x))
}

fmt_signal_value <- function(x, digits = 2, suffix = "") {
  if (!is.finite(as.numeric(x))) return("N/A")
  paste0(formatC(as.numeric(x), format = "f", digits = digits), suffix)
}

score_text_sentiment <- function(text_vec) {
  positive_terms <- c(
    "beat", "beats", "surge", "gain", "growth", "strong", "record", "partnership",
    "launch", "approval", "upgrade", "buy", "bullish", "rebound", "expands", "wins"
  )
  negative_terms <- c(
    "miss", "drop", "cuts", "cut", "lawsuit", "downgrade", "probe", "investigation",
    "delay", "tariff", "risk", "decline", "fall", "sell", "warning", "slump", "weak"
  )

  text_vec <- tolower(ifelse(is.na(text_vec), "", text_vec))
  vapply(text_vec, function(txt) {
    pos <- sum(vapply(positive_terms, function(term) grepl(term, txt, fixed = TRUE), logical(1)))
    neg <- sum(vapply(negative_terms, function(term) grepl(term, txt, fixed = TRUE), logical(1)))
    pos - neg
  }, numeric(1))
}

compute_short_signals <- function(price_df) {
  if (is.null(price_df) || nrow(price_df) < 3) {
    return(list(
      return_1d_pct = NA_real_,
      return_3d_pct = NA_real_,
      return_5d_pct = NA_real_,
      return_10d_pct = NA_real_,
      rsi_5 = NA_real_,
      rsi_14 = NA_real_,
      sma_5 = NA_real_,
      sma_10 = NA_real_,
      sma_20 = NA_real_,
      price_vs_sma10_pct = NA_real_,
      volume_ratio_20d = NA_real_,
      volatility_20d_pct = NA_real_,
      volatility_change_5d_pct = NA_real_,
      streak_days = 0L,
      drawdown_from_20d_high_pct = NA_real_
    ))
  }

  price_df <- price_df[order(price_df$Date), , drop = FALSE]
  close <- as.numeric(price_df$Close)
  volume <- as.numeric(price_df$Volume)
  n <- length(close)
  latest_close <- tail(close, 1)

  ret_pct <- function(k) {
    if (n <= k) return(NA_real_)
    base <- close[[n - k]]
    if (!is.finite(base) || base == 0) return(NA_real_)
    (latest_close - base) / base * 100
  }

  sma_5 <- if (n >= 5) mean(tail(close, 5), na.rm = TRUE) else NA_real_
  sma_10 <- if (n >= 10) mean(tail(close, 10), na.rm = TRUE) else NA_real_
  sma_20 <- if (n >= 20) mean(tail(close, 20), na.rm = TRUE) else NA_real_

  daily_returns <- diff(log(close))
  vol_20 <- if (length(daily_returns) >= 20) sd(tail(daily_returns, 20), na.rm = TRUE) * sqrt(252) * 100 else NA_real_
  prev_vol_20 <- if (length(daily_returns) >= 25) sd(tail(head(daily_returns, -5), 20), na.rm = TRUE) * sqrt(252) * 100 else NA_real_

  streak <- 0L
  if (n >= 2) {
    deltas <- diff(close)
    signs <- sign(deltas)
    last_non_zero <- tail(signs[signs != 0], 1)
    if (length(last_non_zero) == 1 && !is.na(last_non_zero) && last_non_zero != 0) {
      streak <- 0L
      for (s in rev(signs)) {
        if (s == last_non_zero) streak <- streak + 1L else break
      }
      streak <- as.integer(streak * last_non_zero)
    }
  }

  recent_high <- if (n >= 20) max(tail(close, 20), na.rm = TRUE) else max(close, na.rm = TRUE)
  drawdown <- if (is.finite(recent_high) && recent_high > 0) (latest_close / recent_high - 1) * 100 else NA_real_

  list(
    return_1d_pct = ret_pct(1),
    return_3d_pct = ret_pct(3),
    return_5d_pct = ret_pct(5),
    return_10d_pct = ret_pct(10),
    rsi_5 = if (n >= 6) as.numeric(tail(TTR::RSI(close, n = 5), 1)) else NA_real_,
    rsi_14 = if (n >= 15) as.numeric(tail(TTR::RSI(close, n = 14), 1)) else NA_real_,
    sma_5 = sma_5,
    sma_10 = sma_10,
    sma_20 = sma_20,
    price_vs_sma10_pct = if (is.finite(sma_10) && sma_10 != 0) (latest_close / sma_10 - 1) * 100 else NA_real_,
    volume_ratio_20d = if (n >= 20 && is.finite(mean(tail(volume, 20), na.rm = TRUE)) &&
      mean(tail(volume, 20), na.rm = TRUE) > 0) tail(volume, 1) / mean(tail(volume, 20), na.rm = TRUE) else NA_real_,
    volatility_20d_pct = vol_20,
    volatility_change_5d_pct = if (is.finite(vol_20) && is.finite(prev_vol_20) && prev_vol_20 > 0) {
      (vol_20 / prev_vol_20 - 1) * 100
    } else {
      NA_real_
    },
    streak_days = streak,
    drawdown_from_20d_high_pct = drawdown
  )
}

compute_news_signals <- function(news_headlines, decision_date = NULL) {
  if (is.null(news_headlines) || nrow(news_headlines) == 0) {
    return(list(
      article_count = 0L,
      company_article_count = 0L,
      macro_article_count = 0L,
      sentiment_today = 0,
      sentiment_prev = 0,
      sentiment_change = 0,
      event_impact_score = 0,
      high_impact_event_count = 0L,
      macro_risk_score = 0,
      dominant_event_direction = "neutral"
    ))
  }

  news_headlines <- news_headlines[order(news_headlines$Published, decreasing = TRUE), , drop = FALSE]
  text_blob <- paste(news_headlines$Title %||% "", news_headlines$Summary %||% "")
  article_scores <- score_text_sentiment(text_blob)
  event_terms <- c("earnings", "guidance", "partnership", "approval", "launch", "lawsuit", "investigation", "tariff", "regulation")
  macro_terms <- c("rate", "fed", "tariff", "inflation", "war", "geopolitical", "sanction", "macro")

  impact_hits <- vapply(text_blob, function(txt) {
    sum(vapply(event_terms, function(term) grepl(term, tolower(txt), fixed = TRUE), logical(1)))
  }, numeric(1))
  macro_hits <- vapply(text_blob, function(txt) {
    sum(vapply(macro_terms, function(term) grepl(term, tolower(txt), fixed = TRUE), logical(1)))
  }, numeric(1))

  published_dates <- as.Date(as.POSIXct(news_headlines$Published, origin = "1970-01-01", tz = "UTC"))
  today_date <- as.Date(decision_date %||% Sys.Date())
  today_idx <- published_dates == today_date
  prev_idx <- published_dates == (today_date - 1)

  sentiment_today <- if (any(today_idx)) mean(article_scores[today_idx], na.rm = TRUE) else mean(head(article_scores, min(3L, length(article_scores))), na.rm = TRUE)
  sentiment_prev <- if (any(prev_idx)) mean(article_scores[prev_idx], na.rm = TRUE) else 0
  total_sentiment <- mean(article_scores, na.rm = TRUE)

  list(
    article_count = nrow(news_headlines),
    company_article_count = sum(news_headlines$Symbol != "MACRO", na.rm = TRUE),
    macro_article_count = sum(news_headlines$Symbol == "MACRO", na.rm = TRUE),
    sentiment_today = round(sentiment_today, 3),
    sentiment_prev = round(sentiment_prev, 3),
    sentiment_change = round(sentiment_today - sentiment_prev, 3),
    event_impact_score = round(mean(abs(article_scores) + impact_hits, na.rm = TRUE), 3),
    high_impact_event_count = sum((abs(article_scores) + impact_hits) >= 2, na.rm = TRUE),
    macro_risk_score = round(mean(macro_hits + pmax(0, -article_scores), na.rm = TRUE), 3),
    dominant_event_direction = if (total_sentiment > 0.3) {
      "bullish"
    } else if (total_sentiment < -0.3) {
      "bearish"
    } else {
      "neutral"
    }
  )
}

compute_strategy_scores <- function(short_signals, news_signals, trend = NULL, gbm_params = NULL,
                                    strategy_mode = "auto") {
  trend_score <- 0.35 * clamp((short_signals$return_5d_pct %||% 0) / 8, 0, 1) +
    0.25 * clamp((short_signals$price_vs_sma10_pct %||% 0) / 4, 0, 1) +
    0.15 * clamp((short_signals$streak_days %||% 0) / 4, 0, 1) +
    0.10 * clamp((short_signals$volume_ratio_20d %||% 1) - 0.9, 0, 1) +
    0.15 * clamp((gbm_params$mu_annual %||% 0) / 0.25, 0, 1)

  reversion_score <- 0.45 * clamp((35 - (short_signals$rsi_14 %||% 50)) / 20, 0, 1) +
    0.30 * clamp((0 - (short_signals$return_3d_pct %||% 0)) / 6, 0, 1) +
    0.25 * clamp((0 - (short_signals$drawdown_from_20d_high_pct %||% 0)) / 12, 0, 1)

  event_score <- 0.35 * clamp(abs(news_signals$sentiment_change %||% 0) / 2, 0, 1) +
    0.30 * clamp((news_signals$event_impact_score %||% 0) / 3, 0, 1) +
    0.20 * clamp((news_signals$company_article_count %||% 0) / 6, 0, 1) +
    0.15 * clamp(abs(news_signals$sentiment_today %||% 0) / 2, 0, 1)

  defensive_score <- 0.30 * clamp((short_signals$volatility_20d_pct %||% 0) / 60, 0, 1) +
    0.25 * clamp((short_signals$volatility_change_5d_pct %||% 0) / 40, 0, 1) +
    0.25 * clamp((0 - (short_signals$drawdown_from_20d_high_pct %||% 0)) / 10, 0, 1) +
    0.20 * clamp((news_signals$macro_risk_score %||% 0) / 3, 0, 1)

  scores <- c(
    trend_following = clamp(trend_score, 0, 1),
    mean_reversion = clamp(reversion_score, 0, 1),
    event_momentum = clamp(event_score, 0, 1),
    defensive_risk_off = clamp(defensive_score, 0, 1)
  )

  mode <- normalize_strategy_mode(strategy_mode)
  if (mode != "auto") {
    forced_name <- switch(
      mode,
      trend_only = "trend_following",
      mean_reversion_only = "mean_reversion",
      event_momentum_only = "event_momentum",
      defensive_only = "defensive_risk_off",
      NULL
    )
    if (!is.null(forced_name)) {
      scores[names(scores) != forced_name] <- pmin(scores[names(scores) != forced_name], 0.25)
      scores[[forced_name]] <- max(scores[[forced_name]], 0.85)
    }
  }

  round(scores, 4)
}

format_recent_decision_memory <- function(decision_log, max_items = 3L) {
  logs <- tail(decision_log %||% list(), max_items)
  if (length(logs) == 0) return("No prior decision memory in this window.")

  paste(vapply(logs, function(entry) {
    paste0(
      entry$date %||% "-", " | strategy=", entry$selected_strategy %||% "unknown",
      " | action=", entry$action %||% "HOLD",
      " | target=", entry$target_position_pct %||% 0, "%",
      " | executed=", entry$executed_shares %||% 0, " shares",
      " | reason=", substr(entry$reasoning %||% "", 1, 160)
    )
  }, character(1)), collapse = "\n")
}

build_trading_decision_prompt <- function(symbol, current_date, current_price,
                                          portfolio, trend, gbm_params,
                                          news_headlines, lookback_prices,
                                          short_signals, news_signals, strategy_scores,
                                          decision_memory = NULL,
                                          strategy_mode = "auto",
                                          fee_rate = 0.001) {
  mode <- normalize_strategy_mode(strategy_mode)
  allowed_strategies <- names(backtest_strategy_catalog())
  forced_text <- switch(
    mode,
    trend_only = "You MUST use selected_strategy = trend_following.",
    mean_reversion_only = "You MUST use selected_strategy = mean_reversion.",
    event_momentum_only = "You MUST use selected_strategy = event_momentum.",
    defensive_only = "You MUST use selected_strategy = defensive_risk_off.",
    "You may choose the strongest strategy from the provided scores."
  )

  system_prompt <- paste0(
    "You are an AI trading agent making one masked daily decision inside a backtest.\n",
    "TODAY'S DATE: ", format(current_date, "%Y-%m-%d"), "\n\n",
    "STRICT RULES:\n",
    "- You can ONLY use the provided historical prices, structured signals, and masked news.\n",
    "- You can ONLY go long. No shorting, leverage, or derivatives.\n",
    "- Think in terms of target portfolio exposure, not just one-off trades.\n",
    "- Choose a strategy from: ", paste(allowed_strategies, collapse = ", "), ".\n",
    "- ", forced_text, "\n",
    "- Transaction cost is ", sprintf("%.2f%%", fee_rate * 100), " per trade.\n",
    "- Return JSON with EXACTLY these keys: selected_strategy, target_position_pct, confidence, rebalance_reason, signal_scores, risk_flags, holding_horizon.\n",
    "- target_position_pct must be an integer from 0 to 100.\n",
    "- confidence must be an integer from 0 to 100.\n",
    "- rebalance_reason must be 2-4 sentences grounded only in the provided data.\n",
    "- signal_scores must contain numeric scores for trend_following, mean_reversion, event_momentum, defensive_risk_off.\n",
    "- risk_flags must be a JSON array of 0-4 short strings.\n",
    "- holding_horizon must be a short string like '1-3 trading days' or '3-7 trading days'.\n",
    "- Be willing to rebalance if the visible evidence changed meaningfully from prior days."
  )

  price_text <- "Insufficient price history."
  if (!is.null(lookback_prices) && nrow(lookback_prices) > 0) {
    recent <- lookback_prices[order(lookback_prices$Date), ]
    recent <- tail(recent, min(15L, nrow(recent)))
    rows <- sprintf(
      "%s | Close $%.2f | Volume %s",
      as.character(recent$Date),
      recent$Close,
      formatC(recent$Volume, format = "f", digits = 0, big.mark = ",")
    )
    price_text <- paste(rows, collapse = "\n")
  }

  gbm_text <- if (!is.null(gbm_params)) {
    sprintf(
      "GBM drift %.4f annualized, volatility %.4f annualized, estimated from %d observations through %s.",
      gbm_params$mu_annual,
      gbm_params$sigma_annual,
      gbm_params$n_obs %||% 0L,
      as.character(gbm_params$last_date %||% current_date)
    )
  } else {
    "GBM signal unavailable because the masked history is too short."
  }

  trend_text <- if (!is.null(trend)) {
    sprintf(
      "1d %s | 7d %s | 30d %s | 20d annualized volatility %s",
      fmt_pct(trend$change1d_pct),
      fmt_pct(trend$change7d_pct),
      fmt_pct(trend$change30d_pct),
      ifelse(is.na(trend$mean_20d_volatility), "N/A", paste0(trend$mean_20d_volatility, "%"))
    )
  } else {
    "Trend metrics unavailable."
  }

  short_signal_text <- paste(
    sprintf("1D return: %s", fmt_signal_value(short_signals$return_1d_pct, 2, "%")),
    sprintf("3D return: %s", fmt_signal_value(short_signals$return_3d_pct, 2, "%")),
    sprintf("5D return: %s", fmt_signal_value(short_signals$return_5d_pct, 2, "%")),
    sprintf("RSI(5): %s", fmt_signal_value(short_signals$rsi_5, 1)),
    sprintf("RSI(14): %s", fmt_signal_value(short_signals$rsi_14, 1)),
    sprintf("Price vs SMA10: %s", fmt_signal_value(short_signals$price_vs_sma10_pct, 2, "%")),
    sprintf("Volume ratio vs 20D: %s", fmt_signal_value(short_signals$volume_ratio_20d, 2, "x")),
    sprintf("20D vol: %s", fmt_signal_value(short_signals$volatility_20d_pct, 1, "%")),
    sprintf("Vol change vs prior 5D: %s", fmt_signal_value(short_signals$volatility_change_5d_pct, 1, "%")),
    sprintf("Streak days: %s", short_signals$streak_days %||% 0L),
    sprintf("Drawdown from 20D high: %s", fmt_signal_value(short_signals$drawdown_from_20d_high_pct, 2, "%")),
    sep = "\n"
  )

  news_signal_text <- paste(
    sprintf("Article count: %s", news_signals$article_count %||% 0L),
    sprintf("Company articles: %s", news_signals$company_article_count %||% 0L),
    sprintf("Macro articles: %s", news_signals$macro_article_count %||% 0L),
    sprintf("Sentiment today: %s", fmt_signal_value(news_signals$sentiment_today, 2)),
    sprintf("Sentiment previous day: %s", fmt_signal_value(news_signals$sentiment_prev, 2)),
    sprintf("Sentiment change: %s", fmt_signal_value(news_signals$sentiment_change, 2)),
    sprintf("Event impact score: %s", fmt_signal_value(news_signals$event_impact_score, 2)),
    sprintf("High-impact event count: %s", news_signals$high_impact_event_count %||% 0L),
    sprintf("Macro risk score: %s", fmt_signal_value(news_signals$macro_risk_score, 2)),
    sprintf("Dominant event direction: %s", news_signals$dominant_event_direction %||% "neutral"),
    sep = "\n"
  )

  score_text <- paste(sprintf("%s: %.2f", names(strategy_scores), as.numeric(strategy_scores)), collapse = "\n")

  news_text <- "No relevant news published on or before this date."
  if (!is.null(news_headlines) && nrow(news_headlines) > 0) {
    news_rows <- head(news_headlines, min(8L, nrow(news_headlines)))
    published <- format(as.POSIXct(news_rows$Published, origin = "1970-01-01", tz = "UTC"),
      "%Y-%m-%d %H:%M UTC")
    news_text <- paste(sprintf("[%s] %s | %s", published, news_rows$Symbol, news_rows$Title), collapse = "\n")
  }

  current_equity <- portfolio$cash + portfolio$shares * current_price
  current_position_pct <- if (current_equity > 0) 100 * (portfolio$shares * current_price) / current_equity else 0

  user_prompt <- paste0(
    "=== PORTFOLIO ===\n",
    sprintf(
      "Cash: $%.2f\nShares held: %d\nPosition value: $%.2f\nTotal equity: $%.2f\nCurrent position pct: %.1f%%\n\n",
      portfolio$cash,
      portfolio$shares,
      portfolio$shares * current_price,
      current_equity,
      current_position_pct
    ),
    "=== MARKET ===\n",
    sprintf("Symbol: %s\nCurrent price: $%.2f\nDecision date: %s\n%s\n\n", symbol, current_price, as.character(current_date), trend_text),
    "=== MASKED PRICE HISTORY ===\n", price_text, "\n\n",
    "=== SHORT-TERM SIGNALS ===\n", short_signal_text, "\n\n",
    "=== NEWS SIGNALS ===\n", news_signal_text, "\n\n",
    "=== STRATEGY ROUTER SCORES ===\n", score_text, "\n\n",
    "=== QUANT SIGNAL ===\n", gbm_text, "\n\n",
    "=== RECENT DECISION MEMORY ===\n", decision_memory %||% "No prior decision memory in this window.", "\n\n",
    "=== NEWS PUBLISHED ON OR BEFORE TODAY ===\n", news_text, "\n\n",
    "Respond with your trading decision."
  )

  list(system = system_prompt, user = user_prompt)
}

sanitize_trading_decision <- function(decision, portfolio, current_price, fee_rate = 0.001,
                                      strategy_mode = "auto") {
  confidence <- suppressWarnings(as.integer(decision$confidence %||% 50))
  if (is.na(confidence)) confidence <- 50L
  confidence <- max(0L, min(100L, confidence))

  reasoning <- trimws(as.character(decision$rebalance_reason %||% decision$reasoning %||% ""))
  if (!nzchar(reasoning)) reasoning <- "No reasoning returned by the agent."

  selected_strategy <- tolower(trimws(as.character(decision$selected_strategy %||% "")))
  if (!selected_strategy %in% names(backtest_strategy_catalog())) {
    selected_strategy <- "legacy_action"
  }

  signal_scores <- decision$signal_scores %||% list()
  if (is.data.frame(signal_scores)) signal_scores <- as.list(signal_scores[1, , drop = FALSE])
  if (!is.list(signal_scores)) signal_scores <- as.list(signal_scores)
  for (nm in names(backtest_strategy_catalog())) {
    signal_scores[[nm]] <- round(clamp(suppressWarnings(as.numeric(signal_scores[[nm]] %||% 0)), 0, 1), 4)
  }
  signal_scores <- signal_scores[names(backtest_strategy_catalog())]

  risk_flags <- decision$risk_flags %||% character(0)
  if (is.list(risk_flags)) risk_flags <- unlist(risk_flags, use.names = FALSE)
  risk_flags <- trimws(as.character(risk_flags))
  risk_flags <- risk_flags[nzchar(risk_flags)]
  if (length(risk_flags) > 4) risk_flags <- risk_flags[seq_len(4)]

  holding_horizon <- trimws(as.character(decision$holding_horizon %||% "1-3 trading days"))
  if (!nzchar(holding_horizon)) holding_horizon <- "1-3 trading days"

  has_target_pct <- !is.null(decision$target_position_pct)
  equity <- portfolio$cash + portfolio$shares * current_price
  target_position_pct <- clamp_pct_value(decision$target_position_pct %||% NA_real_)

  if (!has_target_pct || !is.finite(target_position_pct)) {
    action <- toupper(as.character(decision$action %||% "HOLD"))
    if (!action %in% c("BUY", "SELL", "HOLD")) action <- "HOLD"
    shares <- suppressWarnings(as.integer(decision$shares %||% 0))
    if (is.na(shares) || shares < 0) shares <- 0L

    if (action == "BUY") {
      max_affordable <- floor(portfolio$cash / (current_price * (1 + fee_rate)))
      shares <- min(shares, max(0L, max_affordable))
      target_shares <- portfolio$shares + shares
    } else if (action == "SELL") {
      shares <- min(shares, portfolio$shares)
      target_shares <- max(0L, portfolio$shares - shares)
    } else {
      shares <- 0L
      target_shares <- portfolio$shares
    }

    target_position_pct <- if (equity > 0) clamp_pct_value(100 * target_shares * current_price / equity) else 0
  } else {
    desired_value <- equity * target_position_pct / 100
    target_shares <- floor(desired_value / current_price)
    max_affordable <- floor(portfolio$cash / (current_price * (1 + fee_rate)))
    target_shares <- min(target_shares, portfolio$shares + max(0L, max_affordable))
    target_shares <- max(0L, target_shares)
    shares <- abs(target_shares - portfolio$shares)
    action <- if (target_shares > portfolio$shares) {
      "BUY"
    } else if (target_shares < portfolio$shares) {
      "SELL"
    } else {
      "HOLD"
    }
  }

  if (shares <= 0L) {
    action <- "HOLD"
    shares <- 0L
  }

  if (selected_strategy == "legacy_action") {
    selected_strategy <- switch(
      normalize_strategy_mode(strategy_mode),
      trend_only = "trend_following",
      mean_reversion_only = "mean_reversion",
      event_momentum_only = "event_momentum",
      defensive_only = "defensive_risk_off",
      if (action == "SELL") "defensive_risk_off" else if (action == "BUY") "trend_following" else "mean_reversion"
    )
  }

  list(
    action = action,
    shares = as.integer(shares),
    confidence = as.integer(confidence),
    reasoning = reasoning,
    selected_strategy = selected_strategy,
    target_position_pct = round(target_position_pct, 1),
    signal_scores = signal_scores,
    risk_flags = as.list(risk_flags),
    holding_horizon = holding_horizon
  )
}

call_trading_agent <- function(symbol, current_date, current_price, portfolio,
                               trend, gbm_params, news_headlines, lookback_prices,
                               short_signals = NULL,
                               news_signals = NULL,
                               strategy_scores = NULL,
                               decision_memory = NULL,
                               strategy_mode = "auto",
                               api_key = Sys.getenv("OPENAI_API_KEY"),
                               fee_rate = 0.001,
                               decision_fn = NULL,
                               max_tokens = 500,
                               model = "gpt-4o-mini") {
  decision_payload <- list(
    symbol = symbol,
    current_date = as.character(current_date),
    current_price = current_price,
    portfolio = portfolio,
    trend = trend,
    gbm_params = gbm_params,
    news_headlines = news_headlines,
    lookback_prices = lookback_prices,
    short_signals = short_signals,
    news_signals = news_signals,
    strategy_scores = strategy_scores,
    decision_memory = decision_memory,
    strategy_mode = strategy_mode
  )

  if (is.function(decision_fn)) {
    raw <- tryCatch(decision_fn(decision_payload), error = function(e) {
      list(action = "HOLD", shares = 0L, confidence = 0L, reasoning = conditionMessage(e))
    })
    if (is.character(raw) && length(raw) == 1L) {
      raw <- tryCatch(jsonlite::fromJSON(raw), error = function(e) NULL)
    }
    if (is.null(raw)) {
      raw <- list(action = "HOLD", shares = 0L, confidence = 0L, reasoning = "Decision function returned invalid output.")
    }
    result <- sanitize_trading_decision(raw, portfolio, current_price, fee_rate, strategy_mode = strategy_mode)
    result$parse_ok <- TRUE
    return(result)
  }

  if (!nzchar(api_key)) {
    return(list(
      action = "HOLD", shares = 0L, confidence = 0L,
      reasoning = "OPENAI_API_KEY missing. Defaulted to HOLD.",
      parse_ok = FALSE,
      selected_strategy = "defensive_risk_off",
      target_position_pct = 0
    ))
  }

  prompts <- build_trading_decision_prompt(
    symbol = symbol,
    current_date = current_date,
    current_price = current_price,
    portfolio = portfolio,
    trend = trend,
    gbm_params = gbm_params,
    news_headlines = news_headlines,
    lookback_prices = lookback_prices,
    short_signals = short_signals,
    news_signals = news_signals,
    strategy_scores = strategy_scores,
    decision_memory = decision_memory,
    strategy_mode = strategy_mode,
    fee_rate = fee_rate
  )

  raw <- NULL
  parse_ok <- FALSE
  for (attempt in 1:2) {
    raw <- call_openai_text(prompts$system, prompts$user, api_key, max_tokens = max_tokens, model = model)
    if (!is.null(raw)) {
      parse_ok <- TRUE
      break
    }
  }

  if (is.null(raw)) {
    return(list(
      action = "HOLD", shares = 0L, confidence = 0L,
      reasoning = "Trading agent response could not be parsed. Defaulted to HOLD.",
      parse_ok = FALSE,
      selected_strategy = "defensive_risk_off",
      target_position_pct = 0
    ))
  }

  result <- sanitize_trading_decision(raw, portfolio, current_price, fee_rate, strategy_mode = strategy_mode)
  result$parse_ok <- parse_ok
  result
}

execute_trade <- function(portfolio, decision, price, fee_rate = 0.001) {
  action <- decision$action %||% "HOLD"
  shares <- as.integer(decision$shares %||% 0)

  if (action == "BUY" && shares > 0L) {
    cost <- shares * price * (1 + fee_rate)
    if (cost > portfolio$cash) {
      shares <- floor(portfolio$cash / (price * (1 + fee_rate)))
      cost <- shares * price * (1 + fee_rate)
    }
    if (shares > 0L && is.finite(cost) && cost <= portfolio$cash) {
      portfolio$cash <- portfolio$cash - cost
      portfolio$shares <- portfolio$shares + shares
    }
  } else if (action == "SELL" && shares > 0L) {
    shares <- min(shares, portfolio$shares)
    revenue <- shares * price * (1 - fee_rate)
    if (shares > 0L && is.finite(revenue)) {
      portfolio$cash <- portfolio$cash + revenue
      portfolio$shares <- portfolio$shares - shares
    }
  }

  portfolio
}

build_buy_hold_curve <- function(trading_days, price_df, initial_capital, fee_rate = 0.001) {
  if (length(trading_days) == 0) return(NULL)
  start_price <- price_df$Close[match(trading_days[1], price_df$Date)]
  if (length(start_price) == 0 || is.na(start_price) || start_price <= 0) return(NULL)

  shares <- floor(initial_capital / (start_price * (1 + fee_rate)))
  cash_left <- initial_capital - shares * start_price * (1 + fee_rate)
  if (shares < 0) shares <- 0L
  curve <- vector("list", length(trading_days) + 1L)
  curve[[1]] <- list(date = as.character(trading_days[1]), equity = round(initial_capital, 2))

  for (i in seq_along(trading_days)) {
    px <- price_df$Close[match(trading_days[i], price_df$Date)]
    curve[[i + 1L]] <- list(
      date = as.character(trading_days[i]),
      equity = round(cash_left + shares * px, 2)
    )
  }
  curve
}

compute_decision_metrics <- function(decision_log) {
  logs <- decision_log %||% list()
  if (length(logs) == 0) {
    return(list(
      trade_count = 0L,
      active_decision_ratio = 0,
      strategy_diversity = 0L,
      max_hold_streak = 0L,
      avg_target_position_pct = 0,
      dominant_strategy = "none"
    ))
  }

  actions <- vapply(logs, function(x) toupper(x$action %||% "HOLD"), character(1))
  target_pcts <- vapply(logs, function(x) as.numeric(x$target_position_pct %||% 0), numeric(1))
  strategies <- vapply(logs, function(x) as.character(x$selected_strategy %||% "unknown"), character(1))
  executed <- vapply(logs, function(x) as.integer(x$executed_shares %||% 0L), integer(1))

  max_hold_streak <- 0L
  current_hold <- 0L
  for (action in actions) {
    if (identical(action, "HOLD")) {
      current_hold <- current_hold + 1L
      max_hold_streak <- max(max_hold_streak, current_hold)
    } else {
      current_hold <- 0L
    }
  }

  strategy_counts <- sort(table(strategies[strategies != "unknown"]), decreasing = TRUE)
  dominant_strategy <- if (length(strategy_counts) > 0) names(strategy_counts)[1] else "none"

  list(
    trade_count = sum(executed > 0L),
    active_decision_ratio = round(mean(executed > 0L), 2),
    strategy_diversity = length(unique(strategies[strategies != "unknown"])),
    max_hold_streak = as.integer(max_hold_streak),
    avg_target_position_pct = round(mean(target_pcts, na.rm = TRUE), 1),
    dominant_strategy = dominant_strategy
  )
}

compute_backtest_metrics <- function(equity_curve, initial_capital, baseline_curve = NULL) {
  equities <- vapply(equity_curve, function(x) as.numeric(x$equity %||% NA_real_), numeric(1))
  equities <- equities[is.finite(equities)]
  if (length(equities) < 2L) {
    return(list(
      total_return_pct = 0,
      sharpe_ratio = 0,
      max_drawdown_pct = 0,
      win_rate = 0,
      buy_hold_return_pct = if (is.null(baseline_curve)) NA_real_ else 0,
      alpha_vs_buy_hold_pct = if (is.null(baseline_curve)) NA_real_ else 0
    ))
  }

  total_return <- (tail(equities, 1) - initial_capital) / initial_capital * 100
  daily_returns <- diff(equities) / head(equities, -1)
  daily_returns <- daily_returns[is.finite(daily_returns)]

  sharpe <- if (length(daily_returns) >= 2L && sd(daily_returns) > 0) {
    mean(daily_returns) / sd(daily_returns) * sqrt(252)
  } else {
    0
  }

  cummax_eq <- cummax(equities)
  drawdowns <- (equities - cummax_eq) / cummax_eq * 100
  wins <- if (length(daily_returns) > 0) sum(daily_returns > 0) / length(daily_returns) else 0

  buy_hold_return <- NA_real_
  if (!is.null(baseline_curve) && length(baseline_curve) >= 2L) {
    bh_equity <- vapply(baseline_curve, function(x) as.numeric(x$equity %||% NA_real_), numeric(1))
    bh_equity <- bh_equity[is.finite(bh_equity)]
    if (length(bh_equity) >= 2L) {
      buy_hold_return <- (tail(bh_equity, 1) - initial_capital) / initial_capital * 100
    }
  }

  list(
    total_return_pct = round(total_return, 2),
    sharpe_ratio = round(sharpe, 2),
    max_drawdown_pct = round(min(drawdowns, na.rm = TRUE), 2),
    win_rate = round(wins, 2),
    buy_hold_return_pct = round(buy_hold_return, 2),
    alpha_vs_buy_hold_pct = round(total_return - buy_hold_return, 2)
  )
}

validate_no_lookahead <- function(backtest_result) {
  logs <- backtest_result$decision_log %||% list()
  if (length(logs) == 0) {
    return(list(passed = FALSE, violations = 1L, detail = "No decision log available."))
  }

  violations <- character(0)
  for (entry in logs) {
    decision_date <- as.Date(entry$date %||% NA_character_)
    audit <- entry$mask_audit %||% list()
    visible_price_end <- as.Date(audit$visible_price_end_date %||% NA_character_)
    visible_model_end <- as.Date(audit$model_input_end_date %||% NA_character_)
    news_cutoff <- suppressWarnings(as.POSIXct(audit$visible_news_max_time %||% NA_character_, tz = "UTC"))
    expected_cutoff <- as.POSIXct(paste0(entry$date, " 23:59:59"), tz = "UTC")

    if (!is.na(visible_price_end) && visible_price_end > decision_date) {
      violations <- c(violations, paste("Price mask leaked future data on", entry$date))
    }
    if (!is.na(visible_model_end) && visible_model_end > decision_date) {
      violations <- c(violations, paste("Model input leaked future data on", entry$date))
    }
    if (!is.na(news_cutoff) && !is.na(expected_cutoff) && news_cutoff > expected_cutoff) {
      violations <- c(violations, paste("News mask leaked future data on", entry$date))
    }
  }

  list(
    passed = length(violations) == 0L,
    violations = length(violations),
    detail = if (length(violations) == 0L) "All decision contexts were time-safe." else paste(violations, collapse = "; ")
  )
}

validate_trade_constraints <- function(backtest_result) {
  logs <- backtest_result$decision_log %||% list()
  if (length(logs) == 0) {
    return(list(passed = FALSE, violations = 1L, detail = "No decision log available."))
  }

  violations <- character(0)
  for (entry in logs) {
    after <- entry$portfolio_after %||% list()
    before <- entry$portfolio_before %||% list()
    if (isTRUE(is.finite(after$cash)) && after$cash < -1e-6) {
      violations <- c(violations, paste("Negative cash after", entry$date))
    }
    if (isTRUE(is.finite(after$shares)) && after$shares < 0) {
      violations <- c(violations, paste("Negative shares after", entry$date))
    }
    if (!is.null(before$shares) && !is.null(after$shares)) {
      delta <- as.integer(after$shares) - as.integer(before$shares)
      if (entry$action == "SELL" && delta > 0) {
        violations <- c(violations, paste("SELL increased shares on", entry$date))
      }
      if (entry$action == "BUY" && delta < 0) {
        violations <- c(violations, paste("BUY reduced shares on", entry$date))
      }
    }
  }

  list(
    passed = length(violations) == 0L,
    violations = length(violations),
    detail = if (length(violations) == 0L) "Cash and share constraints held for every decision." else paste(violations, collapse = "; ")
  )
}

build_backtest_quality_matrix <- function(backtest_result) {
  logs <- backtest_result$decision_log %||% list()
  parse_rate <- if (length(logs) == 0L) 0 else {
    mean(vapply(logs, function(x) isTRUE(x$parse_ok), logical(1)))
  }
  curve_points <- length(backtest_result$equity_curve %||% list())
  expected_points <- (backtest_result$trading_days %||% 0L) + 1L
  lookahead <- validate_no_lookahead(backtest_result)
  constraints <- validate_trade_constraints(backtest_result)
  min_trades <- if ((backtest_result$trading_days %||% 0L) >= 20L) 3L else if ((backtest_result$trading_days %||% 0L) >= 10L) 2L else 1L
  min_strategies <- if ((backtest_result$trading_days %||% 0L) >= 20L) 2L else 1L

  data.frame(
    dimension = c(
      "No Look-Ahead Bias",
      "Decision Schema Parse Rate",
      "Equity Curve Length",
      "Trade Constraints",
      "Trade Activity",
      "Strategy Diversity",
      "Performance vs Buy & Hold"
    ),
    status = c(
      if (lookahead$passed) "PASS" else "FAIL",
      if (parse_rate >= 0.95) "PASS" else "WARN",
      if (curve_points == expected_points) "PASS" else "FAIL",
      if (constraints$passed) "PASS" else "FAIL",
      if ((backtest_result$metrics$trade_count %||% 0L) >= min_trades) "PASS" else "WARN",
      if ((backtest_result$metrics$strategy_diversity %||% 0L) >= min_strategies) "PASS" else "WARN",
      if (!is.null(backtest_result$metrics$alpha_vs_buy_hold_pct) &&
            is.finite(backtest_result$metrics$alpha_vs_buy_hold_pct) &&
            backtest_result$metrics$alpha_vs_buy_hold_pct >= 0) "PASS" else "WARN"
    ),
    detail = c(
      lookahead$detail,
      sprintf("Parse success %.1f%% (%d/%d)", parse_rate * 100, round(parse_rate * length(logs)), length(logs)),
      sprintf("Equity curve has %d points; expected %d.", curve_points, expected_points),
      constraints$detail,
      sprintf(
        "Executed %d trades across %d decision days; active decision ratio %.0f%%.",
        backtest_result$metrics$trade_count %||% 0L,
        backtest_result$trading_days %||% 0L,
        100 * (backtest_result$metrics$active_decision_ratio %||% 0)
      ),
      sprintf(
        "Observed %d strategy types; dominant strategy %s.",
        backtest_result$metrics$strategy_diversity %||% 0L,
        backtest_result$metrics$dominant_strategy %||% "none"
      ),
      sprintf(
        "Strategy %.2f%% vs buy-and-hold %.2f%% (alpha %.2f%%).",
        backtest_result$metrics$total_return_pct %||% NA_real_,
        backtest_result$metrics$buy_hold_return_pct %||% NA_real_,
        backtest_result$metrics$alpha_vs_buy_hold_pct %||% NA_real_
      )
    ),
    stringsAsFactors = FALSE
  )
}

# ----------------------------
# Forward-Looking Outlook
# Uses GBM (already in models.R) to project price next N trading days,
# then derives base/bull/bear scenario stats + actionable signals.
# All numbers are deterministic and mask-safe (only uses hist <= end_date).
# ----------------------------
build_forward_outlook <- function(backtest_result, hist_prices = NULL, horizon_days = 30L) {
  end_date <- as.Date(backtest_result$end_date %||% NA)
  if (is.na(end_date)) return(NULL)

  hist <- hist_prices
  if (is.null(hist) || !"Close" %in% names(hist)) return(NULL)
  if ("Date" %in% names(hist)) hist$Date <- as.Date(hist$Date)
  hist <- hist[!is.na(hist$Date) & hist$Date <= end_date, , drop = FALSE]
  hist <- hist[order(hist$Date), ]
  if (nrow(hist) < 30L) return(NULL)

  gbm <- tryCatch(estimate_gbm_params(hist, max_lookback = 252L), error = function(e) NULL)
  if (is.null(gbm)) return(NULL)

  forecast_path <- tryCatch(
    gbm_forecast_path(gbm$s0, gbm$mu_annual, gbm$sigma_annual, horizon_days, gbm$last_date,
                      p_lo = 0.05, p_hi = 0.95),
    error = function(e) NULL
  )
  if (is.null(forecast_path) || nrow(forecast_path) < 2) return(NULL)

  s0 <- gbm$s0
  end_row <- forecast_path[nrow(forecast_path), , drop = FALSE]
  median_ret <- (end_row$Median - s0) / s0
  bull_ret <- (end_row$Hi - s0) / s0
  bear_ret <- (end_row$Lo - s0) / s0
  ann_vol <- gbm$sigma_annual
  horizon_vol <- ann_vol * sqrt(horizon_days / 252)

  pct <- function(x) sprintf("%+.1f%%", 100 * x)

  base_case <- list(
    label = "Base Case · Median path",
    forecast = sprintf("Price drifts toward %s by %s (median GBM trajectory).",
                       fmt_price(end_row$Median), format(end_row$Date, "%b %d")),
    return_band = sprintf("Expected return %s · 90%% confidence cone [%s, %s]",
                          pct(median_ret), pct(bear_ret), pct(bull_ret)),
    triggers = c(
      sprintf("Realized vol stays within %.1f%% (annualized).", 100 * ann_vol),
      "No earnings or macro shock during the window."
    ),
    actions = c(
      "Hold current allocation; rebalance only if drawdown breaches -7%.",
      "Re-run backtest weekly; promote to live only after 3 consecutive WARN-free quality reports."
    )
  )

  bull_case <- list(
    label = "Bull Case · 95th percentile path",
    forecast = sprintf("Tail-up path reaches %s (+%.1f%% vs today).",
                       fmt_price(end_row$Hi), 100 * bull_ret),
    triggers = c(
      "Positive earnings surprise, accelerating sector momentum, or favorable macro print.",
      sprintf("Confirmation: 5-day return > +%.1f%% AND streak >= 3 up-days.",
              100 * max(0.02, bull_ret / 6))
    ),
    actions = c(
      "Increase target position by +10pp (cap at 80% allocation).",
      "Tighten stop-loss to -3% from latest high to lock in gains."
    )
  )

  bear_case <- list(
    label = "Bear Case · 5th percentile path",
    forecast = sprintf("Tail-down path falls to %s (%.1f%% vs today).",
                       fmt_price(end_row$Lo), 100 * bear_ret),
    triggers = c(
      "Earnings miss, regulatory headline, or VIX spike above 25.",
      sprintf("Confirmation: 5-day return < %.1f%% OR streak >= 3 down-days.",
              100 * min(-0.02, bear_ret / 6))
    ),
    actions = c(
      "Cut position by 50% on first confirmation; rotate to cash.",
      "Switch Strategy Mode to Defensive only and re-run backtest before re-entry."
    )
  )

  watch_signals <- c(
    sprintf("90D realized volatility (now %.1f%% annualized)", 100 * ann_vol),
    "5-day return streak (sign + magnitude)",
    "News sentiment: 7-day MACRO + ticker-specific count",
    "Quality Check 'Performance vs Buy & Hold' validator (currently a leading indicator)"
  )

  list(
    horizon_days = horizon_days,
    horizon_label = sprintf("Next %d trading days (~%d weeks)", horizon_days, round(horizon_days / 5)),
    annualized_vol = ann_vol,
    horizon_vol = horizon_vol,
    forecast_path = forecast_path,
    base_case = base_case,
    bull_case = bull_case,
    bear_case = bear_case,
    watch_signals = watch_signals,
    method = "GBM (Geometric Brownian Motion) · 90% confidence cone · mask-safe (uses hist on/before end_date only)"
  )
}

build_backtest_explanation_fallback <- function(backtest_result) {
  logs <- backtest_result$decision_log %||% list()
  trades <- backtest_result$trades %||% list()
  metrics <- backtest_result$metrics %||% list()

  actions <- if (length(logs) > 0) vapply(logs, function(x) toupper(x$action %||% "HOLD"), character(1)) else character(0)
  action_counts <- table(factor(actions, levels = c("BUY", "SELL", "HOLD")))
  buy_count <- as.integer(action_counts[["BUY"]] %||% 0L)
  sell_count <- as.integer(action_counts[["SELL"]] %||% 0L)
  hold_count <- as.integer(action_counts[["HOLD"]] %||% 0L)
  trade_count <- length(trades)
  day_count <- length(logs)

  summary_title <- if (trade_count <= 1L) {
    "Sparse trading over a short masked window"
  } else {
    "Selective trading based on masked daily evidence"
  }

  executive_summary <- paste0(
    "This run covered ", backtest_result$start_date %||% "-", " to ", backtest_result$end_date %||% "-",
    " with ", day_count, " masked decision day", if (day_count == 1) "" else "s",
    ". The strategy executed ", trade_count, " trade", if (trade_count == 1) "" else "s",
    " and finished at ", fmt_num(as.numeric(metrics$total_return_pct %||% 0), 2), "% versus ",
    fmt_num(as.numeric(metrics$buy_hold_return_pct %||% 0), 2), "% for buy-and-hold."
  )

  sparsity_explanation <- if (trade_count <= 1L) {
    paste0(
      "The low trade count is not automatically a model failure. ",
      "This run was long-only, capital-constrained, and mask-safe, so the agent had only ", day_count,
      " daily chances to act and chose to enter once and then hold rather than churn the position."
    )
  } else {
    "The agent still traded selectively because it only acted when the masked price and news context cleared its internal threshold."
  }

  decision_basis <- c(
    paste0("BUY signals: ", buy_count, " | SELL signals: ", sell_count, " | HOLD signals: ", hold_count, "."),
    paste0("The run used a fixed masked lookback of ", backtest_result$lookback_days %||% BACKTEST_LOOKBACK_DAYS, " trading days per decision."),
    paste0("Sharpe ratio was ", fmt_num(as.numeric(metrics$sharpe_ratio %||% 0), 2),
           " and max drawdown was ", fmt_num(as.numeric(metrics$max_drawdown_pct %||% 0), 2), "%."),
    paste0(
      "Trade activity ratio was ", fmt_num(100 * as.numeric(metrics$active_decision_ratio %||% 0), 0),
      "% and strategy diversity was ", metrics$strategy_diversity %||% 0L,
      " with dominant strategy ", metrics$dominant_strategy %||% "none", "."
    )
  )

  if (length(logs) > 0) {
    first_reason <- trimws(as.character(logs[[1]]$reasoning %||% ""))
    if (nzchar(first_reason)) {
      decision_basis <- c(decision_basis, paste0("Example rationale from the run: ", substr(first_reason, 1, 220)))
    }
  }

  list(
    summary_title = summary_title,
    executive_summary = executive_summary,
    decision_basis = decision_basis,
    sparsity_explanation = sparsity_explanation,
    model_role = paste0(
      "The trading loop used model ", backtest_result$llm_model %||% "gpt-4o-mini",
      " to generate one masked decision per day from visible prices, quant signals, and visible news only."
    ),
    improvement_ideas = c(
      "Test a longer custom range so the model experiences more than one short market regime.",
      "Compare GPT-4o mini and GPT-4o with the new model selector to see whether trade frequency changes.",
      "Tighten or loosen the trading prompt later if you want a more active style instead of conservative hold behavior."
    )
  )
}

generate_backtest_explanation <- function(backtest_result,
                                          api_key = Sys.getenv("OPENAI_API_KEY"),
                                          model = backtest_result$llm_model %||% "gpt-4o-mini",
                                          max_tokens = 1200) {
  fallback <- build_backtest_explanation_fallback(backtest_result)
  if (!nzchar(api_key)) return(fallback)

  logs <- backtest_result$decision_log %||% list()
  trades <- backtest_result$trades %||% list()
  metrics <- backtest_result$metrics %||% list()
  quality_df <- backtest_result$quality_matrix
  if (!is.data.frame(quality_df)) quality_df <- coerce_record_df(quality_df)

  decision_lines <- if (length(logs) == 0) {
    "No decision log available."
  } else {
    paste(vapply(logs, function(entry) {
      paste0(
        entry$date %||% "-", " | action=", entry$action %||% "HOLD",
        " | strategy=", entry$selected_strategy %||% "unknown",
        " | target_pct=", entry$target_position_pct %||% 0,
        " | executed_shares=", entry$executed_shares %||% 0,
        " | confidence=", entry$confidence %||% 0,
        " | reasoning=", substr(entry$reasoning %||% "", 1, 220)
      )
    }, character(1)), collapse = "\n")
  }

  trade_lines <- if (length(trades) == 0) {
    "No executed trades."
  } else {
    paste(vapply(trades, function(entry) {
      paste0(entry$date %||% "-", " | ", entry$selected_strategy %||% "unknown", " | ",
             entry$action %||% "-", " ", entry$shares %||% 0,
             " shares @ $", fmt_num(as.numeric(entry$price %||% 0), 2),
             " | target=", entry$target_position_pct %||% 0, "%")
    }, character(1)), collapse = "\n")
  }

  quality_text <- if (is.null(quality_df) || nrow(quality_df) == 0) {
    "No quality checks available."
  } else {
    paste(apply(quality_df, 1, function(row) paste(row[[1]], row[[2]], row[[3]], sep = " | ")), collapse = "\n")
  }

  system_prompt <- paste0(
    "You are reviewing a masked trading backtest for a stakeholder.\n",
    "STRICT RULES:\n",
    "- Use ONLY the backtest evidence provided below.\n",
    "- Do NOT invent signals, market events, or metrics.\n",
    "- Explain clearly why the strategy traded sparsely or actively.\n",
    "- Return JSON with EXACTLY these keys: summary_title, executive_summary, decision_basis, sparsity_explanation, model_role, improvement_ideas.\n",
    "- decision_basis must be a JSON array of 4-6 strings.\n",
    "- improvement_ideas must be a JSON array of 3 strings.\n"
  )

  user_prompt <- paste0(
    "=== RUN SUMMARY ===\n",
    "Ticker: ", backtest_result$ticker %||% "-", "\n",
    "Model: ", backtest_result$llm_model %||% model, "\n",
    "Date range: ", backtest_result$start_date %||% "-", " to ", backtest_result$end_date %||% "-", "\n",
    "Trading days: ", backtest_result$trading_days %||% 0, "\n",
    "Lookback days: ", backtest_result$lookback_days %||% BACKTEST_LOOKBACK_DAYS, "\n\n",
    "=== PERFORMANCE ===\n",
    "Strategy return: ", metrics$total_return_pct %||% NA_real_, "%\n",
    "Buy and hold return: ", metrics$buy_hold_return_pct %||% NA_real_, "%\n",
    "Alpha vs buy and hold: ", metrics$alpha_vs_buy_hold_pct %||% NA_real_, "%\n",
    "Sharpe ratio: ", metrics$sharpe_ratio %||% NA_real_, "\n",
    "Max drawdown: ", metrics$max_drawdown_pct %||% NA_real_, "%\n",
    "Win rate: ", metrics$win_rate %||% NA_real_, "\n\n",
    "=== EXECUTED TRADES ===\n", trade_lines, "\n\n",
    "=== DAILY DECISIONS ===\n", decision_lines, "\n\n",
    "=== QUALITY CHECKS ===\n", quality_text, "\n\n",
    "Explain the core decision basis and explicitly address why trade count was low or high."
  )

  explanation <- call_openai_text(
    system_prompt = system_prompt,
    user_prompt = user_prompt,
    api_key = api_key,
    max_tokens = max_tokens,
    model = model
  )

  if (is.null(explanation)) fallback else explanation
}

run_backtest <- function(symbol, start_date, end_date = Sys.Date(),
                         initial_capital = 10000,
                         lookback_days = 90,
                         fee_rate = 0.001,
                         price_data = NULL,
                         news_data = NULL,
                         decision_fn = NULL,
                         llm_model = "gpt-4o-mini",
                         strategy_mode = "auto",
                         on_progress = NULL,
                         sleep_sec = 0,
                         generate_explanation = TRUE,
                         persist = TRUE) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if (is.na(start_date) || is.na(end_date) || start_date > end_date) {
    return(list(ok = FALSE, error = "Invalid backtest date range."))
  }

  symbol <- toupper(trimws(symbol))
  strategy_mode <- normalize_strategy_mode(strategy_mode)
  if (!nzchar(symbol)) {
    return(list(ok = FALSE, error = "Ticker is required."))
  }

  if (is.function(on_progress)) on_progress(0.05, "Preparing market data...")

  all_prices <- price_data
  if (!is.null(all_prices)) {
    all_prices <- all_prices[all_prices$Symbol == symbol, , drop = FALSE]
    if ("Date" %in% names(all_prices)) all_prices$Date <- as.Date(all_prices$Date)
  }

  if (is.null(all_prices) || nrow(all_prices) < 5L) {
    api_key <- Sys.getenv("FINNHUB_API_KEY")
    days_back <- max(as.integer(end_date - (start_date - lookback_days)) + 5L, 30L)
    all_prices <- fetch_finnhub_candles(symbol, api_key, days_back = days_back)
  }

  if (is.null(all_prices) || nrow(all_prices) < 5L) {
    return(list(ok = FALSE, error = "Insufficient price data for backtest."))
  }

  all_prices <- all_prices[order(all_prices$Date), ]
  all_prices <- all_prices[!duplicated(all_prices$Date), ]

  all_news <- if (!is.null(news_data)) news_data else {
    news_path <- file.path(get_export_dir(), "news_archive.csv")
    if (file.exists(news_path)) tryCatch(read.csv(news_path, stringsAsFactors = FALSE), error = function(e) NULL) else NULL
  }
  all_news <- normalize_news_archive(all_news)

  trading_days <- all_prices$Date[all_prices$Date >= start_date & all_prices$Date <= end_date]
  trading_days <- sort(unique(trading_days))
  if (length(trading_days) < 2L) {
    return(list(ok = FALSE, error = "Not enough trading days in the selected range."))
  }

  portfolio <- list(cash = as.numeric(initial_capital), shares = 0L)
  equity_curve <- list(list(date = as.character(trading_days[1]), equity = round(initial_capital, 2)))
  trades <- list()
  decision_log <- list()

  baseline_curve <- build_buy_hold_curve(trading_days, all_prices, initial_capital, fee_rate = fee_rate)

  if (is.function(on_progress)) on_progress(0.10, "Starting masked backtest loop...")

  for (i in seq_along(trading_days)) {
    decision_date <- trading_days[i]
    masked_prices <- all_prices[all_prices$Date <= decision_date, , drop = FALSE]
    masked_prices <- tail(masked_prices, min(nrow(masked_prices), max(lookback_days, 30L)))
    current_price <- tail(masked_prices$Close, 1)

    masked_news <- select_relevant_news(
      news_df = all_news,
      symbol = symbol,
      as_of = decision_date,
      n_company = 8L,
      n_macro = 6L,
      keywords = c("earnings", "guidance", "ai", "chip", "rate", "tariff", "regulation")
    )

    trend <- compute_ohlc_metrics(masked_prices, lookback = min(nrow(masked_prices), lookback_days))
    gbm_params <- estimate_gbm_params(masked_prices, max_lookback = min(252L, nrow(masked_prices)))
    short_signals <- compute_short_signals(masked_prices)
    news_signals <- compute_news_signals(masked_news, decision_date = decision_date)
    strategy_scores <- compute_strategy_scores(
      short_signals = short_signals,
      news_signals = news_signals,
      trend = trend,
      gbm_params = gbm_params,
      strategy_mode = strategy_mode
    )
    decision_memory <- format_recent_decision_memory(decision_log, max_items = 3L)

    if (is.function(on_progress)) {
      on_progress(
        0.10 + 0.75 * (i / length(trading_days)),
        sprintf("Running agent decision %d/%d for %s", i, length(trading_days), as.character(decision_date))
      )
    }

    portfolio_before <- portfolio
    decision <- call_trading_agent(
      symbol = symbol,
      current_date = decision_date,
      current_price = current_price,
      portfolio = portfolio,
      trend = trend,
      gbm_params = gbm_params,
      news_headlines = masked_news,
      lookback_prices = tail(masked_prices, min(20L, nrow(masked_prices))),
      short_signals = short_signals,
      news_signals = news_signals,
      strategy_scores = strategy_scores,
      decision_memory = decision_memory,
      strategy_mode = strategy_mode,
      fee_rate = fee_rate,
      decision_fn = decision_fn,
      model = llm_model
    )

    portfolio <- execute_trade(portfolio, decision, current_price, fee_rate = fee_rate)
    current_equity <- round(portfolio$cash + portfolio$shares * current_price, 2)
    equity_curve[[length(equity_curve) + 1L]] <- list(
      date = as.character(decision_date),
      equity = current_equity
    )

    shares_delta <- as.integer(portfolio$shares) - as.integer(portfolio_before$shares)
    executed_shares <- abs(shares_delta)

    decision_entry <- list(
      date = as.character(decision_date),
      action = decision$action,
      shares = as.integer(decision$shares),
      executed_shares = as.integer(executed_shares),
      price = round(current_price, 2),
      confidence = as.integer(decision$confidence %||% 0L),
      reasoning = decision$reasoning,
      selected_strategy = decision$selected_strategy %||% "unknown",
      target_position_pct = as.numeric(decision$target_position_pct %||% 0),
      signal_scores = decision$signal_scores %||% list(),
      risk_flags = decision$risk_flags %||% list(),
      holding_horizon = decision$holding_horizon %||% "1-3 trading days",
      parse_ok = isTRUE(decision$parse_ok),
      portfolio_before = list(
        cash = round(portfolio_before$cash, 2),
        shares = as.integer(portfolio_before$shares),
        equity = round(portfolio_before$cash + portfolio_before$shares * current_price, 2)
      ),
      portfolio_after = list(
        cash = round(portfolio$cash, 2),
        shares = as.integer(portfolio$shares),
        equity = current_equity
      ),
      mask_audit = list(
        visible_price_start_date = as.character(head(masked_prices$Date, 1)),
        visible_price_end_date = as.character(tail(masked_prices$Date, 1)),
        visible_price_rows = nrow(masked_prices),
        model_input_end_date = as.character(gbm_params$last_date %||% tail(masked_prices$Date, 1)),
        visible_news_count = if (is.null(masked_news)) 0L else nrow(masked_news),
        visible_news_max_time = if (is.null(masked_news) || nrow(masked_news) == 0) {
          NA_character_
        } else {
          format(max(as.POSIXct(masked_news$Published, origin = "1970-01-01", tz = "UTC")),
                 "%Y-%m-%d %H:%M:%S")
        }
      ),
      short_signals = short_signals,
      news_signals = news_signals,
      strategy_scores = as.list(strategy_scores)
    )
    decision_log[[length(decision_log) + 1L]] <- decision_entry

    if (executed_shares > 0L) {
      trades[[length(trades) + 1L]] <- list(
        date = as.character(decision_date),
        action = decision$action,
        shares = as.integer(executed_shares),
        price = round(current_price, 2),
        confidence = as.integer(decision$confidence %||% 0L),
        reasoning = decision$reasoning,
        selected_strategy = decision$selected_strategy %||% "unknown",
        target_position_pct = as.numeric(decision$target_position_pct %||% 0)
      )
    }

    if (sleep_sec > 0 && i < length(trading_days)) Sys.sleep(sleep_sec)
  }

  metrics <- c(
    compute_backtest_metrics(equity_curve, initial_capital, baseline_curve = baseline_curve),
    compute_decision_metrics(decision_log)
  )
  result <- list(
    ok = TRUE,
    ticker = symbol,
    start_date = as.character(start_date),
    end_date = as.character(end_date),
    initial_capital = as.numeric(initial_capital),
    borrowed_capital = as.numeric(initial_capital),
    lookback_days = as.integer(lookback_days),
    llm_model = llm_model,
    strategy_mode = strategy_mode,
    trading_days = length(trading_days),
    trades = trades,
    decision_log = decision_log,
    equity_curve = equity_curve,
    baseline_curve = baseline_curve,
    metrics = metrics,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  result$quality_matrix <- build_backtest_quality_matrix(result)
  result$forward_outlook <- tryCatch(
    build_forward_outlook(result, hist_prices = all_prices, horizon_days = 30L),
    error = function(e) NULL
  )
  result$llm_explanation <- if (isTRUE(generate_explanation)) {
    if (is.function(on_progress)) on_progress(0.92, "Generating LLM explanation...")
    generate_backtest_explanation(result, model = llm_model)
  } else {
    build_backtest_explanation_fallback(result)
  }

  if (persist) {
    result$storage_path <- write_latest_result_json(
      paste0("latest_backtest_", symbol, ".json"),
      result
    )
  }

  result
}

load_latest_backtest <- function(symbol) {
  read_latest_result_json(paste0("latest_backtest_", toupper(symbol), ".json"))
}
