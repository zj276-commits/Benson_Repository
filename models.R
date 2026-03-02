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
    n$Published <- as.POSIXct(n$Published, origin = "1970-01-01", tz = "UTC")
    n
  }, error = function(e) NULL)

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

build_news_snapshot <- function(symbol, news_df, max_rows = 5) {
  if (is.null(news_df) || nrow(news_df) == 0) return("No recent news.")
  sub <- news_df[news_df$Symbol == symbol, ]
  if (nrow(sub) == 0) return("No recent news.")
  sub <- sub[order(as.POSIXct(sub$Published, origin = "1970-01-01", tz = "UTC"), decreasing = TRUE), ]
  top <- head(sub, max_rows)
  items <- paste0(
    seq_len(nrow(top)), ". ", top$Symbol, ": ", top$Title,
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
# Binomial Lattice (CRR)
# ----------------------------

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

# ----------------------------
# forecast prompt (pure data, no news bias)
# ----------------------------

build_forecast_prompt <- function(symbol, window_days, trend, news_text, tone_score) {
  paste(
    sprintf("Forecast: %s, time horizon %d trading days.", symbol, window_days),
    sprintf(
      "Price metrics: Latest close %s, 1d %s, 7d %s, 30d %s, 1y(last %d days approx) %s, Volatility(annualized 20d) %s.",
      fmt_price(trend$latest),
      fmt_pct(trend$change1d_pct), fmt_pct(trend$change7d_pct),
      fmt_pct(trend$change30d_pct), min(window_days, 252),
      fmt_pct(trend$changeN_pct),
      ifelse(is.na(trend$mean_20d_volatility), "N/A", paste0(trend$mean_20d_volatility, "%"))
    ),
    paste("Latest company news text:", news_text, "."),
    paste(
      "Return JSON exactly with keys:",
      "trend (UP/DOWN/NEUTRAL), confidence (0-100),",
      "forecast_signal (UP / DOWN / FLAT),",
      "reasoning (3 bullets max in one JSON array),",
      "expected_price_level (short text),",
      "risk_note (short text)."
    ),
    sep = "\n"
  )
}
