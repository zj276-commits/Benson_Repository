# rag.R
# RAG (Retrieval-Augmented Generation) data retrieval and formatting
#
# Capabilities:
#   1. Semantic search  — embedding-based relevance scoring for news
#   2. Dynamic trimming — token budget management per agent
#   3. Freshness labels — timestamp + staleness warnings on every section
#   4. Relevance scoring — cosine-similarity ranking of news headlines
#   5. Broad retrieval   — expanded context for multi-round RAG retry
#
# Depends on: httr2, jsonlite, dplyr

# ============================================================
# Configuration
# ============================================================

RAG_CONFIG <- list(
  embedding_model    = "text-embedding-3-small",
  token_budget       = 3000L,
  stale_warning_days = 3L,
  stale_critical_days = 7L
)

# ============================================================
# Embedding cache (cleared per report generation)
# ============================================================

.rag_cache <- new.env(parent = emptyenv())

rag_clear_cache <- function() {
  rm(list = ls(.rag_cache), envir = .rag_cache)
}

# ============================================================
# Data directory resolution
# ============================================================

rag_data_dir <- function() {
  if (exists("get_export_dir", mode = "function")) get_export_dir() else getwd()
}

# ============================================================
# Safe CSV read + ticker filter
# ============================================================

rag_read_csv <- function(filename, symbol, col = "ticker", data_dir = NULL) {
  if (is.null(data_dir)) data_dir <- rag_data_dir()
  path <- file.path(data_dir, filename)
  if (!file.exists(path)) return(NULL)
  df <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  if (col %in% names(df))           df <- df[!is.na(df[[col]]) & df[[col]] == symbol, , drop = FALSE]
  else if ("Symbol" %in% names(df))  df <- df[!is.na(df$Symbol) & df$Symbol == symbol, , drop = FALSE]
  if (nrow(df) == 0) return(NULL)
  df
}

# ============================================================
# Enhancement 1 & 4: Semantic Search + Relevance Scoring
# ============================================================

rag_get_embeddings <- function(texts, api_key = NULL) {
  if (is.null(api_key)) api_key <- Sys.getenv("OPENAI_API_KEY")
  if (!nzchar(api_key)) return(NULL)
  texts <- trimws(texts)
  if (length(texts) == 0 || all(!nzchar(texts))) return(NULL)

  resp <- tryCatch({
    request("https://api.openai.com/v1/embeddings") |>
      req_headers(Authorization = paste0("Bearer ", api_key),
                  `Content-Type` = "application/json") |>
      req_body_json(list(model = RAG_CONFIG$embedding_model, input = as.list(texts))) |>
      req_timeout(30) |>
      req_retry(max_tries = 2, backoff = ~ 1) |>
      req_perform()
  }, error = function(e) {
    message("[RAG-Embed] Request failed: ", e$message)
    NULL
  })
  if (is.null(resp) || resp_status(resp) != 200) return(NULL)

  result <- tryCatch(resp_body_json(resp), error = function(e) NULL)
  if (is.null(result) || is.null(result$data)) return(NULL)
  do.call(rbind, lapply(result$data, function(d) unlist(d$embedding)))
}

rag_cosine_sim <- function(query_vec, doc_matrix) {
  if (is.null(doc_matrix) || nrow(doc_matrix) == 0) return(numeric(0))
  norms_doc <- sqrt(rowSums(doc_matrix^2))
  norm_q    <- sqrt(sum(query_vec^2))
  norms_doc[norms_doc == 0] <- 1
  as.numeric(doc_matrix %*% query_vec) / (norms_doc * norm_q)
}

rag_agent_focus <- function(agent_type) {
  switch(agent_type,
    fundamentals = "company financial performance earnings revenue profit margins valuation balance sheet growth",
    news         = "major market events company announcements regulatory changes industry news M&A earnings guidance",
    technical    = "stock price momentum trading volume technical indicators volatility trend reversal",
    bull         = "growth opportunities positive catalysts upside potential competitive advantages expansion innovation",
    bear         = "risks threats competitive pressure margin compression regulatory headwinds downside overvaluation",
    risk         = "risk factors market volatility leverage debt exposure macro uncertainty geopolitical threats drawdown",
    "financial analysis stock market"
  )
}

# ============================================================
# Enhancement 2: Token Estimation & Dynamic Trimming
# ============================================================

rag_estimate_tokens <- function(text) {
  if (is.null(text) || !nzchar(text)) return(0L)
  as.integer(ceiling(nchar(text) / 3.5))
}

rag_trim_to_budget <- function(sections, max_tokens = NULL) {
  if (is.null(max_tokens)) max_tokens <- RAG_CONFIG$token_budget
  sections <- Filter(function(s) nzchar(s$text), sections)
  if (length(sections) == 0) return("")

  for (i in seq_along(sections)) {
    sections[[i]]$tokens <- rag_estimate_tokens(sections[[i]]$text)
  }

  sections <- sections[order(vapply(sections, `[[`, numeric(1), "priority"))]

  total <- sum(vapply(sections, `[[`, integer(1), "tokens"))
  if (total <= max_tokens) {
    return(paste(vapply(sections, `[[`, character(1), "text"), collapse = "\n\n"))
  }

  kept <- character(0)
  remaining <- max_tokens
  for (s in sections) {
    if (s$tokens <= remaining) {
      kept <- c(kept, s$text)
      remaining <- remaining - s$tokens
    } else if (remaining > 200L) {
      char_limit <- as.integer(remaining * 3.5)
      truncated <- substr(s$text, 1, char_limit)
      last_nl <- regexpr("\n[^\n]*$", truncated)
      if (last_nl > 0) truncated <- substr(truncated, 1, last_nl - 1)
      kept <- c(kept, paste0(truncated, "\n... [trimmed to fit token budget]"))
      remaining <- 0L
    }
  }
  paste(kept, collapse = "\n\n")
}

# ============================================================
# Enhancement 3: Data Freshness Annotation
# ============================================================

rag_freshness_label <- function(date_value) {
  if (is.null(date_value) || is.na(date_value)) return("")
  d <- tryCatch(as.Date(date_value), error = function(e) NA)
  if (is.na(d)) return("")
  age_days <- as.integer(Sys.Date() - d)
  status <- if (age_days <= RAG_CONFIG$stale_warning_days) {
    "FRESH"
  } else if (age_days <= RAG_CONFIG$stale_critical_days) {
    "AGING"
  } else {
    "STALE"
  }
  sprintf("[Data: %s | %s (%d day%s ago)]", d, status, age_days, if (age_days != 1) "s" else "")
}

rag_annotate_section <- function(section_text, date_value, section_label = "") {
  if (!nzchar(section_text)) return("")
  label <- rag_freshness_label(date_value)
  if (!nzchar(label)) return(section_text)
  first_nl <- regexpr("\n", section_text, fixed = TRUE)
  if (first_nl > 0) {
    header <- substr(section_text, 1, first_nl - 1)
    rest   <- substr(section_text, first_nl, nchar(section_text))
    paste0(header, " ", label, rest)
  } else {
    paste0(section_text, " ", label)
  }
}

# ============================================================
# News: Raw retrieval + semantic scoring + formatting
# ============================================================

rag_read_news_df <- function(symbol, n_company = 10, n_macro = 8, data_dir = NULL) {
  if (is.null(data_dir)) data_dir <- rag_data_dir()
  path <- file.path(data_dir, "news_archive.csv")
  if (!file.exists(path)) return(NULL)
  all_news <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(all_news) || !("Symbol" %in% names(all_news))) return(NULL)
  all_news <- all_news[order(all_news$Published, decreasing = TRUE), ]
  company <- head(all_news[!is.na(all_news$Symbol) & all_news$Symbol == symbol, , drop = FALSE], n_company)
  macro   <- head(all_news[!is.na(all_news$Symbol) & all_news$Symbol == "MACRO", , drop = FALSE], n_macro)
  combined <- dplyr::bind_rows(company, macro)
  if (nrow(combined) == 0) return(NULL)
  combined
}

rag_score_news <- function(news_df, agent_type, api_key = NULL) {
  if (is.null(news_df) || nrow(news_df) == 0) return(news_df)

  cache_key <- paste0("news_emb_", digest::digest(paste(news_df$Title, collapse = "|")))
  if (requireNamespace("digest", quietly = TRUE)) {
    cache_key <- paste0("news_emb_", digest::digest(paste(news_df$Title, collapse = "|")))
  } else {
    cache_key <- paste0("news_emb_", nrow(news_df), "_", sum(nchar(news_df$Title)))
  }

  news_embs <- NULL
  if (exists(cache_key, envir = .rag_cache)) {
    news_embs <- get(cache_key, envir = .rag_cache)
  } else {
    texts <- vapply(seq_len(nrow(news_df)), function(i) {
      title <- news_df$Title[i]
      summary <- if ("Summary" %in% names(news_df) && !is.na(news_df$Summary[i])) news_df$Summary[i] else ""
      paste(title, summary)
    }, character(1))
    news_embs <- rag_get_embeddings(texts, api_key)
    if (!is.null(news_embs)) {
      assign(cache_key, news_embs, envir = .rag_cache)
    }
  }

  if (is.null(news_embs)) {
    news_df$relevance_score <- seq(1, 0.5, length.out = nrow(news_df))
    return(news_df)
  }

  query <- rag_agent_focus(agent_type)
  q_emb <- rag_get_embeddings(query, api_key)
  if (is.null(q_emb)) {
    news_df$relevance_score <- seq(1, 0.5, length.out = nrow(news_df))
    return(news_df)
  }

  scores <- rag_cosine_sim(q_emb[1, ], news_embs)
  news_df$relevance_score <- round(scores, 4)
  news_df[order(news_df$relevance_score, decreasing = TRUE), ]
}

rag_format_news <- function(news_df, show_scores = TRUE) {
  if (is.null(news_df) || nrow(news_df) == 0) return("")
  lines <- vapply(seq_len(nrow(news_df)), function(i) {
    r <- news_df[i, ]
    tag <- if (r$Symbol == "MACRO") "[MACRO]" else paste0("[", r$Symbol, "]")
    src <- if ("Source" %in% names(r) && !is.na(r$Source)) r$Source else ""
    ts  <- if ("published_et" %in% names(r) && !is.na(r$published_et)) r$published_et else ""
    summary <- if ("Summary" %in% names(r) && !is.na(r$Summary) && nzchar(r$Summary))
      paste0(" -- ", substr(r$Summary, 1, 150)) else ""
    score_tag <- if (show_scores && "relevance_score" %in% names(r) && !is.na(r$relevance_score))
      sprintf(" [rel: %.2f]", r$relevance_score) else ""
    sprintf("  %s %s (%s%s)%s%s", tag, r$Title, src,
            if (nzchar(ts)) paste0(", ", ts) else "", summary, score_tag)
  }, character(1))
  paste(lines, collapse = "\n")
}

rag_get_news_context <- function(symbol, agent_type, n_company, n_macro,
                                 data_dir = NULL, use_semantic = TRUE) {
  news_df <- rag_read_news_df(symbol, n_company, n_macro, data_dir)
  if (is.null(news_df)) return("")
  if (use_semantic) {
    news_df <- rag_score_news(news_df, agent_type)
    rag_format_news(news_df, show_scores = TRUE)
  } else {
    rag_format_news(news_df, show_scores = FALSE)
  }
}

# ============================================================
# Number formatting (skips NA)
# ============================================================

rag_fmt_val <- function(x, suffix = "", prefix = "", mult = 1) {
  if (is.null(x) || is.na(x)) return(NULL)
  val <- as.numeric(x) * mult
  if (is.na(val)) return(NULL)
  paste0(prefix, formatC(val, format = "f", big.mark = ",", digits = if (abs(val) >= 10) 1 else 2), suffix)
}

# ============================================================
# Formatting: Key Financials
# ============================================================

rag_fmt_key_fin <- function(kf) {
  if (is.null(kf) || nrow(kf) == 0) return("")
  k <- kf[1, ]
  parts <- Filter(Negate(is.null), list(
    rag_fmt_val(k$share_price, prefix = "Share Price: $"),
    rag_fmt_val(k$target_price, prefix = "Analyst Target Price: $"),
    rag_fmt_val(k$market_cap_bn, suffix = "B", prefix = "Market Cap: $"),
    rag_fmt_val(k$beta, prefix = "Beta: "),
    rag_fmt_val(k$pe_ttm, suffix = "x", prefix = "P/E (TTM): "),
    rag_fmt_val(k$fwd_pe, suffix = "x", prefix = "Forward P/E: "),
    rag_fmt_val(k$pb, suffix = "x", prefix = "P/B: "),
    rag_fmt_val(k$roe_pct, suffix = "%", prefix = "ROE: "),
    rag_fmt_val(k$eps_ttm, prefix = "EPS (TTM): $"),
    rag_fmt_val(k$book_value, prefix = "Book Value/Share: $"),
    rag_fmt_val(k$dividend_yield_pct, suffix = "%", prefix = "Dividend Yield: "),
    rag_fmt_val(k$net_debt_equity_pct, suffix = "%", prefix = "Net Debt/Equity: "),
    rag_fmt_val(k$free_float_pct, suffix = "%", prefix = "Free Float: "),
    if (length(k$fifty_two_wk_low) > 0 && length(k$fifty_two_wk_high) > 0 &&
        !is.na(k$fifty_two_wk_low) && !is.na(k$fifty_two_wk_high))
      sprintf("52-Week Range: $%.2f - $%.2f", k$fifty_two_wk_low, k$fifty_two_wk_high)
  ))
  date_val <- if (length(k$fetch_date) > 0 && !is.na(k$fetch_date)) k$fetch_date else NULL
  section <- paste0("=== KEY FINANCIALS ===\n", paste(parts, collapse = "\n"))
  rag_annotate_section(section, date_val)
}

# ============================================================
# Formatting: Company Financials (multi-year table)
# ============================================================

rag_fmt_company_fin <- function(cf) {
  if (is.null(cf) || nrow(cf) == 0) return("")
  cf <- cf[order(cf$fiscal_year_end), ]
  if (nrow(cf) > 4) cf <- tail(cf, 4)
  years <- cf$fiscal_year

  fmt_row <- function(label, vals, suffix = "", prefix = "", is_pct = FALSE) {
    if (is.null(vals) || length(vals) == 0) return(sprintf("  %-22s %s", label,
      paste(sprintf("%12s", rep("  --", length(years))), collapse = "")))
    formatted <- vapply(vals, function(v) {
      if (is.null(v) || length(v) == 0 || is.na(v)) return("  --")
      if (is_pct) sprintf("%+.1f%%", v)
      else if (abs(v) >= 1000) paste0(prefix, formatC(v, format = "f", big.mark = ",", digits = 0), suffix)
      else paste0(prefix, formatC(v, format = "f", digits = 1), suffix)
    }, character(1))
    sprintf("  %-22s %s", label, paste(sprintf("%12s", formatted), collapse = ""))
  }

  header <- sprintf("  %-22s %s", "", paste(sprintf("%12s", years), collapse = ""))

  income_rows <- c(
    fmt_row("Revenue ($M)", cf$revenue_mn),
    fmt_row("Revenue YoY", cf$revenue_yoy, suffix = "%", is_pct = TRUE),
    fmt_row("Gross Profit ($M)", cf$gross_profit_mn),
    fmt_row("EBITDA ($M)", cf$ebitda_mn),
    fmt_row("Net Income ($M)", cf$net_income_mn),
    fmt_row("Net Income YoY", cf$ni_yoy, suffix = "%", is_pct = TRUE),
    fmt_row("FCF ($M)", cf$fcf_mn),
    fmt_row("CAPEX ($M)", cf$capex_mn),
    fmt_row("EBITDA Margin", cf$ebitda_margin_pct, suffix = "%"),
    fmt_row("Net Margin", cf$net_margin_pct, suffix = "%"),
    fmt_row("ROE", cf$roe_pct, suffix = "%"),
    fmt_row("ROA", cf$roa_pct, suffix = "%")
  )

  credit_rows <- c(
    fmt_row("Debt/Equity", cf$debt_equity_pct, suffix = "%"),
    fmt_row("Net Debt/Equity", cf$net_debt_equity_pct, suffix = "%"),
    fmt_row("Debt/Assets", cf$debt_assets_pct, suffix = "%"),
    fmt_row("Debt/EBITDA", cf$debt_ebitda, suffix = "x"),
    fmt_row("EBITDA/Interest", cf$ebitda_int_exp, suffix = "x"),
    fmt_row("Receivables Days", cf$receivables_days),
    fmt_row("Inventory Days", cf$inventory_days)
  )

  latest_date <- max(cf$fiscal_year_end, na.rm = TRUE)
  section <- paste0("=== INCOME STATEMENT & PROFITABILITY (Annual, $M) ===\n",
         "LEGEND: YoY = Year-over-Year growth, FCF = Free Cash Flow, CAPEX = Capital Expenditure\n",
         header, "\n", paste(income_rows, collapse = "\n"),
         "\n\n=== BALANCE SHEET & CREDIT METRICS ===\n",
         "LEGEND: D/E = Debt-to-Equity ratio, EBITDA/Interest = Interest Coverage ratio\n",
         header, "\n", paste(credit_rows, collapse = "\n"))
  rag_annotate_section(section, latest_date)
}

# ============================================================
# Formatting: Valuation Multiples
# ============================================================

rag_fmt_valuation <- function(vm) {
  if (is.null(vm) || nrow(vm) == 0) return("")
  vm <- vm[order(vm$fiscal_year), ]
  if (nrow(vm) > 4) vm <- tail(vm, 4)
  years <- vm$fiscal_year
  header <- sprintf("  %-22s %s", "", paste(sprintf("%12s", years), collapse = ""))
  fmt_row <- function(label, vals, suffix = "") {
    formatted <- vapply(vals, function(v) {
      if (is.null(v) || is.na(v)) return("  --")
      paste0(formatC(v, format = "f", digits = 1), suffix)
    }, character(1))
    sprintf("  %-22s %s", label, paste(sprintf("%12s", formatted), collapse = ""))
  }
  rows <- c(
    fmt_row("P/E", vm$pe_x, "x"),
    fmt_row("P/B", vm$pb_x, "x"),
    fmt_row("Dividend Yield", vm$dividend_yield_pct, "%"),
    fmt_row("EV/EBITDA", vm$ev_ebitda_x, "x"),
    fmt_row("FCF Yield", vm$fcf_yield_pct, "%")
  )
  section <- paste0("=== VALUATION MULTIPLES (Historical) ===\n",
         "LEGEND: P/E = Price/Earnings, P/B = Price/Book, EV/EBITDA = Enterprise Value / EBITDA, FCF = Free Cash Flow\n",
         header, "\n", paste(rows, collapse = "\n"))
  latest_fy <- if ("fiscal_year_end" %in% names(vm)) max(vm$fiscal_year_end, na.rm = TRUE) else NULL
  rag_annotate_section(section, latest_fy)
}

# ============================================================
# Formatting: Model Parameters
# ============================================================

rag_fmt_model_params <- function(mp) {
  if (is.null(mp) || nrow(mp) == 0) return("")
  m <- mp[1, ]
  sections <- list()

  mu_ci <- if (length(m$gbm_mu_ci_lo) > 0 && !is.na(m$gbm_mu_ci_lo) &&
               length(m$gbm_mu_ci_hi) > 0 && !is.na(m$gbm_mu_ci_hi))
    sprintf("Mu 95%% CI: [%.2f, %.2f]", m$gbm_mu_ci_lo, m$gbm_mu_ci_hi)
  sigma_ci <- if (length(m$gbm_sigma_ci_lo) > 0 && !is.na(m$gbm_sigma_ci_lo) &&
                  length(m$gbm_sigma_ci_hi) > 0 && !is.na(m$gbm_sigma_ci_hi))
    sprintf("Sigma 95%% CI: [%.2f, %.2f]", m$gbm_sigma_ci_lo, m$gbm_sigma_ci_hi)
  gbm <- Filter(Negate(is.null), list(
    rag_fmt_val(m$gbm_mu_annual, prefix = "Drift (mu, annualized): "),
    rag_fmt_val(m$gbm_sigma_annual, prefix = "Volatility (sigma, annualized): "),
    mu_ci, sigma_ci
  ))
  if (length(gbm) > 0) sections <- c(sections, "  --- GBM (Geometric Brownian Motion) ---", paste0("  ", gbm))

  lattice <- Filter(Negate(is.null), list(
    rag_fmt_val(m$lattice_u, prefix = "Up factor (u): "),
    rag_fmt_val(m$lattice_d, prefix = "Down factor (d): "),
    rag_fmt_val(m$lattice_p_real, prefix = "Real-world up probability: "),
    rag_fmt_val(m$lattice_p_rn, prefix = "Risk-neutral up probability: ")
  ))
  if (length(lattice) > 0) sections <- c(sections, "  --- Binomial Lattice (RWPM) ---", paste0("  ", lattice))

  sim <- Filter(Negate(is.null), list(
    rag_fmt_val(m$sim_beta, prefix = "Beta vs SPY: "),
    rag_fmt_val(m$sim_alpha, prefix = "Alpha (excess return): "),
    rag_fmt_val(m$sim_r_squared, prefix = "R-squared: "),
    rag_fmt_val(m$sim_residual_std, prefix = "Residual Std (idiosyncratic risk): ")
  ))
  if (length(sim) > 0) sections <- c(sections, "  --- Single Index Model (vs SPY) ---",
                                      "  LEGEND: Beta = systematic risk, Alpha = excess return, R-squared = market dependence",
                                      paste0("  ", sim))

  shapiro_text <- if (length(m$shapiro_p) > 0 && !is.na(m$shapiro_p))
    sprintf("Shapiro-Wilk p-value: %.4f (%s)", m$shapiro_p,
      ifelse(m$shapiro_p < 0.05, "non-normal returns", "approximately normal"))
  tags_text <- if (length(m$tags) > 0 && !is.na(m$tags) && nzchar(m$tags))
    paste0("Model Tags: ", m$tags)
  vol <- Filter(Negate(is.null), list(
    rag_fmt_val(m$vol_annual_realized, suffix = "%", prefix = "Realized Volatility (annualized): "),
    rag_fmt_val(m$vol_annual_downside, suffix = "%", prefix = "Downside Volatility: "),
    rag_fmt_val(m$vol_20d_annualized, suffix = "%", prefix = "20-Day Rolling Volatility: "),
    rag_fmt_val(m$skewness, prefix = "Skewness: "),
    rag_fmt_val(m$kurtosis, prefix = "Kurtosis: "),
    rag_fmt_val(m$max_drawdown, suffix = "%", prefix = "Max Drawdown: "),
    shapiro_text,
    rag_fmt_val(m$outlier_fraction, suffix = "%", prefix = "Outlier Fraction: "),
    tags_text
  ))
  if (length(vol) > 0) sections <- c(sections, "  --- Volatility & Distribution ---",
                                      "  LEGEND: Kurtosis > 3 = heavy tails, Skewness < 0 = left-skewed (more downside)",
                                      paste0("  ", vol))

  snapshot_date <- if (length(m$snapshot_date) > 0 && !is.na(m$snapshot_date)) m$snapshot_date else NULL
  section <- paste0("=== QUANTITATIVE MODEL PARAMETERS ===\n",
         if (!is.null(snapshot_date)) paste0("Snapshot Date: ", snapshot_date, "\n") else "",
         paste(sections, collapse = "\n"))
  rag_annotate_section(section, snapshot_date)
}

# ============================================================
# Formatting: Recent Prices (OHLCV)
# ============================================================

rag_fmt_prices <- function(hp) {
  if (is.null(hp) || nrow(hp) == 0) return("")
  hp <- hp[order(hp$Date, decreasing = TRUE), ]
  hp <- head(hp, 15)
  lines <- vapply(seq_len(nrow(hp)), function(i) {
    r <- hp[i, ]
    sprintf("  %s  O:%.2f  H:%.2f  L:%.2f  C:%.2f  Vol:%s",
            r$Date, r$Open, r$High, r$Low, r$Close,
            formatC(r$Volume, format = "d", big.mark = ","))
  }, character(1))
  latest_date <- hp$Date[1]
  section <- paste0("=== RECENT DAILY PRICES (last 15 trading days, OHLCV) ===\n",
         paste(lines, collapse = "\n"))
  rag_annotate_section(section, latest_date)
}

# ============================================================
# Formatting: Risk Indicators (bear agent)
# ============================================================

rag_fmt_risk_indicators <- function(mp) {
  if (is.null(mp) || nrow(mp) == 0) return("")
  m <- mp[1, ]
  parts <- Filter(Negate(is.null), list(
    rag_fmt_val(m$vol_annual_realized, suffix = "%", prefix = "Realized Volatility: "),
    rag_fmt_val(m$max_drawdown, suffix = "%", prefix = "Max Drawdown: "),
    rag_fmt_val(m$kurtosis, prefix = "Kurtosis: "),
    rag_fmt_val(m$sim_beta, prefix = "Beta vs SPY: "),
    rag_fmt_val(m$gbm_sigma_annual, prefix = "GBM Sigma (annualized): ")
  ))
  if (length(parts) == 0) return("")
  paste0("=== RISK INDICATORS ===\n", paste(parts, collapse = "\n"))
}

# ============================================================
# Formatting: Credit Metrics (risk agent)
# ============================================================

rag_fmt_credit <- function(cf) {
  if (is.null(cf) || nrow(cf) == 0) return("")
  cf <- cf[order(cf$fiscal_year_end), ]
  if (nrow(cf) > 4) cf <- tail(cf, 4)
  years <- cf$fiscal_year
  header <- sprintf("  %-22s %s", "", paste(sprintf("%12s", years), collapse = ""))
  fmt_row <- function(label, vals, suffix = "") {
    formatted <- vapply(vals, function(v) {
      if (is.null(v) || is.na(v)) return("  --")
      paste0(formatC(v, format = "f", digits = 1), suffix)
    }, character(1))
    sprintf("  %-22s %s", label, paste(sprintf("%12s", formatted), collapse = ""))
  }
  rows <- c(
    fmt_row("Debt/Equity", cf$debt_equity_pct, "%"),
    fmt_row("Net Debt/Equity", cf$net_debt_equity_pct, "%"),
    fmt_row("Debt/Assets", cf$debt_assets_pct, "%"),
    fmt_row("Debt/EBITDA", cf$debt_ebitda, "x"),
    fmt_row("EBITDA/Interest", cf$ebitda_int_exp, "x")
  )
  paste0("=== CREDIT & LEVERAGE METRICS ===\n",
         "LEGEND: D/E = Debt-to-Equity, EBITDA/Interest = Interest Coverage\n",
         header, "\n", paste(rows, collapse = "\n"))
}

# ============================================================
# Main RAG retrieval (with all 5 enhancements)
# ============================================================
# use_semantic: use embedding-based news scoring (TRUE/FALSE)
# token_budget: max tokens for context (NULL = use RAG_CONFIG default)
# broad:        expanded retrieval for multi-round RAG retry

rag_retrieve <- function(symbol, agent_type, data_dir = NULL,
                         use_semantic = TRUE, token_budget = NULL,
                         broad = FALSE) {

  if (is.null(data_dir)) data_dir <- rag_data_dir()
  if (is.null(token_budget)) token_budget <- RAG_CONFIG$token_budget

  n_extra <- if (broad) 8 else 0

  build_sections <- function(...) {
    args <- list(...)
    lapply(args, function(a) list(text = a$text, priority = a$priority))
  }

  news_section <- function(n_company, n_macro, label = "RECENT NEWS") {
    news_text <- rag_get_news_context(symbol, agent_type,
                                       n_company + n_extra, n_macro + n_extra,
                                       data_dir, use_semantic)
    if (!nzchar(news_text)) return("")
    header <- if (use_semantic) paste0("=== ", label, " (ranked by relevance) ===\n")
              else paste0("=== ", label, " ===\n")
    paste0(header, news_text)
  }

  sections <- switch(agent_type,
    fundamentals = {
      kf <- rag_read_csv("key_financials.csv", symbol, data_dir = data_dir)
      cf <- rag_read_csv("company_financials.csv", symbol, data_dir = data_dir)
      vm <- rag_read_csv("valuation_metrics.csv", symbol, data_dir = data_dir)
      list(
        list(text = rag_fmt_key_fin(kf), priority = 1),
        list(text = rag_fmt_company_fin(cf), priority = 2),
        list(text = rag_fmt_valuation(vm), priority = 3),
        list(text = news_section(8, 5), priority = 4),
        if (broad) list(text = rag_fmt_model_params(
          { mp <- rag_read_csv("model_parameters.csv", symbol, data_dir = data_dir)
            if (!is.null(mp) && nrow(mp) > 0) { mp <- mp[order(mp$snapshot_date, decreasing = TRUE), ]; mp[1, , drop = FALSE] } else mp }
        ), priority = 5)
      )
    },

    news = list(
      list(text = news_section(15, 10, "NEWS HEADLINES"), priority = 1)
    ),

    technical = {
      mp <- rag_read_csv("model_parameters.csv", symbol, data_dir = data_dir)
      if (!is.null(mp) && nrow(mp) > 0) {
        mp <- mp[order(mp$snapshot_date, decreasing = TRUE), ]
        mp <- mp[1, , drop = FALSE]
      }
      hp <- rag_read_csv("historical_prices.csv", symbol, col = "Symbol", data_dir = data_dir)
      c(
        list(
          list(text = rag_fmt_model_params(mp), priority = 1),
          list(text = rag_fmt_prices(hp), priority = 2)
        ),
        if (broad) list(
          list(text = news_section(5, 5), priority = 3),
          list(text = rag_fmt_key_fin(rag_read_csv("key_financials.csv", symbol, data_dir = data_dir)), priority = 4)
        )
      )
    },

    bull = {
      kf <- rag_read_csv("key_financials.csv", symbol, data_dir = data_dir)
      cf <- rag_read_csv("company_financials.csv", symbol, data_dir = data_dir)
      c(
        list(
          list(text = rag_fmt_key_fin(kf), priority = 1),
          list(text = rag_fmt_company_fin(cf), priority = 2),
          list(text = news_section(10, 5), priority = 3)
        ),
        if (broad) list(
          list(text = rag_fmt_valuation(rag_read_csv("valuation_metrics.csv", symbol, data_dir = data_dir)), priority = 4)
        )
      )
    },

    bear = {
      kf <- rag_read_csv("key_financials.csv", symbol, data_dir = data_dir)
      cf <- rag_read_csv("company_financials.csv", symbol, data_dir = data_dir)
      mp <- rag_read_csv("model_parameters.csv", symbol, data_dir = data_dir)
      if (!is.null(mp) && nrow(mp) > 0) {
        mp <- mp[order(mp$snapshot_date, decreasing = TRUE), ]
        mp <- mp[1, , drop = FALSE]
      }
      c(
        list(
          list(text = rag_fmt_key_fin(kf), priority = 1),
          list(text = rag_fmt_company_fin(cf), priority = 2),
          list(text = rag_fmt_risk_indicators(mp), priority = 3),
          list(text = news_section(10, 8), priority = 4)
        ),
        if (broad) list(
          list(text = rag_fmt_valuation(rag_read_csv("valuation_metrics.csv", symbol, data_dir = data_dir)), priority = 5),
          list(text = rag_fmt_model_params(mp), priority = 6)
        )
      )
    },

    risk = {
      mp <- rag_read_csv("model_parameters.csv", symbol, data_dir = data_dir)
      if (!is.null(mp) && nrow(mp) > 0) {
        mp <- mp[order(mp$snapshot_date, decreasing = TRUE), ]
        mp <- mp[1, , drop = FALSE]
      }
      cf <- rag_read_csv("company_financials.csv", symbol, data_dir = data_dir)
      c(
        list(
          list(text = rag_fmt_model_params(mp), priority = 1),
          list(text = rag_fmt_credit(cf), priority = 2),
          list(text = news_section(5, 8, "RECENT NEWS (Macro Focus)"), priority = 3)
        ),
        if (broad) list(
          list(text = rag_fmt_key_fin(rag_read_csv("key_financials.csv", symbol, data_dir = data_dir)), priority = 4)
        )
      )
    },

    list()
  )

  sections <- Filter(Negate(is.null), sections)
  if (length(sections) == 0) return("")

  rag_trim_to_budget(sections, token_budget)
}
