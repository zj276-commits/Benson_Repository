# rag.R
# Retrieval helpers for agent grounding and RAG context assembly.

RAG_CONFIG <- list(
  embedding_model = "text-embedding-3-small",
  token_budget = 3000L,
  stale_warning_days = 3L,
  stale_critical_days = 7L
)

.rag_cache <- new.env(parent = emptyenv())

rag_clear_cache <- function() {
  keys <- ls(.rag_cache, all.names = TRUE)
  if (length(keys) > 0) rm(list = keys, envir = .rag_cache)
  invisible(TRUE)
}

rag_data_dir <- function() {
  if (exists("get_export_dir", mode = "function")) get_export_dir() else getwd()
}

rag_read_csv <- function(filename, symbol = NULL, col = "ticker", data_dir = NULL) {
  if (is.null(data_dir)) data_dir <- rag_data_dir()
  path <- file.path(data_dir, filename)
  if (!file.exists(path)) return(NULL)

  df <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  if (is.null(symbol)) return(df)

  if (col %in% names(df)) {
    df <- df[!is.na(df[[col]]) & df[[col]] == symbol, , drop = FALSE]
  } else if ("Symbol" %in% names(df)) {
    df <- df[!is.na(df$Symbol) & df$Symbol == symbol, , drop = FALSE]
  }
  if (nrow(df) == 0) return(NULL)
  df
}

rag_scale01 <- function(x) {
  if (length(x) == 0) return(numeric(0))
  rng <- range(x, na.rm = TRUE)
  if (!all(is.finite(rng))) return(rep(0, length(x)))
  if (identical(rng[[1]], rng[[2]])) return(rep(1, length(x)))
  (x - rng[[1]]) / (rng[[2]] - rng[[1]])
}

rag_get_embeddings <- function(texts, api_key = Sys.getenv("OPENAI_API_KEY")) {
  if (!nzchar(api_key)) return(NULL)
  texts <- trimws(as.character(texts))
  texts <- texts[nzchar(texts)]
  if (length(texts) == 0) return(NULL)

  resp <- tryCatch({
    request("https://api.openai.com/v1/embeddings") |>
      req_headers(
        Authorization = paste0("Bearer ", api_key),
        `Content-Type` = "application/json"
      ) |>
      req_body_json(list(model = RAG_CONFIG$embedding_model, input = texts)) |>
      req_timeout(30) |>
      req_retry(max_tries = 2, backoff = ~ 1) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
  }, error = function(e) {
    message("[RAG] Embedding request failed: ", conditionMessage(e))
    NULL
  })

  if (is.null(resp) || resp_status(resp) != 200) return(NULL)

  payload <- tryCatch(resp_body_json(resp), error = function(e) NULL)
  if (is.null(payload) || is.null(payload$data) || length(payload$data) == 0) return(NULL)
  do.call(rbind, lapply(payload$data, function(item) unlist(item$embedding)))
}

rag_cosine_sim <- function(query_vec, doc_matrix) {
  if (is.null(doc_matrix) || nrow(doc_matrix) == 0) return(numeric(0))
  denom <- sqrt(rowSums(doc_matrix^2)) * sqrt(sum(query_vec^2))
  denom[denom == 0] <- 1
  as.numeric(doc_matrix %*% query_vec) / denom
}

rag_agent_focus <- function(agent_type) {
  switch(agent_type,
    fundamentals = "financial performance earnings revenue margins valuation cash flow balance sheet",
    news = "major market events company announcements regulation macro headlines guidance earnings",
    technical = "price momentum volatility technical signals trend reversal trading regime",
    bull = "positive catalysts growth opportunities upside demand innovation competitive advantage",
    bear = "downside risks headwinds competition valuation pressure margin compression regulation",
    risk = "risk factors leverage volatility drawdown macro uncertainty geopolitical threats",
    "financial analysis stock market"
  )
}

rag_agent_keywords <- function(agent_type) {
  switch(agent_type,
    fundamentals = c("earnings", "guidance", "margin", "cash flow", "regulation"),
    news = c("earnings", "guidance", "tariff", "rate", "regulation", "ai"),
    technical = c("volatility", "momentum", "drawdown", "price", "trend"),
    bull = c("growth", "launch", "partnership", "demand", "ai"),
    bear = c("tariff", "rate", "investigation", "downturn", "competition"),
    risk = c("tariff", "rate", "geopolitical", "regulation", "supply chain"),
    character(0)
  )
}

rag_estimate_tokens <- function(text) {
  if (is.null(text) || !nzchar(text)) return(0L)
  as.integer(ceiling(nchar(text) / 3.5))
}

rag_trim_to_budget <- function(sections, max_tokens = NULL) {
  if (is.null(max_tokens)) max_tokens <- RAG_CONFIG$token_budget
  sections <- Filter(function(section) nzchar(section$text), sections)
  if (length(sections) == 0) return("")

  for (i in seq_along(sections)) {
    sections[[i]]$tokens <- rag_estimate_tokens(sections[[i]]$text)
  }
  sections <- sections[order(vapply(sections, `[[`, numeric(1), "priority"))]

  total_tokens <- sum(vapply(sections, `[[`, integer(1), "tokens"))
  if (total_tokens <= max_tokens) {
    return(paste(vapply(sections, `[[`, character(1), "text"), collapse = "\n\n"))
  }

  kept <- character(0)
  remaining <- max_tokens
  for (section in sections) {
    if (section$tokens <= remaining) {
      kept <- c(kept, section$text)
      remaining <- remaining - section$tokens
      next
    }
    if (remaining <= 200L) next

    char_limit <- as.integer(remaining * 3.5)
    truncated <- substr(section$text, 1, char_limit)
    last_break <- regexpr("\n[^\n]*$", truncated)
    if (last_break > 0) truncated <- substr(truncated, 1, last_break - 1)
    kept <- c(kept, paste0(truncated, "\n... [trimmed to fit token budget]"))
    remaining <- 0L
  }

  paste(kept, collapse = "\n\n")
}

rag_freshness_label <- function(date_value) {
  if (is.null(date_value) || length(date_value) == 0) return("")
  parsed <- suppressWarnings(as.Date(date_value[[1]]))
  if (is.na(parsed)) return("")

  age_days <- as.integer(Sys.Date() - parsed)
  status <- if (age_days <= RAG_CONFIG$stale_warning_days) {
    "FRESH"
  } else if (age_days <= RAG_CONFIG$stale_critical_days) {
    "AGING"
  } else {
    "STALE"
  }

  sprintf("[Data: %s | %s (%d day%s ago)]",
          as.character(parsed), status, age_days, if (age_days == 1) "" else "s")
}

rag_annotate_section <- function(section_text, date_value) {
  if (!nzchar(section_text)) return("")
  label <- rag_freshness_label(date_value)
  if (!nzchar(label)) return(section_text)

  first_newline <- regexpr("\n", section_text, fixed = TRUE)
  if (first_newline > 0) {
    header <- substr(section_text, 1, first_newline - 1)
    rest <- substr(section_text, first_newline, nchar(section_text))
    return(paste0(header, " ", label, rest))
  }

  paste0(section_text, " ", label)
}

rag_prepare_news_df <- function(news_df) {
  news_df <- dedupe_news_archive(news_df)
  if (is.null(news_df) || nrow(news_df) == 0) return(NULL)
  news_df$published_et <- format(
    as.POSIXct(news_df$Published, origin = "1970-01-01", tz = "UTC"),
    "%Y-%m-%d %H:%M",
    tz = "America/New_York"
  )
  news_df
}

rag_agent_news_config <- function(agent_type) {
  switch(agent_type,
    fundamentals = list(n_company = 8L, n_macro = 5L, label = "RECENT NEWS"),
    news = list(n_company = 15L, n_macro = 10L, label = "NEWS HEADLINES"),
    technical = list(n_company = 5L, n_macro = 5L, label = "RECENT NEWS"),
    bull = list(n_company = 10L, n_macro = 5L, label = "RECENT NEWS"),
    bear = list(n_company = 10L, n_macro = 8L, label = "RECENT NEWS"),
    risk = list(n_company = 5L, n_macro = 8L, label = "RECENT NEWS (Macro Focus)"),
    NULL
  )
}

rag_read_news_df <- function(symbol, n_company = 10L, n_macro = 8L, data_dir = NULL, broad = FALSE) {
  all_news <- rag_read_csv("news_archive.csv", symbol = NULL, data_dir = data_dir)
  all_news <- rag_prepare_news_df(all_news)
  if (is.null(all_news) || nrow(all_news) == 0) return(NULL)

  company_pool_n <- if (broad) max(n_company * 4L, n_company + 16L) else max(n_company * 2L, n_company + 8L)
  macro_pool_n <- if (broad) max(n_macro * 4L, n_macro + 16L) else max(n_macro * 2L, n_macro + 8L)

  company <- all_news[all_news$Symbol == symbol, , drop = FALSE]
  macro <- all_news[all_news$Symbol == "MACRO", , drop = FALSE]
  company <- head(company[order(company$Published, decreasing = TRUE), , drop = FALSE], company_pool_n)
  macro <- head(macro[order(macro$Published, decreasing = TRUE), , drop = FALSE], macro_pool_n)

  combined <- dplyr::bind_rows(company, macro)
  if (nrow(combined) == 0) return(NULL)
  combined
}

rag_filter_news_candidates <- function(news_df, symbol) {
  if (is.null(news_df) || nrow(news_df) == 0) return(NULL)
  mention_info <- score_company_mentions(news_df, symbol)
  if (nrow(mention_info) != nrow(news_df)) return(news_df)

  news_df <- cbind(news_df, mention_info)
  keep_idx <- news_df$Symbol != symbol | news_df$company_mentioned
  filtered <- news_df[keep_idx, , drop = FALSE]
  if (nrow(filtered) == 0) return(news_df)
  filtered
}

rag_score_news <- function(news_df, symbol, agent_type, api_key = Sys.getenv("OPENAI_API_KEY")) {
  if (is.null(news_df) || nrow(news_df) == 0) return(news_df)

  heuristic <- score_news_relevance(
    news_df,
    symbol = symbol,
    keywords = rag_agent_keywords(agent_type),
    as_of = Sys.Date()
  )
  heuristic_scaled <- rag_scale01(heuristic)

  title_blob <- paste(news_df$Title, news_df$Summary)
  cache_key <- NULL
  if (requireNamespace("digest", quietly = TRUE)) {
    cache_key <- paste0("news_emb_", digest::digest(title_blob))
  }

  doc_embeddings <- if (!is.null(cache_key) && exists(cache_key, envir = .rag_cache, inherits = FALSE)) {
    get(cache_key, envir = .rag_cache, inherits = FALSE)
  } else {
    rag_get_embeddings(title_blob, api_key = api_key)
  }

  if (!is.null(cache_key) && !is.null(doc_embeddings)) {
    assign(cache_key, doc_embeddings, envir = .rag_cache)
  }

  if (is.null(doc_embeddings)) {
    news_df$relevance_score <- round(heuristic_scaled, 4)
    return(news_df)
  }

  query_text <- paste(symbol, TICKER_LABELS[[symbol]] %||% symbol, rag_agent_focus(agent_type))
  query_embedding <- rag_get_embeddings(query_text, api_key = api_key)
  if (is.null(query_embedding)) {
    news_df$relevance_score <- round(heuristic_scaled, 4)
    return(news_df)
  }

  semantic_scaled <- rag_scale01(rag_cosine_sim(query_embedding[1, ], doc_embeddings))
  combined_score <- 0.75 * semantic_scaled + 0.25 * heuristic_scaled
  news_df$relevance_score <- round(combined_score, 4)
  news_df
}

rag_get_ranked_news_df <- function(symbol, agent_type, n_company = NULL, n_macro = NULL,
                                   data_dir = NULL, use_semantic = TRUE, broad = FALSE) {
  if (is.null(n_company) || is.null(n_macro)) {
    cfg <- rag_agent_news_config(agent_type)
    if (is.null(cfg)) return(NULL)
    n_company <- cfg$n_company
    n_macro <- cfg$n_macro
  }

  extra_rows <- if (broad) 8L else 0L

  news_df <- rag_read_news_df(
    symbol,
    n_company = n_company + extra_rows,
    n_macro = n_macro + extra_rows,
    data_dir = data_dir,
    broad = broad
  )
  if (is.null(news_df) || nrow(news_df) == 0) return(NULL)

  news_df <- rag_filter_news_candidates(news_df, symbol)
  if (is.null(news_df) || nrow(news_df) == 0) return(NULL)

  if (use_semantic) {
    news_df <- rag_score_news(news_df, symbol = symbol, agent_type = agent_type)
  } else {
    news_df$relevance_score <- rag_scale01(score_news_relevance(
      news_df,
      symbol = symbol,
      keywords = rag_agent_keywords(agent_type),
      as_of = Sys.Date()
    ))
  }

  rag_rank_news_mix(
    news_df,
    symbol = symbol,
    n_company = n_company + extra_rows,
    n_macro = n_macro + extra_rows
  )
}

rag_rank_news_mix <- function(news_df, symbol, n_company = 10L, n_macro = 8L) {
  if (is.null(news_df) || nrow(news_df) == 0) return(NULL)
  if (!"relevance_score" %in% names(news_df)) news_df$relevance_score <- 0

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
  combined[order(combined$relevance_score, combined$Published, decreasing = TRUE), , drop = FALSE]
}

rag_format_news <- function(news_df, show_scores = TRUE) {
  if (is.null(news_df) || nrow(news_df) == 0) return("")

  lines <- vapply(seq_len(nrow(news_df)), function(i) {
    row <- news_df[i, ]
    tag <- if (identical(row$Symbol, "MACRO")) "[MACRO]" else paste0("[", row$Symbol, "]")
    source <- row$Source %||% ""
    timestamp <- row$published_et %||% ""
    summary <- row$Summary %||% ""
    summary <- if (nzchar(summary)) paste0(" -- ", substr(summary, 1, 160)) else ""
    score_tag <- if (show_scores && !is.na(row$relevance_score)) sprintf(" [rel: %.2f]", row$relevance_score) else ""
    sprintf("  %s %s (%s%s)%s%s",
            tag,
            row$Title,
            source,
            if (nzchar(timestamp)) paste0(", ", timestamp, " ET") else "",
            summary,
            score_tag)
  }, character(1))

  paste(lines, collapse = "\n")
}

rag_get_news_context <- function(symbol, agent_type, n_company, n_macro,
                                 data_dir = NULL, use_semantic = TRUE, broad = FALSE) {
  news_df <- rag_get_ranked_news_df(
    symbol = symbol,
    agent_type = agent_type,
    n_company = n_company,
    n_macro = n_macro,
    data_dir = data_dir,
    use_semantic = use_semantic,
    broad = broad
  )
  if (is.null(news_df)) return("")
  rag_format_news(news_df, show_scores = use_semantic)
}

rag_fmt_val <- function(x, suffix = "", prefix = "", mult = 1, digits = NULL) {
  if (is.null(x) || length(x) == 0) return(NULL)
  value <- suppressWarnings(as.numeric(x[[1]]))
  if (is.na(value)) return(NULL)
  value <- value * mult
  if (is.null(digits)) digits <- if (abs(value) >= 10) 1 else 2
  paste0(prefix, formatC(value, format = "f", digits = digits, big.mark = ","), suffix)
}

rag_latest_date <- function(values) {
  if (is.null(values) || length(values) == 0) return(NULL)
  parsed <- suppressWarnings(as.Date(values))
  parsed <- parsed[!is.na(parsed)]
  if (length(parsed) == 0) return(NULL)
  max(parsed)
}

rag_fmt_key_fin <- function(kf) {
  if (is.null(kf) || nrow(kf) == 0) return("")
  k <- kf[1, , drop = FALSE]

  parts <- Filter(Negate(is.null), list(
    rag_fmt_val(k$share_price, prefix = "Share Price: $", digits = 2),
    rag_fmt_val(k$target_price, prefix = "Analyst Target Price: $", digits = 2),
    rag_fmt_val(k$market_cap_bn, prefix = "Market Cap: $", suffix = "B", digits = 1),
    rag_fmt_val(k$volume_mn_shares, prefix = "Average Volume: ", suffix = "M shares", digits = 1),
    rag_fmt_val(k$free_float_pct, prefix = "Free Float: ", suffix = "%", digits = 1),
    rag_fmt_val(k$dividend_yield_pct, prefix = "Dividend Yield: ", suffix = "%", digits = 2),
    rag_fmt_val(k$net_debt_equity_pct, prefix = "Net Debt/Equity: ", suffix = "%", digits = 1),
    rag_fmt_val(k$fwd_pe, prefix = "Forward P/E: ", suffix = "x", digits = 1),
    rag_fmt_val(k$pe_ttm, prefix = "P/E (TTM): ", suffix = "x", digits = 1),
    rag_fmt_val(k$pb, prefix = "P/B: ", suffix = "x", digits = 1),
    rag_fmt_val(k$roe_pct, prefix = "ROE: ", suffix = "%", digits = 1),
    rag_fmt_val(k$beta, prefix = "Beta: ", digits = 2),
    rag_fmt_val(k$book_value, prefix = "Book Value/Share: $", digits = 2),
    rag_fmt_val(k$eps_ttm, prefix = "EPS (TTM): $", digits = 2),
    if (!is.na(suppressWarnings(as.numeric(k$fifty_two_wk_low))) &&
        !is.na(suppressWarnings(as.numeric(k$fifty_two_wk_high)))) {
      sprintf("52-Week Range: $%.2f - $%.2f",
              as.numeric(k$fifty_two_wk_low), as.numeric(k$fifty_two_wk_high))
    }
  ))

  rag_annotate_section(
    paste0("=== KEY FINANCIALS ===\n", paste(parts, collapse = "\n")),
    k$fetch_date
  )
}

rag_fmt_company_fin <- function(cf) {
  if (is.null(cf) || nrow(cf) == 0) return("")
  cf <- cf[order(cf$fiscal_year_end), , drop = FALSE]
  cf <- tail(cf, min(4L, nrow(cf)))
  years <- cf$fiscal_year

  fmt_row <- function(label, values, suffix = "", is_pct = FALSE) {
    formatted <- vapply(values, function(value) {
      value <- suppressWarnings(as.numeric(value))
      if (is.na(value)) return("--")
      if (is_pct) return(sprintf("%+.1f%%", value))
      if (abs(value) >= 1000) return(formatC(value, format = "f", digits = 0, big.mark = ","))
      paste0(formatC(value, format = "f", digits = 1, big.mark = ","), suffix)
    }, character(1))
    sprintf("  %-22s %s", label, paste(sprintf("%12s", formatted), collapse = ""))
  }

  header <- sprintf("  %-22s %s", "", paste(sprintf("%12s", years), collapse = ""))
  income_rows <- c(
    fmt_row("Revenue ($M)", cf$revenue_mn),
    fmt_row("Revenue YoY", cf$revenue_yoy, is_pct = TRUE),
    fmt_row("Gross Profit ($M)", cf$gross_profit_mn),
    fmt_row("EBITDA ($M)", cf$ebitda_mn),
    fmt_row("EBITDA YoY", cf$ebitda_yoy, is_pct = TRUE),
    fmt_row("Net Income ($M)", cf$net_income_mn),
    fmt_row("Net Income YoY", cf$ni_yoy, is_pct = TRUE),
    fmt_row("FCF ($M)", cf$fcf_mn),
    fmt_row("FCF YoY", cf$fcf_yoy, is_pct = TRUE),
    fmt_row("CAPEX ($M)", cf$capex_mn),
    fmt_row("CAPEX YoY", cf$capex_yoy, is_pct = TRUE),
    fmt_row("EBITDA Margin", cf$ebitda_margin_pct, suffix = "%"),
    fmt_row("Net Margin", cf$net_margin_pct, suffix = "%"),
    fmt_row("ROA", cf$roa_pct, suffix = "%"),
    fmt_row("ROE", cf$roe_pct, suffix = "%")
  )
  credit_rows <- c(
    fmt_row("Debt/Equity", cf$debt_equity_pct, suffix = "%"),
    fmt_row("Net Debt/Equity", cf$net_debt_equity_pct, suffix = "%"),
    fmt_row("Debt/Assets", cf$debt_assets_pct, suffix = "%"),
    fmt_row("Net Debt/Assets", cf$net_debt_assets_pct, suffix = "%"),
    fmt_row("Debt/EBITDA", cf$debt_ebitda, suffix = "x"),
    fmt_row("EBITDA/Interest", cf$ebitda_int_exp, suffix = "x"),
    fmt_row("Cash+CFO/ST Debt", cf$cash_cfo_st_debt, suffix = "x"),
    fmt_row("Receivables Days", cf$receivables_days),
    fmt_row("Days Payable", cf$days_payable),
    fmt_row("Inventory Days", cf$inventory_days)
  )

  section <- paste0(
    "=== INCOME STATEMENT & PROFITABILITY (Annual, $M) ===\n",
    "LEGEND: YoY = year-over-year growth, FCF = free cash flow, CAPEX = capital expenditure\n",
    header, "\n", paste(income_rows, collapse = "\n"),
    "\n\n=== BALANCE SHEET & CREDIT METRICS ===\n",
    "LEGEND: D/E = debt-to-equity, EBITDA/Interest = interest coverage\n",
    header, "\n", paste(credit_rows, collapse = "\n")
  )

  rag_annotate_section(section, rag_latest_date(cf$fiscal_year_end))
}

rag_fmt_valuation <- function(vm) {
  if (is.null(vm) || nrow(vm) == 0) return("")
  vm <- vm[order(vm$fiscal_year), , drop = FALSE]
  vm <- tail(vm, min(4L, nrow(vm)))
  years <- vm$fiscal_year

  fmt_row <- function(label, values, suffix = "") {
    formatted <- vapply(values, function(value) {
      value <- suppressWarnings(as.numeric(value))
      if (is.na(value)) return("--")
      paste0(formatC(value, format = "f", digits = 1), suffix)
    }, character(1))
    sprintf("  %-22s %s", label, paste(sprintf("%12s", formatted), collapse = ""))
  }

  header <- sprintf("  %-22s %s", "", paste(sprintf("%12s", years), collapse = ""))
  section <- paste0(
    "=== VALUATION MULTIPLES (Historical) ===\n",
    "LEGEND: P/E = price/earnings, P/B = price/book, EV/EBITDA = enterprise value / EBITDA\n",
    header, "\n",
    paste(c(
      fmt_row("P/E", vm$pe_x, "x"),
      fmt_row("P/B", vm$pb_x, "x"),
      fmt_row("Dividend Yield", vm$dividend_yield_pct, "%"),
      fmt_row("EV/EBITDA", vm$ev_ebitda_x, "x"),
      fmt_row("FCF Yield", vm$fcf_yield_pct, "%")
    ), collapse = "\n")
  )

  section
}

rag_fmt_model_params <- function(mp) {
  if (is.null(mp) || nrow(mp) == 0) return("")
  m <- mp[1, , drop = FALSE]

  normality_text <- NULL
  normality_value <- suppressWarnings(as.numeric(m$normality_pvalue))
  if (!is.na(normality_value)) {
    normality_text <- paste0(
      "Normality p-value: ", format(normality_value, scientific = TRUE, digits = 2),
      if (normality_value < 0.05) " (non-normal returns)" else " (approximately normal returns)"
    )
  }

  sections <- list(
    c(
      "  --- GBM (Geometric Brownian Motion) ---",
      Filter(Negate(is.null), list(
        rag_fmt_val(m$spot_price, prefix = "  Spot Price: $", digits = 2),
        rag_fmt_val(m$gbm_mu_annual, prefix = "  Drift (mu, annualized): ", suffix = "%", mult = 100, digits = 1),
        rag_fmt_val(m$gbm_sigma_annual, prefix = "  Volatility (sigma, annualized): ", suffix = "%", mult = 100, digits = 1),
        if (!is.na(suppressWarnings(as.numeric(m$gbm_mu_ci95_low))) && !is.na(suppressWarnings(as.numeric(m$gbm_mu_ci95_high)))) {
          sprintf("  Mu 95%% CI: [%.1f%%, %.1f%%]",
                  100 * as.numeric(m$gbm_mu_ci95_low), 100 * as.numeric(m$gbm_mu_ci95_high))
        },
        if (!is.na(suppressWarnings(as.numeric(m$gbm_sigma_ci95_low))) && !is.na(suppressWarnings(as.numeric(m$gbm_sigma_ci95_high)))) {
          sprintf("  Sigma 95%% CI: [%.1f%%, %.1f%%]",
                  100 * as.numeric(m$gbm_sigma_ci95_low), 100 * as.numeric(m$gbm_sigma_ci95_high))
        }
      ))
    ),
    c(
      "  --- Binomial Lattice (RWPM) ---",
      Filter(Negate(is.null), list(
        rag_fmt_val(m$lattice_u, prefix = "  Up factor (u): ", digits = 4),
        rag_fmt_val(m$lattice_d, prefix = "  Down factor (d): ", digits = 4),
        rag_fmt_val(m$lattice_p_real, prefix = "  Real-world up probability: ", digits = 4),
        rag_fmt_val(m$lattice_p_rn, prefix = "  Risk-neutral up probability: ", digits = 4)
      ))
    ),
    c(
      "  --- Single Index Model (vs SPY) ---",
      "  LEGEND: Beta = systematic risk, Alpha = excess return, R-squared = market dependence",
      Filter(Negate(is.null), list(
        rag_fmt_val(m$sim_alpha_annual, prefix = "  Alpha (annualized): ", suffix = "%", mult = 100, digits = 2),
        rag_fmt_val(m$sim_beta, prefix = "  Beta vs SPY: ", digits = 3),
        rag_fmt_val(m$sim_r_squared, prefix = "  R-squared: ", suffix = "%", mult = 100, digits = 1),
        rag_fmt_val(m$sim_resid_std, prefix = "  Residual Std: ", digits = 4)
      ))
    ),
    c(
      "  --- Volatility & Distribution ---",
      "  LEGEND: Kurtosis > 3 implies heavier tails; negative skew implies more downside asymmetry",
      Filter(Negate(is.null), list(
        rag_fmt_val(m$vol_annual_realized, prefix = "  Realized Volatility: ", suffix = "%", mult = 100, digits = 1),
        rag_fmt_val(m$vol_annual_downside, prefix = "  Downside Volatility: ", suffix = "%", mult = 100, digits = 1),
        rag_fmt_val(m$vol_annual_20d, prefix = "  20-Day Volatility: ", suffix = "%", mult = 100, digits = 1),
        rag_fmt_val(m$skewness, prefix = "  Skewness: ", digits = 3),
        rag_fmt_val(m$kurtosis, prefix = "  Kurtosis: ", digits = 3),
        rag_fmt_val(m$max_drawdown, prefix = "  Max Drawdown: ", suffix = "%", mult = 100, digits = 1),
        normality_text,
        rag_fmt_val(m$outlier_fraction, prefix = "  Outlier Fraction: ", suffix = "%", mult = 100, digits = 2),
        if (!is.null(m$tags) && nzchar(m$tags)) paste0("  Model Tags: ", m$tags)
      ))
    )
  )

  section <- paste0(
    "=== QUANTITATIVE MODEL PARAMETERS ===\n",
    if (!is.null(m$snapshot_date) && nzchar(m$snapshot_date)) paste0("Snapshot Date: ", m$snapshot_date, "\n") else "",
    paste(vapply(sections, paste, collapse = "\n", character(1)), collapse = "\n")
  )

  rag_annotate_section(section, m$snapshot_date)
}

rag_fmt_prices <- function(hp) {
  if (is.null(hp) || nrow(hp) == 0) return("")
  hp <- hp[order(hp$Date, decreasing = TRUE), , drop = FALSE]
  hp <- head(hp, 15)

  lines <- vapply(seq_len(nrow(hp)), function(i) {
    row <- hp[i, ]
    sprintf("  %s  O:%.2f  H:%.2f  L:%.2f  C:%.2f  Vol:%s",
            row$Date, row$Open, row$High, row$Low, row$Close,
            formatC(as.numeric(row$Volume), format = "f", digits = 0, big.mark = ","))
  }, character(1))

  rag_annotate_section(
    paste0("=== RECENT DAILY PRICES (last 15 trading days, OHLCV) ===\n", paste(lines, collapse = "\n")),
    hp$Date[1]
  )
}

rag_fmt_risk_indicators <- function(mp) {
  if (is.null(mp) || nrow(mp) == 0) return("")
  m <- mp[1, , drop = FALSE]
  parts <- Filter(Negate(is.null), list(
    rag_fmt_val(m$vol_annual_realized, prefix = "Realized Volatility: ", suffix = "%", mult = 100, digits = 1),
    rag_fmt_val(m$vol_annual_downside, prefix = "Downside Volatility: ", suffix = "%", mult = 100, digits = 1),
    rag_fmt_val(m$max_drawdown, prefix = "Max Drawdown: ", suffix = "%", mult = 100, digits = 1),
    rag_fmt_val(m$kurtosis, prefix = "Kurtosis: ", digits = 3),
    rag_fmt_val(m$sim_beta, prefix = "Beta vs SPY: ", digits = 3),
    rag_fmt_val(m$gbm_sigma_annual, prefix = "GBM Sigma: ", suffix = "%", mult = 100, digits = 1)
  ))
  if (length(parts) == 0) return("")
  paste0("=== RISK INDICATORS ===\n", paste(parts, collapse = "\n"))
}

rag_fmt_credit <- function(cf) {
  if (is.null(cf) || nrow(cf) == 0) return("")
  cf <- cf[order(cf$fiscal_year_end), , drop = FALSE]
  cf <- tail(cf, min(4L, nrow(cf)))
  years <- cf$fiscal_year

  fmt_row <- function(label, values, suffix = "") {
    formatted <- vapply(values, function(value) {
      value <- suppressWarnings(as.numeric(value))
      if (is.na(value)) return("--")
      paste0(formatC(value, format = "f", digits = 1), suffix)
    }, character(1))
    sprintf("  %-22s %s", label, paste(sprintf("%12s", formatted), collapse = ""))
  }

  header <- sprintf("  %-22s %s", "", paste(sprintf("%12s", years), collapse = ""))
  paste0(
    "=== CREDIT & LEVERAGE METRICS ===\n",
    "LEGEND: D/E = debt-to-equity, EBITDA/Interest = interest coverage\n",
    header, "\n",
    paste(c(
      fmt_row("Debt/Equity", cf$debt_equity_pct, "%"),
      fmt_row("Net Debt/Equity", cf$net_debt_equity_pct, "%"),
      fmt_row("Debt/Assets", cf$debt_assets_pct, "%"),
      fmt_row("Net Debt/Assets", cf$net_debt_assets_pct, "%"),
      fmt_row("Debt/EBITDA", cf$debt_ebitda, "x"),
      fmt_row("EBITDA/Interest", cf$ebitda_int_exp, "x"),
      fmt_row("ST Debt/Total Debt", cf$st_debt_total_debt),
      fmt_row("Cash+CFO/ST Debt", cf$cash_cfo_st_debt, "x")
    ), collapse = "\n")
  )
}

rag_build_context_bundle <- function(symbol, agent_type, data_dir = NULL,
                                     use_semantic = TRUE, token_budget = NULL,
                                     broad = FALSE) {
  if (is.null(data_dir)) data_dir <- rag_data_dir()
  if (is.null(token_budget)) token_budget <- RAG_CONFIG$token_budget

  news_df_used <- NULL
  news_section <- function(n_company, n_macro, label = "RECENT NEWS") {
    news_df <- rag_get_ranked_news_df(
      symbol = symbol,
      agent_type = agent_type,
      n_company = n_company,
      n_macro = n_macro,
      data_dir = data_dir,
      use_semantic = use_semantic,
      broad = broad
    )
    if (is.null(news_df)) return("")
    news_df_used <<- news_df
    news_text <- rag_format_news(news_df, show_scores = use_semantic)
    if (!nzchar(news_text)) return("")
    header <- if (use_semantic) {
      paste0("=== ", label, " (ranked by semantic relevance) ===\n")
    } else {
      paste0("=== ", label, " ===\n")
    }
    paste0(header, news_text)
  }

  sections <- switch(agent_type,
    fundamentals = {
      kf <- rag_read_csv("key_financials.csv", symbol = symbol, data_dir = data_dir)
      cf <- rag_read_csv("company_financials.csv", symbol = symbol, data_dir = data_dir)
      vm <- rag_read_csv("valuation_metrics.csv", symbol = symbol, data_dir = data_dir)
      mp <- rag_read_csv("model_parameters.csv", symbol = symbol, data_dir = data_dir)
      if (!is.null(mp) && nrow(mp) > 0) mp <- mp[order(mp$snapshot_date, decreasing = TRUE), , drop = FALSE][1, , drop = FALSE]
      list(
        list(text = rag_fmt_key_fin(kf), priority = 1),
        list(text = rag_fmt_company_fin(cf), priority = 2),
        list(text = rag_fmt_valuation(vm), priority = 3),
        list(text = news_section(8L, 5L), priority = 4),
        if (broad) list(text = rag_fmt_model_params(mp), priority = 5)
      )
    },
    news = list(
      list(text = news_section(15L, 10L, label = "NEWS HEADLINES"), priority = 1)
    ),
    technical = {
      mp <- rag_read_csv("model_parameters.csv", symbol = symbol, data_dir = data_dir)
      if (!is.null(mp) && nrow(mp) > 0) mp <- mp[order(mp$snapshot_date, decreasing = TRUE), , drop = FALSE][1, , drop = FALSE]
      hp <- rag_read_csv("historical_prices.csv", symbol = symbol, col = "Symbol", data_dir = data_dir)
      list(
        list(text = rag_fmt_model_params(mp), priority = 1),
        list(text = rag_fmt_prices(hp), priority = 2),
        if (broad) list(text = news_section(5L, 5L), priority = 3),
        if (broad) list(text = rag_fmt_key_fin(rag_read_csv("key_financials.csv", symbol = symbol, data_dir = data_dir)), priority = 4)
      )
    },
    bull = {
      kf <- rag_read_csv("key_financials.csv", symbol = symbol, data_dir = data_dir)
      cf <- rag_read_csv("company_financials.csv", symbol = symbol, data_dir = data_dir)
      list(
        list(text = rag_fmt_key_fin(kf), priority = 1),
        list(text = rag_fmt_company_fin(cf), priority = 2),
        list(text = news_section(10L, 5L), priority = 3),
        if (broad) list(text = rag_fmt_valuation(rag_read_csv("valuation_metrics.csv", symbol = symbol, data_dir = data_dir)), priority = 4)
      )
    },
    bear = {
      kf <- rag_read_csv("key_financials.csv", symbol = symbol, data_dir = data_dir)
      cf <- rag_read_csv("company_financials.csv", symbol = symbol, data_dir = data_dir)
      mp <- rag_read_csv("model_parameters.csv", symbol = symbol, data_dir = data_dir)
      if (!is.null(mp) && nrow(mp) > 0) mp <- mp[order(mp$snapshot_date, decreasing = TRUE), , drop = FALSE][1, , drop = FALSE]
      list(
        list(text = rag_fmt_key_fin(kf), priority = 1),
        list(text = rag_fmt_company_fin(cf), priority = 2),
        list(text = rag_fmt_risk_indicators(mp), priority = 3),
        list(text = news_section(10L, 8L), priority = 4),
        if (broad) list(text = rag_fmt_valuation(rag_read_csv("valuation_metrics.csv", symbol = symbol, data_dir = data_dir)), priority = 5),
        if (broad) list(text = rag_fmt_model_params(mp), priority = 6)
      )
    },
    risk = {
      mp <- rag_read_csv("model_parameters.csv", symbol = symbol, data_dir = data_dir)
      if (!is.null(mp) && nrow(mp) > 0) mp <- mp[order(mp$snapshot_date, decreasing = TRUE), , drop = FALSE][1, , drop = FALSE]
      cf <- rag_read_csv("company_financials.csv", symbol = symbol, data_dir = data_dir)
      list(
        list(text = rag_fmt_model_params(mp), priority = 1),
        list(text = rag_fmt_credit(cf), priority = 2),
        list(text = news_section(5L, 8L, label = "RECENT NEWS (Macro Focus)"), priority = 3),
        if (broad) list(text = rag_fmt_key_fin(rag_read_csv("key_financials.csv", symbol = symbol, data_dir = data_dir)), priority = 4)
      )
    },
    list()
  )

  sections <- Filter(Negate(is.null), sections)
  sections <- Filter(function(section) nzchar(section$text), sections)
  if (length(sections) == 0) {
    return(list(text = "", sections = list(), news_df = NULL))
  }

  list(
    text = rag_trim_to_budget(sections, max_tokens = token_budget),
    sections = sections,
    news_df = news_df_used
  )
}

rag_retrieve <- function(symbol, agent_type, data_dir = NULL,
                         use_semantic = TRUE, token_budget = NULL,
                         broad = FALSE) {
  rag_build_context_bundle(
    symbol = symbol,
    agent_type = agent_type,
    data_dir = data_dir,
    use_semantic = use_semantic,
    token_budget = token_budget,
    broad = broad
  )$text
}
