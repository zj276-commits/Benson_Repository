# report.R
# AI analyst team report generation & three financial data tables

safe_text <- function(x, fallback = "-") {
  if (is.null(x) || length(x) == 0) return(fallback)
  out <- paste(as.character(x), collapse = " ")
  if (!nzchar(trimws(out))) return(fallback)
  out
}

safe_na <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  as.numeric(x[1])
}

fmt_big <- function(x) {
  if (is.na(x)) return("N/A")
  if (abs(x) >= 1e12) sprintf("$%.2fT", x / 1e12)
  else if (abs(x) >= 1e9) sprintf("$%.2fB", x / 1e9)
  else if (abs(x) >= 1e6) sprintf("$%.2fM", x / 1e6)
  else sprintf("$%.0f", x)
}

fmt_vol <- function(x) {
  if (is.na(x)) return("N/A")
  if (x >= 1e9) sprintf("%.1fB", x / 1e9)
  else if (x >= 1e6) sprintf("%.1fM", x / 1e6)
  else if (x >= 1e3) sprintf("%.1fK", x / 1e3)
  else sprintf("%.0f", x)
}

# ----------------------------
# Load 4 financial tables from CSV (pre-fetched by fetch_financials.R)
# ----------------------------

load_financial_tables <- function(symbol) {
  app_dir <- get_export_dir()
  message("[Tables] Loading for ", symbol, " from ", app_dir)

  kf <- tryCatch(read.csv(file.path(app_dir, "key_financials.csv"), stringsAsFactors=FALSE),
                 error=function(e) { message("[Tables] key_financials.csv error: ", e$message); NULL })
  cf <- tryCatch(read.csv(file.path(app_dir, "company_financials.csv"), stringsAsFactors=FALSE),
                 error=function(e) { message("[Tables] company_financials.csv error: ", e$message); NULL })
  vm <- tryCatch(read.csv(file.path(app_dir, "valuation_metrics.csv"), stringsAsFactors=FALSE),
                 error=function(e) { message("[Tables] valuation_metrics.csv error: ", e$message); NULL })

  message("[Tables] kf=", if(is.null(kf)) "NULL" else nrow(kf), " cf=", if(is.null(cf)) "NULL" else nrow(cf),
          " vm=", if(is.null(vm)) "NULL" else nrow(vm))
  if (is.null(kf) || is.null(cf)) return(NULL)

  kf_row <- kf[kf$ticker == symbol, ]
  cf_sub <- cf[cf$ticker == symbol & !is.na(cf$revenue_mn), ]
  vm_sub <- if (!is.null(vm)) vm[vm$ticker == symbol, ] else NULL

  if (nrow(kf_row) == 0 && nrow(cf_sub) == 0) return(NULL)

  cf_sub <- cf_sub[order(cf_sub$fiscal_year_end), ]
  n <- nrow(cf_sub)
  if (n > 4) cf_sub <- cf_sub[(n-3):n, ]
  if (!is.null(vm_sub) && nrow(vm_sub) > 4) {
    vm_sub <- vm_sub[order(vm_sub$fiscal_year), ]
    vm_sub <- vm_sub[(nrow(vm_sub)-3):nrow(vm_sub), ]
  }

  list(key_financial=kf_row, company_financials=cf_sub, valuation=vm_sub)
}

build_report_tables <- function(symbol) {
  load_financial_tables(symbol)
}

# ----------------------------
# Build prompt-friendly fundamental text from CSV data
# ----------------------------

build_fund_text <- function(tables) {
  if (is.null(tables)) return("No fundamental data available.")
  nf <- function(x, d=1) {
    v <- suppressWarnings(as.numeric(x))
    if (is.null(v) || length(v) == 0 || is.na(v)) "NA" else formatC(round(v, d), format="f", digits=d)
  }
  ni <- function(x) {
    v <- suppressWarnings(as.numeric(x))
    if (is.null(v) || length(v) == 0 || is.na(v)) "NA" else formatC(round(v), format="d", big.mark=",")
  }
  parts <- character(0)
  kf <- tables$key_financial
  if (!is.null(kf) && nrow(kf) > 0) {
    k <- kf[1,]
    parts <- c(parts, paste0(
      "Key Financial: Price=$", nf(k$share_price,2), ", MCap=$", nf(k$market_cap_bn,1), "B",
      ", P/E=", nf(k$pe_ttm,1), ", P/B=", nf(k$pb,1), ", ROE=", nf(k$roe_pct,1), "%",
      ", Fwd P/E=", nf(k$fwd_pe,1), ", Beta=", nf(k$beta,2),
      ", DivYield=", nf(k$dividend_yield_pct,2), "%",
      ", NetDebt/Eq=", nf(k$net_debt_equity_pct,1), "%",
      ", Target=$", nf(k$target_price,2)))
  }
  cf <- tables$company_financials
  if (!is.null(cf) && nrow(cf) > 0) {
    for (i in seq_len(nrow(cf))) {
      r <- cf[i,]
      yoy <- if (!is.na(suppressWarnings(as.numeric(r$revenue_yoy)))) paste0(nf(r$revenue_yoy,1), "%") else "NA"
      parts <- c(parts, paste0(
        as.character(r$fiscal_year), ": Rev=", ni(r$revenue_mn), "M (yoy=", yoy, ")",
        ", GP=", ni(r$gross_profit_mn), "M, EBITDA=", ni(r$ebitda_mn), "M (margin=", nf(r$ebitda_margin_pct,1), "%)",
        ", NI=", ni(r$net_income_mn), "M (margin=", nf(r$net_margin_pct,1), "%)",
        ", FCF=", ni(r$fcf_mn), "M, CAPEX=", ni(r$capex_mn), "M",
        ", ROE=", nf(r$roe_pct,1), "%, ROA=", nf(r$roa_pct,1), "%",
        ", D/E=", nf(r$debt_equity_pct,1), "%, NetD/E=", nf(r$net_debt_equity_pct,1), "%"))
    }
  }
  paste(parts, collapse = "\n")
}

# ----------------------------
# Build multi-analyst report prompt
# ----------------------------

build_report_prompt <- function(symbol, company_name, sector, trend, gbm, lattice, sim,
                                news_text, tables) {

  fund_str <- build_fund_text(tables)

  gbm_str <- if (!is.null(gbm)) {
    sprintf("GBM: annual drift mu=%.4f, annual volatility sigma=%.4f, spot price=$%.2f, %d daily observations",
            gbm$mu_annual, gbm$sigma_annual, gbm$s0, gbm$n_obs)
  } else "GBM parameters unavailable."

  lat_str <- if (!is.null(lattice)) {
    sprintf("Binomial Lattice (RWPM): u=%.6f, d=%.6f, p_real=%.6f, sigma=%.4f",
            lattice$u, lattice$d, lattice$p_real, lattice$sigma_annual)
  } else "Lattice parameters unavailable."

  sim_str <- if (!is.null(sim)) {
    sprintf("Single Index Model vs SPY: beta=%.3f (SE=%.4f, 95%%CI [%.3f, %.3f]), alpha_annual=%.4f, R-squared=%.4f, residual_std=%.6f",
            sim$beta, sim$beta_se, sim$beta_ci95_low, sim$beta_ci95_high,
            sim$alpha_annual, sim$r_squared, sim$resid_std)
  } else "SIM parameters unavailable."

  trend_str <- sprintf(
    "Current price: %s\n1-day change: %s\n7-day change: %s\n30-day change: %s\nAnnualized 20-day volatility: %s",
    fmt_price(trend$latest), fmt_pct(trend$change1d_pct),
    fmt_pct(trend$change7d_pct), fmt_pct(trend$change30d_pct),
    ifelse(is.na(trend$mean_20d_volatility), "N/A", paste0(trend$mean_20d_volatility, "%"))
  )

  system_prompt <- paste0(
    "You are a professional investment research team at a top-tier financial institution producing an institutional-quality equity research report. ",
    "Your team consists of seven specialized analysts who collaboratively produce deep, data-driven analysis:\n\n",
    "ANALYST TEAM:\n",
    "1. Fundamentals Analyst - Evaluates company financials (P/E, P/B, ROE, debt ratios, margins, revenue growth), identifies intrinsic value drivers and red flags.\n",
    "2. News Analyst - Monitors global news and macroeconomic indicators (interest rates, GDP, inflation, tariffs), interpreting their specific impact on this company.\n",
    "3. Technical Analyst - Analyzes quantitative model outputs (GBM drift/volatility, Binomial Lattice up/down factors, Single Index Model beta/alpha/R-squared) and price momentum patterns.\n\n",
    "RESEARCHER TEAM:\n",
    "4. Bull Researcher - Builds the optimistic investment case with specific catalysts, growth drivers, and upside scenarios backed by data.\n",
    "5. Bear Researcher - Builds the pessimistic case with specific risks, competitive threats, valuation concerns, and downside scenarios.\n",
    "They engage in structured debate, referencing actual data points.\n\n",
    "RISK MANAGEMENT & PORTFOLIO MANAGER:\n",
    "6. Risk Manager - Evaluates systematic risk (beta exposure), volatility regime, liquidity risk, concentration risk, supply chain exposure, and regulatory risk.\n",
    "7. Portfolio Manager - Synthesizes all inputs and makes the final purchase recommendation with a specific price target.\n\n",
    "CRITICAL WRITING REQUIREMENTS:\n",
    "- Write at the level of a professional equity research report (like Goldman Sachs, Morgan Stanley, or university investment fund reports).\n",
    "- Every paragraph must be SUBSTANTIVE (8-12 sentences minimum) with SPECIFIC data references.\n",
    "- Cite actual numbers: P/E ratios, market cap, revenue growth rates, beta values, volatility figures, model outputs.\n",
    "- Industry analysis must discuss competitive landscape, key players, market dynamics, growth drivers, and barriers to entry.\n",
    "- Investment overview must weave together fundamentals, technicals, and macro outlook into a cohesive thesis.\n",
    "- Risk analysis must be company-specific (regulatory exposure, supply chain dependencies, competitive threats, valuation concerns).\n",
    "- DO NOT write generic filler text. Every sentence must add analytical value."
  )

  user_prompt <- paste0(
    "Generate a comprehensive equity research report for ", symbol, " (", company_name, "), sector: ", sector, ".\n\n",
    "=== PRICE & TECHNICAL METRICS ===\n", trend_str, "\n\n",
    "=== QUANTITATIVE MODEL OUTPUTS ===\n", gbm_str, "\n", lat_str, "\n", sim_str, "\n\n",
    "=== FUNDAMENTAL DATA ===\n", fund_str, "\n\n",
    "=== RECENT NEWS HEADLINES ===\n", news_text, "\n\n",
    "Return JSON with EXACTLY these keys:\n",
    "- purchase_rating: one of 'Strong Buy', 'Buy', 'Hold', 'Sell', 'Fully Valued'\n",
    "- confidence: integer 0-100 (your team's conviction level)\n",
    "- key_reason: 3-4 sentences explaining the PRIMARY drivers behind the rating, referencing specific financial metrics and model outputs\n",
    "- industry_analysis: a DETAILED paragraph (10-15 sentences minimum) covering: sector overview and market size, competitive landscape and key players (name specific competitors), company's competitive moat and differentiation, technology/product advantages, revenue composition and growth drivers, barriers to entry, regulatory environment, supply chain dynamics, and where this company ranks among peers with specific market share or revenue comparisons\n",
    "- investment_overview: a DETAILED paragraph (10-15 sentences minimum) covering: current valuation assessment citing specific P/E P/B ROE metrics, price momentum analysis (reference the 1d/7d/30d changes), quantitative model interpretation (what the GBM drift and Lattice parameters imply about expected returns), beta and systematic risk exposure (cite the beta value and what it means), historical volatility regime, earnings quality and growth sustainability, balance sheet strength (debt-to-equity), dividend policy, comparison to sector peers, and overall investment thesis tying everything together\n",
    "- macro_environment: 6-8 sentences on the current macro/geopolitical landscape (tariffs, interest rates, trade policy, geopolitical tensions) and how they specifically impact this company\n",
    "- risk_analysis: JSON array of 5-6 STRINGS, each string is one specific risk factor (2-3 sentences with concrete details). Example: [\"Risk factor one...\", \"Risk factor two...\"]. Do NOT use objects, only plain strings.\n",
    "- bull_case: 4-5 sentences from the bull researcher with specific catalysts, growth opportunities, and upside price scenarios backed by data\n",
    "- bear_case: 4-5 sentences from the bear researcher with specific downside scenarios, competitive threats, and warning signs backed by data\n",
    "- target_price_3m: your 3-month price target as a number\n",
    "- risk_level: one of 'Low', 'Medium', 'High', 'Very High'\n",
    "- forecast_trend: one of 'UP', 'DOWN', 'NEUTRAL'"
  )

  list(system = system_prompt, user = user_prompt)
}

# ----------------------------
# Generate full report for one stock
# ----------------------------

generate_stock_report <- function(symbol, stock_data_val, news_data_df, on_progress = NULL) {
  company_name <- TICKER_LABELS[[symbol]] %||% symbol
  sector <- SECTOR_MAP[[symbol]] %||% "Unknown"

  hist <- stock_data_val$daily[stock_data_val$daily$Symbol == symbol, ]
  if (is.null(hist) || nrow(hist) < 5) return(list(ok = FALSE, error = "Insufficient price history."))
  hist <- hist[order(hist$Date), ]

  trend <- compute_ohlc_metrics(hist, lookback = 252)
  gbm <- estimate_gbm_params(hist, 252)
  lattice <- estimate_lattice_params(hist, 1/12, 22)

  mkt <- stock_data_val$daily[stock_data_val$daily$Symbol == MARKET_TICKER, ]
  sim <- compute_sim_params(hist, mkt)

  tables <- tryCatch(build_report_tables(symbol), error = function(e) NULL)

  message("[Report] Generating analyst report for ", symbol)
  agent_outputs <- NULL
  total_tokens <- NULL

  pipeline_result <- tryCatch(
    run_parallel_agents(symbol, stock_data_val, news_data_df,
                        trend = trend, gbm = gbm, lattice = lattice, sim = sim,
                        on_progress = on_progress),
    error = function(e) {
      message("[Report] Parallel analysis error: ", e$message)
      NULL
    }
  )

  if (!is.null(pipeline_result) && !is.null(pipeline_result$report)) {
    report <- pipeline_result$report
    agent_outputs <- pipeline_result$agent_outputs
    total_tokens <- pipeline_result$total_tokens
  } else {
    report <- NULL
  }

  if (is.null(report)) {
    message("[Report] Using fallback approach for ", symbol)
    if (is.function(on_progress)) on_progress(0.50, "Using fallback single-call approach...")
    all_news <- if (!is.null(news_data_df)) {
      nd <- news_data_df[news_data_df$Symbol == symbol, ]
      if (nrow(nd) == 0) NULL else head(nd, 15)
    } else NULL
    news_text <- build_news_snapshot(symbol, all_news, max_rows = 8)

    prompts <- build_report_prompt(symbol, company_name, sector, trend, gbm, lattice, sim,
                                   news_text, tables)
    api_key <- Sys.getenv("OPENAI_API_KEY")
    report <- call_openai_text(prompts$system, prompts$user, api_key, max_tokens = 4000)
  }

  if (is.null(report)) {
    return(list(ok = FALSE, error = "Failed to generate report. Check API key and connectivity."))
  }

  list(
    ok = TRUE, symbol = symbol, company_name = company_name, sector = sector,
    report = report, tables = tables, agent_outputs = agent_outputs,
    total_tokens = total_tokens,
    trend = trend, gbm = gbm, lattice = lattice, sim = sim
  )
}

# ----------------------------
# Rating badge styling
# ----------------------------

rating_color <- function(rating) {
  switch(toupper(rating),
    "STRONG BUY" = list(bg = "rgba(34,197,94,0.25)", border = "#22c55e", text = "#bbf7d0"),
    "BUY" = list(bg = "rgba(74,222,128,0.2)", border = "#4ade80", text = "#dcfce7"),
    "HOLD" = list(bg = "rgba(250,204,21,0.2)", border = "#facc15", text = "#fef9c3"),
    "SELL" = list(bg = "rgba(248,113,113,0.2)", border = "#f87171", text = "#fecaca"),
    "FULLY VALUED" = list(bg = "rgba(239,68,68,0.25)", border = "#ef4444", text = "#fca5a5"),
    list(bg = "rgba(148,163,184,0.2)", border = "#94a3b8", text = "#cbd5e1")
  )
}

risk_color <- function(level) {
  switch(toupper(level %||% "MEDIUM"),
    "LOW" = "#4ade80", "MEDIUM" = "#facc15",
    "HIGH" = "#f87171", "VERY HIGH" = "#ef4444", "#94a3b8")
}

# ----------------------------
# Render HTML tables matching DBS research report style
# ----------------------------

fv <- function(x, digits=1, prefix="", suffix="") {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("-")
  paste0(prefix, formatC(round(as.numeric(x), digits), format="f", digits=digits, big.mark=","), suffix)
}

fi <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("-")
  formatC(round(as.numeric(x)), format="d", big.mark=",")
}

render_kv_table <- function(rows_list, title) {
  if (length(rows_list) == 0) return(tags$p(style="color:#94a3b8;", paste(title, "- unavailable")))
  td_l <- "padding:10px 18px; font-weight:600; color:#e2e8f0; font-size:13px; border-bottom:1px solid rgba(51,65,85,0.5);"
  td_r <- "padding:10px 18px; color:#f1f5f9; text-align:right; font-size:13px; border-bottom:1px solid rgba(51,65,85,0.5); font-variant-numeric:tabular-nums;"
  trs <- lapply(seq_along(rows_list), function(i) {
    r <- rows_list[[i]]
    bg <- if (i %% 2 == 0) "background:rgba(30,41,59,0.25);" else ""
    tags$tr(style=bg, tags$td(style=td_l, r[[1]]), tags$td(style=td_r, r[[2]]))
  })
  tags$div(class="panel-card", style="margin-bottom:16px;",
    tags$h4(style="color:#f1f5f9; margin-bottom:12px; font-size:15px; font-weight:700; display:flex; align-items:center; gap:8px;",
      tags$span(style="width:3px;height:18px;background:linear-gradient(180deg,#6366f1,#818cf8);border-radius:2px;display:inline-block;"),
      title),
    tags$table(style="width:100%; border-collapse:collapse; background:rgba(17,24,39,0.5); border-radius:10px; overflow:hidden;",
      tags$tbody(trs)))
}

render_multi_year_table <- function(metric_rows, fy_labels, title) {
  if (length(metric_rows) == 0 || length(fy_labels) == 0)
    return(tags$p(style="color:#94a3b8;", paste(title, "- unavailable")))
  hd_css <- "padding:10px 14px; background:rgba(30,41,59,0.8); color:#a5b4fc; font-size:12px; text-align:right; font-weight:600; letter-spacing:.3px; text-transform:uppercase; border-bottom:2px solid rgba(99,102,241,0.2);"
  hd_l   <- "padding:10px 14px; background:rgba(30,41,59,0.8); color:#a5b4fc; font-size:12px; text-align:left; font-weight:600; letter-spacing:.3px; text-transform:uppercase; border-bottom:2px solid rgba(99,102,241,0.2);"
  td_l   <- "padding:8px 14px; font-weight:600; color:#e2e8f0; border-bottom:1px solid rgba(51,65,85,0.4); white-space:nowrap; font-size:13px;"
  td_r   <- "padding:8px 14px; color:#f1f5f9; text-align:right; border-bottom:1px solid rgba(51,65,85,0.4); font-variant-numeric:tabular-nums; font-size:13px;"
  td_sub <- "padding:5px 14px 5px 30px; color:#818cf8; font-style:italic; font-size:12px; border-bottom:1px solid rgba(51,65,85,0.3);"
  td_sub_r <- "padding:5px 14px; color:#818cf8; font-style:italic; font-size:12px; text-align:right; border-bottom:1px solid rgba(51,65,85,0.3); font-variant-numeric:tabular-nums;"

  header <- tags$tr(tags$th(style=hd_l, title),
                    lapply(fy_labels, function(fy) tags$th(style=hd_css, fy)))
  body_rows <- list()
  row_idx <- 0L
  for (mr in metric_rows) {
    is_sub <- isTRUE(mr$sub)
    if (!is_sub) row_idx <- row_idx + 1L
    bg <- if (!is_sub && row_idx %% 2 == 0) "background:rgba(30,41,59,0.2);" else ""
    body_rows[[length(body_rows)+1]] <- tags$tr(style=bg,
      tags$td(style=if(is_sub) td_sub else td_l, mr$label),
      lapply(mr$values, function(v) tags$td(style=if(is_sub) td_sub_r else td_r, v)))
  }
  tags$div(class="panel-card", style="margin-bottom:16px;",
    tags$table(style="width:100%; border-collapse:collapse; background:rgba(17,24,39,0.5); border-radius:10px; overflow:hidden;",
      tags$thead(header), tags$tbody(body_rows)))
}

build_key_financial_html <- function(kf, rating) {
  if (is.null(kf) || nrow(kf) == 0) return(tags$p(style="color:#94a3b8;","Key Financial Data unavailable."))
  k <- kf[1,]
  render_kv_table(list(
    list("Ticker",                  k$ticker),
    list("Sector",                  k$sector),
    list("Share Price (USD)",       fv(k$share_price, 2)),
    list("Rating",                  rating),
    list("12-mth Target Price",     fv(k$target_price, 2, "$")),
    list("Market Cap (USDbn)",      fv(k$market_cap_bn, 1)),
    list("Volume (mn shares)",      fv(k$volume_mn_shares, 1)),
    list("Free float (%)",          fv(k$free_float_pct, 1)),
    list("Dividend yield (%)",      fv(k$dividend_yield_pct, 2)),
    list("Net Debt to Equity (%)",  fv(k$net_debt_equity_pct, 1)),
    list("Fwd. P/E (x)",           fv(k$fwd_pe, 1)),
    list("P/Book (x)",             fv(k$pb, 1)),
    list("ROE (%)",                fv(k$roe_pct, 1))
  ), "Key Financial Data")
}

build_pnl_html <- function(cf) {
  if (is.null(cf) || nrow(cf) == 0) return(tags$p(style="color:#94a3b8;","P&L data unavailable."))
  cf <- cf[order(cf$fiscal_year_end), ]
  fy <- paste0(cf$fiscal_year, "(A)")
  mr <- function(label, vals, sub=FALSE) list(label=label, values=vals, sub=sub)
  rows <- list(
    mr("Sales",              lapply(cf$revenue_mn, fi)),
    mr("% y/y",             lapply(cf$revenue_yoy, function(v) fv(v,1)), sub=TRUE),
    mr("Gross Profit",       lapply(cf$gross_profit_mn, fi)),
    mr("% y/y",             lapply(cf$gp_yoy, function(v) fv(v,1)), sub=TRUE),
    mr("EBITDA",             lapply(cf$ebitda_mn, fi)),
    mr("% y/y",             lapply(cf$ebitda_yoy, function(v) fv(v,1)), sub=TRUE),
    mr("Net Profit",         lapply(cf$net_income_mn, fi)),
    mr("% y/y",             lapply(cf$ni_yoy, function(v) fv(v,1)), sub=TRUE),
    mr("FCF",                lapply(cf$fcf_mn, fi)),
    mr("% y/y",             lapply(cf$fcf_yoy, function(v) fv(v,1)), sub=TRUE),
    mr("CAPEX",              lapply(cf$capex_mn, fi)),
    mr("% y/y",             lapply(cf$capex_yoy, function(v) fv(v,1)), sub=TRUE),
    mr("EBITDA Margin (%)",  lapply(cf$ebitda_margin_pct, function(v) fv(v,1))),
    mr("Net Margin (%)",     lapply(cf$net_margin_pct, function(v) fv(v,1))),
    mr("ROA (%)",            lapply(cf$roa_pct, function(v) fv(v,1))),
    mr("ROE (%)",            lapply(cf$roe_pct, function(v) fv(v,1))),
    mr("Tax Rate (%)",       lapply(cf$tax_rate_pct, function(v) fv(v,1)))
  )
  render_multi_year_table(rows, fy, "FY (USDmn)")
}

build_credit_html <- function(cf) {
  if (is.null(cf) || nrow(cf) == 0) return(tags$p(style="color:#94a3b8;","Credit & Cashflow data unavailable."))
  cf <- cf[order(cf$fiscal_year_end), ]
  fy <- paste0(cf$fiscal_year, "(A)")
  mr <- function(label, vals) list(label=label, values=vals, sub=FALSE)
  rows <- list(
    mr("Debt / Equity",          lapply(cf$debt_equity_pct, function(v) fv(v,1))),
    mr("Net Debt / Equity",      lapply(cf$net_debt_equity_pct, function(v) fv(v,1))),
    mr("Debt / Assets",          lapply(cf$debt_assets_pct, function(v) fv(v,1))),
    mr("Net Debt / Assets",      lapply(cf$net_debt_assets_pct, function(v) fv(v,1))),
    mr("EBITDA / Int Exp",       lapply(cf$ebitda_int_exp, function(v) fv(v,1))),
    mr("ST Debt / Total Debt",   lapply(cf$st_debt_total_debt, function(v) fv(v,2))),
    mr("Debt / EBITDA",          lapply(cf$debt_ebitda, function(v) fv(v,1))),
    mr("[Cash + CFO] / ST Debt", lapply(cf$cash_cfo_st_debt, function(v) fv(v,1))),
    mr("Receivables Days",       lapply(cf$receivables_days, function(v) fv(v,1))),
    mr("Days Payable",           lapply(cf$days_payable, function(v) fv(v,1))),
    mr("Inventory Days",         lapply(cf$inventory_days, function(v) fv(v,1)))
  )
  render_multi_year_table(rows, fy, "Credit & Cashflow Metrics")
}

build_valuation_html <- function(vm) {
  if (is.null(vm) || nrow(vm) == 0) return(tags$p(style="color:#94a3b8;","Valuation data unavailable."))
  vm <- vm[order(vm$fiscal_year), ]
  fy <- paste0(vm$fiscal_year, "(A)")
  mr <- function(label, vals) list(label=label, values=vals, sub=FALSE)
  rows <- list(
    mr("P/E (x)",            lapply(vm$pe_x, function(v) fv(v,1))),
    mr("P/B (x)",            lapply(vm$pb_x, function(v) fv(v,1))),
    mr("Dividend Yield (%)", lapply(vm$dividend_yield_pct, function(v) fv(v,2))),
    mr("EV/EBITDA (x)",      lapply(vm$ev_ebitda_x, function(v) fv(v,1))),
    mr("FCF Yield (%)",      lapply(vm$fcf_yield_pct, function(v) fv(v,1)))
  )
  render_multi_year_table(rows, fy, "Valuation Metrics")
}

# ----------------------------
# Render the full report UI
# ----------------------------

render_report_ui <- function(result) {
  if (!isTRUE(result$ok)) {
    return(tags$div(class = "alert alert-danger", result$error %||% "Report generation failed."))
  }

  r <- result$report
  sym <- result$symbol
  company <- result$company_name
  rating <- toupper(safe_text(r$purchase_rating, "HOLD"))
  conf <- safe_na(r$confidence); if (is.na(conf)) conf <- 50
  rc <- rating_color(rating)
  rlevel <- safe_text(r$risk_level, "Medium")
  target <- safe_na(r$target_price_3m)

  tbl <- result$tables

  conf_bar_color <- if (conf >= 70) "#22c55e" else if (conf >= 40) "#facc15" else "#f87171"

  tagList(
    # ---- Purchase Rating Hero Card ----
    tags$div(
      class = "panel-card",
      style = "margin-bottom:20px; padding:24px 28px; border:1px solid rgba(99,102,241,0.25); border-radius:16px; background:linear-gradient(135deg,rgba(15,23,42,0.8),rgba(30,41,59,0.4));",
      tags$div(style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:16px;",
        tags$div(style = "display:flex; align-items:center; gap:20px; flex-wrap:wrap;",
          tags$div(
            style = sprintf(
              "display:inline-block; padding:14px 36px; border-radius:14px; background:%s; border:2px solid %s; font-size:30px; font-weight:800; color:%s; letter-spacing:1.5px; text-shadow:0 1px 8px rgba(0,0,0,0.3);",
              rc$bg, rc$border, rc$text),
            rating
          ),
          tags$div(style = "display:flex; flex-direction:column; gap:6px;",
            tags$div(style = "display:flex; align-items:center; gap:10px;",
              tags$span(style = "color:#94a3b8; font-size:12px; font-weight:600; text-transform:uppercase; letter-spacing:.5px;", "Confidence"),
              tags$div(style = "width:120px; height:6px; background:rgba(51,65,85,0.5); border-radius:3px; overflow:hidden;",
                tags$div(style = sprintf("width:%d%%; height:100%%; background:%s; border-radius:3px; transition:width .6s;", conf, conf_bar_color))
              ),
              tags$span(style = "font-size:16px; color:#f1f5f9; font-weight:700;", sprintf("%d%%", conf))
            ),
            tags$div(style = "display:flex; align-items:center; gap:16px;",
              tags$span(style = sprintf("font-size:13px; color:%s; font-weight:600;", risk_color(rlevel)),
                tags$span(style = sprintf("display:inline-block; width:8px; height:8px; border-radius:50%%; margin-right:4px; background:%s;", risk_color(rlevel))),
                sprintf("Risk: %s", rlevel)),
              if (!is.null(target) && !is.na(target)) {
                tags$span(style = "font-size:13px; color:#38bdf8; font-weight:600;",
                  icon("bullseye", style = "margin-right:4px; font-size:11px;"),
                  sprintf("3M Target: $%.0f", target))
              }
            )
          )
        ),
        tags$div(
          actionButton("report_info_btn", NULL, icon = icon("circle-info"),
                        class = "btn-sm btn-default",
                        style = "border-radius:50%; width:38px; height:38px; padding:0; font-size:16px; background:rgba(99,102,241,0.2); border:1px solid rgba(99,102,241,0.4); color:#a5b4fc; transition:background .2s;",
                        title = "Rating definitions")
        )
      )
    ),

    # ---- Key Reason ----
    tags$div(
      class = "panel-card", style = "margin-bottom:18px;",
      tags$h4(style = "color:#f1f5f9; margin-bottom:10px; font-size:15px; font-weight:700; display:flex; align-items:center; gap:8px;",
        icon("lightbulb", style = "color:#fbbf24; font-size:16px;"), "Key Reason"),
      tags$p(style = "color:#e2e8f0; font-size:14px; line-height:1.8; margin:0;", safe_text(r$key_reason))
    ),

    # ---- Four Financial Tables (stacked, DBS report style) ----
    build_key_financial_html(tbl$key_financial, rating),
    build_pnl_html(tbl$company_financials),
    build_credit_html(tbl$company_financials),
    build_valuation_html(tbl$valuation),

    # ---- Industry Analysis ----
    tags$div(
      class = "panel-card", style = "margin-bottom:18px;",
      tags$h4(style = "color:#f1f5f9; margin-bottom:10px; font-size:15px; font-weight:700; display:flex; align-items:center; gap:8px;",
        icon("industry", style = "color:#60a5fa; font-size:15px;"), "Industry Analysis"),
      tags$p(style = "color:#d1d5db; font-size:14px; line-height:1.85; margin:0;", safe_text(r$industry_analysis))
    ),

    # ---- Investment Overview ----
    tags$div(
      class = "panel-card", style = "margin-bottom:18px;",
      tags$h4(style = "color:#f1f5f9; margin-bottom:10px; font-size:15px; font-weight:700; display:flex; align-items:center; gap:8px;",
        icon("chart-pie", style = "color:#a78bfa; font-size:15px;"), "Investment Overview"),
      tags$p(style = "color:#d1d5db; font-size:14px; line-height:1.85; margin:0;", safe_text(r$investment_overview))
    ),

    # ---- Macro Environment ----
    tags$div(
      class = "panel-card", style = "margin-bottom:18px;",
      tags$h4(style = "color:#f1f5f9; margin-bottom:10px; font-size:15px; font-weight:700; display:flex; align-items:center; gap:8px;",
        icon("globe", style = "color:#38bdf8; font-size:15px;"), "Macro Environment"),
      tags$p(style = "color:#d1d5db; font-size:14px; line-height:1.85; margin:0;",
             safe_text(r$macro_environment, "No macro environment analysis available."))
    ),

    # ---- Bull vs Bear ----
    tags$div(style = "margin-bottom:18px;",
      fluidRow(
        column(6,
          tags$div(
            class = "panel-card",
            style = "border-left:4px solid #22c55e; min-height:200px; background:linear-gradient(135deg,rgba(34,197,94,0.04),rgba(15,23,42,0.6));",
            tags$h5(style = "color:#4ade80; font-weight:700; font-size:15px; margin-bottom:10px; display:flex; align-items:center; gap:6px;",
              icon("arrow-trend-up"), "Bull Case"),
            tags$p(style = "color:#d1d5db; font-size:13px; line-height:1.8; margin:0;", safe_text(r$bull_case))
          )
        ),
        column(6,
          tags$div(
            class = "panel-card",
            style = "border-left:4px solid #ef4444; min-height:200px; background:linear-gradient(135deg,rgba(239,68,68,0.04),rgba(15,23,42,0.6));",
            tags$h5(style = "color:#f87171; font-weight:700; font-size:15px; margin-bottom:10px; display:flex; align-items:center; gap:6px;",
              icon("arrow-trend-down"), "Bear Case"),
            tags$p(style = "color:#d1d5db; font-size:13px; line-height:1.8; margin:0;", safe_text(r$bear_case))
          )
        )
      )
    ),

    # ---- Risk Analysis ----
    tags$div(
      class = "panel-card", style = "margin-bottom:18px;",
      tags$h4(style = "color:#f1f5f9; margin-bottom:12px; font-size:15px; font-weight:700; display:flex; align-items:center; gap:8px;",
        icon("shield-halved", style = "color:#f87171; font-size:15px;"), "Risk Analysis"),
      {
        risks <- r$risk_analysis
        if (is.null(risks) || length(risks) == 0) {
          risks <- list("No risk factors identified")
        } else if (is.data.frame(risks)) {
          risks <- lapply(seq_len(nrow(risks)), function(i) {
            vals <- as.character(unlist(risks[i, ]))
            vals <- vals[!is.na(vals) & nzchar(trimws(vals)) & vals != "NA"]
            paste(vals, collapse = " ")
          })
          risks <- risks[nzchar(trimws(unlist(risks)))]
          if (length(risks) == 0) risks <- list("No risk factors identified")
        } else if (is.list(risks) && !is.character(risks)) {
          risks <- lapply(risks, function(x) {
            if (is.character(x)) paste(x[!is.na(x)], collapse = " ")
            else if (is.list(x)) paste(unlist(x)[!is.na(unlist(x))], collapse = " ")
            else as.character(x)
          })
        }
        tags$div(
          lapply(seq_along(risks), function(i) {
            tags$div(style = "display:flex; gap:10px; padding:8px 0; border-bottom:1px solid rgba(51,65,85,0.3);",
              tags$span(style = "flex-shrink:0; width:24px; height:24px; border-radius:6px; background:rgba(239,68,68,0.15); color:#f87171; display:flex; align-items:center; justify-content:center; font-size:12px; font-weight:700;", as.character(i)),
              tags$span(style = "color:#d1d5db; font-size:13px; line-height:1.7;", safe_text(risks[[i]]))
            )
          })
        )
      }
    ),

    # ---- Agent Transparency Panel ----
    build_agent_panel(result$agent_outputs),

    # ---- End of Report ----
    tags$div(
      style = "margin:24px 0 16px; text-align:center; padding:12px; border-top:1px solid rgba(99,102,241,0.1);",
      tags$p(style = "color:#475569; font-size:12px; font-style:italic; margin:0; letter-spacing:.3px;",
             icon("circle-check", style = "margin-right:4px; color:#475569;"),
             sprintf("End of Report  |  %s (%s)  |  Generated %s  |  AI Multi-Analyst Team",
                     sym, company, format(Sys.time(), "%Y-%m-%d %H:%M ET", tz = "America/New_York")))
    )
  )
}

# ----------------------------
# Agent Transparency Panel (collapsible per-agent outputs)
# ----------------------------

build_agent_panel <- function(agent_outputs) {
  if (is.null(agent_outputs)) return(NULL)
  agent_meta <- list(
    fundamentals = list(icon = "calculator", color = "#60a5fa", title = "Fundamentals Analyst"),
    news         = list(icon = "newspaper",  color = "#fbbf24", title = "News & Macro Analyst"),
    technical    = list(icon = "chart-line",  color = "#a78bfa", title = "Technical Analyst"),
    bull         = list(icon = "arrow-trend-up",   color = "#4ade80", title = "Bull Researcher"),
    bear         = list(icon = "arrow-trend-down", color = "#f87171", title = "Bear Researcher"),
    risk         = list(icon = "shield-halved",    color = "#fb923c", title = "Risk Manager")
  )
  panels <- lapply(names(agent_outputs), function(atype) {
    out <- agent_outputs[[atype]]
    if (is.null(out)) return(NULL)
    meta <- agent_meta[[atype]]
    if (is.null(meta)) return(NULL)
    content_items <- lapply(names(out), function(key) {
      val <- out[[key]]
      val_text <- if (is.character(val) && length(val) == 1) {
        val
      } else if (is.numeric(val) && length(val) == 1) {
        as.character(val)
      } else if (is.character(val) && length(val) > 1) {
        paste0("\u2022 ", val, collapse = "\n")
      } else if (is.list(val)) {
        paste0("\u2022 ", unlist(val), collapse = "\n")
      } else {
        paste(as.character(val), collapse = " ")
      }
      tags$div(style = "margin-bottom:10px;",
        tags$span(style = "color:#a5b4fc; font-weight:600; font-size:11px; text-transform:uppercase; letter-spacing:.5px;",
                  gsub("_", " ", key)),
        tags$p(style = "color:#d1d5db; font-size:13px; line-height:1.7; margin:4px 0 0; white-space:pre-line;", val_text))
    })
    tags$details(style = "margin-bottom:6px; border:1px solid rgba(99,102,241,0.12); border-radius:10px; overflow:hidden;",
      tags$summary(style = "padding:10px 16px; cursor:pointer; display:flex; align-items:center; gap:8px; background:rgba(30,41,59,0.3); color:#f1f5f9; font-weight:600; font-size:13px;",
        icon(meta$icon, style = sprintf("color:%s;", meta$color)), meta$title),
      tags$div(style = "padding:14px 16px; background:rgba(15,23,42,0.3);", content_items))
  })
  panels <- Filter(Negate(is.null), panels)
  if (length(panels) == 0) return(NULL)
  tags$div(class = "panel-card", style = "margin-bottom:18px;",
    tags$h4(style = "color:#f1f5f9; margin-bottom:12px; font-size:15px; font-weight:700; display:flex; align-items:center; gap:8px;",
      icon("users", style = "color:#818cf8; font-size:15px;"), "Analyst Panel",
      tags$span(style = "font-size:12px; color:#64748b; font-weight:400; margin-left:4px;", "(click to expand)")),
    tags$div(panels))
}

# ----------------------------
# Downloadable HTML report
# ----------------------------

build_report_download_html <- function(result) {
  r <- result$report
  sym <- result$symbol
  company <- result$company_name
  sector <- result$sector %||% ""
  rating <- toupper(safe_text(r$purchase_rating, "HOLD"))
  conf <- safe_na(r$confidence); if (is.na(conf)) conf <- 50
  target <- safe_na(r$target_price_3m)
  rlevel <- safe_text(r$risk_level, "Medium")
  trend_dir <- safe_text(r$forecast_trend, "NEUTRAL")

  esc <- function(x) htmltools::htmlEscape(x)

  risks <- r$risk_analysis
  if (is.null(risks) || length(risks) == 0) {
    risks <- list("No risk factors identified")
  } else if (is.data.frame(risks)) {
    risks <- lapply(seq_len(nrow(risks)), function(i) {
      vals <- as.character(unlist(risks[i, ]))
      vals <- vals[!is.na(vals) & nzchar(trimws(vals)) & vals != "NA"]
      paste(vals, collapse = " ")
    })
  } else if (is.list(risks) && !is.character(risks)) {
    risks <- lapply(risks, function(x) paste(unlist(x)[!is.na(unlist(x))], collapse = " "))
  }
  risks_html <- paste(sapply(seq_along(risks), function(i) {
    paste0('<div style="padding:4px 0;"><strong>', i, '.</strong> ', esc(safe_text(risks[[i]])), '</div>')
  }), collapse = "\n")

  target_txt <- if (!is.na(target)) sprintf("$%.0f", target) else "N/A"
  generated_at <- format(Sys.time(), "%Y-%m-%d %H:%M ET", tz = "America/New_York")

  paste0(
    '<!DOCTYPE html>\n<html lang="en">\n<head>\n<meta charset="utf-8">\n',
    '<title>Equity Research Report - ', sym, ' (', esc(company), ')</title>\n',
    '<style>\n',
    'body{font-family:"Segoe UI",Arial,sans-serif;max-width:820px;margin:0 auto;padding:28px;color:#1e293b;line-height:1.75;}\n',
    'h1,h2{color:#0f172a;} h1{border-bottom:3px solid #6366f1;padding-bottom:8px;}\n',
    'h2{border-bottom:1px solid #cbd5e1;padding-bottom:4px;margin-top:28px;}\n',
    '.badge{display:inline-block;padding:8px 24px;border-radius:8px;font-size:22px;font-weight:800;background:#e0e7ff;color:#3730a3;}\n',
    '.meta{color:#64748b;font-size:14px;margin:8px 0 16px;}\n',
    '.bull{border-left:4px solid #16a34a;padding:12px 16px;margin:8px 0;background:#f0fdf4;}\n',
    '.bear{border-left:4px solid #dc2626;padding:12px 16px;margin:8px 0;background:#fef2f2;}\n',
    'table{border-collapse:collapse;width:100%;margin:12px 0;}\n',
    'th,td{padding:8px 12px;border:1px solid #e2e8f0;text-align:left;font-size:14px;}\n',
    'th{background:#f8fafc;font-weight:600;}\n',
    '.footer{margin-top:32px;padding-top:12px;border-top:1px solid #e2e8f0;color:#94a3b8;font-size:12px;text-align:center;}\n',
    '</style>\n</head>\n<body>\n',
    '<h1>', esc(company), ' (', sym, ') &mdash; Equity Research Report</h1>\n',
    '<div class="meta">Sector: ', esc(sector), ' | Generated: ', generated_at, '</div>\n',
    '<div class="badge">', rating, '</div>\n',
    '<span style="margin-left:16px;font-size:15px;">Confidence: ', conf, '% | Risk: ', esc(rlevel),
    ' | 3M Target: ', target_txt, ' | Trend: ', trend_dir, '</span>\n',
    '<h2>Key Reason</h2>\n<p>', esc(safe_text(r$key_reason)), '</p>\n',
    '<h2>Industry Analysis</h2>\n<p>', esc(safe_text(r$industry_analysis)), '</p>\n',
    '<h2>Investment Overview</h2>\n<p>', esc(safe_text(r$investment_overview)), '</p>\n',
    '<h2>Macro Environment</h2>\n<p>', esc(safe_text(r$macro_environment, "No macro analysis available.")), '</p>\n',
    '<div style="display:flex;gap:16px;margin:16px 0;">\n',
    '<div class="bull" style="flex:1;"><h3 style="color:#16a34a;margin-top:0;">Bull Case</h3><p>',
    esc(safe_text(r$bull_case)), '</p></div>\n',
    '<div class="bear" style="flex:1;"><h3 style="color:#dc2626;margin-top:0;">Bear Case</h3><p>',
    esc(safe_text(r$bear_case)), '</p></div>\n</div>\n',
    '<h2>Risk Analysis</h2>\n', risks_html, '\n',
    '<div class="footer">End of Report | ', sym, ' (', esc(company), ') | ', generated_at,
    ' | AI Multi-Analyst Team</div>\n',
    '</body>\n</html>'
  )
}

render_rating_info_modal <- function() {
  rating_row <- function(rating, color, desc) {
    tags$tr(
      tags$td(style = "padding:10px 14px;",
        tags$span(style = sprintf("display:inline-block; padding:4px 14px; border-radius:8px; background:%s; color:#fff; font-weight:700; font-size:13px;", color), rating)),
      tags$td(style = "padding:10px 14px; color:#334155; font-size:13px; line-height:1.6;", desc)
    )
  }
  modalDialog(
    title = tags$span(style = "font-weight:700; font-size:18px;", icon("circle-info", style = "margin-right:6px; color:#6366f1;"), "Rating Definitions"),
    size = "m", easyClose = TRUE,
    tags$div(style = "color:#1e293b;",
      tags$table(class = "table", style = "font-size:14px; border-collapse:separate; border-spacing:0 4px;",
        tags$thead(tags$tr(
          tags$th(style = "color:#64748b; font-size:12px; text-transform:uppercase; letter-spacing:.5px; border:none;", "Rating"),
          tags$th(style = "color:#64748b; font-size:12px; text-transform:uppercase; letter-spacing:.5px; border:none;", "Description"))),
        tags$tbody(
          rating_row("Strong Buy", "#16a34a", "Exceptional upside potential with strong catalysts. High conviction recommendation."),
          rating_row("Buy", "#22c55e", "Above-average return expected. Positive fundamentals and technical signals."),
          rating_row("Hold", "#ca8a04", "Fair-valued. No compelling reason to buy or sell at current levels."),
          rating_row("Sell", "#f87171", "Downside risk exceeds upside. Deteriorating fundamentals or overvaluation."),
          rating_row("Fully Valued", "#dc2626", "Stock price fully reflects intrinsic value. Limited upside, consider taking profits.")
        )
      ),
      tags$hr(style = "border-color:#e2e8f0;"),
      tags$div(style = "display:flex; gap:20px; flex-wrap:wrap;",
        tags$div(style = "flex:1; min-width:180px; padding:12px; background:#f8fafc; border-radius:10px;",
          tags$p(style = "margin:0; font-size:13px;", tags$strong("Confidence"), " (0-100%): Analyst team's conviction level in the rating.")),
        tags$div(style = "flex:1; min-width:180px; padding:12px; background:#f8fafc; border-radius:10px;",
          tags$p(style = "margin:0; font-size:13px;", tags$strong("Risk Level"), ": Low / Medium / High / Very High - based on volatility, liquidity, and macro exposure."))
      )
    )
  )
}
