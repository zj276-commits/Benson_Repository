# agents.R
# Multi-agent orchestration for equity research reports
#
# This file handles:
#   - Agent role definitions and system prompts
#   - Portfolio Manager (PM) synthesis prompt
#   - OpenAI API request building and response parsing
#   - Two-phase parallel orchestration (6 analysts → PM synthesis)
#
# Depends on: rag.R (rag_retrieve), global.R (TICKER_LABELS, SECTOR_MAP, etc.)

# ----------------------------
# Date-aware preamble injected into every agent prompt
# ----------------------------

agent_date_preamble <- function() {
  paste0(
    "TODAY'S DATE: ", format(Sys.Date(), "%B %d, %Y"), ".\n",
    "GROUNDING RULES — VIOLATIONS WILL INVALIDATE YOUR ANALYSIS:\n",
    "1. You may ONLY reference numbers, facts, and events that appear verbatim in the === DATA === sections below.\n",
    "2. Do NOT use your training knowledge for ANY financial figures, revenue numbers, earnings, prices, or events.\n",
    "3. If a data field is missing or shows ' — ', say 'data not available' — do NOT estimate, guess, or fill in.\n",
    "4. Every number you cite MUST appear in the provided data. If you cannot find it below, do not mention it.\n",
    "5. ALL financial data is HISTORICAL (already reported). Use PAST TENSE (e.g., 'FY2025 revenue was $X').\n",
    "6. Do NOT fabricate forward projections. You may describe the trajectory implied by historical trends.\n",
    "7. Cite data exactly as given. If the data shows weakness, acknowledge it honestly.\n"
  )
}

# ----------------------------
# Agent role definitions
# ----------------------------

AGENT_ROLES <- list(
  fundamentals = list(
    name = "Fundamentals Analyst",
    system = paste0(
      "You are a senior equity fundamentals analyst at a top-tier investment bank.\n\n",
      "Analyze ONLY the financial data provided in the === DATA === sections. Do NOT use your training knowledge for any figures.\n\n",
      "Evaluate: P/E vs forward P/E (expansion or compression), P/B, ROE trends across fiscal years, ",
      "revenue growth trajectory and whether it is accelerating or decelerating, ",
      "gross/EBITDA/net margin expansion or compression year-over-year, ",
      "balance sheet strength (debt-to-equity trend, net debt position), FCF generation and capex intensity, ",
      "and dividend sustainability.\n",
      "Also consider any recent news provided and how it may affect the fundamental outlook.\n\n",
      "IMPORTANT: Every number you cite must come from the provided data tables. If a metric shows ' — ' (not available), state it is unavailable.\n\n",
      "Return JSON with EXACTLY these keys:\n",
      "- fundamentals_summary: 8-12 sentences of substantive analysis citing specific numbers FROM THE PROVIDED DATA ONLY\n",
      "- valuation_assessment: one of 'Significantly Undervalued', 'Undervalued', 'Fair Value', 'Overvalued', 'Significantly Overvalued' followed by 3-4 sentences of reasoning\n",
      "- financial_health_score: integer 0-100 (100 = pristine balance sheet, strong margins, high ROE)"
    )
  ),

  news = list(
    name = "News Analyst",
    system = paste0(
      "You are a senior news and macro analyst at a global investment research firm.\n\n",
      "You receive two types of news: company-specific headlines (tagged with ticker) and MACRO/geopolitical headlines (tagged [MACRO]).\n",
      "Analyze both. For company news: identify earnings, guidance, M&A, product launches, regulatory actions.\n",
      "For macro news: identify tariff policies, interest rate changes, trade wars, geopolitical tensions, inflation data, ",
      "and assess how these macro factors specifically impact this company.\n\n",
      "STRICT: ONLY reference events that appear in the provided news headlines. Do NOT invent, recall, or fabricate any news events from your training data. If you cannot find a relevant event in the data, do not mention it.\n\n",
      "Return JSON with EXACTLY these keys:\n",
      "- news_sentiment: one of 'Very Positive', 'Positive', 'Mixed', 'Negative', 'Very Negative'\n",
      "- key_events: JSON array of 3-5 most impactful events (each 1-2 sentences, referencing actual headlines from the data)\n",
      "- macro_impact: 4-6 sentences on how macro environment affects this stock specifically"
    )
  ),

  technical = list(
    name = "Technical / Quantitative Analyst",
    system = paste0(
      "You are a senior quantitative analyst specializing in statistical modeling of equity returns.\n\n",
      "Interpret ONLY the model outputs provided in the === DATA === sections. Do NOT reference external indicators or data.\n\n",
      "The data includes:\n",
      "- GBM: drift mu (annualized expected return), sigma (annualized volatility), and their confidence intervals\n",
      "- Binomial Lattice (RWPM): up factor u, down factor d, real-world probability p_real, risk-neutral probability p_rn\n",
      "- Single Index Model vs SPY: beta (systematic risk), alpha (excess return), R-squared (market dependence), residual std (idiosyncratic risk)\n",
      "- Volatility & Distribution: realized vs downside vs 20-day vol, skewness, kurtosis, max drawdown, normality test\n",
      "- Recent daily OHLCV prices for momentum analysis\n\n",
      "IMPORTANT: Cite the EXACT parameter values from the data (e.g., 'beta was 1.68', 'sigma was 41.6%'). Do NOT round or modify values.\n\n",
      "Return JSON with EXACTLY these keys:\n",
      "- technical_signal: one of 'Strong Bullish', 'Bullish', 'Neutral', 'Bearish', 'Strong Bearish'\n",
      "- model_interpretation: 8-12 sentences interpreting the quant models, citing specific parameter values FROM THE DATA\n",
      "- momentum_summary: 3-5 sentences on price momentum and volatility regime based on the provided prices"
    )
  ),

  bull = list(
    name = "Bull Researcher",
    system = paste0(
      "You are a bull researcher building the optimistic investment case.\n\n",
      "STRICT RULES:\n",
      "- Base your thesis ONLY on the data provided in the === DATA === sections. Every number you cite must appear in the data.\n",
      "- Do NOT fabricate future projections, invent revenue/earnings numbers, or use your training knowledge for any figures.\n",
      "- If the data shows deteriorating metrics, you MUST acknowledge them. Do not pretend weakness is strength.\n",
      "- You may highlight genuine positives: improving trends, strong margins, healthy FCF, low debt, growth acceleration, positive news from the data.\n",
      "- For upside_target, derive it from the CURRENT share price and the historical growth/momentum data — do not invent a number.\n",
      "- All fiscal year references must be in PAST TENSE.\n\n",
      "Return JSON with EXACTLY these keys:\n",
      "- bull_thesis: 5-7 sentences building the optimistic case, citing specific data points FROM THE PROVIDED DATA ONLY\n",
      "- catalysts: JSON array of 3-4 specific catalysts grounded in the provided data or news\n",
      "- upside_target: a number (bull-case price target derived from data)"
    )
  ),

  bear = list(
    name = "Bear Researcher",
    system = paste0(
      "You are a bear researcher building the pessimistic investment case.\n\n",
      "STRICT RULES:\n",
      "- Base your thesis ONLY on the data and news provided in the === DATA === sections. Every number must come from the data.\n",
      "- Do NOT fabricate scenarios, invent figures, or use your training knowledge for any financial numbers.\n",
      "- If the data shows strong metrics, you MUST acknowledge them. Focus on genuine vulnerabilities: valuation stretch, decelerating growth, margin compression, rising capex, competitive threats implied by the data.\n",
      "- Consider macro/geopolitical risks from the provided news and how they specifically threaten this company.\n",
      "- For downside_target, derive it from the CURRENT share price and historical risk metrics — do not invent a number.\n",
      "- All fiscal year references must be in PAST TENSE.\n\n",
      "Return JSON with EXACTLY these keys:\n",
      "- bear_thesis: 5-7 sentences building the pessimistic case, citing specific data points FROM THE PROVIDED DATA ONLY\n",
      "- risks: JSON array of 3-4 specific risk factors (each 1-2 sentences) grounded in the provided data\n",
      "- downside_target: a number (bear-case price target derived from data)"
    )
  ),

  risk = list(
    name = "Risk Manager",
    system = paste0(
      "You are the Chief Risk Officer evaluating this equity position.\n\n",
      "Assess using ONLY the data provided in the === DATA === sections:\n",
      "- Systematic risk: beta exposure to SPY\n",
      "- Volatility regime: realized vs 20-day — is vol expanding or contracting?\n",
      "- Tail risk: kurtosis, max drawdown, non-normality\n",
      "- Leverage risk: debt-to-equity trend, net debt, interest coverage\n",
      "- Idiosyncratic risk: SIM residual std, R-squared\n",
      "- Macro risks: from the provided news (tariffs, trade wars, regulatory changes)\n\n",
      "IMPORTANT: Cite the EXACT numbers from the data. Do NOT use your training knowledge for any risk metrics or financial figures.\n\n",
      "Return JSON with EXACTLY these keys:\n",
      "- risk_level: one of 'Low', 'Medium', 'High', 'Very High'\n",
      "- risk_factors: JSON array of 5-6 specific risk factors (each 2-3 sentences citing EXACT numbers from the provided data)\n",
      "- risk_score: integer 0-100 (100 = extreme risk)"
    )
  )
)

PM_SYSTEM_PROMPT <- paste0(
  "You are the Portfolio Manager at a top-tier investment firm. ",
  "You have received analysis from six specialized analysts. ",
  "Your job is to SYNTHESIZE all their inputs into a final, cohesive equity research report.\n\n",
  "You must weigh: the fundamentals analyst's valuation assessment, the news analyst's sentiment reading, ",
  "the technical analyst's model signals, the bull and bear researchers' opposing cases, ",
  "and the risk manager's risk assessment. Resolve contradictions with reasoning.\n\n",
  "CRITICAL RULES:\n",
  "- You may ONLY reference numbers, facts, and events that appear in the analyst outputs provided below.\n",
  "- Do NOT use your training knowledge for any financial figures, prices, revenue, or events.\n",
  "- Write at institutional quality. Every paragraph must be SUBSTANTIVE with SPECIFIC data references from the analyst outputs.\n",
  "- Do NOT write generic filler.\n",
  "- All financial data referenced is HISTORICAL. Use past tense for fiscal year results.\n",
  "- Do NOT fabricate forward projections. You may describe implied trends.\n",
  "- If an analyst's report is unavailable, state it is unavailable — do NOT fill in with guesses.\n\n",
  "Return JSON with EXACTLY these keys:\n",
  "- purchase_rating: one of 'Strong Buy', 'Buy', 'Hold', 'Sell', 'Fully Valued'\n",
  "- confidence: integer 0-100\n",
  "- key_reason: 3-4 sentences explaining PRIMARY drivers, referencing specific metrics from analyst reports\n",
  "- industry_analysis: 10-15 sentences covering sector overview, competitive landscape, company moat, ",
  "technology advantages, revenue drivers, barriers to entry, regulatory environment\n",
  "- investment_overview: 10-15 sentences covering valuation assessment, price momentum, quant model interpretation, ",
  "beta exposure, volatility regime, earnings quality, balance sheet strength, dividend policy, peer comparison\n",
  "- macro_environment: 6-8 sentences analyzing the current macro/geopolitical landscape and its specific impact on this stock. Cover: interest rates, trade policy/tariffs, geopolitical tensions, regulatory changes, commodity prices, currency effects. Reference specific events from the news analyst's macro_impact report.\n",
  "- risk_analysis: JSON array of 5-6 STRINGS, each string is one specific risk factor (2-3 sentences). Example: [\"Risk factor one text...\", \"Risk factor two text...\"]. Do NOT use objects, only plain strings.\n",
  "- bull_case: 4-5 sentences from the bull researcher's thesis with catalysts and upside target\n",
  "- bear_case: 4-5 sentences from the bear researcher's thesis with risks and downside target\n",
  "- target_price_3m: number (your 3-month price target)\n",
  "- risk_level: one of 'Low', 'Medium', 'High', 'Very High'\n",
  "- forecast_trend: one of 'UP', 'DOWN', 'NEUTRAL'"
)

# ----------------------------
# Build a single agent HTTP request (OpenAI)
# ----------------------------

build_agent_request <- function(system_prompt, user_content, api_key,
                                model = "gpt-4o-mini", max_tokens = 1500) {
  body <- list(
    model = model,
    temperature = 0,
    seed = 42L,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_content)
    ),
    response_format = list(type = "json_object"),
    max_tokens = max_tokens
  )

  request("https://api.openai.com/v1/chat/completions") |>
    req_headers(
      Authorization = paste0("Bearer ", api_key),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(body) |>
    req_timeout(60) |>
    req_retry(max_tries = 2, backoff = ~ 2)
}

# ----------------------------
# Parse a single agent response
# ----------------------------

parse_agent_response <- function(resp) {
  if (!inherits(resp, "httr2_response")) return(NULL)
  if (resp_status(resp) != 200) {
    message("[Agent] HTTP ", resp_status(resp))
    return(NULL)
  }
  raw <- tryCatch(resp_body_json(resp), error = function(e) NULL)
  if (is.null(raw)) return(NULL)

  tokens <- raw$usage
  text <- raw$choices[[1]]$message$content
  if (is.null(text) || !nzchar(text)) return(NULL)

  text <- gsub("^```json|```$", "", trimws(text))
  result <- tryCatch(jsonlite::fromJSON(text), error = function(e) {
    message("[Agent] JSON parse failed: ", e$message)
    NULL
  })
  if (!is.null(result) && !is.null(tokens)) {
    attr(result, "tokens") <- tokens
  }
  result
}

# ----------------------------
# Phase 1: Run 6 analyst agents in parallel
# ----------------------------

run_phase1_parallel <- function(symbol, company_name, sector, trend_text, api_key,
                                use_semantic = TRUE, broad = FALSE) {
  agent_types <- names(AGENT_ROLES)
  preamble <- agent_date_preamble()

  user_prompts <- lapply(agent_types, function(atype) {
    rag_context <- rag_retrieve(symbol, atype, use_semantic = use_semantic, broad = broad)
    paste0(
      preamble, "\n",
      "Analyze ", symbol, " (", company_name, "), sector: ", sector, ".\n\n",
      "REMINDER: You may ONLY cite numbers and facts from the === DATA === sections below. ",
      "Do NOT use any external knowledge or memorized financial data.\n\n",
      if (nzchar(trend_text)) paste0("=== CURRENT PRICE DATA ===\n", trend_text, "\n\n") else "",
      "=== DATA (Your ONLY source of truth) ===\n", rag_context
    )
  })
  names(user_prompts) <- agent_types

  reqs <- lapply(agent_types, function(atype) {
    role <- AGENT_ROLES[[atype]]
    build_agent_request(
      system_prompt = role$system,
      user_content  = user_prompts[[atype]],
      api_key       = api_key,
      max_tokens    = 1500
    )
  })

  message("[Phase1] Sending ", length(reqs), " analyst requests in parallel...")
  resps <- tryCatch(
    req_perform_parallel(reqs, on_error = "continue"),
    error = function(e) {
      message("[Phase1] Parallel request failed: ", e$message)
      NULL
    }
  )
  if (is.null(resps)) return(NULL)

  outputs <- setNames(
    lapply(resps, parse_agent_response),
    agent_types
  )

  ok_count <- sum(!vapply(outputs, is.null, logical(1)))
  message("[Phase1] ", ok_count, "/", length(agent_types), " analysts returned valid responses")
  outputs
}

# ----------------------------
# Enhancement 5: Multi-round RAG — detect data gaps and retry
# ----------------------------

detect_data_gaps <- function(agent_output) {
  if (is.null(agent_output)) return(TRUE)
  text <- tolower(paste(unlist(agent_output), collapse = " "))
  gap_phrases <- c("data not available", "no data provided", "data unavailable",
                   "insufficient data", "no relevant data", "not provided in the data",
                   "no news available", "analysis unavailable")
  gap_count <- sum(vapply(gap_phrases, function(p) grepl(p, text, fixed = TRUE), logical(1)))
  gap_count >= 2
}

retry_weak_agents <- function(symbol, company_name, sector, trend_text, api_key,
                              agent_outputs) {
  gaps <- vapply(agent_outputs, detect_data_gaps, logical(1))
  failed <- vapply(agent_outputs, is.null, logical(1))
  needs_retry <- names(agent_outputs)[gaps | failed]

  if (length(needs_retry) == 0) {
    message("[Multi-RAG] All agents have sufficient data, no retry needed")
    return(agent_outputs)
  }

  message("[Multi-RAG] Retrying ", length(needs_retry), " agent(s) with broader context: ",
          paste(needs_retry, collapse = ", "))
  preamble <- agent_date_preamble()

  retry_reqs <- lapply(needs_retry, function(atype) {
    rag_context <- rag_retrieve(symbol, atype, use_semantic = FALSE,
                                token_budget = 4500L, broad = TRUE)
    user_prompt <- paste0(
      preamble, "\n",
      "Analyze ", symbol, " (", company_name, "), sector: ", sector, ".\n\n",
      "NOTE: This is a RETRY with EXPANDED data context. Please use ALL available data thoroughly.\n\n",
      "REMINDER: You may ONLY cite numbers and facts from the === DATA === sections below.\n\n",
      if (nzchar(trend_text)) paste0("=== CURRENT PRICE DATA ===\n", trend_text, "\n\n") else "",
      "=== DATA (Your ONLY source of truth — EXPANDED) ===\n", rag_context
    )
    build_agent_request(
      system_prompt = AGENT_ROLES[[atype]]$system,
      user_content  = user_prompt,
      api_key       = api_key,
      max_tokens    = 1500
    )
  })

  retry_resps <- tryCatch(
    req_perform_parallel(retry_reqs, on_error = "continue"),
    error = function(e) {
      message("[Multi-RAG] Retry request failed: ", e$message)
      NULL
    }
  )

  if (!is.null(retry_resps)) {
    retry_outputs <- setNames(lapply(retry_resps, parse_agent_response), needs_retry)
    improved <- 0L
    for (atype in needs_retry) {
      if (!is.null(retry_outputs[[atype]])) {
        agent_outputs[[atype]] <- retry_outputs[[atype]]
        improved <- improved + 1L
      }
    }
    message("[Multi-RAG] ", improved, "/", length(needs_retry), " agents improved on retry")
  }

  agent_outputs
}

# ----------------------------
# Phase 2: Portfolio Manager synthesizes all agent outputs
# ----------------------------

run_phase2_pm <- function(symbol, company_name, sector, agent_outputs, api_key) {
  preamble <- agent_date_preamble()

  summary_parts <- lapply(names(agent_outputs), function(atype) {
    out <- agent_outputs[[atype]]
    if (is.null(out)) return(paste0("## ", AGENT_ROLES[[atype]]$name, "\n[Analysis unavailable]\n"))
    paste0(
      "## ", AGENT_ROLES[[atype]]$name, "\n",
      jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE), "\n"
    )
  })

  pm_user <- paste0(
    preamble, "\n",
    "Produce the final equity research report for ", symbol, " (", company_name, "), sector: ", sector, ".\n\n",
    "Below are the analysis outputs from your six specialist analysts:\n\n",
    paste(summary_parts, collapse = "\n"),
    "\n\nSynthesize all inputs into your final recommendation. ",
    "Resolve any contradictions between bull/bear cases with your professional judgment. ",
    "Ensure every section references specific data from the analyst reports."
  )

  message("[Phase2] Sending Portfolio Manager synthesis request...")
  pm_req <- build_agent_request(
    system_prompt = PM_SYSTEM_PROMPT,
    user_content  = pm_user,
    api_key       = api_key,
    max_tokens    = 4000
  )

  pm_resp <- tryCatch(req_perform(pm_req), error = function(e) {
    message("[Phase2] PM request failed: ", e$message)
    NULL
  })

  parse_agent_response(pm_resp)
}

# ----------------------------
# Orchestrator: Full parallel multi-agent pipeline
# ----------------------------

run_parallel_agents <- function(symbol, stock_data_val, news_data_df,
                                trend = NULL, gbm = NULL, lattice = NULL, sim = NULL,
                                on_progress = NULL) {
  company_name <- TICKER_LABELS[[symbol]] %||% symbol
  sector <- SECTOR_MAP[[symbol]] %||% "Unknown"

  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (!nzchar(api_key)) {
    return(list(ok = FALSE, error = "OPENAI_API_KEY not set."))
  }

  trend_text <- ""
  if (!is.null(trend)) {
    trend_text <- sprintf(
      "Current price: %s\n1-day change: %s\n7-day change: %s\n30-day change: %s\nAnnualized 20-day volatility: %s",
      fmt_price(trend$latest), fmt_pct(trend$change1d_pct),
      fmt_pct(trend$change7d_pct), fmt_pct(trend$change30d_pct),
      ifelse(is.na(trend$mean_20d_volatility), "N/A", paste0(trend$mean_20d_volatility, "%"))
    )
  }

  rag_clear_cache()

  if (is.function(on_progress)) on_progress(0.15, "Running semantic search on news data...")
  if (is.function(on_progress)) on_progress(0.20, "Running 6 specialist analysts in parallel...")
  agent_outputs <- run_phase1_parallel(symbol, company_name, sector, trend_text, api_key,
                                        use_semantic = TRUE)
  if (is.null(agent_outputs) || all(vapply(agent_outputs, is.null, logical(1)))) {
    message("[Agents] Phase 1 failed, falling back to single-call approach")
    return(NULL)
  }

  if (is.function(on_progress)) on_progress(0.50, "Checking for data gaps...")
  agent_outputs <- retry_weak_agents(symbol, company_name, sector, trend_text, api_key,
                                      agent_outputs)

  total_tokens <- list(prompt = 0L, completion = 0L, total = 0L)
  for (out in agent_outputs) {
    tk <- attr(out, "tokens")
    if (!is.null(tk)) {
      total_tokens$prompt <- total_tokens$prompt + (tk$prompt_tokens %||% 0)
      total_tokens$completion <- total_tokens$completion + (tk$completion_tokens %||% 0)
      total_tokens$total <- total_tokens$total + (tk$total_tokens %||% 0)
    }
  }

  if (is.function(on_progress)) on_progress(0.70, "Portfolio Manager synthesizing final report...")
  report <- run_phase2_pm(symbol, company_name, sector, agent_outputs, api_key)
  if (is.null(report)) {
    message("[Agents] Phase 2 failed, falling back to single-call approach")
    return(NULL)
  }

  tk <- attr(report, "tokens")
  if (!is.null(tk)) {
    total_tokens$prompt <- total_tokens$prompt + (tk$prompt_tokens %||% 0)
    total_tokens$completion <- total_tokens$completion + (tk$completion_tokens %||% 0)
    total_tokens$total <- total_tokens$total + (tk$total_tokens %||% 0)
  }

  list(report = report, agent_outputs = agent_outputs, total_tokens = total_tokens)
}
