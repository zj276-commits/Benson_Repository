# tests/test_rag_eval.R
# Deterministic RAG evaluation matrix for retrieval quality and freshness

RAG_HEADER_EXPECTATIONS <- list(
  fundamentals = c(
    "=== KEY FINANCIALS ===",
    "=== INCOME STATEMENT & PROFITABILITY",
    "=== VALUATION MULTIPLES",
    "=== RECENT NEWS"
  ),
  news = c("=== NEWS HEADLINES"),
  technical = c(
    "=== QUANTITATIVE MODEL PARAMETERS ===",
    "=== RECENT DAILY PRICES"
  ),
  bull = c(
    "=== KEY FINANCIALS ===",
    "=== INCOME STATEMENT & PROFITABILITY",
    "=== RECENT NEWS"
  ),
  bear = c(
    "=== KEY FINANCIALS ===",
    "=== INCOME STATEMENT & PROFITABILITY",
    "=== RISK INDICATORS ===",
    "=== RECENT NEWS"
  ),
  risk = c(
    "=== QUANTITATIVE MODEL PARAMETERS ===",
    "=== CREDIT & LEVERAGE METRICS ===",
    "=== RECENT NEWS"
  )
)

RAG_NEWS_REQUIREMENTS <- list(
  fundamentals = list(min_company_rows = 2L, min_precision = 0.75),
  news = list(min_company_rows = 4L, min_precision = 0.80),
  technical = list(min_company_rows = 0L, min_precision = NA_real_),
  bull = list(min_company_rows = 3L, min_precision = 0.75),
  bear = list(min_company_rows = 3L, min_precision = 0.75),
  risk = list(min_company_rows = 2L, min_precision = 0.60)
)

rag_extract_headers <- function(text) {
  if (is.null(text) || !nzchar(text)) return(character(0))
  grep("^===", strsplit(text, "\n", fixed = TRUE)[[1]], value = TRUE)
}

rag_agent_uses_news <- function(agent_type) {
  agent_type %in% c("fundamentals", "news", "bull", "bear", "risk")
}

rag_dataset_freshness_matrix <- function() {
  load_dates <- function(path, column, parser) {
    df <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0 || !column %in% names(df)) return(as.Date(NA))
    parsed <- parser(df[[column]])
    parsed <- parsed[!is.na(parsed)]
    if (length(parsed) == 0) return(as.Date(NA))
    max(parsed)
  }

  app_dir <- get_export_dir()
  checks <- list(
    list(
      source = "key_financials",
      latest_date = load_dates(file.path(app_dir, "key_financials.csv"), "fetch_date", as.Date),
      threshold_days = 3L
    ),
    list(
      source = "model_parameters",
      latest_date = load_dates(file.path(app_dir, "model_parameters.csv"), "snapshot_date", as.Date),
      threshold_days = 3L
    ),
    list(
      source = "historical_prices",
      latest_date = load_dates(file.path(app_dir, "historical_prices.csv"), "Date", as.Date),
      threshold_days = 3L
    ),
    list(
      source = "news_archive",
      latest_date = load_dates(
        file.path(app_dir, "news_archive.csv"),
        "Published",
        function(x) as.Date(as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC"))
      ),
      threshold_days = 2L
    )
  )

  do.call(rbind, lapply(checks, function(item) {
    age_days <- if (is.na(item$latest_date)) NA_integer_ else as.integer(Sys.Date() - item$latest_date)
    data.frame(
      source = item$source,
      latest_date = if (is.na(item$latest_date)) NA_character_ else as.character(item$latest_date),
      age_days = age_days,
      threshold_days = item$threshold_days,
      pass = !is.na(age_days) && age_days >= 0 && age_days <= item$threshold_days,
      stringsAsFactors = FALSE
    )
  }))
}

test_rag_dataset_freshness <- function() {
  cat("=== RAG Freshness Tests ===\n")
  freshness <- rag_dataset_freshness_matrix()
  passed <- sum(freshness$pass, na.rm = TRUE)
  total <- nrow(freshness)

  if (passed < total) {
    failing <- freshness[!freshness$pass, , drop = FALSE]
    apply(failing, 1, function(row) {
      cat(sprintf(
        "  FAIL: %s latest=%s age=%s threshold=%s\n",
        row[["source"]],
        row[["latest_date"]],
        row[["age_days"]],
        row[["threshold_days"]]
      ))
    })
  }

  cat(sprintf("RAG Freshness: %d/%d passed (%.0f%%)\n\n", passed, total, passed / total * 100))
  list(name = "rag_freshness", passed = passed, total = total, details = freshness)
}

run_rag_eval <- function(symbols = c("AAPL", "NVDA", "TSLA"),
                         agents = names(AGENT_ROLES),
                         use_semantic = TRUE,
                         token_budget = RAG_CONFIG$token_budget,
                         persist = TRUE,
                         verbose = TRUE) {
  if (use_semantic && !nzchar(Sys.getenv("OPENAI_API_KEY"))) {
    if (verbose) cat("OPENAI_API_KEY missing; falling back to lexical-only RAG eval.\n")
    use_semantic <- FALSE
  }

  rag_clear_cache()
  rows <- vector("list", length(symbols) * length(agents))
  idx <- 0L

  for (symbol in symbols) {
    for (agent_type in agents) {
      idx <- idx + 1L
      started_at <- Sys.time()
      bundle <- rag_build_context_bundle(
        symbol = symbol,
        agent_type = agent_type,
        use_semantic = use_semantic,
        token_budget = token_budget
      )
      latency_sec <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))

      context_text <- bundle$text %||% ""
      headers <- rag_extract_headers(context_text)
      expected_headers <- RAG_HEADER_EXPECTATIONS[[agent_type]] %||% character(0)
      header_hits <- if (length(expected_headers) == 0) logical(0) else {
        vapply(expected_headers, function(pattern) {
          any(grepl(pattern, headers, fixed = TRUE))
        }, logical(1))
      }

      context_ok <- nzchar(context_text)
      header_pass <- if (length(expected_headers) == 0) TRUE else all(header_hits)
      header_coverage <- if (length(expected_headers) == 0) 1 else mean(header_hits)
      token_estimate <- if (context_ok) rag_estimate_tokens(context_text) else NA_integer_
      budget_pass <- !is.na(token_estimate) && token_estimate <= token_budget

      news_df <- bundle$news_df
      contamination_pass <- TRUE
      company_rows <- NA_integer_
      macro_rows <- NA_integer_
      company_precision <- NA_real_
      company_count_pass <- TRUE
      company_precision_pass <- TRUE

      if (rag_agent_uses_news(agent_type)) {
        contamination_pass <- !is.null(news_df) && nrow(news_df) > 0 &&
          all(news_df$Symbol %in% c(symbol, "MACRO"))
        company_rows <- if (is.null(news_df)) 0L else sum(news_df$Symbol == symbol, na.rm = TRUE)
        macro_rows <- if (is.null(news_df)) 0L else sum(news_df$Symbol == "MACRO", na.rm = TRUE)
        company_precision <- if (!is.null(news_df) && company_rows > 0 && "company_mentioned" %in% names(news_df)) {
          mean(news_df$company_mentioned[news_df$Symbol == symbol], na.rm = TRUE)
        } else if (company_rows > 0) {
          NA_real_
        } else {
          0
        }

        requirements <- RAG_NEWS_REQUIREMENTS[[agent_type]]
        company_count_pass <- company_rows >= requirements$min_company_rows
        company_precision_pass <- if (is.na(requirements$min_precision)) {
          TRUE
        } else {
          !is.na(company_precision) && company_precision >= requirements$min_precision
        }
      }

      checks <- c(
        context_ok = context_ok,
        header_pass = header_pass,
        budget_pass = budget_pass,
        contamination_pass = contamination_pass,
        company_count_pass = company_count_pass,
        company_precision_pass = company_precision_pass
      )

      rows[[idx]] <- data.frame(
        symbol = symbol,
        agent_type = agent_type,
        use_semantic = use_semantic,
        latency_sec = round(latency_sec, 3),
        context_chars = nchar(context_text),
        token_estimate = token_estimate,
        header_coverage = round(header_coverage, 3),
        company_rows = company_rows,
        macro_rows = macro_rows,
        company_precision = round(company_precision, 3),
        context_ok = context_ok,
        header_pass = header_pass,
        budget_pass = budget_pass,
        contamination_pass = contamination_pass,
        company_count_pass = company_count_pass,
        company_precision_pass = company_precision_pass,
        case_pass = all(checks),
        failed_checks = paste(names(checks)[!checks], collapse = ", "),
        stringsAsFactors = FALSE
      )
    }
  }

  metrics <- dplyr::bind_rows(rows)
  freshness <- rag_dataset_freshness_matrix()

  aggregate <- list(
    total_cases = nrow(metrics),
    passed_cases = sum(metrics$case_pass),
    pass_rate = round(mean(metrics$case_pass), 3),
    avg_latency_sec = round(mean(metrics$latency_sec), 3),
    avg_header_coverage = round(mean(metrics$header_coverage), 3),
    avg_company_precision = round(mean(metrics$company_precision, na.rm = TRUE), 3),
    avg_company_rows = round(mean(metrics$company_rows, na.rm = TRUE), 2),
    freshness_pass_rate = round(mean(freshness$pass), 3)
  )

  payload <- list(
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    use_semantic = use_semantic,
    token_budget = token_budget,
    symbols = symbols,
    thresholds = list(
      freshness_days = as.list(setNames(freshness$threshold_days, freshness$source)),
      min_company_rows = lapply(RAG_NEWS_REQUIREMENTS, `[[`, "min_company_rows"),
      min_company_precision = lapply(RAG_NEWS_REQUIREMENTS, `[[`, "min_precision")
    ),
    freshness = freshness,
    aggregate = aggregate,
    metrics = metrics
  )

  if (persist) {
    write_latest_result_json("latest_rag_eval.json", payload)
    utils::write.csv(metrics, file.path(get_results_dir(), "latest_rag_eval_metrics.csv"), row.names = FALSE)
  }

  if (verbose) {
    cat(sprintf(
      "RAG matrix: %d/%d cases passed (%.0f%%)\n",
      aggregate$passed_cases,
      aggregate$total_cases,
      aggregate$pass_rate * 100
    ))
    cat(sprintf(
      "Average company-news precision: %.2f | average latency: %.2fs\n",
      aggregate$avg_company_precision,
      aggregate$avg_latency_sec
    ))

    if (aggregate$passed_cases < aggregate$total_cases) {
      failing <- metrics[!metrics$case_pass, c("symbol", "agent_type", "failed_checks"), drop = FALSE]
      print(failing, row.names = FALSE)
    }
  }

  invisible(payload)
}

test_rag_retrieval <- function(symbols = c("AAPL", "NVDA", "TSLA"),
                               use_semantic = TRUE,
                               token_budget = RAG_CONFIG$token_budget,
                               persist = TRUE) {
  cat("=== RAG Retrieval Matrix ===\n")
  result <- run_rag_eval(
    symbols = symbols,
    use_semantic = use_semantic,
    token_budget = token_budget,
    persist = persist,
    verbose = TRUE
  )

  metrics <- result$metrics
  passed <- sum(metrics$case_pass)
  total <- nrow(metrics)
  cat(sprintf("RAG Retrieval Matrix: %d/%d passed (%.0f%%)\n\n", passed, total, passed / total * 100))
  list(name = "rag_retrieval_matrix", passed = passed, total = total, details = result)
}
