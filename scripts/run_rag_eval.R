#!/usr/bin/env Rscript

Sys.setenv(SKIP_STARTUP_REFRESH = "1")

source("global.R")
source("tests/test_rag_eval.R")

cat("Running full RAG evaluation across all supported tickers...\n")
run_rag_eval(
  symbols = TICKERS,
  use_semantic = TRUE,
  token_budget = RAG_CONFIG$token_budget,
  persist = TRUE,
  verbose = TRUE
)
