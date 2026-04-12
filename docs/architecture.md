# Architecture Notes

## Current Runtime Structure

The app now uses a layered runtime layout while preserving a thin root entrypoint for Shiny and scripts:

- `app.R`
  Thin Shiny entrypoint. Loads the compatibility shim and then sources the frontend modules.
- `global.R`
  Compatibility shim so existing commands like `source("global.R")` still work.
- `R/shared/global.R`
  Loads packages, environment variables, shared constants, cache helpers, project-path helpers, and sources the backend modules.
- `R/backend/api.R`
  Market/news/LLM API clients and response caching.
- `R/backend/models.R`
  Quant models, CSV export/cache handling, news normalization, and persisted refresh metadata.
- `R/backend/rag.R`
  RAG retrieval, ranking, and context assembly helpers.
- `R/backend/agents.R`
  Multi-agent orchestration for research reports.
- `R/backend/report.R`
  Report prompt construction and report rendering helpers.
- `R/backend/backtester.R`
  Mask-safe agent backtesting, persistence, and deterministic quality checks.
- `R/backend/fetch_financials.R`
  Yahoo-based financial statement refresh script.
- `R/frontend/ui.R`
  Shiny UI, including Data, News, Backtest, and Reporting tabs.
- `R/frontend/server.R`
  Shiny server orchestration and user-triggered workflows.

## Persisted Outputs

- `results/latest_backtest_{ticker}.json`
  Latest agent backtest for each ticker. New runs overwrite the previous version.
- `results/latest_news_summary.json`
  Latest persisted cross-ticker news summary for the next session/user.
- `results/latest_refresh_status.json`
  Latest market refresh metadata, including success/failure state and price-source mix.
- `tests/test_suite.R`
  Deterministic regression suite for RAG retrieval, backtest masking, execution logic, metrics, persistence, and cache helpers.

## Operational Notes

The refactor intentionally keeps:

1. `app.R` at the repo root so `shiny::runApp('.')` and DigitalOcean/Docker entrypoints do not change.
2. `global.R` at the repo root as a thin shim so legacy scripts and test commands remain valid.
3. Exported CSVs at the repo root for now, because the RAG and reporting pipelines already depend on `get_export_dir()`.

The next cleanup step, if desired, would be moving exported CSVs into a dedicated tracked directory such as `exports/` and updating `get_export_dir()` accordingly.
