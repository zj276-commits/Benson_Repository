# Market Insight Studio

A real-time financial analysis and AI prediction dashboard built with R/Shiny, tracking 7 major tech stocks with live quotes, news aggregation, and quantitative model forecasting.

**Live App:** [market-insight-studio on DigitalOcean](https://your-app-url.ondigitalocean.app)

---

## Tracked Stocks

| Ticker | Company |
|--------|---------|
| AAPL | Apple |
| TSLA | Tesla |
| META | Meta |
| NVDA | NVIDIA |
| GOOGL | Alphabet |
| AMZN | Amazon |
| MSFT | Microsoft |

Benchmark index: **SPY** (S&P 500 ETF)

---

## Features

### Data Tab
- **Live stock quotes** with auto-refresh (Finnhub real-time API)
- **Automatic historical market data freshness check** on startup
- Price cards showing current price, daily change, and volume
- **Analytics sub-page** with:
  - Model Parameters table (GBM, Binomial Lattice, Single Index Model)
  - Historical Prices table (OHLCV)
  - Downloadable CSV export
  - Refresh status panel showing latest exported trading date, preferred price source, and source mix (`tiingo`, `finnhub`, `yahoo_fallback`, etc.)

### News Tab
- Company news from Finnhub (filterable by stock, 7/30/90 day range)
- Persisted latest news summary for the next user/session
- Timestamps in US Eastern Time
- Clickable article links

### Backtest Tab
- **Mask-safe agent backtesting** over `7D`, `30D`, `90D`, or custom windows
- Uses $10,000 borrowed capital baseline and prevents future data leakage
- Persists the latest result to `results/latest_backtest_{ticker}.json`
- Shows strategy curve vs buy-and-hold, executed trades, and quality matrix

### Reporting Tab (AI Multi-Agent)
- **Geometric Brownian Motion (GBM)** forecast with confidence bands
- **Binomial Lattice (CRR)** price tree projection
- **Single Index Model (SIM)** regression against SPY
- **RAG + Parallel Multi-Agent pipeline** — 6 specialist analysts run in parallel via `httr2::req_perform_parallel`, each with role-specific RAG-retrieved CSV data, followed by a Portfolio Manager synthesis phase
- Automatic fallback to single-call approach if multi-agent pipeline fails
- Supports OpenAI GPT-4o-mini and Ollama backends

---

## Quantitative Models

| Model | Description |
|-------|-------------|
| GBM | Estimates annualized drift (mu) and volatility (sigma) from log returns; projects expected price path with 5-95% confidence interval |
| Binomial Lattice | CRR parameterization with risk-neutral and real-world up-probabilities; 22-step tree over 1-month horizon |
| Single Index Model | OLS regression of stock returns vs SPY; produces alpha, beta, R-squared, and confidence intervals |

Additional statistics: Shapiro-Wilk normality test, skewness, kurtosis, max drawdown, realized/downside/20-day volatility, outlier fraction.

---

## RAG Data Files

These CSV files are auto-exported on each data refresh for downstream RAG pipelines:

| File | Description |
|------|-------------|
| `historical_prices.csv` | Daily OHLCV prices for all 7 stocks + SPY |
| `model_parameters.csv` | Daily snapshots of computed model parameters (from 2026-02-01 onward) |
| `news_archive.csv` | Accumulated company news with timestamps |
| `rag_data_column_dictionary.csv` | Column definitions for the model parameters |

Latest persisted user-facing outputs:

| File | Description |
|------|-------------|
| `results/latest_backtest_{ticker}.json` | Latest backtest result for a ticker (overwrite mode) |
| `results/latest_news_summary.json` | Latest cross-ticker news summary |
| `results/latest_refresh_status.json` | Latest market refresh status and source summary |

---

## Setup

### Prerequisites

- R >= 4.1 (required for native pipe `|>`)
- API keys for Finnhub, OpenAI, and optionally Ollama

### 1. Install R packages

```bash
Rscript install_packages.R
```

### 2. Configure API keys

Create a `.env` file in this directory:

```
FINNHUB_API_KEY=your_finnhub_key
OPENAI_API_KEY=your_openai_key
OLLAMA_API_KEY=your_ollama_key_or_leave_empty
TIINGO_API_TOKEN=your_tiingo_token
MARKETAUX_API_TOKEN=your_marketaux_token
ALPHA_VANTAGE_API_KEY=your_alpha_vantage_key
BRAINTRUST_API_KEY=your_braintrust_key
REFRESH_TOKEN=your_refresh_token
PREFERRED_PRICE_SOURCE=auto
```

Do not commit the real `.env` file or any live API keys to GitHub. `.env` is already gitignored in this repo. For cloud deployment, configure these values in the hosting platform as secrets/environment variables instead.

- **Finnhub** (free, 60 req/min): [finnhub.io](https://finnhub.io/register) — real-time quotes, historical candles, company news
- **OpenAI** — GPT-4o-mini for AI predictions and reports
- **Ollama** (optional) — leave empty or set to `local` for local Ollama; or set cloud key
- **Tiingo** — preferred daily candle source when available
- **MarketAux** — entity-filtered company news
- **Alpha Vantage** — supplemental news sentiment feed
- **Braintrust** (optional, not on hot path) — evaluation/observability integration

`PREFERRED_PRICE_SOURCE` accepts `auto`, `tiingo`, `finnhub`, or `yahoo`. `auto` currently prefers Tiingo when a `TIINGO_API_TOKEN` is present.

### 3. Run locally

```bash
R -e "shiny::runApp('.', port = 3838)"
```

Or use the restart script:

```bash
./restart_app.sh 3838 --open
```

The app will now check whether exported market data is stale on startup and refresh it automatically when needed. To skip this during tests:

```bash
SKIP_STARTUP_REFRESH=1 R -e "shiny::runApp('.', port = 3838)"
```

---

## Docker Deployment

```bash
docker build -t market-insight .
docker run -p 8080:8080 \
  -e FINNHUB_API_KEY=your_key \
  -e OPENAI_API_KEY=your_key \
  -e OLLAMA_API_KEY=your_key \
  -e TIINGO_API_TOKEN=your_key \
  -e MARKETAUX_API_TOKEN=your_key \
  -e ALPHA_VANTAGE_API_KEY=your_key \
  -e BRAINTRUST_API_KEY=your_key \
  -e REFRESH_TOKEN=your_key \
  market-insight
```

The app is configured for DigitalOcean App Platform with environment variables set in the dashboard. Do not upload `.env` to the repo to make DigitalOcean work. Set each key in DigitalOcean under `App Settings -> Environment Variables`, and mark secrets such as API keys and `REFRESH_TOKEN` as encrypted.

Operational notes:

- Daily candles are cached on disk for 6 hours under `data/api/candles/`
- News responses are cached on disk for 20 minutes under `data/api/news/`
- Live quotes are cached on disk for 10 seconds under `data/api/quotes/`
- This reduces cold-start latency and avoids wasting quota when multiple users hit the app

### Protected refresh endpoints

Set `REFRESH_TOKEN` in the deployment environment, then call:

```text
/?action=refresh-market-data&token=YOUR_TOKEN
/?action=refresh-financials&token=YOUR_TOKEN
/?action=refresh-all&token=YOUR_TOKEN
```

Recommended production pattern:

- Use startup auto-refresh as a safety net
- Schedule `refresh-market-data` during market-close/off-hours
- Schedule `refresh-all` less frequently because Yahoo financial statement pulls are slower

Helper script:

```bash
./scripts/trigger_refresh.sh http://localhost:3838 your_token refresh-market-data
```

---

## Multi-Agent Architecture

The reporting pipeline uses a two-phase RAG + parallel multi-agent workflow:

```
Phase 1 (Parallel):  6 specialist agents run simultaneously
┌──────────────────┬──────────────────┬──────────────────┐
│ Fundamentals     │ News Analyst     │ Technical/Quant  │
│ key_financials   │ news_archive     │ model_parameters │
│ company_fin      │                  │ historical_prices│
│ valuation        │                  │ column_dict      │
├──────────────────┼──────────────────┼──────────────────┤
│ Bull Researcher  │ Bear Researcher  │ Risk Manager     │
│ key_financials   │ key_financials   │ model_parameters │
│ company_fin      │ company_fin      │ credit_metrics   │
│ recent_news      │ risk_indicators  │                  │
└──────────────────┴──────────────────┴──────────────────┘
                          │
                          ▼
Phase 2 (Sequential):  Portfolio Manager synthesizes all outputs
                          │
                          ▼
                   Final JSON Report
         (same schema as single-call approach)
```

Each agent retrieves only the CSV data relevant to its role (RAG retrieval), receives a focused system prompt, and returns structured JSON. The Portfolio Manager aggregates all 6 outputs into the final report.

If the multi-agent pipeline fails, the system falls back to the original single monolithic LLM call.

## Project Structure

```
.
├── app.R                           # Main Shiny entry point
├── global.R                        # Thin compatibility shim that loads R/shared/global.R
├── R/
│   ├── shared/
│   │   └── global.R                # Shared config, package loading, API keys, module loading
│   ├── backend/
│   │   ├── api.R                   # API functions (Finnhub, Yahoo, OpenAI, Ollama)
│   │   ├── models.R                # Financial models (GBM, Lattice, SIM), caching, export
│   │   ├── rag.R                   # Retrieval and RAG context assembly
│   │   ├── agents.R                # RAG retrieval + parallel multi-agent orchestration
│   │   ├── report.R                # Report generation (multi-agent + fallback), HTML rendering
│   │   ├── backtester.R            # Mask-safe backtesting and persistence
│   │   └── fetch_financials.R      # Yahoo Finance financial data fetcher
│   └── frontend/
│       ├── ui.R                    # Shiny UI definition
│       └── server.R                # Shiny server logic
├── install_packages.R              # R package installer
├── Dockerfile                      # Docker config (rocker/shiny:4.4.0)
├── .env                            # Local-only secrets (gitignored, never commit)
├── .gitignore
├── historical_prices.csv           # Exported price data for RAG
├── model_parameters.csv            # Exported model params for RAG
├── key_financials.csv              # Key financial snapshot for RAG
├── company_financials.csv          # Multi-year P&L and credit for RAG
├── valuation_metrics.csv           # Multi-year valuation for RAG
├── news_archive.csv                # Accumulated news archive for RAG
├── rag_data_column_dictionary.csv  # Column definitions for model_parameters
└── data/                           # Local cache (gitignored)
    ├── daily_prices.csv
    ├── news.csv
    ├── rag_history.csv
    └── api/                        # Disk cache for candles / news / quotes
```

---

## API Rate Limits

| API | Limit | Usage |
|-----|-------|-------|
| Finnhub | 60 req/min (free) | Quotes, candles, news |
| Yahoo Finance | Unofficial, no key | Fallback for historical data |
| OpenAI | Per-plan | AI predictions, reports |
| Ollama | Unlimited (local) | Fallback LLM |

---

## Tech Stack

- **R / Shiny** — reactive web framework
- **Plotly** — interactive charts
- **DT** — data tables
- **httr2** — HTTP client for API calls
- **Docker** — containerized deployment
- **DigitalOcean App Platform** — cloud hosting
