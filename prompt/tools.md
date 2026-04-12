# LLM Multi-Agent Trading Toolbox Selection Matrix
# Context: R/Shiny Frontend + Python/Docker Sidecar Architecture

architecture_directive: "Frontend (R/Shiny) handles UI/visualization. Backend/Agents (Python) handles LLM orchestration and backtesting. Communication via REST API or shared database (DuckDB)."

tool_categories:
  1_backtesting_frameworks:
    - name: "FinRL-X"
      repo: "AI4Finance-Foundation/FinRL-Trading"
      type: "RL + LLM Trading Framework"
      integration: "Deploy in Python Docker container -> Expose FastAPI -> Call via R httr2."
      cost: "Open-source (Free)"
    - name: "TradingAgents"
      repo: "TauricResearch/TradingAgents"
      type: "Multi-Agent LLM Trading Framework"
      integration: "Python backend handles multi-agent dispatch (Analyst/Risk/Trader). Send JSON payload from R."
      cost: "Open-source (Free)"

  2_news_and_sentiment_nlp:
    - name: "Finlight API"
      url: "finlight.me"
      type: "Real-time News REST API"
      features: "Built-in sentiment scoring, ticker filtering, boolean logic."
      integration: "Direct R httr2 call."
      cost: "Freemium"
    - name: "FinBERT"
      repo: "ProsusAI/finbert (HuggingFace)"
      type: "Local NLP Model"
      features: "Financial text embedding + positive/negative/neutral classification."
      integration: "Run via Python sidecar API or R reticulate. Feed outputs to Vector DB."
      cost: "Open-source (Compute cost only)"

  3_market_data_apis:
    - name: "Polygon.io"
      type: "REST API"
      features: "OHLCV, Tick. 15-min delayed for free tier."
      limits: "5 requests/minute (Free)"
      integration: "Direct R httr2 call. Best for simulated daily/intraday."
    - name: "Twelve Data"
      type: "REST API"
      features: "Good historical daily data."
      limits: "800 requests/day, 8/min (Free)"
      integration: "Direct R httr2 call."
    - name: "Tiingo"
      type: "REST API"
      features: "10+ years historical daily data."
      limits: "Generous hobbyist free tier."
      integration: "Direct R httr2 call."

  4_agent_evaluation_evals:
    - name: "promptfoo"
      url: "promptfoo.dev"
      type: "CLI Eval Framework"
      features: "LLM-as-a-judge, regression testing, YAML driven."
      integration: "Node CLI. Trigger via R `system()` and parse JSON results."
    - name: "DeepEval"
      repo: "confident-ai/deepeval"
      type: "Python Eval Library"
      features: "Conversation simulation, context relevance metrics."
      integration: "Run as Python test suite alongside Agent backend."

  5_vector_database:
    - name: "DuckDB + VSS Extension"
      url: "duckdb.org/docs/extensions/vss"
      type: "In-memory/File SQL Database"
      features: "HNSW indexing, array_cosine_similarity."
      integration: "Native R packages (`duckdb`, `DBI`). Ultra-low latency, zero external ops."
      recommendation: "Highest priority for R/Shiny integration."
    - name: "ChromaDB"
      url: "trychroma.com"
      type: "LLM-Native Vector DB"
      features: "Text embedding & similarity search."
      integration: "Deploy via Docker Compose -> R requests via HTTP API."

  6_trade_execution_mcp:
    - name: "Alpaca MCP Server"
      repo: "alpacahq/alpaca-mcp-server"
      type: "Model Context Protocol Server"
      features: "Allows Claude/LLM to directly execute paper trades and read portfolio."
      integration: "Mount to Python LLM Agent environment. R reads trade logs from DB."
      cost: "$100k Paper Trading (Free)"



# Market Insight Studio: complete integration toolkit across 6 domains

**TradingAgents, Alpaca, ChromaDB, and a handful of specialized APIs form the optimal expansion stack for an R/Shiny financial dashboard moving into AI-agent territory.** After evaluating 60+ tools across backtesting frameworks, sentiment APIs, data providers, eval systems, vector databases, and trading execution platforms, a clear architecture emerges: Python-based AI agent frameworks wrapped as Docker microservices, called from R/Shiny via httr2, with ChromaDB for semantic retrieval and Alpaca for paper trading. Every recommendation below prioritizes R compatibility, free-tier viability for 7 US tech stocks, and Docker-native deployment.

---

## Topic 1: LLM trading agent backtesting frameworks

No R-native LLM trading backtesting package exists. All significant frameworks are Python-based, but several offer REST APIs or can be wrapped in FastAPI microservices callable from R via httr2. The critical differentiator is **date fidelity** — preventing future data leakage during backtests.

### TradingAgents — the clear frontrunner

| Detail | Value |
|---|---|
| **GitHub** | https://github.com/TauricResearch/TradingAgents |
| **Stars** | **~49,000** |
| **Last update** | April 4, 2026 (v0.2.3) |
| **License** | Apache 2.0 |
| **R interface** | No native — wrap in FastAPI or use `reticulate` |
| **Extra API keys** | Alpha Vantage (free tier sufficient) |

TradingAgents simulates a complete trading firm with **7 specialized agent roles**: Fundamentals Analyst, Sentiment Analyst, News Analyst, Technical Analyst, Bull & Bear Researchers (adversarial debate), Trader, Risk Manager, and Portfolio Manager. Version 0.2.3 introduced explicit "backtesting date fidelity" — agents operate day-by-day without access to future data. It supports **OpenAI, Anthropic, Google Gemini, xAI, OpenRouter, and Ollama** for local inference, making it the most LLM-flexible option. Docker deployment is built-in via `docker compose run --rm tradingagents`. Integration path: wrap in a FastAPI microservice exposing `POST /analyze {ticker, date, llm_provider}`, add to docker-compose alongside R/Shiny, call via httr2. Cost per analysis runs **$0.01–0.05 with GPT-4o-mini**.

### AI-Trader and other contenders

**AI-Trader** (HKUDS, ~9,500 stars, https://github.com/HKUDS/AI-Trader) already has a **FastAPI backend** — the easiest REST API integration for R. It targets NASDAQ 100 stocks with automatic future information filtering and starts agents with $10,000 simulated capital. Requires OpenAI + Alpha Vantage + Jina AI API keys.

**FINSABER** (https://github.com/waylonli/FINSABER, 30 stars) is purpose-built for **bias-aware LLM backtesting** — the entire framework is designed around survivorship bias, look-ahead bias, and data-snooping mitigation with rolling window evaluation. Academic-grade but pip-installable.

**LiveTradeBench** (https://github.com/ulab-uiuc/live-trade-bench, 106 stars) is notable for using **Yahoo Finance + Finnhub** — matching the existing stack exactly — and exposes a full FastAPI REST API. Caveat: PolyForm Noncommercial license.

**FinRL** (~14,700 stars, https://github.com/AI4Finance-Foundation/FinRL) is **reinforcement-learning-based, not LLM-based**, requires GPU training, and has no REST API. Not recommended for this project's R/Shiny stack. Its companion project FinGPT (~19,000 stars) handles sentiment analysis but not backtesting.

### Integration comparison

| Framework | REST API | httr2 Ready | Date Fidelity | Multi-LLM | Integration Effort |
|---|---|---|---|---|---|
| **TradingAgents** | Needs FastAPI wrapper | Via wrapper | ✅ v0.2.3 | ✅ 6 providers + Ollama | Medium |
| **AI-Trader** | ✅ Built-in FastAPI | ✅ Direct | ✅ Auto-filtering | Partial | Medium |
| **LiveTradeBench** | ✅ Built-in FastAPI | ✅ Direct | Partial | ✅ GPT + Claude + Gemini | Low |
| **FINSABER** | ❌ Library only | Needs wrapper | ✅ Strongest | Via LangChain | Medium-High |
| **FinRL** | ❌ CLI only | Needs wrapper | Basic train/test | ❌ RL only | High — **not recommended** |

---

## Topic 2: Financial news semantic analysis

The current Finnhub news pipeline lacks sentiment labels and semantic relevance scoring. Three complementary layers solve this: an API with built-in sentiment, a local FinBERT microservice for unlimited free scoring, and GPT-4o-mini for deep event extraction.

### APIs with built-in sentiment scores

**Alpha Vantage NEWS_SENTIMENT** (https://www.alphavantage.co/documentation/#news-sentiment) provides AI-powered **per-ticker sentiment scores** ranging from −0.35 (Bearish) to +0.35 (Bullish), plus a relevance score per mentioned ticker. Supports ticker filtering (`tickers=AAPL,MSFT`). Free tier: **25 requests/day** — tight but workable by batching all 7 tickers in one comma-separated call. REST endpoint directly callable from httr2. Requires free API key.

**MarketAux** (https://www.marketaux.com/) offers **100 free requests/day** with per-entity sentiment scores (−1 to +1), relevance match scores, and highlight-level sentiment. Supports `symbols=AAPL,TSLA&filter_entities=true`. Best free tier for this use case. Free API token, no billing details needed.

**Polygon.io** provides ticker-filtered news but **no built-in sentiment scores** — requires adding your own NLP layer. Benzinga similarly lacks API-level sentiment (enterprise-oriented, opaque pricing). **AYLIEN** (now Quantexa) has the most powerful NLP features (event clustering, entity-level sentiment, 26 enrichment tags) but offers only a 14-day trial with enterprise-only pricing.

### Open-source models for local deployment

**ProsusAI/FinBERT** (https://huggingface.co/ProsusAI/finbert, 2,100 GitHub stars) is the go-to model: 110M parameters, classifies financial text as positive/negative/neutral with softmax probabilities. **Runs on CPU** (~4GB RAM, 100–500ms per article). Deploy as a FastAPI Docker sidecar:

```yaml
finbert-api:
  build: ./finbert-service
  ports: ["5000:5000"]
```

Call from R: `httr2::request("http://finbert-api:5000/sentiment") |> req_body_json(list(texts = headlines)) |> req_perform()`. Cost: **$0 forever**, unlimited throughput. For 7 stocks × 20 articles/day = 140 articles, total CPU processing is 15–70 seconds — fine for periodic dashboard refresh.

**SEC-BERT** (https://huggingface.co/nlpaueb/sec-bert-base) is pretrained on 260K SEC 10-K filings but is **not a sentiment classifier** — it's a domain-adapted language model requiring fine-tuning. A fine-tuned variant at `nickmuchi/sec-bert-finetuned-finance-classification` does 3-class sentiment but is less proven than FinBERT.

### Recommended hybrid architecture

The most cost-effective approach layers three systems: (1) **FinBERT microservice** for instant, free sentiment on all articles, (2) **GPT-4o-mini** (already in the stack) for event extraction and deep analysis on top-5 articles per ticker at ~**$0.45/month**, and (3) **MarketAux** as supplementary news source with built-in sentiment (100 free requests/day). This combination delivers unlimited sentiment scoring, structured event extraction (earnings, M&A, analyst actions), and cross-validated signals — all callable from R/Shiny via httr2 with no new paid subscriptions.

---

## Topic 3: Real-time and historical market data APIs

### IEX Cloud is dead — and Alpha Vantage's free tier has collapsed

**IEX Cloud shut down permanently on August 31, 2024.** All endpoints and accounts were deactivated. Separately, **Alpha Vantage reduced its free tier to just 25 requests/day** (down from a historical 500/day), making it effectively unusable for a live dashboard of 7 stocks. **Polygon.io rebranded to Massive.com** on October 30, 2025, though both domains and the `api.polygon.io` endpoint still work.

### Comprehensive API comparison

| API | Free Req/min | Free Req/day | History | Real-time (free?) | R Package | Notes |
|---|---|---|---|---|---|---|
| **Tiingo** | ~50 sym/hr | ~1,000 | 30+ yrs | ✅ IEX ~15min delay | ✅ `riingo` (CRAN) | **Best R integration** |
| **Twelve Data** | 8 | 800 | 30+ yrs | ✅ Real-time US | No (httr2) | **Only free real-time** |
| **Polygon/Massive** | 5 | ~7,200 | 2 yrs | ❌ EOD only free | No (httr2) | $29/mo for real-time |
| **Alpha Vantage** | 5 | **25** | 20+ yrs | ❌ Premium only | ✅ `alphavantager` | Severely limited |
| **FMP** | N/A | 250 | 5 yrs | ❌ EOD only free | No (httr2) | Best for fundamentals |
| **Yahoo Finance** | Unofficial | Unofficial | 20+ yrs | ✅ ~15min delay | ✅ `quantmod`, `tidyquant` | ToS risk, no API key |
| **Finnhub (current)** | **60** | Unlimited | Few yrs | ✅ 15min delay | No (httr2) | Current baseline |

### Tiingo is the strongest free-tier fit for R/Shiny

**Tiingo** (https://www.tiingo.com) stands out with the **`riingo` CRAN package** built by the Business Science team (same as `tidyquant`), returning tidy tibbles perfectly suited for R/Shiny + plotly. Free tier includes IEX intraday data (~15-minute delay), 30+ years of adjusted OHLCV history (AAPL data back to December 1980), and ~1,000 requests/day — more than sufficient for 7 stocks. One call: `riingo_prices(c("AAPL","MSFT","GOOGL","AMZN","META","NVDA","TSLA"))`. Requires free API token.

**Twelve Data** (https://twelvedata.com) uniquely offers **real-time US stock data on its free tier** — most competitors only provide EOD or premium real-time. Credit-based system: 8 credits/min, 800/day. No R package, but straightforward httr2 calls. Best complement to Tiingo when live price display matters.

**Yahoo Finance** via `quantmod`/`tidyquant` requires **no API key** and provides 20+ years of history with ~15-minute delayed quotes. Ideal as a fallback source, but the undocumented API has broken repeatedly and technically violates Yahoo's ToS for commercial use.

Recommended strategy: **Tiingo (primary) + Yahoo Finance/quantmod (fallback) + Twelve Data (real-time supplement)**. This provides deep history, near-real-time quotes, and redundancy — all on free tiers.

---

## Topic 4: Agent evaluation frameworks

Anthropic's "Demystifying Evals for AI Agents" (published January 9, 2026) defines the reference methodology: **code-based graders** (deterministic checks), **model-based graders** (LLM-as-judge), and **human graders** (production monitoring). Capability evals with high pass rates graduate into regression suites. Even frontier models score **under 60%** on realistic financial tasks (the Finance Agent Benchmark by Vals.AI + Stanford found GPT o3 achieved only 46.8% on 537 expert questions), underscoring the critical need for robust evaluation.

### vitals — the only R-native LLM evaluation package

| Detail | Value |
|---|---|
| **CRAN** | https://cran.r-project.org/package=vitals |
| **GitHub** | https://github.com/tidyverse/vitals |
| **Author** | Simon Couch (Posit/Tidyverse) |
| **License** | MIT |
| **Released** | April 2025, latest update Nov 2025 |

**vitals** is a port of Python's Inspect framework built by the Tidyverse team. It integrates with Posit's `ellmer` package for LLM calls and supports `model_graded_qa()` (LLM-as-judge), `model_graded_fact()`, `detect_exact()`, `detect_match()`, and `detect_pattern()` scorers. Results return as tibbles with `$get_samples()` for direct Shiny integration. Supports epochs (multiple trials per sample) and tracks API costs. **This is the primary evaluation tool for R/Shiny projects** — install with `install.packages("vitals")`.

### Other eval frameworks ranked by R compatibility

**Braintrust** (https://www.braintrust.dev) has a **full REST API** directly callable from R/httr2 — the best commercial option. Free tier includes **1M trace spans and 10K scores**, unlimited users. Built-in AutoEvals library provides LLM-as-judge factuality, relevance, and security scoring. Production monitoring with alerting on quality regression.

**LangSmith** (https://smith.langchain.com) similarly offers a documented REST API (`x-api-key` header auth), with 5,000 free traces/month. Online + offline evals, annotation queues, anomaly detection. Most powerful when paired with LangChain, less so for direct R integration.

**promptfoo** (~13,200 stars, https://github.com/promptfoo/promptfoo) was **acquired by OpenAI in March 2026**. MIT licensed, YAML-driven eval configs callable as CLI from R via `system2("promptfoo", ...)`. Supports `llm-rubric` assertions for LLM-as-judge. Future uncertain under OpenAI ownership.

**Langfuse** (~25,600 stars, https://langfuse.com) is the standout open-source observability platform — **self-hostable via Docker** alongside the existing stack, 50K free observations/month on cloud, unlimited when self-hosted. REST API, LLM-as-judge evaluators, production monitoring.

**DeepEval** (~14,300 stars, https://github.com/confident-ai/deepeval) and **Ragas** (https://github.com/explodinggradients/ragas) are Python-only with no REST APIs, requiring `reticulate` bridges. Ragas is specifically valuable for evaluating RAG pipeline faithfulness and context precision.

### Financial-specific eval metrics to implement

A layered approach maps directly to Anthropic's framework:

- **Layer 1 (Deterministic)**: Required fields present (summary, risk factors, valuation, recommendation), numerical data is valid and internally consistent, disclaimers included, data recency verified
- **Layer 2 (LLM-as-judge)**: Faithfulness to retrieved data, risk assessment balance, bull/bear perspective quality, speculative language detection, directional accuracy vs. analyst consensus
- **Layer 3 (Production)**: All metrics tracked in SQLite over time, regression detection via trend analysis in Shiny

Recommended stack: **vitals** (primary R-native eval) + **Braintrust** (production monitoring via REST API) + custom R deterministic checks.

---

## Topic 5: Lightweight vector database for semantic retrieval

The current `rag_retrieve()` switch-case reading CSVs needs to become: embed text → store vectors → similarity search with metadata filtering by ticker and date.

### ChromaDB with rchroma — simplest path from R/Shiny

| Detail | Value |
|---|---|
| **GitHub** | https://github.com/chroma-core/chroma |
| **Stars** | **~26,000** |
| **Latest version** | v1.5.6 (April 7, 2026) |
| **R package** | ✅ `rchroma` v0.2.0 on CRAN (https://cran.r-project.org/package=rchroma) |
| **Built with** | httr2 (exact tech stack match) |
| **License** | Apache 2.0 |

ChromaDB is the **only vector database with a dedicated CRAN-published R package built on httr2**. The `rchroma` package (by cynkra LLC) provides `chroma_connect()`, `create_collection()`, `add_documents()`, `query()`, and even `chroma_docker_run()` to start the ChromaDB container directly from R. Docker deployment is trivial: add `chromadb/chroma:latest` to docker-compose with a volume mount for persistence. The 2025 Rust-core rewrite significantly improved performance, and it now supports hybrid search with metadata filtering.

Migration path from CSV-based RAG: pre-compute embeddings for existing data using OpenAI API (~$0.10–$1.00 one-time), load into ChromaDB with ticker/date/agent metadata, replace `rag_retrieve()` with `rchroma::query()`. New articles get embedded on ingest.

### Qdrant — most powerful filtering for financial data

**Qdrant** (~28,100 stars, https://github.com/qdrant/qdrant) is a Rust-based vector database with a rich REST API on port 6333 — directly callable via httr2. Its standout feature for financial data is **advanced payload filtering**: `must`/`should`/`must_not` clauses support exact keyword matching, numerical ranges, and full-text search combined with vector similarity. This enables queries like "find news semantically similar to this query, filtered to AAPL articles from the last 7 days." Docker deployment is one line. Free cloud tier: 1 cluster with 0.5 vCPU, 1GB RAM, 4GB disk — no credit card. No R package, but httr2 calls are straightforward.

### Other viable options

**DuckDB + VSS extension** (https://github.com/duckdb/duckdb-vss) offers the most embedded R-native approach — the `duckdb` R package is excellent, and `INSTALL vss; LOAD vss;` adds HNSW vector indexing. A CRAN package **RAGFlowChainR** wraps a complete RAG pipeline using DuckDB as vector store. Caveat: **persistence is experimental** and may corrupt files.

**RcppAnnoy** (CRAN, by Dirk Eddelbuettel) wraps Spotify's Annoy library for pure R approximate nearest neighbor search. Sub-millisecond queries, file-backed indexes. Limitation: **static indexes** — cannot add/remove items without full rebuild. Best for nightly batch-updated news indexes paired with a separate SQLite database for metadata.

**sqlite-vec** (https://github.com/asg017/sqlite-vec) supersedes the deprecated sqlite-vss. Pure C, MIT/Apache licensed, theoretically loadable as an RSQLite extension. Companion `sqlite-rembed` generates embeddings from OpenAI/Ollama. Promising but requires C compilation and RSQLite may not support arbitrary extensions without recompilation.

### Embedding cost for this project

**OpenAI text-embedding-3-small**: $0.02/1M tokens (1536 dimensions). For 7 stocks × 20 articles/day × 500 tokens each = 70K tokens/day = **~$0.50/year**. Trivially cheap. Dimension truncation supported via Matryoshka for smaller vectors if storage is a concern.

Recommended architecture: **ChromaDB + rchroma** (simplest), with Qdrant as an upgrade path when filtering complexity grows.

---

## Topic 6: Real-time interactive AI trading tools

### Alpaca dominates the R/Shiny integration landscape

| Detail | Value |
|---|---|
| **Website** | https://alpaca.markets |
| **Paper trading** | ✅ Free globally, $100K virtual cash, real market data |
| **REST API** | Full orders, positions, account, portfolio history |
| **Commission** | Commission-free (stocks, options, crypto) |
| **R packages** | `AlpacaforR` (GitHub), `alpacar` (GitHub) |
| **MCP server** | ✅ Official (https://github.com/alpacahq/alpaca-mcp-server, 496 stars) |
| **Docker** | ✅ No GUI dependency |

Alpaca's paper trading mode mirrors the live API exactly — same code works for both. The paper endpoint at `https://paper-api.alpaca.markets` provides realistic simulation with actual market data, including Pattern Day Trader checks. Two R packages exist: **AlpacaforR** (comprehensive, covers all endpoints including WebSocket streaming) and **alpacar** (newer, lighter). Neither is on CRAN; both install from GitHub. Alternatively, the REST API is trivially callable via httr2: `POST /v2/orders` for trades, `GET /v2/positions` for portfolio, `GET /v2/account/portfolio/history` for equity curves.

The **official Alpaca MCP server** (MIT licensed) enables Claude, Cursor, and VS Code to trade stocks, ETFs, crypto, and options via natural language — paper mode by default. For R/Shiny, the MCP server serves as an optional AI-agent overlay while the REST API handles direct dashboard integration.

### Other platforms assessed

**Interactive Brokers** (https://www.interactivebrokers.com) has the `IBrokers` R package on CRAN (v0.10-2, maintained by Joshua Ulrich, updated July 2025). However, integration complexity is **high**: it requires TWS or IB Gateway running (Java process), uses socket-based communication (not REST), and R's single-threaded nature conflicts with real-time streaming. Not recommended for R/Shiny Docker deployment.

**Tradier** (https://tradier.com) provides a clean REST/JSON API with a sandbox at `sandbox.tradier.com/v1` and $100K paper money. Simple Bearer token auth, commission-free equities in some plans. No R package, but httr2 integration is straightforward. Good alternative to Alpaca.

**QuantConnect** (https://www.quantconnect.com) offers free unlimited backtesting via the open-source **LEAN engine** (~17,800 stars, Apache 2.0). Its AI assistant **Mia V2** (late 2025) can independently ideate strategies, write code, run backtests, and deploy — at ~$2–4 per session. A QuantConnect MCP server bridges LLMs to the API. For R/Shiny: use the REST API to trigger backtests and retrieve results; LEAN can run locally in Docker. Free tier includes all backtesting but **no live trading** ($60/month minimum for that).

**Schwab** (absorbed TD Ameritrade, whose API shut down May 2024) has a developer API but no sandbox, slow manual approval, and 7-day token expiry. **Robinhood** has no official stocks API — only crypto trading. Neither is recommended.

### MCP servers for finance — a growing ecosystem

Beyond Alpaca's official MCP server, several finance MCP servers have emerged:

- **Alpha Vantage MCP** (https://mcp.alphavantage.co/) — stock data, technicals, company info
- **MaverickMCP** (https://github.com/wshobson/maverick-mcp) — technical analysis + VectorBT backtesting + 20 indicators
- **Polymarket MCP servers** — multiple options enabling Claude to interact with prediction markets (`caiovicentino/polymarket-mcp-server` with 45 tools, `JamesANZ/prediction-market-mcp` covering Polymarket + PredictIt + Kalshi with no API keys)

---

## Conclusion: the integrated architecture

The optimal expansion path for Market Insight Studio centers on a Docker Compose stack adding three sidecar services to the existing R/Shiny app:

```yaml
services:
  shiny-app:           # Existing R/Shiny dashboard
  tradingagents-api:   # FastAPI wrapper → TradingAgents backtesting
  finbert-api:         # FinBERT sentiment microservice (CPU, ~4GB RAM)
  chromadb:            # Vector store for semantic RAG retrieval
```

**Five new API keys** complete the picture (all free): Tiingo (market data via `riingo`), Alpha Vantage (news sentiment), MarketAux (supplementary news), Alpaca (paper trading), and Braintrust (eval monitoring). The existing OpenAI key powers embeddings ($0.50/year), GPT-4o-mini event extraction ($0.45/month), and LLM-as-judge evaluation via `vitals`.

The most counterintuitive finding: **no R-native LLM trading tools exist**, yet the R ecosystem compensates with excellent bridge packages (`rchroma`, `riingo`, `vitals`, `quantmod`) that make Python-centric AI tools accessible through REST APIs and Docker sidecars. The total incremental cost for free tiers across all 6 domains is under **$10/month** — almost entirely GPT-4o-mini API calls — while providing multi-agent backtesting, unlimited sentiment scoring, 30+ years of market history, semantic RAG retrieval, layered eval, and paper trading execution.