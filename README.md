# Trading Hint Shiny App

*Uses Alpha Vantage for daily price and dividend/company data (6 stocks) and Ollama Cloud for LLM trading hints.*

---

## Overview

This app focuses on six tickers: **Apple (AAPL), Tesla (TSLA), Meta (META), NVIDIA (NVDA), Google (GOOGL), Coca-Cola (KO)**.

- **Data**: Daily price (~100 trading days) and dividend/company summary from Alpha Vantage. Data is **cached locally** in `data/` so the app loads the last saved dataset on startup—no need to click “fetch” every time. Use **Refresh data** to pull the latest from the API (~2–3 min, rate limit 5/min).
- **Trading hint**: **Get buy/sell suggestion** asks the LLM for 买入/卖出/观望 per symbol with a short reason. You can also send a custom prompt with current data as context.

---

## Setup

1. **Install R packages** (run once):

   ```bash
   Rscript install_packages.R
   ```

2. **Configure `.env`** in this directory (or project root):

   ```
   ALPHAVANTAGE_API_KEY=your_alpha_vantage_key
   OLLAMA_API_KEY=your_ollama_cloud_key
   ```

   Get an Alpha Vantage key at [alphavantage.co](https://www.alphavantage.co/support/#api-key).  
   **LLM (Ollama):** Use **local Ollama** (no key): leave `OLLAMA_API_KEY` empty or set to `local`; start Ollama (`ollama serve`), pull a model (`ollama pull gemma3:latest`). Or set `OLLAMA_API_KEY` to your Ollama Cloud key for cloud inference.

3. **Run the app**:

   ```bash
   R -e "shiny::runApp('.', port = 3838)"
   ```

   Or from R: `shiny::runApp("02_productivity/shiny_app")`.

---

## Using local Ollama (no API key)

1. **Start the Ollama server** (in a terminal):  
   `ollama serve`  
   Or run `01_ollama.sh` from the repo root to start it in the background.
2. **Pull a model**:  
   `ollama pull gemma3:latest`  
   (Or use the model name in `03_query_ai/02_ollama.R`; the app uses `OLLAMA_MODEL` from `.env`, default `gemma3:latest`.)
3. **Test**: from repo root,  
   `Rscript 03_query_ai/02_ollama.R`  
   You should see a short reply from the model. If it fails, check that Ollama is running on **port 11434**.
4. In the app’s `.env`, leave `OLLAMA_API_KEY=` empty (or set to `local`). The Trading hint tab will then use local Ollama.

---

## API notes

- **Alpha Vantage** free tier: 5 requests/minute, 25/day. The app throttles requests (~13 s between calls). A full refresh takes about 2–3 minutes.
- **Cache**: Fetched data is saved under `data/` (daily_prices.csv, overview.csv). On startup the app loads this cache so tables and LLM hints work without re-fetching. Refresh when you want up-to-date numbers.
- **Daily data**: Free keys get `outputsize=compact` (last 100 data points). For 2+ years of history you need a premium key with `outputsize=full`.
- **Dividend**: Summary comes from `OVERVIEW`. Full dividend history may require a premium endpoint.

---

## Files

| File | Purpose |
|------|--------|
| `app.R` | Shiny UI and server; Alpha Vantage, cache, Ollama Cloud |
| `install_packages.R` | One-time install of `shiny`, `httr2`, `DT`, `dplyr` |
| `.env` | API keys (do not commit) |
| `data/` | Cached daily prices and overview (created on first refresh; gitignored) |

---

---

## 交作业 / Submission (学校提交)

- **不要提交**：`.env` 文件（含真实 API key）、`data/` 缓存。它们已在 `.gitignore` 中，提交代码时不会包含。
- **提交代码**：把整个仓库（或 `02_productivity/shiny_app/` 等作业要求的部分）通过 Git 提交，并在 Canvas 的 .docx 里附上**仓库链接**和**具体文件链接**（如 `app.R`、README）。
- **批改者运行方式**：复制 `.env.example` 为 `.env`，在 `.env` 里填入自己的 `ALPHAVANTAGE_API_KEY` 和（可选）`OLLAMA_API_KEY`，然后按 README 的 Setup 安装依赖并运行即可。
- **.docx 内容**：按 HOMEWORK 要求写清：软件说明、Git 链接、截图、使用说明（含“复制 .env.example 为 .env 并填 key”）。

---

← [Back to 02_productivity](../README.md)
