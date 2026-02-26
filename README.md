# Trading Hint Shiny App

*Uses Alpha Vantage for daily price and dividend/company data (6 stocks), Finnhub for company news, and Ollama Cloud / OpenAI for LLM trading hints.*

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
   FINNHUB_API_KEY=your_finnhub_key
   OLLAMA_API_KEY=your_ollama_cloud_key
   ```

   Get an Alpha Vantage key at [alphavantage.co](https://www.alphavantage.co/support/#api-key).  
   Get a Finnhub key at [finnhub.io](https://finnhub.io/register) (free tier: 60 req/min, no daily limit).  
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
- **Finnhub** free tier: 60 requests/minute, **no daily limit**. Company news covers the past 7 days per stock.
  - Important: some API keys only have quote/news permissions and return 403/404 for candle history. If this happens, the app now falls back to Yahoo Finance for historical OHLC data so charts and metrics still render.
- **Cache**: Fetched data is saved under `data/` (daily_prices.csv, overview.csv, news.csv). On startup the app loads this cache so tables and LLM hints work without re-fetching. Refresh when you want up-to-date numbers.
- **Daily data**: Free keys get `outputsize=compact` (last 100 data points). For 2+ years of history you need a premium key with `outputsize=full`.
- **Dividend**: Summary comes from `OVERVIEW`. Full dividend history may require a premium endpoint.

---

## Files

| File | Purpose |
|------|--------|
| `app.R` | Shiny UI and server; Alpha Vantage, Finnhub, cache, AI |
| `install_packages.R` | One-time install of `shiny`, `httr2`, `DT`, `dplyr`, `plotly`, `jsonlite` |
| `.env` | API keys (do not commit) |
| `data/` | Cached daily prices, overview, and news (created on first refresh; gitignored) |

← [Back to 02_productivity](../README.md)
## 快速重启（Windows/Mac/Linux）

### 1) 一键重启 Shiny（杀旧进程 + 重启）

```bash
# 停掉旧进程并重启（默认 3838）
./restart_app.sh

# 指定端口重启（比如 8080）
./restart_app.sh 8080

# 重启并自动打开浏览器
./restart_app.sh 3838 --open

# 前台启动（可直接看到实时日志，适合排查“什么都没有返回”）
./restart_app.sh 3838 --foreground

# 前台+浏览器
./restart_app.sh 3838 --foreground --open

# 后台启动但实时查看终端日志
./restart_app.sh 3838 --tail
```

说明：
- 脚本会先杀掉当前监听该端口的进程（含老的 Shiny 进程）
- 重新启动 `app.R`
- 如果页面很久没变，刷新浏览器（建议使用 `Cmd/Ctrl + Shift + R`）
- 如果你执行后页面还是空白，先看日志：
  `tail -f logs/app_3838.log`
- 再确认服务是否起好：`curl -I http://localhost:3838`
