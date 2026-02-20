# app.R
# Trading Hint Shiny App
# Topic: Alpha Vantage daily price & dividend data for 6 stocks; Ollama LLM for hints.
# Uses Alpha Vantage (daily only), loads last ~100 trading days; optional 2-year filter if full.
# Ollama Cloud API for trading hints. Keys in .env (ALPHAVANTAGE_API_KEY, OLLAMA_API_KEY).
# Requires R >= 4.1 (for native pipe |>). Install deps: run install_packages.R once.

# 0. SETUP ###################################

if (getRversion() < "4.1") {
  stop("This app requires R >= 4.1 (for native pipe |>). You have ", getRversion())
}

## 0.1 Load Packages #################################

for (pkg in c("shiny", "httr2", "DT", "dplyr")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Missing package: ", pkg, ". Run: install.packages(c('shiny','httr2','DT','dplyr'))")
  }
}
library(shiny)
library(httr2)
library(DT)
library(dplyr)

## 0.2 Load .env ####################################

# Load .env from app directory or project root so API keys are available
env_path = file.path(".", ".env")
if (!file.exists(env_path)) env_path = file.path("..", "..", ".env")
if (file.exists(env_path)) {
  tryCatch(readRenviron(env_path), error = function(e) warning(".env load failed: ", conditionMessage(e)))
} else {
  warning(".env not found. Set ALPHAVANTAGE_API_KEY and optionally OLLAMA_API_KEY.")
}

# Stock tickers: Apple, Tesla, Meta, NVIDIA, Google, Coca-Cola
TICKERS = c("AAPL", "TSLA", "META", "NVDA", "GOOGL", "KO")
TICKER_NAMES = c("Apple" = "AAPL", "Tesla" = "TSLA", "Meta" = "META", "NVIDIA" = "NVDA", "Google" = "GOOGL", "Coca-Cola" = "KO")
`%||%` = function(x, y) if (is.null(x) || length(x) == 0) y else x

# -----------------------------------------------------------------------------
# Helper: fetch Alpha Vantage TIME_SERIES_DAILY for one symbol (daily only; compact = 100 pts)
# -----------------------------------------------------------------------------
fetch_av_daily = function(symbol, api_key) {
  url = "https://www.alphavantage.co/query"
  resp = request(url) |>
    req_url_query(
      `function` = "TIME_SERIES_DAILY",
      symbol = symbol,
      outputsize = "compact",
      apikey = api_key,
      datatype = "json"
    ) |>
    req_retry(max_tries = 2) |>
    req_perform()
  data = resp_body_json(resp)
  ts = data[["Time Series (Daily)"]]
  if (is.null(ts)) return(NULL)
  # Build data.frame: Date, Open, High, Low, Close, Volume
  dates = names(ts)
  out = data.frame(
    Symbol = symbol,
    Date = dates,
    Open = as.numeric(vapply(ts, function(x) x[["1. open"]], character(1))),
    High = as.numeric(vapply(ts, function(x) x[["2. high"]], character(1))),
    Low = as.numeric(vapply(ts, function(x) x[["3. low"]], character(1))),
    Close = as.numeric(vapply(ts, function(x) x[["4. close"]], character(1))),
    Volume = as.numeric(vapply(ts, function(x) x[["5. volume"]], character(1))),
    stringsAsFactors = FALSE
  )
  out = out[order(out$Date, decreasing = TRUE), ]
  # Keep last 2 years if we have more than 100 rows (e.g. full output)
  out$Date = as.Date(out$Date)
  cutoff = Sys.Date() - (2 * 365)
  out = out[out$Date >= cutoff, ]
  if (nrow(out) == 0) out = out[order(out$Date, decreasing = TRUE), ]
  out
}

# -----------------------------------------------------------------------------
# Helper: fetch Company OVERVIEW for one symbol (dividend yield, ex-date, etc.)
# -----------------------------------------------------------------------------
fetch_av_overview = function(symbol, api_key) {
  url = "https://www.alphavantage.co/query"
  resp = tryCatch({
    request(url) |>
      req_url_query(`function` = "OVERVIEW", symbol = symbol, apikey = api_key) |>
      req_retry(max_tries = 2) |>
      req_perform()
  }, error = function(e) NULL)
  if (is.null(resp)) return(NULL)
  data = resp_body_json(resp)
  if (is.null(data) || length(data) == 0 || !is.null(data[["Note"]])) return(NULL)
  data.frame(
    Symbol = data[["Symbol"]] %||% symbol,
    Name = data[["Name"]] %||% "",
    DividendYield = as.character(data[["DividendYield"]] %||% ""),
    DividendPerShare = as.character(data[["DividendPerShare"]] %||% ""),
    ExDividendDate = as.character(data[["ExDividendDate"]] %||% ""),
    DividendDate = as.character(data[["DividendDate"]] %||% ""),
    PERatio = as.character(data[["PERatio"]] %||% ""),
    MarketCap = as.character(data[["MarketCapitalization"]] %||% ""),
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# Helper: fetch all 6 stocks daily (throttle to avoid 5/min limit)
# -----------------------------------------------------------------------------
fetch_all_daily = function(api_key) {
  all_df = list()
  for (i in seq_along(TICKERS)) {
    Sys.sleep(13)
    df = fetch_av_daily(TICKERS[i], api_key)
    if (!is.null(df)) all_df[[length(all_df) + 1]] = df
  }
  if (length(all_df) == 0) return(NULL)
  dplyr::bind_rows(all_df)
}

# -----------------------------------------------------------------------------
# Helper: fetch OVERVIEW (dividend summary) for all 6 symbols (throttled)
# -----------------------------------------------------------------------------
fetch_all_overview = function(api_key) {
  all_df = list()
  for (i in seq_along(TICKERS)) {
    Sys.sleep(13)
    df = fetch_av_overview(TICKERS[i], api_key)
    if (!is.null(df)) all_df[[length(all_df) + 1]] = df
  }
  if (length(all_df) == 0) return(NULL)
  dplyr::bind_rows(all_df)
}

# -----------------------------------------------------------------------------
# Cache: persist daily + overview to disk so app can load without re-fetching
# -----------------------------------------------------------------------------
CACHE_DIR = "data"
CACHE_MAX_AGE_HOURS = 24L   # treat cache as stale after 24h (still load it, but show "consider refresh")

cache_path = function(name) file.path(CACHE_DIR, name)

load_cached_data = function() {
  daily_path = cache_path("daily_prices.csv")
  overview_path = cache_path("overview.csv")
  meta_path = cache_path("cache_updated.txt")
  if (!file.exists(daily_path) || !file.exists(overview_path)) return(NULL)
  daily_df = tryCatch(
    { d = read.csv(daily_path, stringsAsFactors = FALSE); d$Date = as.Date(d$Date); d },
    error = function(e) NULL
  )
  overview_df = tryCatch(read.csv(overview_path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(daily_df) || is.null(overview_df) || nrow(daily_df) == 0) return(NULL)
  updated_at = if (file.exists(meta_path)) trimws(readLines(meta_path, 1L)) else ""
  list(daily = daily_df, overview = overview_df, updated_at = updated_at)
}

save_cached_data = function(daily_df, overview_df) {
  if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)
  write.csv(daily_df, cache_path("daily_prices.csv"), row.names = FALSE)
  write.csv(overview_df, cache_path("overview.csv"), row.names = FALSE)
  writeLines(format(Sys.time(), "%Y-%m-%d %H:%M"), cache_path("cache_updated.txt"))
}

# Build a short summary table for LLM (latest close, 5d change %, dividend) per symbol
build_summary_for_llm = function(daily_df, overview_df) {
  if (is.null(daily_df) || nrow(daily_df) == 0) return("(No price data)")
  latest = daily_df |> group_by(Symbol) |> slice(1) |> ungroup()
  # 5-day ago close if available (row 6 when sorted by Date desc)
  five_d = daily_df |> group_by(Symbol) |> filter(n() >= 6) |> slice(6) |> ungroup() |> select(Symbol, Close5d = Close)
  latest = left_join(latest, five_d, by = "Symbol")
  latest = latest |> mutate(
    Change5dPct = ifelse(!is.na(Close5d) & Close5d > 0, round((Close - Close5d) / Close5d * 100, 2), NA_real_)
  )
  if (!is.null(overview_df) && nrow(overview_df) > 0) {
    latest = left_join(latest, overview_df |> select(Symbol, DividendYield, PERatio), by = "Symbol")
  } else {
    latest$DividendYield = NA
    latest$PERatio = NA
  }
  paste0(
    "Latest data per symbol:\n",
    paste(sprintf("%s: Close=%.2f, 5d%%=%.2f, DivYield=%s, P/E=%s",
                  latest$Symbol, latest$Close, latest$Change5dPct,
                  ifelse(is.na(latest$DividendYield), "N/A", latest$DividendYield),
                  ifelse(is.na(latest$PERatio), "N/A", latest$PERatio)),
          collapse = "\n")
  )
}

# -----------------------------------------------------------------------------
# Helper: Ollama chat (local or Cloud). If api_key is empty or "local", use localhost:11434.
# -----------------------------------------------------------------------------
ollama_chat = function(user_message, api_key) {
  use_local = !nzchar(api_key) || trimws(tolower(api_key)) == "local"
  if (use_local) {
    url = "http://localhost:11434/api/chat"
    model = trimws(Sys.getenv("OLLAMA_MODEL", "gemma3:latest"))
    body = list(
      model = model,
      messages = list(list(role = "user", content = user_message)),
      stream = FALSE
    )
    resp = tryCatch({
      request(url) |>
        req_body_json(body) |>
        req_perform()
    }, error = function(e) list(ok = FALSE, error = paste0("Local Ollama: ", conditionMessage(e), ". Is ollama serve running on port 11434?")))
  } else {
    url = "https://ollama.com/api/chat"
    body = list(
      model = "gpt-oss:20b-cloud",
      messages = list(list(role = "user", content = user_message)),
      stream = FALSE
    )
    resp = tryCatch({
      request(url) |>
        req_headers(
          "Authorization" = paste0("Bearer ", api_key),
          "Content-Type" = "application/json"
        ) |>
        req_body_json(body) |>
        req_perform()
    }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
  }
  if (!inherits(resp, "httr2_response")) return(resp)
  if (resp_status(resp) != 200) return(list(ok = FALSE, error = resp_body_string(resp)))
  data = resp_body_json(resp)
  text = data[["message"]][["content"]]
  list(ok = TRUE, text = if (is.null(text)) "" else text)
}

# -----------------------------------------------------------------------------
# Helper: OpenAI chat (gpt-4o-mini). Returns same shape as ollama_chat.
# -----------------------------------------------------------------------------
openai_chat = function(user_message, api_key) {
  if (!nzchar(api_key)) return(list(ok = FALSE, error = "OPENAI_API_KEY not set."))
  url = "https://api.openai.com/v1/chat/completions"
  body = list(
    model = "gpt-4o-mini",
    messages = list(list(role = "user", content = user_message))
  )
  resp = tryCatch({
    request(url) |>
      req_headers(
        "Authorization" = paste0("Bearer ", api_key),
        "Content-Type" = "application/json"
      ) |>
      req_body_json(body) |>
      req_perform()
  }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
  if (!inherits(resp, "httr2_response")) return(resp)
  if (resp_status(resp) != 200) return(list(ok = FALSE, error = resp_body_string(resp)))
  data = resp_body_json(resp)
  text = data[["choices"]][[1]][["message"]][["content"]]
  list(ok = TRUE, text = if (is.null(text)) "" else text)
}

# -----------------------------------------------------------------------------
# UI (dark theme)
# -----------------------------------------------------------------------------
ui = fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #0d1117 !important; }
      .well, .shiny-input-container { background-color: #161b22; border-color: #30363d; color: #e6edf3; }
      .form-control { background-color: #21262d; color: #e6edf3; border-color: #30363d; }
      .nav-tabs { border-color: #30363d; }
      .nav-tabs > li > a { background: #161b22; color: #8b949e; border-color: #30363d; }
      .nav-tabs > li.active > a { background: #21262d; color: #e6edf3; }
      .nav-tabs > li > a:hover { color: #e6edf3; }
      .dataTables_wrapper { color: #e6edf3; }
      table.dataTable thead th { background: #161b22 !important; color: #e6edf3 !important; border-color: #30363d !important; }
      table.dataTable tbody td { color: #e6edf3 !important; border-color: #30363d !important; }
      .btn-primary { background-color: #238636; border-color: #238636; }
      .btn-primary:hover { background-color: #2ea043; }
      h2, h3 { color: #e6edf3; }
      .alert-danger { background-color: #3d1f1f; border-color: #6e2a2a; color: #f85149; }
      .alert-info { background-color: #1c2d41; border-color: #388bfd; color: #58a6ff; }
    "))
  ),
  titlePanel(
    h2("Trading Hint App", style = "color: #e6edf3; font-weight: 600;"),
    windowTitle = "Trading Hint"
  ),
  tabsetPanel(
    tabPanel(
      "Price & dividend data",
      fluidRow(
        column(8, actionButton("fetch_data", "Refresh data (fetch from API, ~2–3 min)", class = "btn-primary")),
        column(4, uiOutput("last_updated"))
      ),
      fluidRow(column(12, uiOutput("data_error"))),
      fluidRow(column(12, helpText("Data is cached locally. On startup the app loads the last saved data so you don’t need to click every time.", style = "color: #8b949e;"))),
      fluidRow(column(12, h4("Daily price (last ~100 trading days)", style = "color: #e6edf3;"))),
      fluidRow(column(12, DTOutput("price_table"))),
      fluidRow(column(12, h4("Dividend & company summary", style = "color: #e6edf3; margin-top: 20px;"))),
      fluidRow(column(12, DTOutput("overview_table"))),
      helpText("Stocks: Apple, Tesla, Meta, NVIDIA, Google, Coca-Cola. Data: Alpha Vantage (daily).", style = "color: #8b949e;")
    ),
    tabPanel(
      "Trading hint (Ollama)",
      fluidRow(
        column(12, actionButton("ask_suggestion", "获取买卖建议 / Get buy-sell suggestion", class = "btn-primary"))
      ),
      fluidRow(
        column(8, textAreaInput("user_prompt", "Or ask your own question (e.g. risk summary)", value = "", rows = 3, width = "100%")),
        column(4, actionButton("ask_ollama", "Send custom prompt", class = "btn-primary"))
      ),
      fluidRow(column(12, uiOutput("ollama_error"))),
      fluidRow(column(12, uiOutput("ollama_reply")))
    ),
    tabPanel(
      "Report (.txt)",
      fluidRow(
        column(12, helpText("Generate a short report from current stock data (Summary, Key findings, Recommendations). Uses OpenAI gpt-4o-mini if OPENAI_API_KEY is set, otherwise Ollama.", style = "color: #8b949e;"))
      ),
      fluidRow(
        column(6, actionButton("gen_report", "Generate report", class = "btn-primary")),
        column(6, uiOutput("download_report_ui"))
      ),
      fluidRow(column(12, uiOutput("report_error"))),
      fluidRow(column(12, uiOutput("report_preview")))
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server = function(input, output, session) {
  stock_data = reactiveVal(NULL)
  last_updated = reactiveVal("")

  # On startup: load from cache so data appears without clicking
  cached = load_cached_data()
  if (!is.null(cached)) {
    stock_data(list(daily = cached$daily, overview = cached$overview))
    last_updated(cached$updated_at)
  }

  output$last_updated = renderUI({
    t = last_updated()
    if (!nzchar(t)) return(span("No data yet. Click “Refresh data” to fetch.", style = "color: #8b949e;"))
    span("Last updated: ", t, style = "color: #8b949e;")
  })

  observeEvent(input$fetch_data, {
    output$data_error = renderUI(NULL)
    api_key = Sys.getenv("ALPHAVANTAGE_API_KEY")
    if (!nzchar(api_key)) {
      output$data_error = renderUI(div(class = "alert alert-danger", "ALPHAVANTAGE_API_KEY not set in .env."))
      return()
    }
    withProgress(message = "Fetching 6 stocks (rate limit 5/min)...", value = 0, {
      daily_df = fetch_all_daily(api_key)
      if (is.null(daily_df)) {
        output$data_error = renderUI(div(class = "alert alert-danger", "No daily data returned. Check API key and rate limit."))
        stock_data(NULL)
        setProgress(1)
        return()
      }
      setProgress(0.5, detail = "Fetching dividend & company overview...")
      overview_df = fetch_all_overview(api_key)
      stock_data(list(daily = daily_df, overview = overview_df))
      save_cached_data(daily_df, overview_df)
      last_updated(format(Sys.time(), "%Y-%m-%d %H:%M"))
      output$data_error = renderUI(NULL)
      setProgress(1)
    })
  })

  output$price_table = renderDT({
    d = stock_data()
    if (is.null(d) || is.null(d$daily)) return(NULL)
    d$daily
  }, options = list(pageLength = 20, dom = "tip", ordering = TRUE), rownames = FALSE)

  output$overview_table = renderDT({
    d = stock_data()
    if (is.null(d) || is.null(d$overview)) return(NULL)
    d$overview
  }, options = list(pageLength = 10, dom = "tip"), rownames = FALSE)

  observeEvent(input$ask_suggestion, {
    output$ollama_error = renderUI(NULL)
    output$ollama_reply = renderUI(NULL)
    api_key = Sys.getenv("OLLAMA_API_KEY")
    d = stock_data()
    summary_txt = build_summary_for_llm(d$daily, d$overview)
    prompt = paste0(
      "You are a concise trading assistant. Based ONLY on the following data, give a short suggestion for each stock: BUY / SELL / HOLD. One line per symbol with brief reason. Reply in English ONLY.\n\n",
      summary_txt,
      "\n\nFormat: SYMBOL: BUY/SELL/HOLD — reason."
    )
    withProgress(message = "Getting buy/sell suggestion...", value = 0.5, {
      result = ollama_chat(prompt, api_key)
      setProgress(1)
    })
    if (!result$ok) {
      output$ollama_error = renderUI(div(class = "alert alert-danger", result$error))
    } else {
      output$ollama_reply = renderUI(div(class = "alert alert-info", style = "white-space: pre-wrap;", result$text))
    }
  })

  observeEvent(input$ask_ollama, {
    output$ollama_error = renderUI(NULL)
    output$ollama_reply = renderUI(NULL)
    api_key = Sys.getenv("OLLAMA_API_KEY")
    d = stock_data()
    context = build_summary_for_llm(d$daily, d$overview)
    user_text = paste0(context, "\n\nUser question: ", input$user_prompt)
    withProgress(message = "Calling Ollama...", value = 0.5, {
      result = ollama_chat(user_text, api_key)
      setProgress(1)
    })
    if (!result$ok) {
      output$ollama_error = renderUI(div(class = "alert alert-danger", result$error))
    } else {
      output$ollama_reply = renderUI(div(class = "alert alert-info", style = "white-space: pre-wrap;", result$text))
    }
  })

  # Report (.txt): generate with OpenAI gpt-4o-mini if key set, else Ollama; download as .txt
  report_text = reactiveVal(NULL)

  observeEvent(input$gen_report, {
    output$report_error = renderUI(NULL)
    report_text(NULL)
    d = stock_data()
    summary_txt = build_summary_for_llm(d$daily, d$overview)
    prompt = paste0(
      "Based ONLY on the following stock data, write a short plain-text report (no markdown). Use this structure:\n\n",
      "Summary\n(2-3 sentences overall.)\n\n",
      "Key Findings\n(Bullet points, one per symbol or theme.)\n\n",
      "Recommendations\n(1-2 sentences.)\n\n",
      "Data:\n", summary_txt
    )
    openai_key = Sys.getenv("OPENAI_API_KEY")
    ollama_key = Sys.getenv("OLLAMA_API_KEY")
    withProgress(message = "Generating report...", value = 0.5, {
      if (nzchar(openai_key)) {
        result = openai_chat(prompt, openai_key)
      } else {
        result = ollama_chat(prompt, ollama_key)
      }
      setProgress(1)
    })
    if (!result$ok) {
      output$report_error = renderUI(div(class = "alert alert-danger", result$error))
    } else {
      report_text(result$text)
      output$report_error = renderUI(NULL)
    }
  })

  output$download_report_ui = renderUI({
    if (is.null(report_text()) || !nzchar(report_text())) return(NULL)
    downloadButton("download_report", "Download report.txt", class = "btn-primary")
  })

  output$download_report = downloadHandler(
    filename = function() paste0("report_", format(Sys.time(), "%Y%m%d_%H%M"), ".txt"),
    content = function(file) writeLines(report_text(), file)
  )

  output$report_preview = renderUI({
    if (is.null(report_text()) || !nzchar(report_text())) return(NULL)
    div(
      class = "alert alert-info",
      style = "white-space: pre-wrap; max-height: 300px; overflow-y: auto;",
      report_text()
    )
  })
}

shinyApp(ui = ui, server = server)
