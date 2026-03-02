# server.R
# Shiny server logic

server <- function(input, output, session) {

  # ----------------------------
  # reactive values
  # ----------------------------

  stock_data <- reactiveVal(NULL)
  news_data <- reactiveVal(NULL)
  ai_payload <- reactiveVal(NULL)
  last_updated <- reactiveVal("")
  rag_history <- reactiveVal(NULL)
  report_result <- reactiveVal(NULL)

  cached <- load_cached_data()
  if (!is.null(cached)) {
    stock_data(list(daily = cached$daily, news = cached$news))
    news_data(cached$news)
    last_updated(cached$updated_at)
  }
  rag_file <- cache_path("rag_history.csv")
  if (file.exists(rag_file)) {
    existing_rag <- tryCatch(read.csv(rag_file, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(existing_rag) && nrow(existing_rag) > 0) rag_history(existing_rag)
  }

  live_quotes <- reactiveVal(list())
  quote_time <- reactiveVal("")
  data_view <- reactiveVal("prices")

  # ----------------------------
  # live quote polling
  # ----------------------------

  do_fetch_quotes <- function() {
    key <- Sys.getenv("FINNHUB_API_KEY")
    if (!nzchar(key)) return()
    qs <- fetch_all_quotes(key)
    if (length(qs) > 0) {
      live_quotes(qs)
      quote_time(format(Sys.time(), "%H:%M:%S"))
    }
  }

  observe({
    invalidateLater(15000)
    do_fetch_quotes()
  })

  # ----------------------------
  # helpers
  # ----------------------------

  gather_all_news <- function(news_days = 7L) {
    key <- Sys.getenv("FINNHUB_API_KEY")
    if (!nzchar(key)) return(NULL)
    out <- list()
    for (sym in TICKERS) {
      news <- fetch_finnhub_news(sym, key, days_back = as.integer(news_days))
      if (is.null(news)) next
      out <- c(out, news)
      Sys.sleep(0.15)
    }
    if (length(out) == 0) return(NULL)
    df <- dplyr::bind_rows(out)
    df <- df[!duplicated(df$Title), ]
    df$Published <- as.POSIXct(df$Published, origin = "1970-01-01", tz = "UTC")
    df <- df[order(df$Published, decreasing = TRUE), ]
    head(df, 400)
  }

  fetch_all_histories <- function() {
    key <- Sys.getenv("FINNHUB_API_KEY")
    if (!nzchar(key)) {
      showNotification("FINNHUB_API_KEY missing.", type = "error")
      return(NULL)
    }
    all_rows <- list()
    all_tickers <- c(TICKERS, MARKET_TICKER)
    withProgress(message = "Fetching stock history", value = 0, {
      for (i in seq_along(all_tickers)) {
        sym <- all_tickers[[i]]
        setProgress((i - 1) / length(all_tickers), detail = paste("Fetching", sym))
        df <- fetch_finnhub_candles(sym, key, days_back = 365)
        if (!is.null(df)) all_rows[[length(all_rows) + 1]] <- df
        if (i < length(all_tickers)) Sys.sleep(0.2)
      }
      setProgress(0.95, detail = "Finalizing")
    })
    if (length(all_rows) == 0) return(NULL)
    dplyr::bind_rows(all_rows)
  }

  # ----------------------------
  # Data refresh
  # ----------------------------

  observeEvent(input$refresh_data, {
    req(input$refresh_data)
    shiny::withProgress(message = "Refreshing market data", value = 0, {
      hist_df <- fetch_all_histories()
      if (is.null(hist_df)) {
        showNotification("No stock data fetched. Check FINNHUB key / rate limit.", type = "error")
        return(NULL)
      }
      setProgress(0.6, detail = "Fetching news")
      news_df <- gather_all_news(90)
      stock_data(list(daily = hist_df, news = news_df))
      news_data(news_df)
      save_cached_data(hist_df, news_df)
      setProgress(0.75, detail = "Computing RAG parameters...")
      rag_result <- update_rag_history(hist_df)
      rag_history(rag_result)
      setProgress(0.85, detail = "Fetching key financials...")
      fin_df <- tryCatch(fetch_all_key_financials(), error = function(e) NULL)
      if (!is.null(fin_df)) export_key_financials_csv(fin_df)
      setProgress(0.95, detail = "Exporting CSVs...")
      export_csv_files(hist_df, rag_result)
      export_news_csv(news_df)
      last_updated(safe_now())
      setProgress(1)
      showNotification("Data refreshed.", type = "message")
    })
  })

  # ----------------------------
  # News refresh
  # ----------------------------

  observeEvent(input$refresh_news, {
    if (!nzchar(Sys.getenv("FINNHUB_API_KEY"))) {
      output$news_error <- renderUI(div(class = "alert alert-danger", "FINNHUB_API_KEY missing."))
      return()
    }
    days <- as.integer(input$news_window)
    news_df <- gather_all_news(days)
    if (is.null(news_df) || nrow(news_df) == 0) {
      output$news_error <- renderUI(div(class = "alert alert-warning", "No news returned. Try again later."))
      return()
    }
    news_data(news_df)
    d <- stock_data()
    if (!is.null(d) && is.data.frame(d$daily)) { d$news <- news_df; stock_data(d) }
    if (!is.null(d) && !is.null(d$daily)) save_cached_data(d$daily, news_df)
    export_news_csv(news_df)
    output$news_error <- renderUI(NULL)
  }, ignoreInit = TRUE)

  observe({
    if (is.null(news_data())) {
      output$news_error <- renderUI(div(class = "alert alert-warning", "News not loaded. Click Refresh News."))
    } else {
      output$news_error <- renderUI(NULL)
    }
  })

  # ----------------------------
  # Data page navigation
  # ----------------------------

  observeEvent(input$goto_analytics, {
    d <- stock_data()
    if (!is.null(d) && !is.null(d$daily) && is.null(rag_history())) {
      withProgress(message = "Computing model parameters...", value = 0.5, {
        rag_result <- update_rag_history(d$daily)
        rag_history(rag_result)
        export_csv_files(d$daily, rag_result)
      })
    }
    data_view("analytics")
  })

  observeEvent(input$back_to_prices, { data_view("prices") })

  # ----------------------------
  # Live quotes UI
  # ----------------------------

  output$live_quotes_ui <- renderUI({
    qs <- live_quotes()
    if (length(qs) == 0) return(tags$div(style = "color:#475569; text-align:center; padding:24px; font-size:14px;",
      icon("circle-notch", class = "fa-spin", style = "margin-right:6px;"), "Loading live quotes..."))
    cards <- lapply(names(qs), function(sym) {
      q <- qs[[sym]]
      is_up <- q$change >= 0
      chg_col <- if (is_up) "#34d399" else "#f87171"
      chg_sign <- if (is_up) "+" else ""
      arrow_icon <- if (is_up) "caret-up" else "caret-down"
      border_accent <- if (is_up) "rgba(34,197,94,0.2)" else "rgba(248,113,113,0.2)"
      tags$div(
        style = sprintf("flex:1 1 140px; border:1px solid %s; background:linear-gradient(160deg,rgba(30,41,59,.6),rgba(15,23,42,.8)); border-radius:14px; padding:14px 16px; text-align:center; transition:transform .2s, box-shadow .2s; cursor:default;", border_accent),
        class = "metric-card",
        tags$div(style = "font-weight:700; color:#94a3b8; font-size:11px; text-transform:uppercase; letter-spacing:.5px; margin-bottom:4px;",
          paste0(sym)),
        tags$div(style = "color:#cbd5e1; font-size:12px; margin-bottom:6px;", TICKER_LABELS[[sym]]),
        tags$div(style = "font-size:24px; font-weight:800; color:#f8fafc; margin:4px 0; font-variant-numeric:tabular-nums;", fmt_price(q$price)),
        tags$div(style = paste0("color:", chg_col, "; font-size:13px; font-weight:600;"),
          icon(arrow_icon, style = sprintf("margin-right:2px; color:%s;", chg_col)),
          sprintf("%s%s (%s%s%%)", chg_sign, fmt_num(q$change), chg_sign, fmt_num(q$change_pct)))
      )
    })
    tags$div(style = "display:flex; flex-wrap:wrap; gap:12px; margin-bottom:14px;", cards)
  })

  output$quote_timestamp <- renderUI({
    qt <- quote_time()
    if (!nzchar(qt)) return(NULL)
    tags$div(style = "color:#475569; font-size:11px; text-align:right; display:flex; align-items:center; justify-content:flex-end; gap:6px;",
      tags$span(style = "display:inline-block; width:6px; height:6px; border-radius:50%; background:#22c55e; animation:pulse-dot 2s infinite;"),
      tags$span(paste("Live |", qt, "| Auto-refresh 15s")),
      tags$style(HTML("@keyframes pulse-dot { 0%,100% { opacity:1; } 50% { opacity:0.3; } }"))
    )
  })

  output$last_updated <- renderText({
    lu <- last_updated()
    if (nzchar(lu)) lu else "No cached data. Click Refresh Market Data."
  })

  # ----------------------------
  # Data page content
  # ----------------------------

  output$data_page_content <- renderUI({
    if (data_view() == "analytics") {
      tagList(
        tags$div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:8px; flex-wrap:wrap; gap:10px;",
          actionButton("back_to_prices", tags$span(icon("arrow-left"), " Live Prices"), class = "btn-primary"),
          downloadButton("download_rag_csv", tags$span(icon("download"), " Download CSV"), class = "btn-success")
        ),
        tags$div(class = "panel-card",
          h3("Model Parameters", class = "section-title"), DTOutput("rag_table")),
        tags$div(class = "panel-card",
          h3("Historical Prices", class = "section-title"), DTOutput("analytics_price_table"))
      )
    } else {
      tagList(
        fluidRow(column(12,
          h3("Live Stock Price", class = "section-title"),
          uiOutput("live_quotes_ui"), uiOutput("quote_timestamp")
        )),
        tags$div(
          class = "panel-card", style = "margin-bottom:8px; padding:16px 20px;",
          fluidRow(
            column(3, selectInput("data_ticker", "Select Stock",
                                  choices = setNames(TICKERS, paste0(TICKERS, " - ", TICKER_LABELS[TICKERS])),
                                  selected = "AAPL", width = "100%")),
            column(3, radioButtons("data_window", "Time Range", choices = c("7D", "1M", "1Y"), selected = "1M", inline = TRUE)),
            column(3, tags$div(style = "padding-top:24px;",
              actionButton("refresh_data", tags$span(icon("sync"), " Refresh Data"), class = "btn-primary"))),
            column(3, tags$div(style = "padding-top:28px; color:#64748b; font-size:12px;",
              icon("clock", style = "margin-right:4px;"), textOutput("last_updated", inline = TRUE)))
          )
        ),
        tags$div(class = "panel-card",
          h3("Price Trend", class = "section-title"),
          plotlyOutput("price_chart", height = "460px")
        ),
        tags$div(style = "margin-top:16px;",
          actionButton("goto_analytics", tags$span(icon("table-columns"), " View Analytics & Download CSV"),
                       class = "btn-info", style = "padding:10px 24px; font-size:14px;"),
          tags$p(style = "color:#475569; margin-top:8px; font-size:12px;",
            "Model parameters, RAG-ready CSV exports, and full historical price data")
        )
      )
    }
  })

  # ----------------------------
  # Price chart
  # ----------------------------

  selected_history <- reactive({
    d <- stock_data()
    if (is.null(d) || is.null(d$daily)) return(NULL)
    sub <- d$daily[d$daily$Symbol == input$data_ticker, ]
    if (nrow(sub) == 0) return(NULL)
    sub <- sub[order(sub$Date), ]
    n <- WINDOW_MAP[[input$data_window]]
    if (is.null(n)) n <- 22L
    tail(sub, min(n, nrow(sub)))
  })

  output$price_chart <- renderPlotly({
    sub <- selected_history()
    if (is.null(sub) || nrow(sub) < 2) return(NULL)
    x <- sub$Date; y <- sub$Close
    first <- head(y, 1); last <- tail(y, 1)
    plot_ly(sub, x = x, y = y, type = "scatter", mode = "lines",
            name = "Close", line = list(color = "#60a5fa", width = 3)) |>
      add_trace(x = x, y = y, type = "scatter", mode = "markers",
                marker = list(color = "#93c5fd", size = 4), showlegend = FALSE, name = "Points") |>
      add_trace(x = x[1], y = first, type = "scatter", mode = "markers",
                marker = list(color = "#34d399", size = 8), name = "Start") |>
      add_trace(x = x[length(x)], y = last, type = "scatter", mode = "markers",
                marker = list(color = "#f97316", size = 8), name = "End") |>
      layout(
        title = list(text = paste(TICKER_LABELS[[input$data_ticker]], "-", input$data_window),
                     font = list(color = "#e5e7eb", size = 16, family = "Inter, sans-serif")),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        xaxis = list(title = "", gridcolor = "rgba(51,65,85,0.5)", color = "#94a3b8",
                     tickformat = "%Y-%m-%d", tickfont = list(size = 11),
                     zeroline = FALSE, showline = FALSE),
        yaxis = list(title = "Price (USD)", gridcolor = "rgba(51,65,85,0.5)", color = "#94a3b8",
                     tickprefix = "$", tickfont = list(size = 11),
                     zeroline = FALSE, showline = FALSE),
        font = list(color = "#e2e8f0", family = "Inter, sans-serif"),
        legend = list(orientation = "h", y = -0.12,
                      font = list(color = "#94a3b8", size = 12, family = "Inter, sans-serif")),
        margin = list(t = 50, b = 60),
        hovermode = "x unified"
      )
  })

  # ----------------------------
  # News table
  # ----------------------------

  output$news_table <- renderDT({
    news_df <- news_data()
    if (is.null(news_df) || nrow(news_df) == 0) return(NULL)
    selected <- input$news_ticker
    if (!is.null(selected) && selected != "ALL") news_df <- news_df[news_df$Symbol == selected, ]
    if (nrow(news_df) == 0) return(NULL)

    shown <- news_df[order(news_df$Published, decreasing = TRUE), ]
    ts <- shown$Published
    local_ts <- format(ts, "%Y-%m-%d %H:%M", tz = "America/New_York")
    local_hm <- format(ts, "%H:%M", tz = "America/New_York")
    shown$Published <- ifelse(local_hm == "00:00", format(ts, "%Y-%m-%d", tz = "America/New_York"), paste0(local_ts, " ET"))
    url <- as.character(shown$Url)
    needs_search <- grepl("^https?://finnhub\\.io/api/news\\?id=", url)
    q <- utils::URLencode(paste0(shown$Title, " ", shown$Source), reserved = TRUE)
    url[needs_search] <- paste0("https://news.google.com/search?q=", q[needs_search])
    shown$Link <- sprintf('<a href="%s" target="_blank" rel="noopener noreferrer" style="color:#38bdf8">Open</a>', url)

    datatable(
      shown[, c("Symbol", "Published", "Title", "Source", "Link")],
      rownames = FALSE, escape = FALSE,
      options = list(dom = "tip", pageLength = 20, autoWidth = TRUE, order = list(list(1, 'desc'))),
      selection = "none"
    ) |> formatStyle("Symbol", `font-weight` = "bold", color = "#e2e8f0")
  })

  # ----------------------------
  # Reporting: prediction + report in one flow
  # ----------------------------

  observeEvent(input$run_prediction, {
    d <- stock_data()
    req(d)

    if (is.null(d$daily) || nrow(d$daily) < 5) {
      output$ai_error <- renderUI(div(class = "alert alert-danger", "No market data. Refresh Data first."))
      return()
    }

    sym <- input$ai_ticker
    hist <- d$daily[d$daily$Symbol == sym, ]
    if (nrow(hist) < 5) {
      output$ai_error <- renderUI(div(class = "alert alert-danger", "Not enough history for selected ticker."))
      return()
    }

    n_window <- as.integer(input$forecast_window)
    hist <- hist[order(hist$Date), ]
    trend <- compute_ohlc_metrics(hist, lookback = max(n_window, nrow(hist)))

    gbm <- estimate_gbm_params(hist, max_lookback = 252)
    gbm_path <- NULL
    if (!is.null(gbm)) {
      gbm_path <- gbm_forecast_path(gbm$s0, gbm$mu_annual, gbm$sigma_annual, n_window, gbm$last_date)
    }

    lattice_T <- n_window / 252
    lattice <- estimate_lattice_params(hist, T_years = lattice_T, N_steps = n_window)
    lattice_path <- NULL
    if (!is.null(lattice)) {
      lattice_path <- lattice_forecast_path(lattice$s0, lattice$u, lattice$d,
                                            lattice$p_real, lattice$N_steps, lattice$last_date)
    }

    output$ai_error <- renderUI(NULL)
    ai_payload(NULL)
    report_result(NULL)

    withProgress(message = paste("Generating report for", sym), value = 0, {
      setProgress(0.1, detail = "Computing quantitative models...")
      ai_payload(list(
        symbol = sym, window = n_window, trend = trend,
        gbm = gbm, gbm_path = gbm_path,
        lattice = lattice, lattice_path = lattice_path
      ))

      setProgress(0.3, detail = "Fetching fundamentals & AI analysis...")
      result <- generate_stock_report(sym, d, news_data())

      if (!isTRUE(result$ok)) {
        output$ai_error <- renderUI(div(class = "alert alert-danger", result$error %||% "Report generation failed."))
      }
      report_result(result)
      setProgress(1)
    })
  }, ignoreInit = TRUE)

  # ----------------------------
  # Combined GBM + Lattice chart
  # ----------------------------

  output$combined_forecast_chart <- renderPlotly({
    payload <- ai_payload()
    if (is.null(payload)) return(NULL)

    sym <- payload$symbol
    gbm_path <- payload$gbm_path
    lattice_path <- payload$lattice_path
    if (is.null(gbm_path) && is.null(lattice_path)) return(NULL)

    d <- stock_data()
    hist <- NULL
    if (!is.null(d) && !is.null(d$daily)) {
      h <- d$daily[d$daily$Symbol == sym, ]
      if (nrow(h) > 0) { h <- h[order(h$Date), ]; hist <- tail(h, min(nrow(h), 90)) }
    }

    p <- plot_ly()
    if (!is.null(hist) && nrow(hist) > 1) {
      p <- p |> add_trace(data = hist, x = ~Date, y = ~Close, type = "scatter", mode = "lines",
                           name = "History", line = list(color = "rgba(148,163,184,0.75)", width = 2))
    }

    if (!is.null(gbm_path)) {
      p <- p |>
        add_trace(data = gbm_path, x = ~Date, y = ~Lo, type = "scatter", mode = "lines",
                  name = "GBM 5-95%", line = list(color = "rgba(96,165,250,0)"), showlegend = FALSE) |>
        add_trace(data = gbm_path, x = ~Date, y = ~Hi, type = "scatter", mode = "lines",
                  name = "GBM 5-95%", fill = "tonexty", fillcolor = "rgba(96,165,250,0.15)",
                  line = list(color = "rgba(96,165,250,0)"), hoverinfo = "skip") |>
        add_trace(data = gbm_path, x = ~Date, y = ~Expected, type = "scatter", mode = "lines",
                  name = "GBM E[S]", line = list(color = "#60a5fa", width = 3))
    }

    if (!is.null(lattice_path)) {
      p <- p |>
        add_trace(data = lattice_path, x = ~Date, y = ~Lo, type = "scatter", mode = "lines",
                  name = "Lattice 5-95%", line = list(color = "rgba(251,191,36,0)"), showlegend = FALSE) |>
        add_trace(data = lattice_path, x = ~Date, y = ~Hi, type = "scatter", mode = "lines",
                  name = "Lattice 5-95%", fill = "tonexty", fillcolor = "rgba(251,191,36,0.12)",
                  line = list(color = "rgba(251,191,36,0)"), hoverinfo = "skip") |>
        add_trace(data = lattice_path, x = ~Date, y = ~Expected, type = "scatter", mode = "lines",
                  name = "Lattice E[S]", line = list(color = "#fbbf24", width = 3))
    }

    p |> layout(
      title = list(text = paste("GBM & Binomial Lattice Prediction -", sym),
                   font = list(color = "#e5e7eb", size = 16, family = "Inter, sans-serif")),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(title = "", gridcolor = "rgba(51,65,85,0.5)", color = "#94a3b8",
                   tickformat = "%Y-%m-%d", tickfont = list(size = 11),
                   zeroline = FALSE, showline = FALSE),
      yaxis = list(title = "Price (USD)", gridcolor = "rgba(51,65,85,0.5)", color = "#94a3b8",
                   tickprefix = "$", tickfont = list(size = 11),
                   zeroline = FALSE, showline = FALSE),
      legend = list(orientation = "h", y = -0.12,
                    font = list(color = "#94a3b8", size = 12, family = "Inter, sans-serif")),
      font = list(color = "#e2e8f0", family = "Inter, sans-serif"),
      margin = list(t = 50, b = 60),
      hovermode = "x unified"
    )
  })

  # ----------------------------
  # Report content (below chart)
  # ----------------------------

  output$report_content <- renderUI({
    result <- report_result()
    if (is.null(result)) return(NULL)
    render_report_ui(result)
  })

  observeEvent(input$report_info_btn, {
    showModal(render_rating_info_modal())
  })

  # ----------------------------
  # Analytics tables & download
  # ----------------------------

  output$rag_table <- renderDT({
    rag <- rag_history()
    if (is.null(rag) || nrow(rag) == 0) return(NULL)
    display <- rag[order(rag$snapshot_date, rag$ticker, decreasing = TRUE), ]
    datatable(display, rownames = FALSE,
              options = list(dom = "tip", pageLength = 20, scrollX = TRUE, autoWidth = FALSE,
                             columnDefs = list(list(className = "dt-center dt-nowrap", targets = "_all"))),
              selection = "none")
  })

  output$analytics_price_table <- renderDT({
    d <- stock_data()
    if (is.null(d) || is.null(d$daily)) return(NULL)
    display <- d$daily[order(d$daily$Date, decreasing = TRUE), ]
    datatable(display, rownames = FALSE,
              options = list(dom = "tip", pageLength = 20, scrollX = TRUE, autoWidth = FALSE,
                             columnDefs = list(list(className = "dt-center dt-nowrap", targets = "_all"))),
              selection = "none")
  })

  output$download_rag_csv <- downloadHandler(
    filename = function() paste0("rag_data_", Sys.Date(), ".csv"),
    content = function(file) {
      rag <- rag_history()
      if (!is.null(rag) && nrow(rag) > 0) write.csv(rag, file, row.names = FALSE)
    }
  )

  # ----------------------------
  # Protected refresh endpoint for external cron
  # Usage: https://YOUR_APP/?action=refresh-financials&token=SECRET
  # ----------------------------

  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (identical(query$action, "refresh-financials")) {
      expected_token <- Sys.getenv("REFRESH_TOKEN")
      if (!nzchar(expected_token) || !identical(query$token, expected_token)) {
        message("[Refresh] Unauthorized attempt")
        return()
      }
      message("[Refresh] Cron triggered - refreshing financial CSVs...")
      result <- tryCatch({
        source("fetch_financials.R", local = TRUE)
        message("[Refresh] Financial CSVs updated successfully at ", Sys.time())
        TRUE
      }, error = function(e) {
        message("[Refresh] Error: ", e$message)
        FALSE
      })
    }
  })
}
