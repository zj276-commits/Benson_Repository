# server.R
# Shiny server logic

server <- function(input, output, session) {

  # ----------------------------
  # reactive values
  # ----------------------------

  initial_news_summary <- load_latest_news_summary()
  initial_refresh_status <- load_latest_refresh_status()

  stock_data <- reactiveVal(NULL)
  news_data <- reactiveVal(NULL)
  ai_payload <- reactiveVal(NULL)
  last_updated <- reactiveVal("")
  rag_history <- reactiveVal(NULL)
  report_result <- reactiveVal(NULL)
  report_cache <- reactiveVal(list())
  news_summary <- reactiveVal(initial_news_summary)
  refresh_status <- reactiveVal(initial_refresh_status)
  backtest_result <- reactiveVal(NULL)
  backtest_error <- reactiveVal(NULL)

  cached <- load_cached_data()
  if (is.null(cached)) {
    cached <- load_exported_data()
  }
  if (!is.null(cached)) {
    stock_data(list(daily = cached$daily, news = cached$news))
    news_data(cached$news)
    last_updated(cached$updated_at)
  }
  if (is.null(initial_news_summary) && !is.null(cached$news)) {
    news_summary(build_news_summary_payload(cached$news))
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
    df <- gather_all_news_data(news_days = news_days)
    if (is.null(df)) return(NULL)
    head(df, 500)
  }

  fetch_all_histories <- function() {
    fetch_all_histories_data()
  }

  resolve_backtest_dates <- function() {
    range <- as.Date(input$backtest_custom_range %||% c(Sys.Date() - 7, Sys.Date()))
    list(start = range[[1]], end = range[[2]])
  }

  selected_backtest_model <- function() {
    input$backtest_model %||% unname(BACKTEST_MODEL_CHOICES[[1]])
  }

  selected_backtest_strategy_mode <- function() {
    input$backtest_strategy_mode %||% unname(BACKTEST_STRATEGY_MODES[[1]])
  }

  get_backtest_market_data <- function() {
    current <- stock_data()
    exported <- if (is.null(current) || is.null(current$daily)) load_exported_data() else NULL
    list(
      prices = current$daily %||% exported$daily,
      news = news_data() %||% current$news %||% exported$news
    )
  }

  # ----------------------------
  # Data refresh
  # ----------------------------

  observeEvent(input$refresh_data, {
    req(input$refresh_data)
    shiny::withProgress(message = "Refreshing market data", value = 0, {
      progress_cb <- function(value, detail) setProgress(value, detail = detail)
      refreshed <- refresh_market_data_exports(
        news_days = 90,
        days_back = 365,
        include_financials = TRUE,
        on_progress = progress_cb
      )
      if (!isTRUE(refreshed$ok)) {
        showNotification(refreshed$error %||% "Market data refresh failed.", type = "error")
        return(NULL)
      }
      stock_data(list(daily = refreshed$daily, news = refreshed$news))
      news_data(refreshed$news)
      rag_history(refreshed$rag)
      news_summary(load_latest_news_summary())
      refresh_status(load_latest_refresh_status())
      last_updated(refreshed$updated_at %||% safe_now())
      if (isTRUE(refreshed$skipped)) {
        showNotification("Skipped refresh because market data was refreshed recently.", type = "message", duration = 5)
        return(NULL)
      }
      if (!isTRUE(refreshed$financial_refresh_ok)) {
        showNotification(
          paste("Market data refreshed, but financial CSV refresh failed:", refreshed$financial_refresh_error %||% "unknown error"),
          type = "warning",
          duration = 8
        )
      } else {
        showNotification("Data refreshed.", type = "message")
      }
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
    output$news_error <- renderUI(div(class = "alert alert-info", "Refreshing news. This can take a few seconds..."))
    news_df <- withProgress(message = "Refreshing news", value = 0.2, {
      gather_all_news(days)
    })
    if (is.null(news_df) || nrow(news_df) == 0) {
      output$news_error <- renderUI(div(class = "alert alert-warning", "No news returned. Try again later."))
      return()
    }
    news_data(news_df)
    d <- stock_data()
    if (!is.null(d) && is.data.frame(d$daily)) { d$news <- news_df; stock_data(d) }
    if (!is.null(d) && !is.null(d$daily)) save_cached_data(d$daily, news_df)
    export_news_csv(news_df)
    news_summary(load_latest_news_summary())
    refresh_status(load_latest_refresh_status())
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
    if (length(qs) == 0) return(tags$div(style = "color:#6c6a64; text-align:center; padding:24px; font-size:14px;",
      icon("circle-notch", class = "fa-spin", style = "margin-right:6px;"), "Loading live quotes..."))
    cards <- lapply(names(qs), function(sym) {
      q <- qs[[sym]]
      is_up <- q$change >= 0
      chg_col <- if (is_up) "#5db872" else "#c64545"
      chg_sign <- if (is_up) "+" else ""
      arrow_icon <- if (is_up) "caret-up" else "caret-down"
      border_accent <- if (is_up) "rgba(93,184,114,0.35)" else "rgba(198,69,69,0.35)"
      tags$div(
        style = sprintf("flex:1 1 140px; border:1px solid %s; background:#faf9f5; border-radius:12px; padding:14px 16px; text-align:center; transition:border-color .2s; cursor:default;", border_accent),
        class = "metric-card",
        tags$div(style = "font-weight:500; color:#6c6a64; font-size:11px; text-transform:uppercase; letter-spacing:.06em; margin-bottom:4px;",
          paste0(sym)),
        tags$div(style = "color:#6c6a64; font-size:12px; margin-bottom:6px;", TICKER_LABELS[[sym]]),
        tags$div(style = "font-size:24px; font-weight:600; color:#141413; margin:4px 0; font-variant-numeric:tabular-nums;", fmt_price(q$price)),
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
    tags$div(style = "color:#6c6a64; font-size:11px; text-align:right; display:flex; align-items:center; justify-content:flex-end; gap:6px;",
      tags$span(style = "display:inline-block; width:6px; height:6px; border-radius:50%; background:#5db872; animation:pulse-dot 2s infinite;"),
      tags$span(paste("Live |", qt, "| Auto-refresh 15s")),
      tags$style(HTML("@keyframes pulse-dot { 0%,100% { opacity:1; } 50% { opacity:0.3; } }"))
    )
  })

  output$last_updated <- renderText({
    lu <- last_updated()
    if (nzchar(lu)) lu else "No cached data. Click Refresh Market Data."
  })

  output$data_refresh_status_ui <- renderUI({
    status <- market_data_status()
    latest <- refresh_status()
    is_stale <- isTRUE(status$stale)
    dot_class <- if (is_stale) "warn" else "ok"
    label <- if (is_stale) "Demo dataset" else "Live"
    last_ts <- status$latest_price_date %||% latest$ended_at %||% "-"
    show_debug <- isTRUE(getOption("philover.debug_status", default = FALSE))

    pill <- tags$div(class = "data-status-pill",
      tags$span(class = paste("status-dot", dot_class)),
      tags$span(style = "font-weight:500; color:#141413;", label),
      tags$span(style = "color:#6c6a64;", sprintf(" · Last update %s", last_ts)),
      tags$span(style = "color:#8e8b82;", " · Source: Finnhub")
    )

    if (!show_debug) {
      return(tags$div(style = "margin: 4px 0 12px;", pill))
    }

    source_counts <- latest$price_source_counts %||% list()
    source_text <- if (length(source_counts) == 0) "No recent source summary" else {
      paste(vapply(names(source_counts), function(nm) paste0(nm, ": ", source_counts[[nm]]), character(1)),
            collapse = "  •  ")
    }
    tagList(
      tags$div(style = "margin: 4px 0 8px;", pill),
      tags$details(style = "margin-bottom:12px; color:#6c6a64; font-size:11.5px;",
        tags$summary(style = "cursor:pointer; color:#6c6a64;", "Diagnostics (debug mode)"),
        tags$div(style = "padding:8px 12px; line-height:1.6;",
          sprintf("Expected: %s · Latest CSV: %s · News age: %sh",
                  status$expected_price_date %||% "-",
                  status$latest_price_date %||% "-",
                  fmt_num(as.numeric(status$news_age_hours %||% NA_real_), 1)),
          tags$br(),
          sprintf("Source: %s · Mix: %s · Last status: %s",
                  status$preferred_price_source %||% "auto",
                  source_text,
                  latest$status %||% "unknown")))
    )
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
        uiOutput("data_refresh_status_ui"),
        tags$div(
          class = "panel-card", style = "margin-bottom:8px; padding:16px 20px;",
          fluidRow(
            column(3, selectInput("data_ticker", "Select Stock",
                                  choices = setNames(TICKERS, paste0(TICKERS, " - ", TICKER_LABELS[TICKERS])),
                                  selected = "AAPL", width = "100%")),
            column(2, radioButtons("data_window", "Time Range", choices = c("7D", "1M", "1Y"), selected = "1M", inline = TRUE)),
            column(2, radioButtons("chart_type", "Chart", choices = c("Line" = "line", "Candle" = "candle"), selected = "line", inline = TRUE)),
            column(3, tags$div(style = "padding-top:24px;",
              actionButton("refresh_data", tags$span(icon("sync"), " Refresh Data"), class = "btn-primary"))),
            column(2, tags$div(style = "padding-top:28px; color:#6c6a64; font-size:12px;",
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
          tags$p(style = "color:#6c6a64; margin-top:8px; font-size:12px;",
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

    chart_mode <- input$chart_type %||% "line"
    common_layout <- list(
      title = list(text = paste(TICKER_LABELS[[input$data_ticker]], "-", input$data_window),
                   font = list(color = "#141413", size = 16, family = "Inter, sans-serif")),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(title = "", gridcolor = "#ebe6df", color = "#6c6a64",
                   tickformat = "%Y-%m-%d", tickfont = list(size = 11),
                   zeroline = FALSE, showline = FALSE, rangeslider = list(visible = FALSE)),
      yaxis = list(title = "Price (USD)", gridcolor = "#ebe6df", color = "#6c6a64",
                   tickprefix = "$", tickfont = list(size = 11),
                   zeroline = FALSE, showline = FALSE),
      font = list(color = "#3d3d3a", family = "Inter, sans-serif"),
      legend = list(orientation = "h", y = -0.12,
                    font = list(color = "#6c6a64", size = 12, family = "Inter, sans-serif")),
      margin = list(t = 50, b = 60),
      hovermode = "x unified"
    )

    if (chart_mode == "candle") {
      p <- plot_ly(sub, x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Close,
                   type = "candlestick", name = "OHLC",
                   increasing = list(line = list(color = "#5db872"), fillcolor = "rgba(93,184,114,0.25)"),
                   decreasing = list(line = list(color = "#c64545"), fillcolor = "rgba(198,69,69,0.22)"))
      do.call(layout, c(list(p), common_layout))
    } else {
      x <- sub$Date; y <- sub$Close
      first <- head(y, 1); last <- tail(y, 1)
      p <- plot_ly(sub, x = x, y = y, type = "scatter", mode = "lines",
                   name = "Close", line = list(color = "#cc785c", width = 3)) |>
        add_trace(x = x, y = y, type = "scatter", mode = "markers",
                  marker = list(color = "#cc785c", size = 4), showlegend = FALSE, name = "Points") |>
        add_trace(x = x[1], y = first, type = "scatter", mode = "markers",
                  marker = list(color = "#5db8a6", size = 8), name = "Start") |>
        add_trace(x = x[length(x)], y = last, type = "scatter", mode = "markers",
                  marker = list(color = "#e8a55a", size = 8), name = "End")
      do.call(layout, c(list(p), common_layout))
    }
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
    ts <- as.POSIXct(as.numeric(shown$Published), origin = "1970-01-01", tz = "UTC")
    local_ts <- format(ts, "%Y-%m-%d %H:%M", tz = "America/New_York")
    local_hm <- format(ts, "%H:%M", tz = "America/New_York")
    shown$Published <- ifelse(local_hm == "00:00", format(ts, "%Y-%m-%d", tz = "America/New_York"), paste0(local_ts, " ET"))
    url <- if ("Url" %in% names(shown)) as.character(shown$Url) else rep("", nrow(shown))
    url[is.na(url)] <- ""
    needs_search <- !nzchar(url) | grepl("^https?://finnhub\\.io/api/news\\?id=", url)
    short_title <- substr(shown$Title, 1, 80)
    q <- utils::URLencode(paste0(short_title, " ", shown$Source), reserved = TRUE)
    url[needs_search] <- paste0("https://www.google.com/search?q=", q[needs_search])
    shown$Link <- sprintf('<a href="%s" target="_blank" rel="noopener noreferrer" style="color:#cc785c">Open</a>', url)

    datatable(
      shown[, c("Symbol", "Published", "Title", "Source", "Link")],
      rownames = FALSE, escape = FALSE,
      options = list(
        dom = "tip", pageLength = 20, autoWidth = FALSE,
        order = list(list(1, 'desc')),
        columnDefs = list(
          list(targets = 0, width = "70px"),
          list(targets = 1, width = "150px"),
          list(targets = 2, width = "400px"),
          list(targets = 3, width = "80px"),
          list(targets = 4, width = "50px")
        )
      ),
      selection = "none"
    ) |>
      formatStyle("Symbol", `font-weight` = "500", color = "#141413") |>
      formatStyle("Title", `white-space` = "normal", `word-wrap` = "break-word", `line-height` = "1.4")
  })

  output$news_summary_ui <- renderUI({
    summary <- news_summary()
    if (is.null(summary)) {
      return(tags$div(class = "alert alert-info", "No persisted news summary yet. Refresh news to generate one."))
    }

    counts <- summary$counts_by_symbol %||% list()
    count_text <- vapply(counts, function(x) sprintf("%s: %s", x$symbol %||% "-", x$count %||% 0), character(1))

    top_sections <- lapply(summary$top_headlines %||% list(), function(group) {
      headlines <- group$headlines %||% list()
      if (length(headlines) == 0) return(NULL)
      tags$div(
        style = "min-width:220px; flex:1 1 220px; border:1px solid #e6dfd8; border-radius:12px; padding:14px; background:#faf9f5;",
        tags$div(style = "font-size:12px; color:#cc785c; font-weight:500; text-transform:uppercase; letter-spacing:.06em; margin-bottom:8px;",
                 group$symbol %||% "-"),
        lapply(headlines, function(item) {
          tags$div(
            style = "margin-bottom:10px;",
            tags$div(style = "color:#252523; font-size:14px; line-height:1.45;", item$title %||% "-"),
            tags$div(style = "color:#6c6a64; font-size:12px; margin-top:2px;",
                     paste(item$source %||% "-", item$published %||% "", sep = " | "))
          )
        })
      )
    })

    tags$div(
      class = "panel-card", style = "margin-bottom:16px;",
      tags$div(style = "display:flex; justify-content:space-between; gap:12px; align-items:flex-start; flex-wrap:wrap; margin-bottom:12px;",
        tags$div(
          tags$div(style = "color:#141413; font-size:18px; font-weight:500; font-family:'Cormorant Garamond',serif; margin-bottom:4px; letter-spacing:-0.02em;", "Latest Persisted News Summary"),
          tags$div(style = "color:#6c6a64; font-size:13px;",
                   sprintf("Generated %s | %s articles", summary$generated_at %||% "-", summary$total_articles %||% 0))
        ),
        tags$div(style = "color:#3d3d3a; font-size:13px;", paste(count_text, collapse = "  •  "))
      ),
      tags$div(style = "display:flex; flex-wrap:wrap; gap:12px;", top_sections)
    )
  })

  # ----------------------------
  # Reporting: prediction + report in one flow
  # ----------------------------

  observeEvent(input$run_prediction, {
    shinyjs::disable("run_prediction")
    shinyjs::html("run_prediction",
      as.character(tags$span(icon("spinner", class = "fa-spin"), " Drafting report…")))
    on.exit({
      shinyjs::enable("run_prediction")
      shinyjs::html("run_prediction",
        as.character(tags$span(icon("wand-magic-sparkles"), " Generate Report")))
    }, add = TRUE)

    d <- stock_data()
    if (is.null(d) || is.null(d$daily) || nrow(d$daily) < 5) {
      output$ai_error <- renderUI(div(class = "alert alert-danger", "No market data is loaded yet. Click Refresh Data first."))
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
    ai_payload(list(
      symbol = sym, window = n_window, trend = trend,
      gbm = gbm, gbm_path = gbm_path,
      lattice = lattice, lattice_path = lattice_path
    ))

    cache <- report_cache()
    cache_key <- paste0(sym, "_", n_window, "_", Sys.Date())
    if (!is.null(cache[[cache_key]])) {
      output$ai_error <- renderUI(NULL)
      report_result(cache[[cache_key]])
      showNotification(paste("Report loaded from cache (", sym, "today)"), type = "message", duration = 3)
      return()
    }

    report_result(NULL)
    output$ai_error <- renderUI(div(class = "alert alert-info", "Generating report. The full multi-agent workflow can take around 30-60 seconds on the first run."))
    withProgress(message = paste("Generating report for", sym), value = 0, {
      setProgress(0.05, detail = "Computing quantitative models...")
      progress_cb <- function(value, detail) setProgress(value, detail = detail)
      result <- generate_stock_report(sym, d, news_data(), on_progress = progress_cb)

      if (!isTRUE(result$ok)) {
        output$ai_error <- renderUI(div(class = "alert alert-danger", result$error %||% "Report generation failed."))
      } else {
        output$ai_error <- renderUI(NULL)
        cache[[cache_key]] <- result
        report_cache(cache)
      }
      report_result(result)
      setProgress(1, detail = "Report complete")
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
                           name = "History", line = list(color = "rgba(108,106,100,0.65)", width = 2))
    }

    if (!is.null(gbm_path)) {
      p <- p |>
        add_trace(data = gbm_path, x = ~Date, y = ~Lo, type = "scatter", mode = "lines",
                  name = "GBM 5-95%", line = list(color = "rgba(204,120,92,0)"), showlegend = FALSE) |>
        add_trace(data = gbm_path, x = ~Date, y = ~Hi, type = "scatter", mode = "lines",
                  name = "GBM 5-95%", fill = "tonexty", fillcolor = "rgba(204,120,92,0.12)",
                  line = list(color = "rgba(204,120,92,0)"), hoverinfo = "skip") |>
        add_trace(data = gbm_path, x = ~Date, y = ~Expected, type = "scatter", mode = "lines",
                  name = "GBM E[S]", line = list(color = "#cc785c", width = 3))
    }

    if (!is.null(lattice_path)) {
      p <- p |>
        add_trace(data = lattice_path, x = ~Date, y = ~Lo, type = "scatter", mode = "lines",
                  name = "Lattice 5-95%", line = list(color = "rgba(232,165,90,0)"), showlegend = FALSE) |>
        add_trace(data = lattice_path, x = ~Date, y = ~Hi, type = "scatter", mode = "lines",
                  name = "Lattice 5-95%", fill = "tonexty", fillcolor = "rgba(232,165,90,0.12)",
                  line = list(color = "rgba(232,165,90,0)"), hoverinfo = "skip") |>
        add_trace(data = lattice_path, x = ~Date, y = ~Expected, type = "scatter", mode = "lines",
                  name = "Lattice E[S]", line = list(color = "#e8a55a", width = 3))
    }

    p |> layout(
      title = list(text = paste("GBM & Binomial Lattice Prediction -", sym),
                   font = list(color = "#141413", size = 16, family = "Inter, sans-serif")),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(title = "", gridcolor = "#ebe6df", color = "#6c6a64",
                   tickformat = "%Y-%m-%d", tickfont = list(size = 11),
                   zeroline = FALSE, showline = FALSE),
      yaxis = list(title = "Price (USD)", gridcolor = "#ebe6df", color = "#6c6a64",
                   tickprefix = "$", tickfont = list(size = 11),
                   zeroline = FALSE, showline = FALSE),
      legend = list(orientation = "h", y = -0.12,
                    font = list(color = "#6c6a64", size = 12, family = "Inter, sans-serif")),
      font = list(color = "#3d3d3a", family = "Inter, sans-serif"),
      margin = list(t = 50, b = 60),
      hovermode = "x unified"
    )
  })

  # ----------------------------
  # Backtesting
  # ----------------------------

  observeEvent(input$backtest_ticker, {
    latest <- load_latest_backtest(input$backtest_ticker)
    backtest_result(latest)
    backtest_error(NULL)
  }, ignoreInit = FALSE)

  observeEvent(input$load_latest_backtest_action, {
    latest <- load_latest_backtest(input$backtest_ticker)
    if (is.null(latest)) {
      backtest_result(NULL)
      backtest_error(list(type = "warning", message = "No persisted backtest found for this ticker yet."))
    } else {
      backtest_result(latest)
      backtest_error(list(type = "info", message = "Loaded the latest persisted backtest from disk."))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$run_backtest_action, {
    shinyjs::disable("run_backtest_action")
    shinyjs::disable("load_latest_backtest_action")
    shinyjs::html("run_backtest_action",
      as.character(tags$span(icon("spinner", class = "fa-spin"), " Running…")))
    on.exit({
      shinyjs::enable("run_backtest_action")
      shinyjs::enable("load_latest_backtest_action")
      shinyjs::html("run_backtest_action",
        as.character(tags$span(icon("play"), " Run Agent Backtest")))
    }, add = TRUE)

    dates <- resolve_backtest_dates()
    if (any(is.na(unlist(dates))) || dates$start > dates$end) {
      backtest_error(list(type = "danger", message = "Invalid backtest date range."))
      return()
    }

    market_data <- get_backtest_market_data()
    backtest_error(list(
      type = "info",
      message = paste(
        "Running masked backtest with", selected_backtest_model(), ".",
        "Each decision only sees data available on that date."
      )
    ))

    withProgress(message = paste("Running backtest for", input$backtest_ticker), value = 0, {
      progress_cb <- function(value, detail) setProgress(value, detail = detail)
      result <- run_backtest(
        symbol = input$backtest_ticker,
        start_date = dates$start,
        end_date = dates$end,
        initial_capital = 10000,
        lookback_days = BACKTEST_LOOKBACK_DAYS,
        price_data = market_data$prices,
        news_data = market_data$news,
        llm_model = selected_backtest_model(),
        strategy_mode = selected_backtest_strategy_mode(),
        on_progress = progress_cb,
        sleep_sec = 0
      )

      if (!isTRUE(result$ok)) {
        backtest_result(NULL)
        backtest_error(list(type = "danger", message = result$error %||% "Backtest failed."))
      } else {
        backtest_result(result)
        backtest_error(list(type = "info", message = "Backtest complete. Latest result has been persisted for the next session."))
      }
      setProgress(1)
    })
  }, ignoreInit = TRUE)

  output$backtest_error_ui <- renderUI({
    err <- backtest_error()
    if (is.null(err)) return(NULL)
    cls <- switch(
      err$type %||% "info",
      danger = "alert alert-danger",
      warning = "alert alert-warning",
      "alert alert-info"
    )
    tags$div(class = cls, err$message %||% "")
  })

  output$backtest_header_ui <- renderUI({
    result <- backtest_result()
    if (is.null(result)) {
      return(tags$div(class = "alert alert-info", "Run a backtest or load the latest persisted result."))
    }
    tags$div(
      class = "panel-card", style = "margin-bottom:16px;",
      tags$div(style = "display:flex; justify-content:space-between; gap:12px; flex-wrap:wrap;",
        tags$div(
          tags$div(style = "color:#141413; font-size:22px; font-weight:500; font-family:'Cormorant Garamond',serif; letter-spacing:-0.02em; margin-bottom:6px;",
                   paste(result$ticker %||% "-", "Agent Backtest")),
          tags$div(style = "color:#6c6a64; font-size:14px;",
                   sprintf("Range %s to %s | Model %s | Strategy mode %s | Borrowed capital $%s | %s trading days",
                           result$start_date %||% "-", result$end_date %||% "-",
                           result$llm_model %||% "gpt-4o-mini",
                           result$strategy_mode %||% "auto",
                           fmt_num(as.numeric(result$borrowed_capital %||% result$initial_capital %||% 0), 0),
                           result$trading_days %||% 0))
        ),
        tags$div(style = "color:#8e8b82; font-size:12px;",
                 paste("Generated", result$generated_at %||% "-"))
      )
    )
  })

  output$backtest_metrics_ui <- renderUI({
    result <- backtest_result()
    if (is.null(result) || is.null(result$metrics)) return(NULL)
    metrics <- result$metrics
    cards <- list(
      list("Strategy Return", paste0(fmt_num(as.numeric(metrics$total_return_pct %||% 0), 2), "%"), "#cc785c"),
      list("Buy & Hold", paste0(fmt_num(as.numeric(metrics$buy_hold_return_pct %||% 0), 2), "%"), "#6c6a64"),
      list("Alpha", paste0(fmt_num(as.numeric(metrics$alpha_vs_buy_hold_pct %||% 0), 2), "%"), "#5db872"),
      list("Sharpe", fmt_num(as.numeric(metrics$sharpe_ratio %||% 0), 2), "#e8a55a"),
      list("Max Drawdown", paste0(fmt_num(as.numeric(metrics$max_drawdown_pct %||% 0), 2), "%"), "#c64545"),
      list("Win Rate", paste0(fmt_num(as.numeric(metrics$win_rate %||% 0) * 100, 0), "%"), "#5db8a6"),
      list("Trade Count", as.character(metrics$trade_count %||% 0L), "#252523"),
      list("Strategy Diversity", as.character(metrics$strategy_diversity %||% 0L), "#cc785c")
    )

    tags$div(
      style = "display:flex; flex-wrap:wrap; gap:12px; margin-bottom:16px;",
      lapply(cards, function(card) {
        tags$div(
          class = "panel-card",
          style = "flex:1 1 180px; margin-top:0; padding:16px;",
          tags$div(style = "color:#6c6a64; font-size:11px; font-weight:500; text-transform:uppercase; letter-spacing:.06em;",
                   card[[1]]),
          tags$div(style = sprintf("color:%s; font-size:24px; font-weight:600; margin-top:6px;", card[[3]]),
                   card[[2]])
        )
      })
    )
  })

  output$backtest_equity_chart <- renderPlotly({
    result <- backtest_result()
    result <- ensure_forward_outlook(result)
    eq_df <- coerce_record_df(result$equity_curve)
    if (is.null(eq_df) || nrow(eq_df) == 0) return(NULL)
    eq_df$date <- as.Date(eq_df$date)
    eq_df$equity <- as.numeric(eq_df$equity)

    baseline_df <- coerce_record_df(result$baseline_curve)
    if (!is.null(baseline_df) && nrow(baseline_df) > 0) {
      baseline_df$date <- as.Date(baseline_df$date)
      baseline_df$equity <- as.numeric(baseline_df$equity)
    }

    trades_df <- coerce_record_df(result$trades)
    if (!is.null(trades_df) && nrow(trades_df) > 0) {
      trades_df$date <- as.Date(trades_df$date)
      trades_df$price <- as.numeric(trades_df$price)
    }

    p <- plot_ly(eq_df, x = ~date, y = ~equity, type = "scatter", mode = "lines+markers",
                 name = "Agent Equity", line = list(color = "#cc785c", width = 3),
                 marker = list(size = 5, color = "#cc785c"))

    if (!is.null(baseline_df) && nrow(baseline_df) > 0) {
      p <- p |> add_trace(data = baseline_df, x = ~date, y = ~equity, type = "scatter", mode = "lines",
                          name = "Buy & Hold", line = list(color = "#8e8b82", width = 2, dash = "dash"))
    }

    if (!is.null(trades_df) && nrow(trades_df) > 0) {
      decision_df <- coerce_record_df(result$decision_log)
      if (!is.null(decision_df) && nrow(decision_df) > 0) {
        decision_df$date <- as.Date(decision_df$date)
        decision_df$equity_after <- vapply(result$decision_log, function(x) as.numeric(x$portfolio_after$equity %||% NA_real_), numeric(1))
        trades_plot <- merge(trades_df, decision_df[, c("date", "equity_after")], by = "date", all.x = TRUE)
        p <- p |> add_trace(
          data = trades_plot, x = ~date, y = ~equity_after, type = "scatter", mode = "markers",
          name = "Executed Trades",
          marker = list(size = 10, color = ifelse(trades_plot$action == "BUY", "#5db872", "#c64545"),
                        line = list(color = "#faf9f5", width = 1)),
          text = ~paste(action, shares, "shares @", fmt_price(price)),
          hovertemplate = "%{text}<extra></extra>"
        )
      }
    }

    # ---- Forward-looking GBM cone (extends both equity and buy & hold trajectories) ----
    outlook <- result$forward_outlook
    if (!is.null(outlook) && !is.null(outlook$forecast_path) && nrow(outlook$forecast_path) >= 2) {
      fp <- outlook$forecast_path
      fp$Date <- as.Date(fp$Date)

      # Anchor: last actual equity value & last buy-and-hold value
      last_eq <- tail(eq_df$equity, 1)
      last_bh <- if (!is.null(baseline_df) && nrow(baseline_df) > 0) tail(baseline_df$equity, 1) else NA_real_
      s0 <- as.numeric(fp$Median[[1]])

      # Convert price ratios to equity trajectories. Agent line: project at GBM median return.
      # Buy & Hold line: continues with full price exposure.
      ratio_med <- as.numeric(fp$Median) / s0
      ratio_lo  <- as.numeric(fp$Lo) / s0
      ratio_hi  <- as.numeric(fp$Hi) / s0

      eq_med <- last_eq * ratio_med
      eq_lo  <- last_eq * ratio_lo
      eq_hi  <- last_eq * ratio_hi

      # Lower bound trace (invisible line, anchor for fill)
      p <- p |> add_trace(x = fp$Date, y = eq_lo, type = "scatter", mode = "lines",
                          line = list(color = "rgba(0,0,0,0)"),
                          showlegend = FALSE, hoverinfo = "skip", name = "fcst_lo")
      # Upper bound + fill to lower → forms the 90% cone
      p <- p |> add_trace(x = fp$Date, y = eq_hi, type = "scatter", mode = "lines",
                          fill = "tonexty", fillcolor = "rgba(204,120,92,0.15)",
                          line = list(color = "rgba(0,0,0,0)"),
                          name = "Forecast 90% Cone",
                          hovertemplate = "%{x}: 90%% upper $%{y:.2f}<extra></extra>")
      # Median forecast line
      p <- p |> add_trace(x = fp$Date, y = eq_med, type = "scatter", mode = "lines",
                          line = list(color = "#cc785c", width = 2.5, dash = "dot"),
                          name = "Forecast Median (GBM)",
                          hovertemplate = "%{x}: median $%{y:.2f}<extra></extra>")

      # Buy & Hold projection (dashed, lighter)
      if (is.finite(last_bh)) {
        bh_proj <- last_bh * ratio_med
        p <- p |> add_trace(x = fp$Date, y = bh_proj, type = "scatter", mode = "lines",
                            line = list(color = "#6c6a64", width = 1.5, dash = "dot"),
                            name = "Buy & Hold (projected)",
                            hovertemplate = "%{x}: B&H $%{y:.2f}<extra></extra>")
      }

      # Vertical "today" separator
      p <- p |> add_trace(
        x = c(tail(eq_df$date, 1), tail(eq_df$date, 1)),
        y = c(min(c(eq_df$equity, eq_lo), na.rm = TRUE),
              max(c(eq_df$equity, eq_hi), na.rm = TRUE)),
        type = "scatter", mode = "lines",
        line = list(color = "rgba(204,120,92,0.45)", width = 1, dash = "dash"),
        name = "Backtest end",
        hoverinfo = "skip", showlegend = FALSE
      )
    }

    p |> layout(
      title = list(text = paste("Masked Backtest Equity Curve -", result$ticker %||% ""),
                   font = list(color = "#141413", size = 16, family = "Inter, sans-serif")),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(title = "", gridcolor = "#ebe6df", color = "#6c6a64"),
      yaxis = list(title = "Equity (USD)", tickprefix = "$", gridcolor = "#ebe6df", color = "#6c6a64"),
      legend = list(orientation = "h", y = -0.15, font = list(color = "#6c6a64")),
      font = list(color = "#3d3d3a", family = "Inter, sans-serif"),
      margin = list(t = 50, b = 70),
      hovermode = "x unified"
    )
  })

  output$backtest_trades_table <- renderDT({
    result <- backtest_result()
    trades_df <- coerce_record_df(result$trades)
    if (is.null(trades_df) || nrow(trades_df) == 0) {
      return(datatable(data.frame(Status = "No executed trades in this window"), rownames = FALSE,
                       options = list(dom = "t"), selection = "none"))
    }
    trades_df <- trades_df[, intersect(c("date", "selected_strategy", "target_position_pct", "action", "shares", "price", "confidence", "reasoning"), names(trades_df)), drop = FALSE]
    datatable(
      trades_df,
      rownames = FALSE,
      options = list(dom = "tip", pageLength = 10, autoWidth = FALSE, scrollX = TRUE),
      selection = "none"
    ) |>
      formatStyle("action", `font-weight` = "bold")
  })

  output$backtest_setup_ui <- renderUI({
    result <- backtest_result()
    if (is.null(result)) return(NULL)
    trades_df <- coerce_record_df(result$trades)
    trade_count <- if (is.null(trades_df)) 0 else nrow(trades_df)
    tags$div(
      style = "display:flex; flex-direction:column; gap:12px;",
      tags$div(
        tags$div(style = "color:#6c6a64; font-size:11px; text-transform:uppercase; font-weight:500; letter-spacing:.06em;", "Decision Model"),
        tags$div(style = "color:#141413; font-size:20px; font-weight:500; font-family:'Cormorant Garamond',serif; margin-top:4px; letter-spacing:-0.02em;", result$llm_model %||% "gpt-4o-mini")
      ),
      tags$div(
        tags$div(style = "color:#6c6a64; font-size:11px; text-transform:uppercase; font-weight:500; letter-spacing:.06em;", "Masked Setup"),
        tags$div(style = "color:#3d3d3a; font-size:14px; line-height:1.7;",
          sprintf("Range: %s to %s", result$start_date %||% "-", result$end_date %||% "-"),
          tags$br(),
          sprintf("Lookback: %s trading days", result$lookback_days %||% BACKTEST_LOOKBACK_DAYS),
          tags$br(),
          sprintf("Strategy mode: %s", result$strategy_mode %||% "auto"),
          tags$br(),
          sprintf("Decision days: %s", result$trading_days %||% 0),
          tags$br(),
          sprintf("Executed trades: %s", trade_count),
          tags$br(),
          sprintf("Dominant strategy: %s", result$metrics$dominant_strategy %||% "none")
        )
      ),
      tags$div(style = "padding:12px 14px; border-radius:12px; background:rgba(93,184,114,0.1); border:1px solid rgba(93,184,114,0.25); color:#252523; font-size:13px; line-height:1.7;",
        "The agent only sees information available on each decision date.",
        tags$br(),
        "No future prices, no future news, no look-ahead leakage."
      )
    )
  })

  # Lazily compute forward outlook for old cached results that pre-date this feature
  ensure_forward_outlook <- function(result) {
    if (is.null(result)) return(result)
    if (!is.null(result$forward_outlook)) return(result)
    sym <- result$ticker
    if (is.null(sym)) return(result)
    d <- stock_data()
    if (is.null(d) || is.null(d$daily)) return(result)
    hist <- d$daily[d$daily$Symbol == sym, , drop = FALSE]
    if (nrow(hist) == 0) return(result)
    hist <- data.frame(Date = as.Date(hist$Date), Close = as.numeric(hist$Close),
                       stringsAsFactors = FALSE)
    result$forward_outlook <- tryCatch(
      build_forward_outlook(result, hist_prices = hist, horizon_days = 30L),
      error = function(e) NULL
    )
    result
  }

  output$backtest_explanation_ui <- renderUI({
    result <- backtest_result()
    if (is.null(result)) return(NULL)
    result <- ensure_forward_outlook(result)
    explanation <- result$llm_explanation %||% build_backtest_explanation_fallback(result)
    basis <- explanation$decision_basis %||% character(0)
    if (is.list(basis)) basis <- unlist(basis, use.names = FALSE)
    improvements <- explanation$improvement_ideas %||% character(0)
    if (is.list(improvements)) improvements <- unlist(improvements, use.names = FALSE)
    trade_count <- length(result$trades %||% list())

    tagList(
      if (nzchar(explanation$summary_title %||% "")) {
        tags$div(style = "color:#141413; font-size:22px; font-weight:500; font-family:'Cormorant Garamond',serif; letter-spacing:-0.02em; margin-bottom:10px;",
          explanation$summary_title)
      },
      tags$p(style = "color:#3d3d3a; font-size:14px; line-height:1.85; margin-bottom:16px;",
        explanation$executive_summary %||% ""),
      tags$div(style = "padding:14px 16px; border-radius:12px; background:rgba(204,120,92,0.08); border:1px solid rgba(204,120,92,0.22); margin-bottom:16px;",
        tags$div(style = "color:#cc785c; font-size:12px; font-weight:500; text-transform:uppercase; letter-spacing:.06em; margin-bottom:6px;",
          if (trade_count <= 1) "Why Only One Trade?" else "Trade Frequency Interpretation"),
        tags$div(style = "color:#252523; font-size:14px; line-height:1.8;",
          explanation$sparsity_explanation %||% "")
      ),
      if (length(basis) > 0) {
        tagList(
          tags$div(style = "color:#6c6a64; font-size:11px; text-transform:uppercase; font-weight:500; letter-spacing:.06em; margin-bottom:8px;",
            "Decision Basis"),
          tags$ul(style = "color:#3d3d3a; padding-left:18px; line-height:1.8; margin-bottom:16px;",
            lapply(basis, tags$li))
        )
      },
      tags$div(style = "color:#6c6a64; font-size:11px; text-transform:uppercase; font-weight:500; letter-spacing:.06em; margin-bottom:8px;",
        "Model Role"),
      tags$p(style = "color:#3d3d3a; font-size:14px; line-height:1.85; margin-bottom:16px;",
        explanation$model_role %||% ""),
      if (length(improvements) > 0) {
        tagList(
          tags$div(style = "color:#6c6a64; font-size:11px; text-transform:uppercase; font-weight:500; letter-spacing:.06em; margin-bottom:8px;",
            "What To Improve Next"),
          tags$ul(style = "color:#3d3d3a; padding-left:18px; line-height:1.8; margin-bottom:16px;",
            lapply(improvements, tags$li))
        )
      },
      render_forward_outlook_ui(result$forward_outlook)
    )
  })

  render_forward_outlook_ui <- function(outlook) {
    if (is.null(outlook)) return(NULL)

    scenario_card <- function(scn, accent_bg, accent_border, accent_text) {
      tags$div(style = sprintf(
        "padding:14px 16px; border-radius:14px; background:%s; border:1px solid %s; margin-bottom:10px;",
        accent_bg, accent_border),
        tags$div(style = sprintf("color:%s; font-size:12px; font-weight:700; text-transform:uppercase; letter-spacing:.4px; margin-bottom:6px;", accent_text),
          scn$label),
        tags$div(style = "color:#252523; font-size:14px; line-height:1.7; margin-bottom:6px;",
          scn$forecast),
        if (!is.null(scn$return_band))
          tags$div(style = "color:#6c6a64; font-size:13px; margin-bottom:8px;", scn$return_band),
        tags$div(style = "color:#6c6a64; font-size:11px; text-transform:uppercase; font-weight:500; letter-spacing:.06em; margin-bottom:4px;",
          "Triggers to Watch"),
        tags$ul(style = "color:#3d3d3a; padding-left:18px; line-height:1.7; font-size:13px; margin:0 0 8px;",
          lapply(scn$triggers, tags$li)),
        tags$div(style = "color:#6c6a64; font-size:11px; text-transform:uppercase; font-weight:500; letter-spacing:.06em; margin-bottom:4px;",
          "Recommended Actions"),
        tags$ul(style = "color:#3d3d3a; padding-left:18px; line-height:1.7; font-size:13px; margin:0;",
          lapply(scn$actions, tags$li))
      )
    }

    tagList(
      tags$div(style = "border-top:1px dashed #e6dfd8; margin:18px 0 14px;"),
      tags$div(style = "display:flex; justify-content:space-between; align-items:center; gap:10px; flex-wrap:wrap; margin-bottom:8px;",
        tags$div(style = "color:#cc785c; font-size:14px; font-weight:500; font-family:'Cormorant Garamond',serif; text-transform:uppercase; letter-spacing:.06em;",
          "Forward-Looking Outlook"),
        tags$span(style = "background:#efe9de; color:#141413; border:1px solid #e6dfd8; padding:3px 10px; border-radius:999px; font-size:11px; font-weight:500;",
          outlook$horizon_label)
      ),
      tags$p(style = "color:#6c6a64; font-size:13px; line-height:1.6; margin:0 0 12px;",
        outlook$method),
      scenario_card(outlook$base_case,
                    "rgba(204,120,92,0.08)", "rgba(204,120,92,0.28)", "#cc785c"),
      scenario_card(outlook$bull_case,
                    "rgba(93,184,114,0.10)", "rgba(93,184,114,0.28)", "#3d7a4e"),
      scenario_card(outlook$bear_case,
                    "rgba(198,69,69,0.08)", "rgba(198,69,69,0.28)", "#c64545"),
      tags$div(style = "color:#6c6a64; font-size:11px; text-transform:uppercase; font-weight:500; letter-spacing:.06em; margin:14px 0 6px;",
        "Watch Signals"),
      tags$ul(style = "color:#3d3d3a; padding-left:18px; line-height:1.7; font-size:13px; margin:0;",
        lapply(outlook$watch_signals, tags$li))
    )
  }

  output$backtest_quality_card_ui <- renderUI({
    result <- backtest_result()
    quality_df <- result$quality_matrix
    if (!is.data.frame(quality_df)) quality_df <- coerce_record_df(quality_df)
    if (is.null(quality_df) || nrow(quality_df) == 0) return(NULL)

    pass_count <- sum(quality_df$status == "PASS", na.rm = TRUE)
    warn_count <- sum(quality_df$status == "WARN", na.rm = TRUE)
    fail_count <- sum(quality_df$status == "FAIL", na.rm = TRUE)
    total_count <- nrow(quality_df)
    headline_color <- if (fail_count > 0) "#c64545" else if (warn_count > 0) "#d4a017" else "#5db872"

    summary_row <- tags$div(style = "display:flex; gap:16px; align-items:baseline; flex-wrap:wrap; margin-bottom:8px;",
      tags$span(style = sprintf("color:%s; font-size:26px; font-weight:800; letter-spacing:.3px;", headline_color),
        sprintf("%s / %s checks passed", pass_count, total_count)),
      tags$span(style = "color:#6c6a64; font-size:12px;",
        sprintf("PASS %s · WARN %s · FAIL %s", pass_count, warn_count, fail_count))
    )

    tiles <- lapply(seq_len(nrow(quality_df)), function(i) {
      row <- quality_df[i, , drop = FALSE]
      status_txt <- row$status[[1]]
      bg_color <- switch(status_txt,
        PASS = "rgba(93,184,114,0.18)",
        WARN = "rgba(212,160,23,0.18)",
        FAIL = "rgba(198,69,69,0.15)",
        "rgba(230,223,216,0.6)")
      fg_color <- switch(status_txt,
        PASS = "#2d5c38",
        WARN = "#7a5a06",
        FAIL = "#8b2e2e",
        "#252523")
      tags$div(class = "qc-tile",
        tags$div(class = "qc-head",
          tags$div(class = "qc-name", row$dimension[[1]]),
          tags$span(class = "qc-status",
            style = sprintf("background:%s; color:%s;", bg_color, fg_color),
            status_txt)),
        tags$div(class = "qc-detail", row$detail[[1]])
      )
    })

    tagList(
      summary_row,
      tags$div(class = "qc-grid", tiles)
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

  output$download_report <- downloadHandler(
    filename = function() {
      result <- report_result()
      sym <- if (!is.null(result)) result$symbol else "report"
      paste0("report_", sym, "_", Sys.Date(), ".html")
    },
    content = function(file) {
      result <- report_result()
      if (is.null(result) || !isTRUE(result$ok)) {
        writeLines("<html><body><h2>No report available. Generate a report first.</h2></body></html>", file)
        return()
      }
      html <- build_report_download_html(result)
      writeLines(html, file, useBytes = TRUE)
    }
  )

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
  # Usage:
  #   ?action=refresh-financials&token=SECRET
  #   ?action=refresh-market-data&token=SECRET
  #   ?action=refresh-all&token=SECRET
  # ----------------------------

  observe({
    query <- parseQueryString(session$clientData$url_search)
    action <- query$action %||% ""
    if (!action %in% c("refresh-financials", "refresh-market-data", "refresh-all")) return()

    expected_token <- Sys.getenv("REFRESH_TOKEN")
    if (!nzchar(expected_token) || !identical(query$token, expected_token)) {
      message("[Refresh] Unauthorized attempt")
      return()
    }

    if (action %in% c("refresh-financials", "refresh-all")) {
      message("[Refresh] Cron triggered - refreshing financial CSVs...")
      tryCatch({
        run_financial_refresh_script()
        refresh_status(load_latest_refresh_status())
        message("[Refresh] Financial CSVs updated successfully at ", Sys.time())
      }, error = function(e) {
        message("[Refresh] Financial refresh error: ", e$message)
      })
    }

    if (action %in% c("refresh-market-data", "refresh-all")) {
      message("[Refresh] Cron triggered - refreshing market data exports...")
      refreshed <- tryCatch(
        refresh_market_data_exports(include_financials = FALSE),
        error = function(e) list(ok = FALSE, error = conditionMessage(e))
      )
      if (isTRUE(refreshed$ok)) {
        stock_data(list(daily = refreshed$daily, news = refreshed$news))
        news_data(refreshed$news)
        rag_history(refreshed$rag)
        news_summary(load_latest_news_summary())
        refresh_status(load_latest_refresh_status())
        last_updated(refreshed$updated_at %||% safe_now())
        message("[Refresh] Market data exports updated successfully at ", Sys.time())
      } else {
        message("[Refresh] Market data refresh error: ", refreshed$error %||% "unknown error")
      }
    }
  })
}
