# ui.R
# Shiny UI definition

app_css <- HTML("
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap');

  body {
    background: #080d19 !important;
    color: #e5e7eb;
    font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
  }

  /* ---- Navbar ---- */
  .navbar {
    background: rgba(8,13,25,0.85) !important;
    backdrop-filter: blur(16px) saturate(1.4);
    -webkit-backdrop-filter: blur(16px) saturate(1.4);
    border-bottom: 1px solid rgba(99,102,241,0.15);
    box-shadow: 0 1px 24px rgba(0,0,0,0.45);
    min-height: 52px;
  }
  .navbar-brand {
    color: #f8fafc !important; font-weight: 700; letter-spacing: .5px; font-size: 17px;
    background: linear-gradient(135deg, #60a5fa, #a78bfa);
    -webkit-background-clip: text; -webkit-text-fill-color: transparent;
  }
  .navbar-nav > li > a {
    color: #94a3b8 !important; font-weight: 500; font-size: 14px;
    transition: color .2s;
  }
  .navbar-nav > li > a:hover, .navbar-nav > .active > a {
    color: #f8fafc !important; background: transparent !important;
  }
  .navbar-nav > .active > a {
    border-bottom: 2px solid #6366f1; padding-bottom: 13px;
  }
  .tab-content { padding: 68px 18px 24px; max-width: 1320px; margin: 0 auto; }

  /* ---- Glass card ---- */
  .panel-card {
    border: 1px solid rgba(99,102,241,0.12);
    border-radius: 16px;
    background: rgba(15,23,42,0.6);
    backdrop-filter: blur(12px);
    -webkit-backdrop-filter: blur(12px);
    padding: 20px;
    margin-top: 16px;
    box-shadow: 0 4px 24px rgba(0,0,0,0.3), inset 0 1px 0 rgba(255,255,255,0.04);
    transition: box-shadow .25s, border-color .25s;
    overflow: visible;
  }
  .panel-card:hover {
    border-color: rgba(99,102,241,0.25);
    box-shadow: 0 8px 32px rgba(0,0,0,0.4), inset 0 1px 0 rgba(255,255,255,0.06);
  }

  /* ---- Metric cards (live quotes) ---- */
  .metric-card {
    border: 1px solid rgba(99,102,241,0.12);
    background: linear-gradient(160deg, rgba(30,41,59,0.7), rgba(15,23,42,0.85));
    border-radius: 14px; padding: 16px; margin-bottom: 12px;
    box-shadow: 0 4px 20px rgba(2,6,23,0.4);
    transition: transform .2s, box-shadow .2s, border-color .2s;
  }
  .metric-card:hover {
    transform: translateY(-2px);
    box-shadow: 0 8px 28px rgba(2,6,23,0.5);
    border-color: rgba(99,102,241,0.3);
  }
  .metric-title { font-size: 12px; color: #94a3b8; letter-spacing: .3px; text-transform: uppercase; font-weight: 600; }
  .metric-value { font-size: 28px; font-weight: 700; color: #f8fafc; margin-top: 6px; }
  .metric-sub { margin-top: 4px; color: #cbd5e1; font-size: 13px; }
  .trend-up { color: #34d399; font-weight: 700; }
  .trend-down { color: #f87171; font-weight: 700; }
  .trend-flat { color: #cbd5e1; font-weight: 700; }

  /* ---- Section titles ---- */
  .section-title {
    margin: 8px 0 16px; color: #f1f5f9; font-size: 20px;
    font-weight: 700; letter-spacing: .2px;
    display: flex; align-items: center; gap: 10px;
  }
  .section-title::before {
    content: '';
    width: 4px; height: 22px;
    background: linear-gradient(180deg, #6366f1, #818cf8);
    border-radius: 4px;
    display: inline-block;
  }

  /* ---- Buttons ---- */
  .btn {
    border-radius: 10px; font-weight: 600; font-size: 13px;
    letter-spacing: .2px;
    transition: transform .15s, box-shadow .2s, filter .2s;
    padding: 8px 20px;
  }
  .btn:active { transform: scale(0.97); }
  .btn-primary {
    background: linear-gradient(135deg, #3b82f6, #6366f1); border: none;
    box-shadow: 0 2px 12px rgba(99,102,241,0.3);
  }
  .btn-primary:hover { filter: brightness(1.12); box-shadow: 0 4px 18px rgba(99,102,241,0.4); background: linear-gradient(135deg, #3b82f6, #6366f1); }
  .btn-success {
    background: linear-gradient(135deg, #16a34a, #059669); border: none;
    box-shadow: 0 2px 12px rgba(22,163,74,0.3);
  }
  .btn-success:hover { filter: brightness(1.12); box-shadow: 0 4px 18px rgba(22,163,74,0.4); background: linear-gradient(135deg, #16a34a, #059669); }
  .btn-warning {
    background: linear-gradient(135deg, #d97706, #ca8a04); border: none; color: #fff;
    box-shadow: 0 2px 12px rgba(202,138,4,0.3);
  }
  .btn-warning:hover { filter: brightness(1.12); background: linear-gradient(135deg, #d97706, #ca8a04); color: #fff; }
  .btn-info {
    background: linear-gradient(135deg, #0891b2, #0e7490); border: none;
    box-shadow: 0 2px 12px rgba(8,145,178,0.3);
  }
  .btn-info:hover { filter: brightness(1.12); background: linear-gradient(135deg, #0891b2, #0e7490); }

  /* ---- DataTables ---- */
  table.dataTable thead th {
    background: rgba(30,41,59,0.8) !important; color: #c7d2fe !important;
    font-weight: 600; font-size: 12px; text-transform: uppercase; letter-spacing: .4px;
    border-bottom: 1px solid rgba(99,102,241,0.15) !important;
  }
  .dt-container .dataTables_wrapper { color: #e2e8f0; }
  table.dataTable tbody td {
    color: #e5e7eb !important; padding: 8px 12px !important; font-size: 13px;
    white-space: nowrap; border-bottom: 1px solid rgba(51,65,85,0.4) !important;
  }
  table.dataTable thead th { padding: 10px 12px !important; font-size: 12px; white-space: nowrap; }
  table.dataTable tbody tr { background: rgba(17,24,39,0.5) !important; transition: background .15s; }
  table.dataTable tbody tr:hover { background: rgba(30,41,59,0.7) !important; }
  table.dataTable tbody tr:nth-child(even) { background: rgba(15,23,42,0.5) !important; }
  .dataTables_info, .dataTables_paginate { color: #94a3b8 !important; }
  .dataTables_paginate .paginate_button {
    color: #94a3b8 !important; border-radius: 6px !important;
    transition: background .15s;
  }
  .dataTables_paginate .paginate_button:hover { background: rgba(99,102,241,0.15) !important; color: #e5e7eb !important; }
  .dataTables_paginate .paginate_button.current { background: rgba(99,102,241,0.3) !important; color: #f8fafc !important; }

  /* ---- Forecast box ---- */
  .forecast-box {
    border: 1px solid rgba(99,102,241,0.15);
    border-left: 4px solid #6366f1;
    background: rgba(17,24,39,0.6); border-radius: 12px; padding: 18px; margin-top: 12px;
  }
  .forecast-title { font-size: 20px; font-weight: 700; margin-bottom: 8px; }

  /* ---- Chips ---- */
  .chip { display: inline-block; padding: 4px 12px; border-radius: 999px; font-size: 12px; margin-right: 8px; margin-bottom: 4px; font-weight: 500; }
  .chip-up { background: rgba(34,197,94,0.15); color: #86efac; border: 1px solid rgba(34,197,94,0.35); }
  .chip-down { background: rgba(248,113,113,0.15); color: #fca5a5; border: 1px solid rgba(248,113,113,0.3); }
  .chip-flat { background: rgba(148,163,184,0.15); color: #cbd5e1; border: 1px solid rgba(148,163,184,0.35); }

  .report-section { margin-bottom: 20px; }

  /* ---- Form controls (dark theme) ---- */
  .form-control, .selectize-input, .selectize-dropdown {
    background: rgba(30,41,59,0.95) !important; color: #e5e7eb !important;
    border: 1px solid rgba(99,102,241,0.15) !important;
    border-radius: 8px !important;
  }
  .selectize-dropdown {
    z-index: 99999 !important;
    box-shadow: 0 8px 32px rgba(0,0,0,0.6) !important;
    max-height: 320px !important;
  }
  .selectize-dropdown-content {
    max-height: 300px !important;
    overflow-y: auto !important;
  }
  .tab-content, .tab-pane, .container-fluid, .row, .col-sm-3, .col-sm-4, .col-sm-6, .col-sm-12 {
    overflow: visible !important;
  }
  .selectize-dropdown-content .option {
    padding: 8px 14px !important; color: #e5e7eb !important;
  }
  .selectize-dropdown-content .option:hover, .selectize-dropdown-content .option.active {
    background: rgba(99,102,241,0.3) !important; color: #f8fafc !important;
  }
  .control-label { color: #94a3b8 !important; font-weight: 600; font-size: 12px; text-transform: uppercase; letter-spacing: .3px; }
  .radio label, .checkbox label { color: #cbd5e1; }

  /* ---- Scrollbar ---- */
  ::-webkit-scrollbar { width: 8px; height: 8px; }
  ::-webkit-scrollbar-track { background: #0b1220; }
  ::-webkit-scrollbar-thumb { background: #334155; border-radius: 4px; }
  ::-webkit-scrollbar-thumb:hover { background: #475569; }

  /* ---- Smooth animations ---- */
  .shiny-plot-output, .plotly, .datatables { animation: fadeUp .4s ease-out; }
  @keyframes fadeUp {
    from { opacity: 0; transform: translateY(12px); }
    to { opacity: 1; transform: translateY(0); }
  }

  /* ---- Alert overrides ---- */
  .alert-danger { background: rgba(239,68,68,0.15); border: 1px solid rgba(239,68,68,0.3); color: #fca5a5; border-radius: 10px; }
  .alert-warning { background: rgba(234,179,8,0.12); border: 1px solid rgba(234,179,8,0.25); color: #fde68a; border-radius: 10px; }
  .alert-info { background: rgba(59,130,246,0.12); border: 1px solid rgba(59,130,246,0.25); color: #93c5fd; border-radius: 10px; }

  /* ---- Responsive ---- */
  @media (max-width: 768px) {
    .tab-content { padding: 60px 10px 16px; }
    .section-title { font-size: 18px; }
    .metric-value { font-size: 22px; }
    .panel-card { padding: 14px; border-radius: 12px; }
  }
  @media (max-width: 480px) {
    .metric-card { flex: 1 1 100% !important; }
    .panel-card { padding: 10px; }
    .forecast-title { font-size: 16px; }
  }
")

ui <- navbarPage(
  title = tags$span(icon("chart-line", style = "margin-right:6px;"), "Market Insight Studio"),
  id = "main_tabs",
  inverse = TRUE,
  collapsible = TRUE,
  position = "fixed-top",
  header = tags$head(
    tags$style(app_css),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css")
  ),

  # ---- Tab 1: Data ----
  tabPanel(
    tags$span(icon("database", style = "margin-right:4px;"), "Data"),
    fluidPage(uiOutput("data_page_content"))
  ),

  # ---- Tab 2: News ----
  tabPanel(
    tags$span(icon("newspaper", style = "margin-right:4px;"), "News"),
    fluidPage(
      uiOutput("news_summary_ui"),
      tags$div(
        class = "panel-card", style = "margin-top:4px; padding:16px 20px;",
        fluidRow(
          column(3, selectInput("news_ticker", "Filter by Stock",
                                choices = c("ALL" = "ALL", "MACRO - Geopolitical/Macro" = "MACRO",
                                            setNames(names(TICKER_LABELS), paste0(names(TICKER_LABELS), " - ", TICKER_LABELS))),
                                selected = "ALL")),
          column(3, radioButtons("news_window", "Date Range",
                                 choices = c("7D" = 7L, "30D" = 30L, "90D" = 90L),
                                 selected = 7L, inline = TRUE)),
          column(3, tags$div(style = "padding-top:24px;",
            actionButton("refresh_news", tags$span(icon("sync"), " Refresh"), class = "btn-warning")
          )),
          column(3, tags$div(style = "padding-top:28px; color:#64748b; font-size:12px;",
            icon("circle-info", style = "margin-right:4px;"), "Source: Finnhub API"))
        )
      ),
      tags$div(class = "panel-card", DTOutput("news_table")),
      fluidRow(column(12, uiOutput("news_error")))
    )
  ),

  # ---- Tab 3: Backtest ----
  tabPanel(
    tags$span(icon("chart-area", style = "margin-right:4px;"), "Backtest"),
    fluidPage(
      tags$div(
        class = "panel-card", style = "margin-top:4px; padding:16px 20px;",
        fluidRow(
          column(3, selectInput("backtest_ticker", "Select Stock",
                                choices = setNames(TICKERS, paste0(TICKERS, " - ", TICKER_LABELS[TICKERS])),
                                selected = "NVDA")),
          column(3, selectInput("backtest_model", "Decision Model",
                                choices = BACKTEST_MODEL_CHOICES,
                                selected = unname(BACKTEST_MODEL_CHOICES[[1]]))),
          column(3, selectInput("backtest_strategy_mode", "Strategy Mode",
                                choices = BACKTEST_STRATEGY_MODES,
                                selected = unname(BACKTEST_STRATEGY_MODES[[1]]))),
          column(3, dateRangeInput("backtest_custom_range", "Custom Range",
                                   start = Sys.Date() - 7, end = Sys.Date(),
                                   startview = "month", separator = "to"))
        ),
        fluidRow(
          column(6, tags$div(style = "padding-top:24px; display:flex; gap:8px; align-items:center; flex-wrap:wrap;",
            actionButton("run_backtest_action", tags$span(icon("play"), " Run Agent Backtest"), class = "btn-success"),
            actionButton("load_latest_backtest_action", tags$span(icon("folder-open"), " Load Latest"), class = "btn-info")
          )),
          column(6, tags$div(style = "padding-top:28px; color:#64748b; font-size:12px; text-align:right;",
            icon("shield-halved", style = "margin-right:4px;"),
            "$10,000 borrowed capital simulation. Fixed masked lookback: 90 trading days. Each decision is mask-safe: no future data."))
        )
      ),
      uiOutput("backtest_error_ui"),
      uiOutput("backtest_header_ui"),
      uiOutput("backtest_metrics_ui"),
      tags$div(class = "panel-card", plotlyOutput("backtest_equity_chart", height = "520px")),
      fluidRow(
        column(8, tags$div(class = "panel-card",
          h3("LLM Decision Rationale", class = "section-title"),
          uiOutput("backtest_explanation_ui"))),
        column(4, tags$div(class = "panel-card",
          h3("Run Setup", class = "section-title"),
          uiOutput("backtest_setup_ui")))
      ),
      tags$div(class = "panel-card",
        h3("Executed Trades", class = "section-title"),
        DTOutput("backtest_trades_table")),
      fluidRow(
        column(4, tags$div(class = "panel-card",
          h3("Quality Check", class = "section-title"),
          uiOutput("backtest_quality_card_ui")))
      )
    )
  ),

  # ---- Tab 4: Reporting (chart + full analyst report) ----
  tabPanel(
    tags$span(icon("chart-bar", style = "margin-right:4px;"), "Reporting"),
    fluidPage(
      tags$div(
        class = "panel-card", style = "margin-top:4px; padding:16px 20px;",
        fluidRow(
          column(3, selectInput("ai_ticker", "Select Stock",
                                choices = setNames(TICKERS, paste0(TICKERS, " - ", TICKER_LABELS[TICKERS])),
                                selected = "NVDA")),
          column(3, radioButtons("forecast_window", "Forecast Window",
                                 choices = c("7D" = 7L, "30D" = 30L, "90D" = 90L),
                                 selected = 30L, inline = TRUE)),
          column(3, tags$div(style = "padding-top:24px; display:flex; gap:8px; align-items:center;",
            actionButton("run_prediction", tags$span(icon("wand-magic-sparkles"), " Generate Report"),
                         class = "btn-success"),
            downloadButton("download_report", tags$span(icon("file-arrow-down")),
                           class = "btn-info", style = "padding:8px 14px; border-radius:10px;")
          )),
          column(3, tags$div(style = "padding-top:28px; color:#64748b; font-size:12px;",
            icon("robot", style = "margin-right:4px;color:#818cf8;"),
            "AI Multi-Analyst Team (GPT-4o-mini)"))
        )
      ),
      fluidRow(column(12, uiOutput("ai_error"))),
      tags$div(class = "panel-card", plotlyOutput("combined_forecast_chart", height = "540px")),
      fluidRow(column(12, uiOutput("report_content")))
    )
  )
)
