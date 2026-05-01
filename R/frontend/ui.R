# ui.R
# Shiny UI definition

app_css <- HTML("
  /* Claude design system (DESIGN.md) — cream canvas, coral primary, editorial serif */
  @import url('https://fonts.googleapis.com/css2?family=Cormorant+Garamond:ital,wght@0,400;0,500;0,600;1,400&family=Inter:wght@400;500;600&family=JetBrains+Mono:wght@400&display=swap');

  :root {
    --canvas: #faf9f5;
    --ink: #141413;
    --body: #3d3d3a;
    --body-strong: #252523;
    --muted: #6c6a64;
    --muted-soft: #8e8b82;
    --hairline: #e6dfd8;
    --hairline-soft: #ebe6df;
    --surface-card: #efe9de;
    --surface-soft: #f5f0e8;
    --primary: #cc785c;
    --primary-active: #a9583e;
    --on-primary: #ffffff;
    --surface-dark: #181715;
    --surface-dark-elevated: #252320;
    --success: #5db872;
    --warning: #d4a017;
    --error: #c64545;
    --accent-teal: #5db8a6;
    --accent-amber: #e8a55a;
    --rounded-md: 8px;
    --rounded-lg: 12px;
  }

  body {
    background: var(--canvas) !important;
    color: var(--body);
    font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
    font-size: 16px;
    line-height: 1.55;
  }

  /* ---- Navbar (cream top-nav, 64px) ---- */
  .navbar-default {
    background-color: var(--canvas) !important;
    border-bottom: 1px solid var(--hairline) !important;
    box-shadow: none !important;
    min-height: 64px;
  }
  .navbar-default .navbar-brand {
    color: var(--ink) !important;
    font-family: 'Cormorant Garamond', 'Times New Roman', serif;
    font-weight: 500;
    font-size: 1.35rem;
    letter-spacing: -0.02em;
    line-height: 1.2;
    padding-top: 20px;
    padding-bottom: 20px;
  }
  .navbar-default .navbar-nav > li > a {
    color: var(--muted) !important;
    font-weight: 500;
    font-size: 14px;
    line-height: 1.4;
    padding-top: 22px;
    padding-bottom: 22px;
    transition: color .15s, background .15s;
  }
  .navbar-default .navbar-nav > li > a:hover {
    color: var(--ink) !important;
    background: transparent !important;
  }
  .navbar-default .navbar-nav > .active > a,
  .navbar-default .navbar-nav > .active > a:hover,
  .navbar-default .navbar-nav > .active > a:focus {
    color: var(--ink) !important;
    background: var(--surface-card) !important;
    border-radius: var(--rounded-md);
    margin: 14px 2px 0;
    padding: 8px 14px !important;
  }
  .navbar-default .navbar-toggle {
    border-color: var(--hairline);
    margin-top: 14px;
  }
  .navbar-default .navbar-toggle .icon-bar { background-color: var(--ink); }
  .tab-content { padding: 76px 18px 32px; max-width: 1200px; margin: 0 auto; }

  /* ---- Feature-style cards ---- */
  .panel-card {
    border: 1px solid var(--hairline);
    border-radius: var(--rounded-lg);
    background: var(--surface-card);
    padding: 20px;
    margin-top: 16px;
    box-shadow: none;
    transition: border-color .2s;
    overflow: visible;
  }
  .panel-card:hover { border-color: var(--hairline-soft); }

  /* ---- Metric / quote tiles ---- */
  .metric-card {
    border: 1px solid var(--hairline);
    background: var(--canvas);
    border-radius: var(--rounded-lg);
    padding: 16px;
    margin-bottom: 12px;
    box-shadow: none;
    transition: border-color .2s;
  }
  .metric-card:hover { border-color: var(--muted-soft); }
  .metric-title {
    font-size: 12px;
    color: var(--muted);
    letter-spacing: 0.04em;
    text-transform: uppercase;
    font-weight: 500;
  }
  .metric-value {
    font-size: 26px;
    font-weight: 600;
    color: var(--ink);
    margin-top: 6px;
    font-variant-numeric: tabular-nums;
  }
  .metric-sub { margin-top: 4px; color: var(--muted); font-size: 13px; }
  .trend-up { color: var(--success); font-weight: 600; }
  .trend-down { color: var(--error); font-weight: 600; }
  .trend-flat { color: var(--muted); font-weight: 600; }

  /* ---- Section titles (serif display voice) ---- */
  .section-title {
    margin: 8px 0 16px;
    color: var(--ink);
    font-family: 'Cormorant Garamond', 'Times New Roman', serif;
    font-size: 1.5rem;
    font-weight: 500;
    letter-spacing: -0.02em;
    line-height: 1.2;
    display: flex;
    align-items: center;
    gap: 10px;
  }
  .section-title::before {
    content: '';
    width: 3px;
    height: 1.1em;
    background: var(--primary);
    border-radius: 2px;
    display: inline-block;
  }

  /* ---- Buttons ---- */
  .btn {
    border-radius: var(--rounded-md);
    font-weight: 500;
    font-size: 14px;
    line-height: 1;
    letter-spacing: 0;
    transition: background .15s, border-color .15s, transform .1s;
    padding: 12px 20px;
    min-height: 40px;
  }
  .btn:active { transform: scale(0.98); }
  .btn-primary {
    background: var(--primary) !important;
    border: none !important;
    color: var(--on-primary) !important;
    box-shadow: none;
  }
  .btn-primary:hover, .btn-primary:focus {
    background: var(--primary-active) !important;
    color: var(--on-primary) !important;
    border: none !important;
  }
  .btn-success {
    background: var(--primary) !important;
    border: none !important;
    color: var(--on-primary) !important;
  }
  .btn-success:hover, .btn-success:focus {
    background: var(--primary-active) !important;
    color: var(--on-primary) !important;
  }
  .btn-info, .btn-default {
    background: var(--canvas) !important;
    border: 1px solid var(--hairline) !important;
    color: var(--ink) !important;
    box-shadow: none;
  }
  .btn-info:hover, .btn-info:focus,
  .btn-default:hover, .btn-default:focus {
    background: var(--surface-soft) !important;
    border-color: var(--hairline) !important;
    color: var(--ink) !important;
  }
  .btn-warning {
    background: var(--canvas) !important;
    border: 1px solid var(--accent-amber) !important;
    color: var(--body-strong) !important;
  }
  .btn-warning:hover, .btn-warning:focus {
    background: var(--surface-soft) !important;
    border-color: var(--accent-amber) !important;
    color: var(--ink) !important;
  }

  /* ---- DataTables ---- */
  table.dataTable thead th {
    background: var(--surface-soft) !important;
    color: var(--body-strong) !important;
    font-weight: 500;
    font-size: 12px;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    border-bottom: 1px solid var(--hairline) !important;
  }
  .dt-container .dataTables_wrapper { color: var(--body); }
  table.dataTable tbody td {
    color: var(--body) !important;
    padding: 8px 12px !important;
    font-size: 14px;
    white-space: nowrap;
    border-bottom: 1px solid var(--hairline-soft) !important;
  }
  table.dataTable thead th { padding: 10px 12px !important; font-size: 12px; white-space: nowrap; }
  table.dataTable tbody tr { background: var(--canvas) !important; transition: background .15s; }
  table.dataTable tbody tr:hover { background: var(--surface-soft) !important; }
  table.dataTable tbody tr:nth-child(even) { background: rgba(245,240,232,0.5) !important; }
  .dataTables_info, .dataTables_paginate { color: var(--muted) !important; }
  .dataTables_paginate .paginate_button {
    color: var(--muted) !important;
    border-radius: var(--rounded-md) !important;
    transition: background .15s;
  }
  .dataTables_paginate .paginate_button:hover {
    background: var(--surface-card) !important;
    color: var(--ink) !important;
  }
  .dataTables_paginate .paginate_button.current {
    background: var(--surface-card) !important;
    color: var(--ink) !important;
    border: 1px solid var(--hairline) !important;
  }

  .forecast-box {
    border: 1px solid var(--hairline);
    border-left: 4px solid var(--primary);
    background: var(--surface-soft);
    border-radius: var(--rounded-lg);
    padding: 18px;
    margin-top: 12px;
  }
  .forecast-title {
    font-family: 'Cormorant Garamond', serif;
    font-size: 1.35rem;
    font-weight: 500;
    color: var(--ink);
    margin-bottom: 8px;
  }

  .chip { display: inline-block; padding: 4px 12px; border-radius: 999px; font-size: 12px; margin-right: 8px; margin-bottom: 4px; font-weight: 500; }
  .chip-up { background: rgba(93,184,114,0.15); color: var(--body-strong); border: 1px solid rgba(93,184,114,0.35); }
  .chip-down { background: rgba(198,69,69,0.1); color: var(--error); border: 1px solid rgba(198,69,69,0.35); }
  .chip-flat { background: var(--surface-soft); color: var(--muted); border: 1px solid var(--hairline); }

  .report-section { margin-bottom: 20px; }

  .form-control, .selectize-input, .selectize-dropdown {
    background: var(--canvas) !important;
    color: var(--ink) !important;
    border: 1px solid var(--hairline) !important;
    border-radius: var(--rounded-md) !important;
  }
  .form-control:focus {
    border-color: var(--primary) !important;
    box-shadow: 0 0 0 3px rgba(204,120,92,0.15) !important;
  }
  .selectize-dropdown {
    z-index: 99999 !important;
    box-shadow: 0 4px 24px rgba(20,20,19,0.08) !important;
    max-height: 320px !important;
    border: 1px solid var(--hairline) !important;
  }
  .selectize-dropdown-content { max-height: 300px !important; overflow-y: auto !important; }
  .tab-content, .tab-pane, .container-fluid, .row, .col-sm-3, .col-sm-4, .col-sm-6, .col-sm-12 {
    overflow: visible !important;
  }
  .selectize-dropdown-content .option {
    padding: 8px 14px !important;
    color: var(--ink) !important;
  }
  .selectize-dropdown-content .option:hover, .selectize-dropdown-content .option.active {
    background: var(--surface-card) !important;
    color: var(--ink) !important;
  }
  .control-label {
    color: var(--muted) !important;
    font-weight: 500;
    font-size: 12px;
    text-transform: uppercase;
    letter-spacing: 0.06em;
  }
  .radio label, .checkbox label { color: var(--body); }

  ::-webkit-scrollbar { width: 8px; height: 8px; }
  ::-webkit-scrollbar-track { background: var(--surface-soft); }
  ::-webkit-scrollbar-thumb { background: var(--hairline); border-radius: 4px; }
  ::-webkit-scrollbar-thumb:hover { background: var(--muted-soft); }

  .shiny-plot-output, .plotly, .datatables { animation: fadeUp .4s ease-out; }
  @keyframes fadeUp {
    from { opacity: 0; transform: translateY(12px); }
    to { opacity: 1; transform: translateY(0); }
  }

  .hero-band {
    padding: 16px 18px 12px;
    max-width: 1200px;
    margin: 0 auto;
    border-bottom: 1px solid var(--hairline);
  }
  .hero-sub { color: var(--body); margin: 0; font-size: 16px; line-height: 1.55; }
  .hero-sub-2 { color: var(--muted); font-size: 14px; margin: 4px 0 12px; }
  .stepper { display:flex; gap:8px; flex-wrap:wrap; color: var(--muted); font-size: 13px; font-weight: 500; align-items:center; }
  .stepper .step {
    padding: 6px 12px;
    border-radius: 999px;
    background: var(--surface-card);
    border: 1px solid var(--hairline);
  }
  .stepper .arrow { color: var(--muted-soft); font-weight: 600; }

  .status-dot { display:inline-block; width:8px; height:8px; border-radius:50%; margin-right:6px; vertical-align:middle; }
  .status-dot.ok { background: var(--success); }
  .status-dot.warn { background: var(--warning); }
  .data-status-pill {
    display:inline-flex; align-items:center; gap:8px; padding:6px 14px;
    background: var(--surface-soft);
    border: 1px solid var(--hairline);
    border-radius: 999px;
    font-size: 12px;
    color: var(--body);
  }

  .badge-prompt {
    display:inline-flex; align-items:center; gap:6px;
    padding: 4px 12px;
    border-radius: 999px;
    font-size: 12px;
    font-weight: 500;
    letter-spacing: 0.06em;
    text-transform: uppercase;
    background: var(--surface-card);
    color: var(--ink);
    border: 1px solid var(--hairline);
  }

  .qc-grid { display:grid; grid-template-columns:repeat(auto-fit, minmax(220px, 1fr)); gap:10px; margin-top:8px; }
  .qc-tile {
    padding: 12px 14px;
    border-radius: var(--rounded-lg);
    background: var(--canvas);
    border: 1px solid var(--hairline);
  }
  .qc-tile .qc-head { display:flex; justify-content:space-between; align-items:center; gap:8px; }
  .qc-tile .qc-name { color: var(--ink); font-size: 13px; font-weight: 500; }
  .qc-tile .qc-status { font-size: 10px; font-weight: 500; letter-spacing: 0.04em; padding: 2px 8px; border-radius: 999px; }
  .qc-tile .qc-detail { color: var(--muted); font-size: 12px; line-height: 1.55; margin-top: 6px; }

  .load-container .loader { color: var(--primary) !important; }

  .alert-danger {
    background: rgba(198,69,69,0.08);
    border: 1px solid rgba(198,69,69,0.25);
    color: var(--error);
    border-radius: var(--rounded-lg);
  }
  .alert-warning {
    background: rgba(212,160,23,0.08);
    border: 1px solid rgba(212,160,23,0.3);
    color: var(--body-strong);
    border-radius: var(--rounded-lg);
  }
  .alert-info {
    background: rgba(93,184,166,0.08);
    border: 1px solid rgba(93,184,166,0.25);
    color: var(--body-strong);
    border-radius: var(--rounded-lg);
  }

  .modal-content {
    background: var(--canvas);
    border: 1px solid var(--hairline);
    border-radius: var(--rounded-lg);
    color: var(--body);
  }
  .modal-header { border-bottom-color: var(--hairline); }
  .modal-footer { border-top-color: var(--hairline); }

  @media (max-width: 768px) {
    .tab-content { padding: 68px 10px 20px; }
    .section-title { font-size: 1.35rem; }
    .metric-value { font-size: 22px; }
    .panel-card { padding: 16px; }
  }
  @media (max-width: 480px) {
    .metric-card { flex: 1 1 100% !important; }
    .panel-card { padding: 12px; }
    .forecast-title { font-size: 1.2rem; }
  }
")

ui <- navbarPage(
  title = tags$span(icon("chart-line", style = "margin-right:8px; color:#cc785c;"), "Market Insight Studio"),
  id = "main_tabs",
  inverse = FALSE,
  collapsible = TRUE,
  position = "fixed-top",
  header = tagList(
    tags$head(
      tags$style(app_css),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css")
    ),
    shinyjs::useShinyjs()
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
          column(3, tags$div(class = "text-muted", style = "padding-top:28px; font-size:12px; color:#6c6a64 !important;",
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
      tags$div(class = "hero-band",
        tags$p(class = "hero-sub",
          "Algorithmic backtesting + AI-analyst rationale for retail equity traders."),
        tags$p(class = "hero-sub-2",
          "$10K mask-safe simulations · 7-validator quality control · transparent decision logs."),
        tags$div(class = "stepper",
          tags$span(class = "step", "1. Pick a stock"),
          tags$span(class = "arrow", "→"),
          tags$span(class = "step", "2. Choose strategy mode"),
          tags$span(class = "arrow", "→"),
          tags$span(class = "step", "3. Run Agent Backtest"),
          tags$span(class = "arrow", "→"),
          tags$span(class = "step", "4. Read Quality Check + Rationale")
        )
      ),
      tags$div(
        class = "panel-card", style = "margin-top:8px; padding:16px 20px;",
        fluidRow(
          column(3, selectInput("backtest_ticker", "Select Stock",
                                choices = setNames(TICKERS, paste0(TICKERS, " - ", TICKER_LABELS[TICKERS])),
                                selected = "NVDA")),
          column(3, selectInput("backtest_model", "Decision Model",
                                choices = BACKTEST_MODEL_CHOICES,
                                selected = unname(BACKTEST_MODEL_CHOICES[[1]]))),
          column(3,
            selectInput("backtest_strategy_mode", "Strategy Mode",
                        choices = BACKTEST_STRATEGY_MODES,
                        selected = unname(BACKTEST_STRATEGY_MODES[[1]])),
            tags$div(style = "color:#6c6a64; font-size:11px; line-height:1.45; margin-top:-8px;",
              tags$b("Auto"), ": agent picks. ",
              tags$b("Trend"), ": ride momentum. ",
              tags$b("Mean Rev."), ": bet on bouncebacks. ",
              tags$b("Event"), ": react to news. ",
              tags$b("Defensive"), ": cash-heavy.")
          ),
          column(3, dateRangeInput("backtest_custom_range", "Custom Range",
                                   start = Sys.Date() - 7, end = Sys.Date(),
                                   startview = "month", separator = "to"))
        ),
        fluidRow(
          column(8, tags$div(style = "padding-top:8px; display:flex; gap:8px; align-items:center; flex-wrap:wrap;",
            actionButton("run_backtest_action", tags$span(icon("play"), " Run Agent Backtest"), class = "btn-success"),
            actionButton("load_latest_backtest_action", tags$span(icon("folder-open"), " Load Latest"), class = "btn-info"),
            tags$span(class = "data-status-pill", style = "margin-left:6px;",
              icon("shield-halved"),
              tags$span("Mask-safe · 90D lookback · $10K sim"))
          )),
          column(4, tags$div(style = "padding-top:14px; color:#6c6a64; font-size:11.5px; text-align:right;",
            "Each decision sees only data available on that date. No future prices. No look-ahead."))
        )
      ),
      uiOutput("backtest_error_ui"),
      uiOutput("backtest_header_ui"),
      uiOutput("backtest_metrics_ui"),
      tags$div(class = "panel-card",
        shinycssloaders::withSpinner(
          plotlyOutput("backtest_equity_chart", height = "520px"),
          type = 8, color = "#cc785c", size = 0.8)),
      fluidRow(
        column(8, tags$div(class = "panel-card",
          h3("LLM Decision Rationale", class = "section-title"),
          shinycssloaders::withSpinner(uiOutput("backtest_explanation_ui"),
                                       type = 8, color = "#cc785c", size = 0.6,
                                       proxy.height = "120px"))),
        column(4, tags$div(class = "panel-card",
          h3("Run Setup", class = "section-title"),
          uiOutput("backtest_setup_ui")))
      ),
      tags$div(class = "panel-card",
        h3("Executed Trades", class = "section-title"),
        DTOutput("backtest_trades_table")),
      fluidRow(
        column(12, tags$div(class = "panel-card",
          tags$div(style = "display:flex; justify-content:space-between; align-items:center; gap:12px; flex-wrap:wrap; margin-bottom:6px;",
            h3("Quality Check", class = "section-title", style = "margin:0;"),
            tags$span(class = "badge-prompt",
              icon("shield-halved"),
              tags$span("Prompt v2.3 · 7 validators · mask-safe ✓"))),
          shinycssloaders::withSpinner(uiOutput("backtest_quality_card_ui"),
                                       type = 8, color = "#cc785c", size = 0.6,
                                       proxy.height = "120px")))
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
          column(3, tags$div(style = "padding-top:28px; color:#6c6a64; font-size:12px;",
            icon("robot", style = "margin-right:4px;color:#cc785c;"),
            "AI Multi-Analyst Team (GPT-4o-mini)"))
        )
      ),
      fluidRow(column(12, uiOutput("ai_error"))),
      tags$div(class = "panel-card",
        shinycssloaders::withSpinner(
          plotlyOutput("combined_forecast_chart", height = "540px"),
          type = 8, color = "#cc785c", size = 0.8)),
      fluidRow(column(12,
        shinycssloaders::withSpinner(uiOutput("report_content"),
                                     type = 8, color = "#cc785c", size = 0.8,
                                     proxy.height = "300px",
                                     caption = HTML("<span style='color:#6c6a64;'>Drafting institutional analyst report — Industry → Thesis → Risks → Catalysts… (~30–60s on first run, &lt;1s when cached)</span>"))))
    )
  )
)
