# app.R
# Market Insight Studio - Entry Point
#
# File structure:
#   global.R  - Constants, libraries, utilities, API keys; also sources api.R, models.R, report.R
#   api.R     - API functions (Finnhub, Yahoo Finance, OpenAI, Ollama)
#   models.R  - Financial models (GBM, Lattice, SIM), RAG, caching, export
#   report.R  - AI multi-analyst report generation (Purchase Rating, Risk, Financials)
#   ui.R      - Shiny UI definition
#   server.R  - Shiny server logic
#
# Shiny auto-detects global.R + ui.R + server.R, so this file is only needed
# for explicit shiny::runApp() or direct Rscript execution.

source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
