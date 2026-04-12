# app.R
# Market Insight Studio - Entry Point
#
# File structure:
#   global.R            - Thin compatibility shim that loads R/shared/global.R
#   R/shared/global.R   - Constants, libraries, utilities, API keys, module loading
#   R/backend/          - API clients, models, RAG, reporting, backtesting, data refresh
#   R/frontend/         - Shiny UI and server orchestration
#
# app.R remains the explicit entry point for local runs, Docker, and deployment.

source("global.R")
source(file.path("R", "frontend", "ui.R"))
source(file.path("R", "frontend", "server.R"))

shinyApp(ui = ui, server = server)
