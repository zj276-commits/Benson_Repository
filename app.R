# World Bank Population Shiny App
# Implements API query from 01_query_api/My_APP (World Bank SP.POP.TOTL).
# Uses httr2 for HTTP. UI: black background, modern layout.

library(shiny)
library(httr2)
library(DT)

# -----------------------------------------------------------------------------
# Helper: fetch World Bank population for one country
# Returns list with success, data (data.frame), or error message
# -----------------------------------------------------------------------------
fetch_wb_population = function(country_code) {
  base_url = "https://api.worldbank.org/v2"
  url = paste0(base_url, "/country/", country_code, "/indicator/SP.POP.TOTL")
  tryCatch({
    resp = request(url) |>
      req_url_query(format = "json") |>
      req_perform()
    data = resp_body_json(resp)
    if (is.null(data) || length(data) < 2) {
      return(list(success = FALSE, error = "No data returned from API."))
    }
    records = data[[2]]
    if (length(records) == 0) {
      return(list(success = FALSE, error = "No records for this country."))
    }
    tbl = data.frame(
      Year = vapply(records, function(r) r$date, character(1)),
      Population = vapply(records, function(r) {
        v = r$value
        if (is.null(v)) NA else as.numeric(v)
      }, numeric(1))
    )
    tbl = tbl[order(tbl$Year, decreasing = TRUE), ]
    country_name = if (length(records) > 0) records[[1]]$country$value else country_code
    list(success = TRUE, data = tbl, country = country_name)
  }, error = function(e) {
    list(success = FALSE, error = paste0("Request failed: ", conditionMessage(e)))
  })
}

# -----------------------------------------------------------------------------
# UI (black background)
# -----------------------------------------------------------------------------
ui = fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #000000 !important; }
      .well, .shiny-input-container { background-color: #1a1a1a; border-color: #333; color: #e8e8e8; }
      .form-control { background-color: #2a2a2a; color: #e8e8e8; border-color: #444; }
      .dataTables_wrapper { color: #e8e8e8; }
      .dataTables_info, .dataTables_length label { color: #aaa !important; }
      table.dataTable thead th { background: #1a1a1a !important; color: #e8e8e8 !important; border-color: #333 !important; }
      table.dataTable tbody td { color: #e8e8e8 !important; border-color: #333 !important; }
      table.dataTable tbody tr:hover { background-color: #252525 !important; }
      .btn-primary { background-color: #4a9eff; border-color: #4a9eff; }
      .btn-primary:hover { background-color: #3a8eef; border-color: #3a8eef; }
      h2, h3 { color: #e8e8e8; }
      .alert-danger { background-color: #2a1515; border-color: #662; color: #fcc; }
    "))
  ),
  titlePanel(
    h2("World Bank Population Query", style = "color: #e8e8e8; font-weight: 600;"),
    windowTitle = "World Bank Population"
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        "country",
        "Country",
        choices = c(
          "China" = "cn",
          "United States" = "us",
          "India" = "in",
          "United Kingdom" = "gb",
          "France" = "fr",
          "South Korea" = "kr",
          "Russia" = "ru",
          "Japan" = "jp",
          "Brazil" = "br",
          "Germany" = "de"
        ),
        selected = "cn"
      ),
      actionButton("fetch", "Fetch population", class = "btn-primary", width = "100%"),
      hr(),
      helpText(
        "Data: World Bank API, indicator SP.POP.TOTL (Population, total).",
        style = "color: #999; font-size: 0.9em;"
      )
    ),
    mainPanel(
      width = 9,
      uiOutput("title_result"),
      uiOutput("error_message"),
      uiOutput("table_placeholder"),
      DTOutput("pop_table")
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server = function(input, output, session) {
  result = reactiveVal(NULL)

  observeEvent(input$fetch, {
    output$error_message = renderUI(NULL)
    output$table_placeholder = renderUI(NULL)
    result(NULL)
    withProgress(message = "Calling World Bank API...", value = 0.5, {
      res = fetch_wb_population(input$country)
      setProgress(1)
      result(res)
    })
  })

  output$title_result = renderUI({
    r = result()
    if (is.null(r) || !r$success) return(NULL)
    h3(paste("Population:", r$country), style = "color: #e8e8e8; margin-top: 0;")
  })

  output$error_message = renderUI({
    r = result()
    if (is.null(r) || r$success) return(NULL)
    div(
      class = "alert alert-danger",
      role = "alert",
      strong("Error: "), r$error
    )
  })

  output$pop_table = renderDT({
    r = result()
    if (is.null(r) || !r$success) return(NULL)
    r$data
  }, options = list(
    pageLength = 15,
    dom = "tip",
    ordering = TRUE
  ), rownames = FALSE)

  output$table_placeholder = renderUI({
    r = result()
    if (is.null(r)) return(NULL)
    if (!r$success) return(NULL)
    NULL
  })
}

shinyApp(ui = ui, server = server)
