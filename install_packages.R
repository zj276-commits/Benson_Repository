# Run once to install dependencies for the Trading Hint Shiny app.
# Required: shiny, httr2, DT, dplyr, plotly, jsonlite

install.packages(
  c("shiny", "httr2", "DT", "dplyr", "plotly", "jsonlite", "quantmod", "digest"),
  repos = "https://cloud.r-project.org/"
)
