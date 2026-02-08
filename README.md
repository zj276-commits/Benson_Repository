# World Bank Population Shiny App

Shiny app that runs your **World Bank API** population query on demand: choose a country, click **Fetch population**, and view the result in a table. Built for [LAB_cursor_shiny_app.md](../LAB_cursor_shiny_app.md).

---

## What it does

- **Implements your API query:** Calls the World Bank Open Data API (`/country/{code}/indicator/SP.POP.TOTL`) using **httr2** when you click **Fetch population**.
- **Input controls:** Country selector (China, USA, India, UK, France, South Korea, Russia, Japan, Brazil, Germany).
- **Display:** DataTable of **Year** and **Population** (clear, sortable).
- **Error handling:** Shows an alert if the request fails or returns no data.
- **UI:** Black background, light text, simple layout.

---

## Requirements

- **R** (e.g. 4.2+).
- R packages: **shiny**, **httr2**, **DT**.

No API key; the World Bank API is public.

---

## Install dependencies

In R:

```r
install.packages(c("shiny", "httr2", "DT"))
```

---

## Run the app

From R, with working directory set to this folder:

```r
setwd("02_productivity/shiny_app")
shiny::runApp(".")
```

Or from the repo root:

```r
shiny::runApp("02_productivity/shiny_app")
```

---

## Files

| File | Purpose |
|------|--------|
| `app.R` | Single-file app: UI (black background, inputs, table), server (API call via httr2), and helper `fetch_wb_population()`. |
| `README.md` | This file. |

API logic mirrors [01_query_api/My_APP/my_good_query.py](../01_query_api/My_APP/my_good_query.py) and uses the same endpoint and response shape.
