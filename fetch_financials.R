#!/usr/bin/env Rscript
# Fetch comprehensive financial data for all 7 companies using Yahoo timeseries API.
# Outputs: company_financials.csv, key_financials.csv, valuation_metrics.csv

library(httr2)
library(jsonlite)
library(dplyr)
library(quantmod)

TICKERS <- c("AAPL", "TSLA", "META", "NVDA", "GOOGL", "AMZN", "MSFT")
TICKER_LABELS <- c(AAPL="Apple", TSLA="Tesla", META="Meta", NVDA="NVIDIA",
                   GOOGL="Alphabet", AMZN="Amazon", MSFT="Microsoft")
SECTOR_MAP <- c(AAPL="Information Technology", TSLA="Consumer Discretionary",
                META="Communication Services", NVDA="Information Technology",
                GOOGL="Communication Services", AMZN="Consumer Discretionary",
                MSFT="Information Technology")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

get_yahoo_auth <- function() {
  ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"
  sc <- tryCatch({
    r <- request("https://fc.yahoo.com/") |> req_headers(`User-Agent`=ua) |>
      req_error(is_error=function(r) FALSE) |> req_timeout(15) |> req_perform()
    resp_header(r, "Set-Cookie")
  }, error=function(e) NULL)
  if (is.null(sc)) return(NULL)
  cr <- tryCatch({
    r <- request("https://query2.finance.yahoo.com/v1/test/getcrumb") |>
      req_headers(`User-Agent`=ua, Cookie=sc) |> req_error(is_error=function(r) FALSE) |>
      req_timeout(15) |> req_perform()
    if (resp_status(r)==200) resp_body_string(r) else NULL
  }, error=function(e) NULL)
  if (is.null(cr)) return(NULL)
  list(cookie=sc, crumb=cr, ua=ua)
}

ANNUAL_FIELDS <- c(
  "annualTotalRevenue", "annualCostOfRevenue", "annualGrossProfit",
  "annualOperatingIncome", "annualNetIncome", "annualNetIncomeCommonStockholders",
  "annualInterestExpense", "annualIncomeTaxExpense", "annualPretaxIncome",
  "annualEBITDA", "annualNormalizedEBITDA",
  "annualDepreciationAndAmortization",
  "annualTotalDebt", "annualCurrentDebt", "annualLongTermDebt",
  "annualStockholdersEquity", "annualTotalAssets",
  "annualCashAndCashEquivalents",
  "annualFreeCashFlow", "annualCapitalExpenditure", "annualOperatingCashFlow",
  "annualAccountsReceivable", "annualAccountsPayable", "annualInventory"
)

fetch_timeseries <- function(symbol, auth) {
  types <- paste(ANNUAL_FIELDS, collapse=",")
  resp <- tryCatch({
    request(paste0("https://query2.finance.yahoo.com/ws/fundamentals-timeseries/v1/finance/timeseries/", symbol)) |>
      req_url_query(type=types, period1=0, period2=as.integer(Sys.time()), crumb=auth$crumb) |>
      req_headers(`User-Agent`=auth$ua, Cookie=auth$cookie) |>
      req_timeout(30) |> req_error(is_error=function(r) FALSE) |> req_perform()
  }, error=function(e) NULL)
  if (is.null(resp) || resp_status(resp) != 200) return(NULL)
  body <- tryCatch(resp_body_json(resp), error=function(e) NULL)
  if (is.null(body)) return(NULL)

  ts_results <- body$timeseries$result %||% list()
  out <- list()
  for (item in ts_results) {
    meta_type <- item$meta$type[[1]] %||% next
    data_items <- item[[meta_type]] %||% list()
    for (d in data_items) {
      dt <- d$asOfDate
      val <- d$reportedValue$raw %||% NA_real_
      if (!is.null(dt)) {
        if (is.null(out[[dt]])) out[[dt]] <- list()
        out[[dt]][[meta_type]] <- val
      }
    }
  }
  out
}

build_company_rows <- function(symbol, ts_data) {
  dates <- sort(names(ts_data), decreasing=TRUE)
  rows <- list()
  for (dt in dates) {
    d <- ts_data[[dt]]
    g <- function(nm) d[[nm]] %||% NA_real_

    revenue   <- g("annualTotalRevenue")
    cogs      <- g("annualCostOfRevenue")
    gross     <- g("annualGrossProfit")
    if (is.na(gross) && !is.na(revenue) && !is.na(cogs)) gross <- revenue - cogs
    op_inc    <- g("annualOperatingIncome")
    net_inc   <- g("annualNetIncome")
    int_exp   <- abs(g("annualInterestExpense") %||% NA_real_)
    tax       <- g("annualIncomeTaxExpense")
    pretax    <- g("annualPretaxIncome")
    ebitda    <- g("annualEBITDA")
    if (is.na(ebitda)) ebitda <- g("annualNormalizedEBITDA")
    dep_amort <- g("annualDepreciationAndAmortization")
    if (is.na(ebitda) && !is.na(op_inc) && !is.na(dep_amort)) ebitda <- op_inc + dep_amort

    total_debt <- g("annualTotalDebt")
    st_debt    <- g("annualCurrentDebt")
    lt_debt    <- g("annualLongTermDebt")
    equity     <- g("annualStockholdersEquity")
    assets     <- g("annualTotalAssets")
    cash       <- g("annualCashAndCashEquivalents")
    net_debt   <- if (!is.na(total_debt) && !is.na(cash)) total_debt - cash else NA_real_

    fcf       <- g("annualFreeCashFlow")
    capex     <- abs(g("annualCapitalExpenditure") %||% NA_real_)
    op_cf     <- g("annualOperatingCashFlow")
    recv      <- g("annualAccountsReceivable")
    payable   <- g("annualAccountsPayable")
    inv       <- g("annualInventory")

    fy_label  <- format(as.Date(dt), "FY%Y")

    safe_div <- function(a, b, pct=FALSE) {
      if (is.na(a) || is.na(b) || b == 0) return(NA_real_)
      r <- a / b; if (pct) r * 100 else r
    }

    rows[[length(rows)+1]] <- data.frame(
      ticker=symbol, company=TICKER_LABELS[[symbol]], sector=SECTOR_MAP[[symbol]],
      fiscal_year=fy_label, fiscal_year_end=dt, data_type="A",
      revenue_mn       = round(revenue/1e6),
      gross_profit_mn  = round(gross/1e6),
      ebitda_mn        = round(ebitda/1e6),
      net_income_mn    = round(net_inc/1e6),
      fcf_mn           = round(fcf/1e6),
      capex_mn         = round(capex/1e6),
      ebitda_margin_pct= round(safe_div(ebitda, revenue, TRUE), 1),
      net_margin_pct   = round(safe_div(net_inc, revenue, TRUE), 1),
      roa_pct          = round(safe_div(net_inc, assets, TRUE), 1),
      roe_pct          = round(safe_div(net_inc, equity, TRUE), 1),
      tax_rate_pct     = round(safe_div(tax, pretax, TRUE), 1),
      debt_equity_pct  = round(safe_div(total_debt, equity, TRUE), 1),
      net_debt_equity_pct = round(safe_div(net_debt, equity, TRUE), 1),
      debt_assets_pct  = round(safe_div(total_debt, assets, TRUE), 1),
      net_debt_assets_pct = round(safe_div(net_debt, assets, TRUE), 1),
      ebitda_int_exp   = round(safe_div(ebitda, int_exp), 1),
      st_debt_total_debt = round(safe_div(st_debt, total_debt), 2),
      debt_ebitda      = round(safe_div(total_debt, ebitda), 1),
      cash_cfo_st_debt = round(safe_div(cash + (op_cf %||% 0), st_debt), 1),
      receivables_days = round(safe_div(recv, revenue/365), 1),
      days_payable     = round(safe_div(payable, cogs/365), 1),
      inventory_days   = round(safe_div(inv, cogs/365), 1),
      stringsAsFactors=FALSE
    )
  }
  bind_rows(rows)
}

build_key_financial_row <- function(symbol, auth) {
  ks_mods <- "financialData,defaultKeyStatistics,summaryDetail"
  resp <- tryCatch({
    request(paste0("https://query2.finance.yahoo.com/v10/finance/quoteSummary/", symbol)) |>
      req_url_query(modules=ks_mods, crumb=auth$crumb) |>
      req_headers(`User-Agent`=auth$ua, Cookie=auth$cookie) |>
      req_timeout(20) |> req_error(is_error=function(r) FALSE) |> req_perform()
  }, error=function(e) NULL)
  xr <- function(obj, field) {
    val <- obj[[field]]
    if (is.list(val)) val$raw %||% NA_real_ else NA_real_
  }
  fd <- list(); ks <- list(); sd <- list()
  if (!is.null(resp) && resp_status(resp) == 200) {
    body <- tryCatch(resp_body_json(resp), error=function(e) NULL)
    if (!is.null(body)) {
      res <- body$quoteSummary$result[[1]]
      fd <- res$financialData %||% list()
      ks <- res$defaultKeyStatistics %||% list()
      sd <- res$summaryDetail %||% list()
    }
  }

  qf <- tryCatch(getQuote(symbol, what=yahooQF(c(
    "Market Capitalization","Earnings/Share","P/E Ratio",
    "Price/Book","Dividend Yield","52-week High","52-week Low",
    "Average Daily Volume","Price/EPS Estimate Next Year","Shares Outstanding","Book Value"
  ))), error=function(e) NULL)
  grab <- function(col) {
    if (!is.null(qf) && col %in% names(qf)) as.numeric(qf[[col]][1]) else NA_real_
  }
  mc <- grab("Market Capitalization")
  so <- grab("Shares Outstanding")
  cp <- if (!is.na(mc) && !is.na(so) && so > 0) mc / so else xr(fd, "currentPrice")

  data.frame(
    ticker=symbol, company=TICKER_LABELS[[symbol]], sector=SECTOR_MAP[[symbol]],
    share_price=round(cp, 2),
    target_price=round(xr(fd, "targetMeanPrice"), 2),
    market_cap_bn=round(mc/1e9, 1),
    volume_mn_shares=round(grab("Ave. Daily Volume")/1e6, 1),
    free_float_pct=round(xr(ks, "floatShares") / (so %||% NA_real_) * 100, 1),
    dividend_yield_pct=round(grab("Dividend Yield")*100, 2),
    net_debt_equity_pct=round(xr(fd, "debtToEquity"), 1),
    fwd_pe=round(grab("Price/EPS Estimate Next Year"), 1),
    pb=round(grab("Price/Book"), 1),
    roe_pct=round(xr(fd, "returnOnEquity")*100, 1),
    beta=round(xr(ks, "beta"), 2),
    pe_ttm=round(grab("P/E Ratio"), 1),
    book_value=round(grab("Book Value"), 2),
    eps_ttm=round(grab("Earnings/Share"), 2),
    fifty_two_wk_high=round(grab("52-week High"), 2),
    fifty_two_wk_low=round(grab("52-week Low"), 2),
    fetch_date=as.character(Sys.Date()),
    stringsAsFactors=FALSE
  )
}

# ---- Main ----
cat("=== Fetching comprehensive financial data for all 7 companies ===\n\n")

auth <- get_yahoo_auth()
if (is.null(auth)) stop("Yahoo Finance authentication failed.")

all_hist <- list()
all_key <- list()

for (sym in TICKERS) {
  cat(sprintf("[%s] Fetching timeseries...\n", sym))
  ts_data <- fetch_timeseries(sym, auth)
  if (is.null(ts_data) || length(ts_data) == 0) {
    cat("  Timeseries FAILED\n"); next
  }
  hist <- build_company_rows(sym, ts_data)
  if (!is.null(hist) && nrow(hist) > 0) {
    for (j in seq_len(nrow(hist))) {
      cat(sprintf("  %s: Rev=%sM, GP=%sM, EBITDA=%sM, NI=%sM, FCF=%sM, CAPEX=%sM\n",
        hist$fiscal_year[j], hist$revenue_mn[j], hist$gross_profit_mn[j],
        hist$ebitda_mn[j], hist$net_income_mn[j], hist$fcf_mn[j], hist$capex_mn[j]))
    }
    all_hist[[sym]] <- hist
  }
  Sys.sleep(0.3)

  cat(sprintf("[%s] Fetching key financials...\n", sym))
  kf <- tryCatch(build_key_financial_row(sym, auth), error=function(e) {
    cat("  Key financial error:", e$message, "\n"); NULL
  })
  if (!is.null(kf)) {
    cat(sprintf("  Price=%.2f, MCap=%.1fB, PE=%.1f, PB=%.1f, ROE=%.1f%%\n",
      kf$share_price, kf$market_cap_bn, kf$pe_ttm, kf$pb, kf$roe_pct))
    all_key[[sym]] <- kf
  }
  Sys.sleep(1)
}

hist_df <- bind_rows(all_hist)
key_df  <- bind_rows(all_key)

# Compute y/y growth rates within hist_df
hist_df <- hist_df |>
  arrange(ticker, fiscal_year_end) |>
  group_by(ticker) |>
  mutate(
    revenue_yoy   = round((revenue_mn / lag(revenue_mn) - 1) * 100, 1),
    gp_yoy        = round((gross_profit_mn / lag(gross_profit_mn) - 1) * 100, 1),
    ebitda_yoy    = round((ebitda_mn / lag(ebitda_mn) - 1) * 100, 1),
    ni_yoy        = round((net_income_mn / lag(net_income_mn) - 1) * 100, 1),
    fcf_yoy       = round((fcf_mn / lag(fcf_mn) - 1) * 100, 1),
    capex_yoy     = round((capex_mn / lag(capex_mn) - 1) * 100, 1)
  ) |>
  ungroup()

# Build valuation metrics from hist + key
val_rows <- list()
for (sym in unique(hist_df$ticker)) {
  sub <- hist_df |> filter(ticker == sym)
  kf  <- key_df  |> filter(ticker == sym)
  if (nrow(kf) == 0) next
  mc_bn <- kf$market_cap_bn[1]
  for (i in seq_len(nrow(sub))) {
    r <- sub[i, ]
    ni <- r$net_income_mn; ebitda <- r$ebitda_mn; fcf <- r$fcf_mn
    net_d_e <- r$net_debt_equity_pct
    pe <- if (!is.na(mc_bn) && !is.na(ni) && ni > 0) mc_bn * 1e3 / ni else NA_real_
    ev <- if (!is.na(mc_bn) && !is.na(net_d_e)) mc_bn * (1 + net_d_e/100) else mc_bn
    ev_ebitda <- if (!is.na(ev) && !is.na(ebitda) && ebitda > 0) ev * 1e3 / ebitda else NA_real_
    fcf_yield <- if (!is.na(mc_bn) && !is.na(fcf) && mc_bn > 0) fcf / (mc_bn * 1e3) * 100 else NA_real_
    val_rows[[length(val_rows)+1]] <- data.frame(
      ticker=sym, fiscal_year=r$fiscal_year,
      pe_x=round(pe, 1), pb_x=round(kf$pb[1], 1),
      dividend_yield_pct=round(kf$dividend_yield_pct[1], 1),
      ev_ebitda_x=round(ev_ebitda, 1),
      fcf_yield_pct=round(fcf_yield, 1),
      stringsAsFactors=FALSE
    )
  }
}
val_df <- bind_rows(val_rows)

cat(sprintf("\n=== Results ===\nHistorical P&L/Credit: %d rows\nKey Financial: %d rows\nValuation: %d rows\n",
            nrow(hist_df), nrow(key_df), nrow(val_df)))

write.csv(hist_df, "company_financials.csv", row.names=FALSE)
write.csv(key_df, "key_financials.csv", row.names=FALSE)
write.csv(val_df, "valuation_metrics.csv", row.names=FALSE)

cat("\nSaved: company_financials.csv, key_financials.csv, valuation_metrics.csv\n")
cat("Done!\n")
