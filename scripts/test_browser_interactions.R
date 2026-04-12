#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(chromote))

chrome_path <- "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
if (!file.exists(chrome_path)) {
  stop("Google Chrome not found at ", chrome_path)
}

Sys.setenv(CHROMOTE_CHROME = chrome_path)

js_eval <- function(browser, code) {
  browser$Runtime$evaluate(code, returnByValue = TRUE)$result$value
}

browser <- ChromoteSession$new()
on.exit(try(browser$close(), silent = TRUE), add = TRUE)

browser$Page$navigate("http://127.0.0.1:3838")
browser$Page$loadEventFired(wait_ = TRUE)
Sys.sleep(3)

connected <- js_eval(browser, "typeof Shiny !== 'undefined' && !!Shiny.shinyapp && Shiny.shinyapp.isConnected()")
tabs <- js_eval(browser, "Array.from(document.querySelectorAll('.navbar-nav li a')).map(x => x.innerText.trim()).join('|')")
active_before <- js_eval(browser, "document.querySelector('.navbar-nav li.active a') && document.querySelector('.navbar-nav li.active a').innerText.trim()")

js_eval(
  browser,
  "(function(){ var a = Array.from(document.querySelectorAll('.navbar-nav li a')).find(x => x.innerText.includes('News')); if (a) { a.click(); return true; } return false; })()"
)
Sys.sleep(1)
active_after_news <- js_eval(browser, "document.querySelector('.navbar-nav li.active a') && document.querySelector('.navbar-nav li.active a').innerText.trim()")

refresh_news_before <- js_eval(browser, "(Shiny.shinyapp && Shiny.shinyapp.$inputValues['refresh_news:shiny.action']) || 0")
js_eval(
  browser,
  "(function(){ var btn = document.getElementById('refresh_news'); if (btn) { btn.click(); return true; } return false; })()"
)
Sys.sleep(2)
refresh_news_after <- js_eval(browser, "(Shiny.shinyapp && Shiny.shinyapp.$inputValues['refresh_news:shiny.action']) || 0")
news_error <- js_eval(browser, "(document.getElementById('news_error') || {}).innerText || ''")

js_eval(
  browser,
  "(function(){ var a = Array.from(document.querySelectorAll('.navbar-nav li a')).find(x => x.innerText.includes('Reporting')); if (a) { a.click(); return true; } return false; })()"
)
Sys.sleep(1)
active_after_reporting <- js_eval(browser, "document.querySelector('.navbar-nav li.active a') && document.querySelector('.navbar-nav li.active a').innerText.trim()")
run_prediction_before <- js_eval(browser, "(Shiny.shinyapp && Shiny.shinyapp.$inputValues['run_prediction:shiny.action']) || 0")
js_eval(
  browser,
  "(function(){ var btn = document.getElementById('run_prediction'); if (btn) { btn.click(); return true; } return false; })()"
)
Sys.sleep(1)
progress_text <- js_eval(
  browser,
  "Array.from(document.querySelectorAll('.shiny-notification, .shiny-progress-notification')).map(x => x.innerText.trim()).join(' | ')"
)
Sys.sleep(1)
run_prediction_after <- js_eval(browser, "(Shiny.shinyapp && Shiny.shinyapp.$inputValues['run_prediction:shiny.action']) || 0")
ai_error <- js_eval(browser, "(document.getElementById('ai_error') || {}).innerText || ''")
report_ready <- FALSE
for (i in seq_len(20)) {
  report_text <- js_eval(browser, "(document.getElementById('report_content') || {}).innerText || ''")
  ai_error <- js_eval(browser, "(document.getElementById('ai_error') || {}).innerText || ''")
  if (nzchar(trimws(report_text)) || nzchar(trimws(ai_error))) {
    report_ready <- TRUE
    break
  }
  Sys.sleep(1)
}

cat("connected=", connected, "\n", sep = "")
cat("tabs=", tabs, "\n", sep = "")
cat("active_before=", active_before, "\n", sep = "")
cat("active_after_news=", active_after_news, "\n", sep = "")
cat("refresh_news_before=", refresh_news_before, "\n", sep = "")
cat("refresh_news_after=", refresh_news_after, "\n", sep = "")
cat("news_error=", news_error, "\n", sep = "")
cat("active_after_reporting=", active_after_reporting, "\n", sep = "")
cat("run_prediction_before=", run_prediction_before, "\n", sep = "")
cat("run_prediction_after=", run_prediction_after, "\n", sep = "")
cat("progress_text=", progress_text, "\n", sep = "")
cat("ai_error=", ai_error, "\n", sep = "")
cat("report_ready=", report_ready, "\n", sep = "")
