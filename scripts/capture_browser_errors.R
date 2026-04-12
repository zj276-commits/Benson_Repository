#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(chromote))

Sys.setenv(CHROMOTE_CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")

b <- ChromoteSession$new()
on.exit(try(b$close(), silent = TRUE), add = TRUE)

b$Page$addScriptToEvaluateOnNewDocument("
  window.__codexErrors = [];
  window.addEventListener('error', function(e) {
    window.__codexErrors.push({
      type: 'error',
      message: e.message,
      source: e.filename,
      lineno: e.lineno,
      colno: e.colno
    });
  });
  window.addEventListener('unhandledrejection', function(e) {
    window.__codexErrors.push({
      type: 'unhandledrejection',
      message: String(e.reason)
    });
  });
  const origConsoleError = console.error;
  console.error = function() {
    window.__codexErrors.push({
      type: 'console.error',
      message: Array.from(arguments).map(String).join(' ')
    });
    return origConsoleError.apply(console, arguments);
  };
")

b$Page$navigate("http://127.0.0.1:3838")
b$Page$loadEventFired(wait_ = TRUE)
Sys.sleep(5)

errors <- b$Runtime$evaluate(
  "JSON.stringify(window.__codexErrors || [])",
  returnByValue = TRUE
)$result$value

connected <- b$Runtime$evaluate(
  "typeof Shiny !== 'undefined' && !!Shiny.shinyapp && Shiny.shinyapp.isConnected()",
  returnByValue = TRUE
)$result$value

cat("connected=", connected, "\n", sep = "")
cat(errors, "\n")
