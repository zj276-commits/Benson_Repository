#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(chromote))

Sys.setenv(CHROMOTE_CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")

b <- ChromoteSession$new()
on.exit(try(b$close(), silent = TRUE), add = TRUE)

b$Page$navigate(commandArgs(trailingOnly = TRUE)[1])
b$Page$loadEventFired(wait_ = TRUE)
Sys.sleep(3)

connected <- b$Runtime$evaluate(
  "typeof Shiny !== 'undefined' && !!Shiny.shinyapp && Shiny.shinyapp.isConnected()",
  returnByValue = TRUE
)$result$value

body <- b$Runtime$evaluate(
  "document.body.innerText",
  returnByValue = TRUE
)$result$value

cat("connected=", connected, "\n", sep = "")
cat(body, "\n")
