#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(chromote))

chrome_path <- "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
Sys.setenv(CHROMOTE_CHROME = chrome_path)

b <- ChromoteSession$new()
on.exit(try(b$close(), silent = TRUE), add = TRUE)

b$Runtime$enable()
b$Log$enable()

b$Page$navigate("http://127.0.0.1:3838")
b$Page$loadEventFired(wait_ = TRUE)

for (i in seq_len(15)) {
  Sys.sleep(1)
  shiny_type <- b$Runtime$evaluate(
    "typeof Shiny",
    returnByValue = TRUE
  )$result$value
  shinyapp_type <- b$Runtime$evaluate(
    "typeof (window.Shiny && Shiny.shinyapp)",
    returnByValue = TRUE
  )$result$value
  connected <- b$Runtime$evaluate(
    "typeof Shiny !== 'undefined' && !!Shiny.shinyapp && Shiny.shinyapp.isConnected()",
    returnByValue = TRUE
  )$result$value
  shiny_state <- b$Runtime$evaluate(
    "window.Shiny && Shiny.shinyapp ? JSON.stringify({allowReconnect: Shiny.shinyapp.$allowReconnect, socket: !!Shiny.shinyapp.$socket, hasDisconnected: !!Shiny.shinyapp.$hasDisconnected, reconnectDelay: Shiny.shinyapp.reconnectDelay}) : ''",
    returnByValue = TRUE
  )$result$value
  body_text <- b$Runtime$evaluate(
    "document.body ? document.body.innerText.slice(0, 600) : ''",
    returnByValue = TRUE
  )$result$value
  disconnected <- b$Runtime$evaluate(
    "!!document.querySelector('.shiny-disconnected-overlay, #ss-connect-dialog')",
    returnByValue = TRUE
  )$result$value
  cat(sprintf("t=%02d shiny=%s shinyapp=%s connected=%s disconnected_overlay=%s state=%s\n", i, shiny_type, shinyapp_type, connected, disconnected, shiny_state))
  if (nchar(body_text) > 0) {
    cat(body_text, "\n---\n")
  }
}

manual_init <- b$Runtime$evaluate(
  "(() => { try { Shiny.initializeInputs(document.documentElement); Shiny.bindAll(document.documentElement); return 'ok'; } catch (e) { return e && e.stack ? e.stack : String(e); } })()",
  returnByValue = TRUE
)$result$value

post_connected <- b$Runtime$evaluate(
  "typeof Shiny !== 'undefined' && !!Shiny.shinyapp && Shiny.shinyapp.isConnected()",
  returnByValue = TRUE
)$result$value

manual_start <- b$Runtime$evaluate(
  "(() => { try { if (Shiny.initialize) { Shiny.initialize(); return 'initialized'; } return 'no-initialize'; } catch (e) { return e && e.stack ? e.stack : String(e); } })()",
  returnByValue = TRUE
)$result$value

Sys.sleep(3)

post_manual_start_connected <- b$Runtime$evaluate(
  "typeof Shiny !== 'undefined' && !!Shiny.shinyapp && Shiny.shinyapp.isConnected()",
  returnByValue = TRUE
)$result$value

cat('manual_init=', manual_init, '\n', sep = '')
cat('post_connected=', post_connected, '\n', sep = '')
cat('manual_start=', manual_start, '\n', sep = '')
cat('post_manual_start_connected=', post_manual_start_connected, '\n', sep = '')
