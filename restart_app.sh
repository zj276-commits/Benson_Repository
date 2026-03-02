#!/usr/bin/env bash

set -euo pipefail

OPEN_BROWSER=false
FOREGROUND=false
TAIL_LOG=false
PORT=3838
APP_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
APP_FILE="$APP_DIR/app.R"

for arg in "$@"; do
  case "$arg" in
    --open)
      OPEN_BROWSER=true
      ;;
    --foreground|-f)
      FOREGROUND=true
      ;;
    --tail)
      TAIL_LOG=true
      ;;
    -* )
      ;;
    *[!0-9]*)
      ;;
    *)
      PORT="$arg"
      ;;
  esac
done

PID_FILE="$APP_DIR/.shiny_app_${PORT}.pid"
LOG_FILE="$APP_DIR/logs/app_${PORT}.log"

log() {
  printf '[%s] %s\n' "$(date '+%F %T')" "$*"
}

# Kill from last saved pid first (fast path)
if [ -f "$PID_FILE" ]; then
  old_pid="$(cat "$PID_FILE" | tr -d '[:space:]')"
  if [ -n "$old_pid" ] && kill -0 "$old_pid" >/dev/null 2>&1; then
    log "Killing stale PID file process: $old_pid"
    kill -9 "$old_pid" || true
  fi
  rm -f "$PID_FILE"
fi

if [ ! -f "$APP_FILE" ]; then
  log "app.R not found at $APP_FILE"
  exit 1
fi

# Kill processes that already listen on this port.
pids=""
if command -v lsof >/dev/null 2>&1; then
  pids="$(lsof -tiTCP:"$PORT" -sTCP:LISTEN -nP || true)"
fi
if [ -n "$pids" ]; then
  log "Killing existing process(es) on port $PORT: $pids"
  kill -9 $pids || true
  sleep 1
fi

# Also kill stale R processes started with this app file.
if command -v pgrep >/dev/null 2>&1; then
  stale_pids="$(pgrep -f "shiny::runApp.*app.R" || true)"
  if [ -n "$stale_pids" ]; then
    log "Killing matching app.R Shiny processes: $stale_pids"
    kill -9 $stale_pids || true
    sleep 1
  fi
fi

rm -f "$PID_FILE"
mkdir -p "$APP_DIR/logs"

if [ "${FOREGROUND}" = "true" ]; then
  log "Starting in foreground on port $PORT ..."
  log "Running foreground. Ctrl+C to stop."
  log "Working directory: $APP_DIR"
  log "App file: $APP_FILE"
  exec R -e "shiny::runApp('$APP_FILE', host = '0.0.0.0', port = $PORT)"
fi

log "Starting in background on port $PORT ..."
nohup R -e "shiny::runApp('$APP_FILE', host = '0.0.0.0', port = $PORT)" \
  >> "$LOG_FILE" 2>&1 &
new_pid=$!
echo "$new_pid" > "$PID_FILE"
log "Started with PID $new_pid"
log "Log: $LOG_FILE"

if command -v lsof >/dev/null 2>&1; then
  for _ in {1..20}; do
    if lsof -iTCP:"$PORT" -sTCP:LISTEN -nP >/dev/null 2>&1; then
      break
    fi
    sleep 0.3
  done
fi

url="http://localhost:$PORT"
if [ "${OPEN_BROWSER}" = "true" ]; then
  if command -v open >/dev/null 2>&1; then
    open "$url" || true
  elif command -v xdg-open >/dev/null 2>&1; then
    xdg-open "$url" || true
  elif command -v start >/dev/null 2>&1; then
    start "$url" || true
  fi
  log "Browser opened: $url"
else
  log "If you use a browser cache, please hard refresh (Cmd/Ctrl+Shift+R)"
  log "Visit: $url"
fi

if [ "${TAIL_LOG}" = "true" ]; then
  log "Tail log (Ctrl+C exits tail, app keeps running):"
  tail -f "$LOG_FILE"
else
  log "For live logs use: $0 $PORT --tail"
fi

log "Done"
