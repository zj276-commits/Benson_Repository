#!/usr/bin/env bash

set -euo pipefail

if [ $# -lt 3 ]; then
  echo "Usage: $0 <base_url> <refresh_token> <refresh-market-data|refresh-financials|refresh-all>"
  echo "Example: $0 http://localhost:3838 my-secret-token refresh-market-data"
  exit 1
fi

BASE_URL="${1%/}"
REFRESH_TOKEN="$2"
ACTION="$3"

case "$ACTION" in
  refresh-market-data|refresh-financials|refresh-all)
    ;;
  *)
    echo "Invalid action: $ACTION"
    exit 1
    ;;
esac

URL="${BASE_URL}/?action=${ACTION}&token=${REFRESH_TOKEN}"

echo "Triggering: $URL"
curl --fail --silent --show-error "$URL" >/dev/null
echo "Request sent successfully."
