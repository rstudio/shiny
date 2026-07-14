#!/usr/bin/env bash
# Runs the MCP Apps reference host (basic-host from modelcontextprotocol/ext-apps)
# against a local Shiny app's MCP endpoint.
#
# Usage:
#   ./run-mcp-host.sh                       # expects the Shiny app on :7788
#   ./run-mcp-host.sh http://127.0.0.1:9999/mcp
#
# Start the demo Shiny app first (from the repo root):
#   Rscript -e 'devtools::load_all(); shiny::runApp("mcp/demo-app", port = 7788)'
#
# Then open:  http://localhost:8090/?server=shiny&tool=open_shiny_app&call=true
#
# Requires a prepared clone of https://github.com/modelcontextprotocol/ext-apps
# (see setup notes in README-mcp-host.md). Default location is
# .context/ext-apps; override with EXT_APPS_DIR.
#
# Notes:
# - SANDBOX_PORT must stay 8081: the sandbox URL is hardcoded in the built
#   client (examples/basic-host/src/implementation.ts).
# - HOST_PORT defaults to 8090 here because :8080 is often taken locally.
# - serve.ts is plain express; tsx avoids the bun dependency of `npm start`.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
EXT_APPS_DIR="${EXT_APPS_DIR:-}"
if [[ -z "${EXT_APPS_DIR}" ]]; then
  for candidate in "${SCRIPT_DIR}/ext-apps" "${SCRIPT_DIR}/../.context/ext-apps"; do
    if [[ -d "${candidate}" ]]; then
      EXT_APPS_DIR="${candidate}"
      break
    fi
  done
fi
if [[ -z "${EXT_APPS_DIR}" || ! -d "${EXT_APPS_DIR}/examples/basic-host" ]]; then
  echo "ext-apps clone not found. Set EXT_APPS_DIR or see README-mcp-host.md for setup." >&2
  exit 1
fi

cd "${EXT_APPS_DIR}/examples/basic-host"

MCP_URL="${1:-http://127.0.0.1:7788/mcp}"

HOST_PORT="${HOST_PORT:-8090}" \
SANDBOX_PORT=8081 \
SERVERS="[\"${MCP_URL}\"]" \
npx tsx serve.ts
