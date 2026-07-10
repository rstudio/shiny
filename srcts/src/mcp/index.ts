// Entry point for inst/www/shared/shiny-mcp-bridge.js (built by
// `npm run bundle_mcp`; see srcts/build/mcp-bridge.ts).
//
// This script is inlined at the end of <body> in the `ui://shiny/app`
// resource, after shiny.js has created `window.Shiny` but before Shiny's
// DOM-ready initialization runs. It:
//
// 1. starts the MCP Apps handshake with the host (`ui/initialize`), and
// 2. synchronously installs `Shiny.createSocket` so that when Shiny
//    initializes, its "websocket" is a McpTunnelWebSocket that tunnels
//    frames over the host's postMessage channel via app-only tools
//    (`_shiny_connect` / `_shiny_send` / `_shiny_receive` / `_shiny_close`).

import { App } from "@modelcontextprotocol/ext-apps";
import type { ToolCaller } from "./tunnelSocket";
import { McpTunnelWebSocket } from "./tunnelSocket";

type ShinyWithCreateSocket = { createSocket?: () => unknown };

function initShinyMcpBridge(): void {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  const shiny = (window as unknown as { Shiny?: ShinyWithCreateSocket }).Shiny;
  if (!shiny) {
    console.error(
      "shiny-mcp-bridge: window.Shiny not found; load shiny.js first.",
    );
    return;
  }

  const app = new App({
    name: "shiny",
    version: process.env.SHINY_VERSION ?? "0.0.0",
  });

  // Kick off the ui/initialize handshake immediately, but install
  // createSocket synchronously: Shiny's DOM-ready init may run before the
  // handshake completes, and tool calls must wait for connect() anyway.
  const connected = app.connect().catch((err) => {
    console.error("shiny-mcp-bridge: failed to connect to MCP host:", err);
    throw err;
  });

  const callTool: ToolCaller = async (name, args) => {
    await connected;
    const result = await app.callServerTool({ name, arguments: args });
    if (result.isError) {
      const content = result.content as
        | Array<{ type: string; text?: string }>
        | undefined;
      const text = content?.find((c) => c.type === "text")?.text;
      throw new Error(text ?? "MCP tool call failed: " + name);
    }
    return (result.structuredContent ?? {}) as { [key: string]: unknown };
  };

  let socket: McpTunnelWebSocket | null = null;

  shiny.createSocket = () => {
    socket = new McpTunnelWebSocket(callTool);
    // shinyapp.ts assigns onopen/onmessage/onclose right after
    // createSocket() returns; defer start so they're in place.
    setTimeout(() => void socket?.start(), 0);
    return socket;
  };

  app.onteardown = () => {
    socket?.close();
    return {};
  };
}

initShinyMcpBridge();
