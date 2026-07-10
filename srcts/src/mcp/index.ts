// Entry point for inst/www/shared/shiny-mcp-bridge.js (built by
// `npm run bundle_mcp`; see srcts/build/mcp-bridge.ts).
//
// This script is inlined at the end of <body> in the `ui://shiny/app`
// resource, after shiny.js has created `window.Shiny` but before Shiny's
// DOM-ready initialization runs. It:
//
// 1. starts the MCP Apps handshake with the host (`ui/initialize`);
// 2. synchronously installs `Shiny.createSocket` so that when Shiny
//    initializes, its "websocket" is a McpTunnelWebSocket that tunnels
//    frames over the host's postMessage channel via app-only tools
//    (`_shiny_connect` / `_shiny_send` / `_shiny_receive` / `_shiny_close`);
// 3. replaces `window.XMLHttpRequest` with a tunnel implementation so HTTP
//    side channels (uploads, DataTables/selectize, restyles) go through
//    `_shiny_http`;
// 4. intercepts download-link clicks and delivers files as blob URLs;
// 5. applies host theme/styles and forwards tool input/results into
//    `session$clientData`;
// 6. bridges `session$sendCustomMessage("shiny.mcp.updateModelContext", ...)`
//    and `"shiny.mcp.sendMessage"` to the host.

import type { McpUiHostContext } from "@modelcontextprotocol/ext-apps";
import {
  App,
  applyDocumentTheme,
  applyHostFonts,
  applyHostStyleVariables,
} from "@modelcontextprotocol/ext-apps";
import type { ToolCaller } from "./tunnelSocket";
import { McpTunnelWebSocket } from "./tunnelSocket";
import { createTunnelXhrClass } from "./tunnelXhr";

type ShinyGlobal = {
  createSocket?: () => unknown;
  setInputValue?: (name: string, value: unknown) => void;
  addCustomMessageHandler?: (
    type: string,
    handler: (message: unknown) => void,
  ) => void;
};

function getShiny(): ShinyGlobal | undefined {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  return (window as unknown as { Shiny?: ShinyGlobal }).Shiny;
}

// Shiny.setInputValue only exists once Shiny's DOM-ready initialization has
// run; retry briefly so early host notifications aren't dropped.
function setShinyInput(name: string, value: unknown, attempts = 100): void {
  const shiny = getShiny();
  if (shiny?.setInputValue) {
    shiny.setInputValue(name, value);
    return;
  }
  if (attempts > 0) {
    setTimeout(() => setShinyInput(name, value, attempts - 1), 100);
  }
}

function applyHostContext(ctx: Partial<McpUiHostContext> | undefined): void {
  if (!ctx) return;
  if (ctx.theme) applyDocumentTheme(ctx.theme);
  if (ctx.styles) {
    if (ctx.styles.variables) applyHostStyleVariables(ctx.styles.variables);
    if (ctx.styles.css?.fonts) applyHostFonts(ctx.styles.css.fonts);
  }
}

function filenameFromDisposition(disposition: string | null): string {
  if (!disposition) return "download";
  const match = /filename="?([^";]+)"?/.exec(disposition);
  return match ? match[1] : "download";
}

// Intercept clicks on Shiny download links: the sandboxed iframe can't
// navigate to session URLs, so fetch the bytes over the tunnel and hand the
// browser a blob URL instead. (Whether the save dialog is allowed is up to
// the host's sandbox flags, e.g. `allow-downloads`.)
function installDownloadInterceptor(xhrClass: typeof XMLHttpRequest): void {
  document.addEventListener(
    "click",
    (e) => {
      const target = e.target as HTMLElement | null;
      const link = target?.closest?.(
        "a.shiny-download-link",
      ) as HTMLAnchorElement | null;
      if (!link) return;
      const href = link.getAttribute("href");
      if (!href || link.classList.contains("disabled")) return;

      e.preventDefault();
      e.stopPropagation();

      const xhr = new xhrClass();
      xhr.open("GET", href);
      xhr.responseType = "blob";
      xhr.onload = () => {
        if (xhr.status !== 200) {
          console.warn("shiny-mcp-bridge: download failed:", xhr.status);
          return;
        }
        const filename = filenameFromDisposition(
          xhr.getResponseHeader("content-disposition"),
        );
        const url = URL.createObjectURL(xhr.response as Blob);
        const a = document.createElement("a");
        a.href = url;
        a.download = filename;
        document.body.appendChild(a);
        a.click();
        a.remove();
        setTimeout(() => URL.revokeObjectURL(url), 60000);
      };
      xhr.send();
    },
    true,
  );
}

function initShinyMcpBridge(): void {
  const shiny = getShiny();
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
  const connected = app
    .connect()
    .then(() => {
      applyHostContext(app.getHostContext());
    })
    .catch((err) => {
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
  let resolveFirstSocket!: (s: McpTunnelWebSocket) => void;
  const firstSocket = new Promise<McpTunnelWebSocket>((resolve) => {
    resolveFirstSocket = resolve;
  });

  shiny.createSocket = () => {
    socket = new McpTunnelWebSocket(callTool);
    resolveFirstSocket(socket);
    // shinyapp.ts assigns onopen/onmessage/onclose right after
    // createSocket() returns; defer start so they're in place.
    const s = socket;
    setTimeout(() => void s.start(), 0);
    return socket;
  };

  // Tunnel HTTP side channels (uploads, DataTables/selectize ajax, restyle
  // XHRs) through _shiny_http on the session's connection.
  const tunnelXhrClass = createTunnelXhrClass({
    callTool,
    getConnectionId: async () => {
      const s = await firstSocket;
      return await s.whenConnected;
    },
    nativeXhr: window.XMLHttpRequest,
  });
  window.XMLHttpRequest = tunnelXhrClass;
  installDownloadInterceptor(tunnelXhrClass);

  // Host -> app integration -----------------------------------------------

  app.onhostcontextchanged = (params) => {
    applyHostContext(params);
    setShinyInput(
      ".clientdata_mcp_host_context",
      JSON.stringify(app.getHostContext() ?? {}),
    );
  };

  app.ontoolinput = (params) => {
    setShinyInput(
      ".clientdata_mcp_tool_input",
      JSON.stringify(params.arguments ?? {}),
    );
  };

  app.ontoolresult = (params) => {
    setShinyInput(".clientdata_mcp_tool_result", JSON.stringify(params));
  };

  app.onteardown = () => {
    socket?.close();
    return {};
  };

  // App -> host integration (from R via session$sendCustomMessage) ---------

  const registerCustomHandlers = (attempts = 100): void => {
    const s = getShiny();
    if (!s?.addCustomMessageHandler) {
      if (attempts > 0) {
        setTimeout(() => registerCustomHandlers(attempts - 1), 100);
      }
      return;
    }
    s.addCustomMessageHandler("shiny.mcp.updateModelContext", (message) => {
      void connected.then(() =>
        app
          .updateModelContext(
            message as Parameters<typeof app.updateModelContext>[0],
          )
          .catch((err) =>
            console.warn("shiny-mcp-bridge: updateModelContext failed:", err),
          ),
      );
    });
    s.addCustomMessageHandler("shiny.mcp.sendMessage", (message) => {
      void connected.then(() =>
        app
          .sendMessage(message as Parameters<typeof app.sendMessage>[0])
          .catch((err) =>
            console.warn("shiny-mcp-bridge: sendMessage failed:", err),
          ),
      );
    });
  };
  registerCustomHandlers();
}

initShinyMcpBridge();
