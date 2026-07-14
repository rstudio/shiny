#!/usr/bin/env node
// A minimal MCP gateway that merges several Shiny MCP endpoints into one
// MCP server, so a single connector exposes many Shiny apps.
//
// Each backend Shiny app should set options(shiny.mcp.appId = "<unique>")
// so its internal `_shiny_*` tunnel tools and its ui://shiny/<appId>
// resource don't collide with the other apps'. App-facing tool names
// (options(shiny.mcp.tool/tools)) must also be unique across apps.
//
// Usage:
//   stdio (Claude Desktop):
//     node shiny-mcp-gateway.mjs URL [URL...]
//   HTTP (basic-host, claude.ai custom connector):
//     node shiny-mcp-gateway.mjs --port 7790 URL [URL...]
//
// where each URL is a Shiny app's MCP endpoint, e.g.
//   http://127.0.0.1:7788/mcp  https://barret.shinyapps.io/shiny-mcp-demo/mcp
//
// Requests are forwarded verbatim: initialize is answered locally,
// tools/list + resources/list are merged across backends, and tools/call +
// resources/read are routed via name/uri maps built from those lists.

import { createServer } from "node:http";
import { createInterface } from "node:readline";

const args = process.argv.slice(2);
let port = null;
const backends = [];
for (let i = 0; i < args.length; i++) {
  if (args[i] === "--port") {
    port = Number(args[++i]);
  } else {
    backends.push(args[i]);
  }
}
if (backends.length === 0) {
  console.error("usage: shiny-mcp-gateway.mjs [--port N] URL [URL...]");
  process.exit(1);
}

const log = (...msg) => console.error("[shiny-mcp-gateway]", ...msg);

// Sticky-session cookies per backend. shinyapps.io (and similar platforms)
// pin a client to one app instance via a session cookie; without it,
// consecutive _shiny_* calls could hit different instances and lose the
// Shiny session.
const cookieJar = new Map();

function rememberCookies(url, res) {
  const setCookies =
    typeof res.headers.getSetCookie === "function"
      ? res.headers.getSetCookie()
      : [res.headers.get("set-cookie")].filter(Boolean);
  if (setCookies.length === 0) return;
  const jar = cookieJar.get(url) ?? new Map();
  for (const line of setCookies) {
    const [pair] = line.split(";");
    const eq = pair.indexOf("=");
    if (eq > 0) jar.set(pair.slice(0, eq).trim(), pair.slice(eq + 1).trim());
  }
  cookieJar.set(url, jar);
}

function cookieHeader(url) {
  const jar = cookieJar.get(url);
  if (!jar || jar.size === 0) return {};
  const value = [...jar].map(([k, v]) => `${k}=${v}`).join("; ");
  return { cookie: value };
}

async function postBackend(url, method, params, id) {
  const res = await fetch(url, {
    method: "POST",
    headers: { "content-type": "application/json", ...cookieHeader(url) },
    body: JSON.stringify({ jsonrpc: "2.0", id, method, params }),
  });
  rememberCookies(url, res);
  if (!res.ok) throw new Error(`${url} ${method} -> HTTP ${res.status}`);
  return res.json();
}

// Hosted platforms cold-start a sleeping app on GET but reject POST with
// e.g. 405 (observed on shinyapps.io). On failure, GET the app's base URL
// to wake it, then retry the POST with backoff.
async function wakeBackend(url) {
  const base = url.replace(/\/mcp\/?$/, "/");
  try {
    const res = await fetch(base, { headers: cookieHeader(url) });
    rememberCookies(url, res);
  } catch {
    /* the retry loop will surface persistent failures */
  }
}

async function callBackend(url, method, params, id = 0) {
  const delays = [0, 1000, 3000, 8000];
  let lastErr;
  for (const delay of delays) {
    if (delay > 0) {
      await new Promise((resolve) => setTimeout(resolve, delay));
    }
    try {
      return await postBackend(url, method, params, id);
    } catch (err) {
      lastErr = err;
      log(`${method} failed (${err.message}); waking ${url}`);
      await wakeBackend(url);
    }
  }
  throw lastErr;
}

// name -> backend URL, uri -> backend URL. Rebuilt on demand.
const toolRoutes = new Map();
const resourceRoutes = new Map();

async function refreshRoutes() {
  toolRoutes.clear();
  resourceRoutes.clear();
  const mergedTools = [];
  const mergedResources = [];
  for (const url of backends) {
    try {
      const [tools, resources] = await Promise.all([
        callBackend(url, "tools/list", {}),
        callBackend(url, "resources/list", {}),
      ]);
      for (const tool of tools.result?.tools ?? []) {
        if (toolRoutes.has(tool.name)) {
          log(`duplicate tool '${tool.name}' from ${url} ignored; set a`,
            "unique options(shiny.mcp.appId/tool) per app");
          continue;
        }
        toolRoutes.set(tool.name, url);
        mergedTools.push(tool);
      }
      for (const resource of resources.result?.resources ?? []) {
        if (resourceRoutes.has(resource.uri)) {
          log(`duplicate resource '${resource.uri}' from ${url} ignored`);
          continue;
        }
        resourceRoutes.set(resource.uri, url);
        mergedResources.push(resource);
      }
    } catch (err) {
      log(`backend ${url} unavailable:`, err.message);
    }
  }
  return { tools: mergedTools, resources: mergedResources };
}

async function handle(body) {
  const { id, method, params } = body;
  const reply = (result) => ({ jsonrpc: "2.0", id, result });
  const fail = (code, message) => ({
    jsonrpc: "2.0",
    id,
    error: { code, message },
  });

  // Notifications get no response.
  if (id === undefined || id === null) return null;

  try {
    switch (method) {
      case "initialize":
        return reply({
          protocolVersion: params?.protocolVersion ?? "2025-06-18",
          capabilities: {
            tools: { listChanged: false },
            resources: { subscribe: false, listChanged: false },
          },
          serverInfo: { name: "shiny-gateway", version: "0.1.0" },
        });
      case "ping":
        return reply({});
      case "tools/list":
        return reply({ tools: (await refreshRoutes()).tools });
      case "resources/list":
        return reply({ resources: (await refreshRoutes()).resources });
      case "tools/call": {
        let url = toolRoutes.get(params?.name);
        if (!url) {
          await refreshRoutes();
          url = toolRoutes.get(params?.name);
        }
        if (!url) return fail(-32602, `Unknown tool: ${params?.name}`);
        return callBackend(url, method, params, id);
      }
      case "resources/read": {
        let url = resourceRoutes.get(params?.uri);
        if (!url) {
          await refreshRoutes();
          url = resourceRoutes.get(params?.uri);
        }
        if (!url) return fail(-32602, `Unknown resource: ${params?.uri}`);
        return callBackend(url, method, params, id);
      }
      default:
        return fail(-32601, `Method not found: ${method}`);
    }
  } catch (err) {
    return fail(-32603, err.message);
  }
}

if (port !== null) {
  const server = createServer((req, res) => {
    const cors = {
      "access-control-allow-origin": "*",
      "access-control-allow-methods": "GET, POST, OPTIONS",
      "access-control-allow-headers":
        "Content-Type, Authorization, Mcp-Session-Id, Mcp-Protocol-Version",
    };
    if (req.method === "OPTIONS") {
      res.writeHead(204, cors);
      return res.end();
    }
    if (req.method !== "POST") {
      res.writeHead(405, { ...cors, "content-type": "application/json" });
      return res.end(JSON.stringify({ error: "POST only" }));
    }
    let data = "";
    req.on("data", (chunk) => (data += chunk));
    req.on("end", async () => {
      let response = null;
      try {
        response = await handle(JSON.parse(data));
      } catch (err) {
        response = {
          jsonrpc: "2.0",
          id: null,
          error: { code: -32700, message: "Parse error: " + err.message },
        };
      }
      if (response === null) {
        res.writeHead(202, cors);
        return res.end();
      }
      res.writeHead(200, { ...cors, "content-type": "application/json" });
      res.end(JSON.stringify(response));
    });
  });
  server.listen(port, "127.0.0.1", () => {
    log(`HTTP on http://127.0.0.1:${port}/mcp for ${backends.length} app(s)`);
  });
} else {
  let pending = 0;
  let stdinClosed = false;
  const maybeExit = () => {
    if (stdinClosed && pending === 0) process.exit(0);
  };
  const rl = createInterface({ input: process.stdin, terminal: false });
  rl.on("line", (line) => {
    line = line.trim();
    if (!line) return;
    pending++;
    void (async () => {
      let response = null;
      try {
        response = await handle(JSON.parse(line));
      } catch (err) {
        response = {
          jsonrpc: "2.0",
          id: null,
          error: { code: -32700, message: "Parse error: " + err.message },
        };
      }
      if (response !== null) {
        process.stdout.write(JSON.stringify(response) + "\n");
      }
      pending--;
      maybeExit();
    })();
  });
  // Drain in-flight requests before exiting on stdin close.
  rl.on("close", () => {
    stdinClosed = true;
    maybeExit();
  });
  log(`stdio gateway for ${backends.length} app(s)`);
}
