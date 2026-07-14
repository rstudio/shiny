// The direct-connect fast path: when the ui://shiny/app resource declared
// the app's origin in its CSP (`connectDomains`), spec-compliant hosts let
// the sandboxed iframe open a real WebSocket straight to the app —
// dramatically lower latency than tunneling frames through tools/call.
// Hosts that ignore declared CSP block the attempt, so this facade tries a
// direct WebSocket (bounded by a timeout) and falls back to the
// McpTunnelWebSocket. Shiny only sees the facade: onopen fires once, after
// a transport has been adopted.

import type { McpTunnelWebSocket } from "./tunnelSocket";

type SocketEvents = {
  onopen: (() => void) | null;
  onmessage: ((event: { data: string | ArrayBuffer }) => void) | null;
  onclose: ((event: { code: number }) => void) | null;
  onerror: ((event: unknown) => void) | null;
};

export interface HybridSocketOptions {
  // Full websocket URL for the direct attempt (null = tunnel only)
  directWsUrl: string | null;
  makeTunnel: () => McpTunnelWebSocket;
  timeoutMs?: number;
  wsConstructor?: typeof WebSocket;
  // Called once a transport is adopted (for diagnostics/tests)
  onTransport?: (kind: "direct" | "tunnel") => void;
}

export class McpHybridWebSocket implements SocketEvents {
  readyState = 0; // CONNECTING
  binaryType = "arraybuffer";
  allowReconnect = false;
  transportKind: "pending" | "direct" | "tunnel" = "pending";

  onopen: (() => void) | null = null;
  onmessage: ((event: { data: string | ArrayBuffer }) => void) | null = null;
  onclose: ((event: { code: number }) => void) | null = null;
  onerror: ((event: unknown) => void) | null = null;

  private transport: WebSocket | McpTunnelWebSocket | null = null;
  private closedEarly = false;

  constructor(private opts: HybridSocketOptions) {}

  async start(): Promise<void> {
    const direct = this.opts.directWsUrl
      ? await this._tryDirect(this.opts.directWsUrl)
      : null;
    if (this.closedEarly) {
      direct?.close();
      return;
    }
    if (direct) {
      this._adopt(direct, "direct");
      this.readyState = 1;
      this.onopen?.();
    } else {
      const tunnel = this.opts.makeTunnel();
      this._adopt(tunnel, "tunnel");
      tunnel.onopen = () => {
        this.readyState = 1;
        this.onopen?.();
      };
      void tunnel.start();
    }
  }

  send(data: string | ArrayBuffer): void {
    if (this.readyState !== 1 || !this.transport) return;
    this.transport.send(data);
  }

  close(code = 1000): void {
    if (this.readyState >= 2) return;
    if (!this.transport) {
      // Direct attempt still in flight; remember to discard it.
      this.closedEarly = true;
      this.readyState = 3;
      this.onclose?.({ code });
      return;
    }
    this.readyState = 2;
    this.transport.close(code);
  }

  private _adopt(
    transport: WebSocket | McpTunnelWebSocket,
    kind: "direct" | "tunnel",
  ): void {
    this.transport = transport;
    this.transportKind = kind;
    this.opts.onTransport?.(kind);
    transport.onmessage = (e: { data: string | ArrayBuffer }) => {
      this.onmessage?.({ data: e.data });
    };
    transport.onclose = (e: { code: number }) => {
      this.readyState = 3;
      this.onclose?.({ code: e.code });
    };
    transport.onerror = (e: unknown) => {
      this.onerror?.(e);
    };
  }

  private _tryDirect(url: string): Promise<WebSocket | null> {
    const wsCtor =
      this.opts.wsConstructor ??
      (typeof WebSocket !== "undefined" ? WebSocket : undefined);
    if (!wsCtor) return Promise.resolve(null);

    return new Promise((resolve) => {
      let settled = false;
      const settle = (value: WebSocket | null) => {
        if (settled) return;
        settled = true;
        clearTimeout(timer);
        resolve(value);
      };

      let ws: WebSocket;
      try {
        ws = new wsCtor(url);
      } catch {
        // CSP violations can throw synchronously
        resolve(null);
        return;
      }
      ws.binaryType = "arraybuffer";
      const timer = setTimeout(() => {
        try {
          ws.close();
        } catch {
          // ignore
        }
        settle(null);
      }, this.opts.timeoutMs ?? 2500);

      ws.onopen = () => settle(ws);
      ws.onerror = () => settle(null);
      ws.onclose = () => settle(null);
    });
  }
}
