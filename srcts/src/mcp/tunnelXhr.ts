// An XMLHttpRequest replacement for the MCP Apps sandbox: same-origin and
// relative requests are tunneled through the app-only `_shiny_http` tool
// (postMessage -> host -> MCP server -> in-process Shiny handlers). This
// covers every HTTP side channel in the Shiny client without touching
// shiny.js itself: jQuery.ajax (file upload POSTs, DataTables/selectize
// requests) creates its XHR via `new window.XMLHttpRequest()` per request,
// and render.ts's restyle fetch uses raw XHR.
//
// Only the XHR surface those callers use is implemented. Foreign-origin
// absolute URLs are delegated to the native XHR (the sandbox CSP decides
// their fate, same as without the patch).

import type { ToolCaller } from "./tunnelSocket";

export interface TunnelXhrOptions {
  callTool: ToolCaller;
  getConnectionId: () => Promise<string>;
  // Origin whose absolute URLs are treated as same-origin (defaults to
  // window.location.origin when available).
  pageOrigin?: string;
  nativeXhr?: typeof XMLHttpRequest;
}

type HttpToolResult = {
  status?: number;
  headers?: { [key: string]: unknown };
  body?: string; // base64
};

function b64ToBytes(b64: string): Uint8Array {
  const bin = atob(b64);
  const bytes = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
  return bytes;
}

function bytesToB64(bytes: Uint8Array): string {
  let bin = "";
  for (let i = 0; i < bytes.length; i++) bin += String.fromCharCode(bytes[i]);
  return btoa(bin);
}

async function encodeBody(
  body: unknown,
): Promise<{ b64: string | null; error?: string }> {
  if (body === null || body === undefined) return { b64: null };
  if (typeof body === "string") {
    return { b64: bytesToB64(new TextEncoder().encode(body)) };
  }
  if (body instanceof ArrayBuffer) {
    return { b64: bytesToB64(new Uint8Array(body)) };
  }
  if (ArrayBuffer.isView(body)) {
    const view = body as ArrayBufferView;
    return {
      b64: bytesToB64(
        new Uint8Array(view.buffer, view.byteOffset, view.byteLength),
      ),
    };
  }
  if (typeof Blob !== "undefined" && body instanceof Blob) {
    const buf = await body.arrayBuffer();
    return { b64: bytesToB64(new Uint8Array(buf)) };
  }
  return { b64: null, error: "Unsupported request body type for MCP tunnel" };
}

function statusText(status: number): string {
  const texts = new Map<number, string>([
    [200, "OK"],
    [202, "Accepted"],
    [204, "No Content"],
    [302, "Found"],
    [400, "Bad Request"],
    [403, "Forbidden"],
    [404, "Not Found"],
    [405, "Method Not Allowed"],
    [500, "Internal Server Error"],
  ]);
  return texts.get(status) ?? "";
}

export function createTunnelXhrClass(
  opts: TunnelXhrOptions,
): typeof XMLHttpRequest {
  const pageOrigin =
    opts.pageOrigin ??
    (typeof window !== "undefined" ? window.location.origin : "null");
  const nativeXhrClass =
    opts.nativeXhr ??
    (typeof XMLHttpRequest !== "undefined" ? XMLHttpRequest : undefined);

  class TunnelXMLHttpRequest {
    readyState = 0;
    status = 0;
    statusText = "";
    responseText = "";
    response: unknown = "";
    responseType = "";
    responseURL = "";
    timeout = 0;
    withCredentials = false;
    // fileProcessor.ts guards `if (xhr.upload)` before assigning onprogress;
    // tunneled uploads complete without incremental progress events.
    upload: { onprogress: ((e: unknown) => void) | null } = {
      onprogress: null,
    };

    onreadystatechange: (() => void) | null = null;
    onload: (() => void) | null = null;
    onerror: (() => void) | null = null;
    onabort: (() => void) | null = null;
    ontimeout: (() => void) | null = null;

    private reqMethod = "GET";
    private reqUrl = "";
    private tunnelPath: string | null = null;
    private reqHeaders: { [key: string]: string } = {};
    private respHeaders: { [key: string]: string } = {};
    private aborted = false;
    private nativeReq: XMLHttpRequest | null = null;

    open(method: string, url: string): void {
      this.reqMethod = method.toUpperCase();
      this.reqUrl = url;
      this.tunnelPath = this._classify(url);
      this.readyState = 1;
    }

    setRequestHeader(name: string, value: string): void {
      this.reqHeaders[name] = value;
    }

    overrideMimeType(): void {
      // Not needed for tunneled responses.
    }

    getResponseHeader(name: string): string | null {
      return this.respHeaders[name.toLowerCase()] ?? null;
    }

    getAllResponseHeaders(): string {
      return Object.entries(this.respHeaders)
        .map(([k, v]) => `${k}: ${v}\r\n`)
        .join("");
    }

    abort(): void {
      this.aborted = true;
      this.nativeReq?.abort();
      if (this.readyState !== 4) {
        this.readyState = 4;
        this.onabort?.();
      }
    }

    send(body?: unknown): void {
      if (this.tunnelPath === null) {
        this._delegateToNative(body);
        return;
      }
      void this._sendTunneled(body);
    }

    private _classify(url: string): string | null {
      let parsed: URL;
      try {
        parsed = new URL(url, "http://tunnel.invalid");
      } catch {
        return null;
      }
      if (parsed.origin === "http://tunnel.invalid") {
        // Relative URL
        const path = parsed.pathname + parsed.search;
        return path.startsWith("/") ? path : "/" + path;
      }
      if (parsed.origin === pageOrigin) {
        return parsed.pathname + parsed.search;
      }
      return null;
    }

    private async _sendTunneled(body: unknown): Promise<void> {
      try {
        const encoded = await encodeBody(body);
        if (encoded.error) throw new Error(encoded.error);
        const connectionId = await opts.getConnectionId();
        const args: { [key: string]: unknown } = {
          method: this.reqMethod,
          path: this.tunnelPath,
          headers: this.reqHeaders,
        };
        // Empty id = direct-connect session (no tunnel connection to tie to)
        if (connectionId) args.connectionId = connectionId;
        if (encoded.b64 !== null) args.body = encoded.b64;

        const result = (await opts.callTool(
          "_shiny_http",
          args,
        )) as HttpToolResult;
        if (this.aborted) return;

        this.status = result.status ?? 0;
        this.statusText = statusText(this.status);
        this.respHeaders = {};
        for (const [k, v] of Object.entries(result.headers ?? {})) {
          this.respHeaders[k.toLowerCase()] = String(v);
        }

        const bytes = result.body ? b64ToBytes(result.body) : new Uint8Array(0);
        this._setResponse(bytes);
        this.readyState = 4;
        this.onreadystatechange?.();
        this.onload?.();
      } catch (err) {
        if (this.aborted) return;
        this.status = 0;
        this.readyState = 4;
        this.onreadystatechange?.();
        this.onerror?.();
        console.warn("shiny-mcp-bridge: tunneled XHR failed:", err);
      }
    }

    private _setResponse(bytes: Uint8Array): void {
      const contentType = this.respHeaders["content-type"] ?? "";
      switch (this.responseType) {
        case "arraybuffer":
          this.response = bytes.buffer.slice(
            bytes.byteOffset,
            bytes.byteOffset + bytes.byteLength,
          );
          break;
        case "blob":
          this.response = new Blob([bytes], { type: contentType });
          break;
        case "json": {
          const text = new TextDecoder().decode(bytes);
          try {
            this.response = JSON.parse(text);
          } catch {
            this.response = null;
          }
          break;
        }
        default: {
          const text = new TextDecoder().decode(bytes);
          this.responseText = text;
          this.response = text;
        }
      }
    }

    private _delegateToNative(body: unknown): void {
      if (!nativeXhrClass) {
        this.status = 0;
        this.readyState = 4;
        this.onerror?.();
        return;
      }
      const native = new nativeXhrClass();
      this.nativeReq = native;
      native.open(this.reqMethod, this.reqUrl, true);
      for (const [k, v] of Object.entries(this.reqHeaders)) {
        native.setRequestHeader(k, v);
      }
      if (this.responseType) {
        native.responseType = this.responseType as XMLHttpRequestResponseType;
      }
      native.onreadystatechange = () => {
        this.readyState = native.readyState;
        if (native.readyState === 4) {
          this.status = native.status;
          this.statusText = native.statusText;
          this.response = native.response;
          if (!this.responseType || this.responseType === "text") {
            this.responseText = native.responseText;
          }
          this.respHeaders = {};
          for (const line of native.getAllResponseHeaders().split("\r\n")) {
            const idx = line.indexOf(": ");
            if (idx > 0) {
              this.respHeaders[line.slice(0, idx).toLowerCase()] = line.slice(
                idx + 2,
              );
            }
          }
        }
        this.onreadystatechange?.();
      };
      native.onload = () => this.onload?.();
      native.onerror = () => this.onerror?.();
      native.onabort = () => this.onabort?.();
      native.ontimeout = () => this.ontimeout?.();
      native.send(body as XMLHttpRequestBodyInit | null | undefined);
    }
  }

  return TunnelXMLHttpRequest as unknown as typeof XMLHttpRequest;
}
