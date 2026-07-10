// A WebSocket-shaped transport that tunnels Shiny's protocol over MCP Apps
// tool calls (postMessage -> host -> MCP server). Shiny's client uses this
// via the `Shiny.createSocket` extension point; it only requires `send()`,
// `close()`, a truthy `readyState` when open, and settable
// `onopen`/`onmessage`/`onclose` (see srcts/src/shiny/shinyapp.ts).
//
// Outbound frames go through the app-only `_shiny_send` tool. Inbound frames
// are pulled by a long-poll loop on `_shiny_receive`; the server resolves the
// call as soon as frames are available (or with an empty batch on timeout),
// and keeping a single receive in flight guarantees frame ordering.

export type ToolCaller = (
  name: string,
  args: { [key: string]: unknown },
) => Promise<{ [key: string]: unknown }>;

type Frame = { data: string; binary?: boolean };
type ReceiveResult = { frames?: Frame[]; closed?: boolean };

function b64ToArrayBuffer(b64: string): ArrayBuffer {
  const bin = atob(b64);
  const bytes = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
  return bytes.buffer;
}

function arrayBufferToB64(buf: ArrayBuffer): string {
  const bytes = new Uint8Array(buf);
  let bin = "";
  for (let i = 0; i < bytes.length; i++) bin += String.fromCharCode(bytes[i]);
  return btoa(bin);
}

export class McpTunnelWebSocket {
  readyState = 0; // CONNECTING
  binaryType = "arraybuffer";
  allowReconnect = false;

  onopen: (() => void) | null = null;
  onmessage: ((event: { data: string | ArrayBuffer }) => void) | null = null;
  onclose: ((event: { code: number }) => void) | null = null;
  onerror: ((event: unknown) => void) | null = null;

  private connectionId: string | null = null;

  constructor(private callTool: ToolCaller) {}

  async start(): Promise<void> {
    try {
      const result = await this.callTool("_shiny_connect", {});
      this.connectionId = String(result.connectionId);
      this.readyState = 1; // OPEN
      this.onopen?.();
      void this._pollLoop();
    } catch (err) {
      this._fail(err);
    }
  }

  send(data: string | ArrayBuffer): void {
    if (this.readyState !== 1 || this.connectionId === null) return;
    const frame: Frame =
      typeof data === "string"
        ? { data, binary: false }
        : { data: arrayBufferToB64(data), binary: true };
    this.callTool("_shiny_send", {
      connectionId: this.connectionId,
      frames: [frame],
    }).catch((err) => this._fail(err));
  }

  close(code = 1000): void {
    if (this.readyState >= 2) return;
    this.readyState = 2; // CLOSING
    const id = this.connectionId;
    this.connectionId = null;
    if (id !== null) {
      void this.callTool("_shiny_close", { connectionId: id }).catch(() => {
        // Best effort; the server GCs idle connections anyway.
      });
    }
    this._finishClose(code);
  }

  private async _pollLoop(): Promise<void> {
    while (this.readyState === 1 && this.connectionId !== null) {
      let result: ReceiveResult;
      try {
        result = (await this.callTool("_shiny_receive", {
          connectionId: this.connectionId,
        })) as ReceiveResult;
      } catch (err) {
        this._fail(err);
        return;
      }
      for (const frame of result.frames ?? []) {
        if (this.readyState !== 1) break;
        const data = frame.binary ? b64ToArrayBuffer(frame.data) : frame.data;
        this.onmessage?.({ data });
      }
      if (result.closed) {
        this._finishClose(1000);
        return;
      }
    }
  }

  private _fail(err: unknown): void {
    this.onerror?.(err);
    this._finishClose(1006);
  }

  private _finishClose(code: number): void {
    if (this.readyState === 3) return;
    this.readyState = 3; // CLOSED
    this.onclose?.({ code });
  }
}
