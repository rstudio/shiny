import type { McpTunnelWebSocket } from "./tunnelSocket";
type SocketEvents = {
    onopen: (() => void) | null;
    onmessage: ((event: {
        data: string | ArrayBuffer;
    }) => void) | null;
    onclose: ((event: {
        code: number;
    }) => void) | null;
    onerror: ((event: unknown) => void) | null;
};
export interface HybridSocketOptions {
    directWsUrl: string | null;
    makeTunnel: () => McpTunnelWebSocket;
    timeoutMs?: number;
    wsConstructor?: typeof WebSocket;
    onTransport?: (kind: "direct" | "tunnel") => void;
}
export declare class McpHybridWebSocket implements SocketEvents {
    private opts;
    readyState: number;
    binaryType: string;
    allowReconnect: boolean;
    transportKind: "pending" | "direct" | "tunnel";
    onopen: (() => void) | null;
    onmessage: ((event: {
        data: string | ArrayBuffer;
    }) => void) | null;
    onclose: ((event: {
        code: number;
    }) => void) | null;
    onerror: ((event: unknown) => void) | null;
    private transport;
    private closedEarly;
    constructor(opts: HybridSocketOptions);
    start(): Promise<void>;
    send(data: string | ArrayBuffer): void;
    close(code?: number): void;
    private _adopt;
    private _tryDirect;
}
export {};
