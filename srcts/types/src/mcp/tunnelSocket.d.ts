export type ToolCaller = (name: string, args: {
    [key: string]: unknown;
}) => Promise<{
    [key: string]: unknown;
}>;
export declare class McpTunnelWebSocket {
    private callTool;
    readyState: number;
    binaryType: string;
    allowReconnect: boolean;
    onopen: (() => void) | null;
    onmessage: ((event: {
        data: string | ArrayBuffer;
    }) => void) | null;
    onclose: ((event: {
        code: number;
    }) => void) | null;
    onerror: ((event: unknown) => void) | null;
    private connectionId;
    private resolveConnected;
    readonly whenConnected: Promise<string>;
    constructor(callTool: ToolCaller);
    start(): Promise<void>;
    send(data: string | ArrayBuffer): void;
    close(code?: number): void;
    private _pollLoop;
    private _fail;
    private _finishClose;
}
