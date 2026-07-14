import type { ToolCaller } from "./tunnelSocket";
export interface TunnelXhrOptions {
    callTool: ToolCaller;
    getConnectionId: () => Promise<string>;
    pageOrigin?: string;
    nativeXhr?: typeof XMLHttpRequest;
}
export declare function createTunnelXhrClass(opts: TunnelXhrOptions): typeof XMLHttpRequest;
