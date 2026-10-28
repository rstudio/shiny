import type { JSONSchema, ShinyDrive } from "./drive";
type ToolContent = {
    type: "image";
    data: string;
    mimeType: string;
} | {
    type: "text";
    text: string;
};
type ToolResult = {
    content: ToolContent[];
    structuredContent?: unknown;
    isError?: boolean;
};
type AgentTool = {
    name: string;
    description: string;
    title?: string;
    inputSchema?: JSONSchema;
    annotations?: {
        readOnlyHint?: boolean;
        consequentialHint?: boolean;
        untrustedContentHint?: boolean;
    };
    execute: (args: {
        [key: string]: unknown;
    }) => Promise<unknown> | unknown;
};
declare class ShinyWebMCP extends EventTarget {
    #private;
    constructor(drive: ShinyDrive);
    /** Register (or replace) a tool. Returns a function that unregisters it. */
    registerTool(tool: AgentTool): () => void;
    unregisterTool(name: string): void;
    /** Tool descriptors (no `execute`), e.g. to forward to another protocol. */
    listTools(): Array<Omit<AgentTool, "execute">>;
    /** Call a tool by name. Errors are returned as `isError` results. */
    callTool(name: string, args?: {
        [key: string]: unknown;
    }): Promise<ToolResult>;
    /** Unregister the built-in tools. Author-registered tools are untouched. */
    disable(): void;
    /** Register the four built-in tools. Their definitions never change. */
    enable(): void;
}
export { ShinyWebMCP };
export type { AgentTool, ToolResult };
