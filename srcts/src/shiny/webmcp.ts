import type { Filter, JSONSchema, ShinyDrive } from "./drive";
import { previewTextLength } from "./drive";

// WebMCP integration, exposed as `window.Shiny.webmcp`.
//
// A tool registry whose tools are mirrored to `document.modelContext` when the
// browser (or a polyfill) provides it. `enable()` registers four built-in
// tools on top of `Shiny.drive`; their definitions never change, so an app with
// thousands of inputs costs the same context as one with three, and the agent
// discovers ids on demand with `list_inputs` / `list_outputs`.

type ToolContent =
  | { type: "image"; data: string; mimeType: string }
  | { type: "text"; text: string };

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
  execute: (args: { [key: string]: unknown }) => Promise<unknown> | unknown;
};

const filterSchema = {
  type: "object",
  properties: {
    ids: {
      type: "array",
      items: { type: "string" },
      description: "Only these exact ids.",
    },
    module: {
      type: "string",
      description:
        'Only ids inside this Shiny module namespace, e.g. "filters" matches "filters-region". Nested: "outer-inner".',
    },
  },
};

const builtinToolNames = [
  "list_inputs",
  "set_input",
  "list_outputs",
  "get_output",
];

class ShinyWebMCP extends EventTarget {
  #drive: ShinyDrive;
  #tools = new Map<string, AgentTool>();
  #webmcp = new Map<string, AbortController>();
  #enabled = false;

  constructor(drive: ShinyDrive) {
    super();
    this.#drive = drive;
  }

  /** Register (or replace) a tool. Returns a function that unregisters it. */
  registerTool(tool: AgentTool): () => void {
    if (!/^[A-Za-z0-9_.-]{1,128}$/.test(tool.name)) {
      throw new Error(`Invalid tool name "${tool.name}"`);
    }
    this.#tools.set(tool.name, tool);
    this.#mirrorToWebMCP(tool);
    this.dispatchEvent(new Event("toolchange"));
    return () => this.unregisterTool(tool.name);
  }

  unregisterTool(name: string): void {
    if (this.#tools.delete(name)) {
      this.#webmcp.get(name)?.abort();
      this.#webmcp.delete(name);
      this.dispatchEvent(new Event("toolchange"));
    }
  }

  /** Tool descriptors (no `execute`), e.g. to forward to another protocol. */
  listTools(): Array<Omit<AgentTool, "execute">> {
    return Array.from(
      this.#tools.values(),
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      ({ execute, ...rest }) => rest,
    );
  }

  /** Call a tool by name. Errors are returned as `isError` results. */
  async callTool(
    name: string,
    args: { [key: string]: unknown } = {},
  ): Promise<ToolResult> {
    try {
      const tool = this.#tools.get(name);

      if (!tool) throw new Error(`Unknown tool "${name}"`);
      return toToolResult(await tool.execute(args ?? {}));
    } catch (e) {
      return {
        isError: true,
        content: [{ type: "text", text: String((e as Error)?.message ?? e) }],
      };
    }
  }

  // ---- Built-in tools ----------------------------------------------------

  /** Unregister the built-in tools. Author-registered tools are untouched. */
  disable(): void {
    if (!this.#enabled) return;
    this.#enabled = false;
    builtinToolNames.forEach((name) => this.unregisterTool(name));
  }

  /** Register the four built-in tools. Their definitions never change. */
  enable(): void {
    if (this.#enabled) return;
    this.#enabled = true;

    this.registerTool({
      name: "list_inputs",
      description:
        "List the inputs currently visible in this Shiny app: id, label, type, access (read-write, read-only, or write-only), current value (omitted when write-only), disabled, and `schema` (the JSON Schema that set_input's `value` must satisfy; null means a button that set_input clicks; omitted when read-only). Filter with `ids` and/or a `module` namespace to keep results small. The visible UI changes as inputs change (tabs, dynamic UI), so list again if an id goes missing.",
      inputSchema: filterSchema,
      annotations: { readOnlyHint: true },
      execute: (args) => ({ inputs: this.#drive.listInputs(toFilter(args)) }),
    });
    this.registerTool({
      name: "set_input",
      description:
        "Set a visible input as if the user changed it, wait for the app to finish updating, and return the input's new state. `value` must satisfy the input's `schema` from list_inputs; omit it to click a button. If the app has a submit/apply button, changes may not take effect until you click it. Can trigger application side effects.",
      inputSchema: {
        type: "object",
        properties: {
          id: { type: "string", description: "Input id from list_inputs." },
          value: { description: "New value; omit for buttons." },
        },
        required: ["id"],
      },
      execute: ({ id, value }) => this.#drive.setInput(String(id), value),
    });
    this.registerTool({
      name: "list_outputs",
      description: `List the outputs currently visible in this Shiny app: id, type, status (ready, recalculating, error), the first ${previewTextLength} characters of displayed text, and whether it shows an image. Filter with \`ids\` and/or a \`module\` namespace.`,
      inputSchema: filterSchema,
      annotations: { readOnlyHint: true, untrustedContentHint: true },
      execute: (args) => ({ outputs: this.#drive.listOutputs(toFilter(args)) }),
    });
    this.registerTool({
      name: "get_output",
      description:
        "Get everything a visible output currently displays: status, full visible text, and the image for plots.",
      inputSchema: {
        type: "object",
        properties: {
          id: { type: "string", description: "Output id from list_outputs." },
        },
        required: ["id"],
      },
      annotations: { readOnlyHint: true, untrustedContentHint: true },
      execute: ({ id }) => this.#drive.getOutput(String(id)),
    });
  }

  // ---- WebMCP mirror -----------------------------------------------------

  #mirrorToWebMCP(tool: AgentTool): void {
    const modelContext = (
      document as Document & {
        modelContext?: {
          registerTool: (
            tool: AgentTool,
            opts: { signal: AbortSignal },
          ) => Promise<void>;
        };
      }
    ).modelContext;

    if (!modelContext?.registerTool) return;

    // WebMCP has no update; abort + re-register
    this.#webmcp.get(tool.name)?.abort();
    const controller = new AbortController();

    this.#webmcp.set(tool.name, controller);
    modelContext
      .registerTool(
        { ...tool, execute: (args) => this.callTool(tool.name, args) },
        { signal: controller.signal },
      )
      .catch((e) => {
        if (!controller.signal.aborted) {
          console.warn(`WebMCP: failed to register tool "${tool.name}"`, e);
        }
      });
  }
}

// Tool args come from the agent; don't trust that the host checked them.
function toFilter({ ids, module }: { [key: string]: unknown }): Filter {
  if (
    ids !== undefined &&
    !(Array.isArray(ids) && ids.every((id) => typeof id === "string"))
  ) {
    throw new Error("`ids` must be an array of strings");
  }
  if (module !== undefined && typeof module !== "string") {
    throw new Error("`module` must be a string");
  }
  return { ids: ids as string[] | undefined, module };
}

// MCP-style CallToolResult. Images (e.g. plots) become image content so
// multimodal agents can see them.
function toToolResult(value: unknown): ToolResult {
  if (typeof value !== "object" || value === null) {
    return { content: [{ type: "text", text: String(value ?? "") }] };
  }
  // structuredContent must be an object
  if (Array.isArray(value)) {
    return { content: [{ type: "text", text: JSON.stringify(value) }] };
  }
  // Custom tools may already return a CallToolResult
  if (Array.isArray((value as ToolResult).content)) return value as ToolResult;

  const { image, ...rest } = value as { image?: string };
  const content: ToolContent[] = [{ type: "text", text: JSON.stringify(rest) }];
  const match = image?.match(/^data:(image\/[^;]+);base64,(.*)$/);

  if (match)
    content.push({ type: "image", mimeType: match[1], data: match[2] });
  return { content, structuredContent: rest };
}

export { ShinyWebMCP };
export type { AgentTool, ToolResult };
