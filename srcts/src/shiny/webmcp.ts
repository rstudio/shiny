import type { Filter, JSONSchema, OutputImage, ShinyDrive } from "./drive";
import { defaultImageLimit, previewTextLength } from "./drive";
import { parseToolArgs } from "./driveSchema";

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
  additionalProperties: false,
};

type RegisterStatus = "failed" | "pending" | "registered";

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
  #status = new Map<string, RegisterStatus>();
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
      if (this.#status.has(name)) this.#setStatus(name, undefined);
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

  /**
   * Call a tool by name. `args` may be an object or a JSON string of one.
   * Errors are returned as `isError` results.
   */
  async callTool(name: string, args: unknown = {}): Promise<ToolResult> {
    try {
      const tool = this.#tools.get(name);

      if (!tool) {
        throw new Error(
          `Unknown tool "${name}". Tools: ${[...this.#tools.keys()].join(", ")}`,
        );
      }
      return toToolResult(
        await tool.execute(parseToolArgs(tool.inputSchema, args)),
      );
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
        additionalProperties: false,
      },
      execute: ({ id, value }) => this.#drive.setInput(String(id), value),
    });
    this.registerTool({
      name: "list_outputs",
      description: `List the outputs currently visible in this Shiny app: id, type, status (ready, recalculating, error), the first ${previewTextLength} characters of displayed text, and \`imageCount\` (how many images, such as plots, it shows). Filter with \`ids\` and/or a \`module\` namespace.`,
      inputSchema: filterSchema,
      annotations: { readOnlyHint: true, untrustedContentHint: true },
      execute: (args) => ({ outputs: this.#drive.listOutputs(toFilter(args)) }),
    });
    this.registerTool({
      name: "get_output",
      description: `Get everything a visible output currently displays: status, full visible text, and up to ${defaultImageLimit} of its images (plots, <img>, <canvas>). Each \`images\` entry has an \`index\` and a \`type\`: "data" (attached as image content, in order), "url", or "error" with a \`reason\`. More images remain while \`imageOffset\` + the number of entries < \`imageCount\`; get them with \`imageOffset\`.`,
      inputSchema: {
        type: "object",
        properties: {
          id: { type: "string", description: "Output id from list_outputs." },
          imageOffset: {
            type: "integer",
            minimum: 0,
            description: "Index of the first image to return. Default 0.",
          },
          imageLimit: {
            type: "integer",
            minimum: 0,
            maximum: defaultImageLimit,
            description: `Most images to return. Default ${defaultImageLimit}.`,
          },
        },
        required: ["id"],
        additionalProperties: false,
      },
      annotations: { readOnlyHint: true, untrustedContentHint: true },
      execute: ({ id, imageOffset, imageLimit }) =>
        this.#drive.getOutput(String(id), {
          imageOffset: imageOffset as number | undefined,
          // Hosts may not enforce `maximum`; keep responses bounded
          imageLimit: Math.min(
            (imageLimit as number | undefined) ?? defaultImageLimit,
            defaultImageLimit,
          ),
        }),
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

    if (!modelContext?.registerTool) {
      setStatusAttr("unavailable");
      return;
    }

    // WebMCP has no update; abort + re-register
    this.#webmcp.get(tool.name)?.abort();
    const controller = new AbortController();

    this.#webmcp.set(tool.name, controller);
    this.#setStatus(tool.name, "pending");
    // registerTool() may return nothing or throw synchronously
    Promise.resolve()
      .then(() =>
        modelContext.registerTool(
          { ...tool, execute: (args) => this.callTool(tool.name, args) },
          { signal: controller.signal },
        ),
      )
      .then(
        () => {
          if (!controller.signal.aborted) {
            this.#setStatus(tool.name, "registered");
          }
        },
        (e) => {
          if (controller.signal.aborted) return;
          this.#setStatus(tool.name, "failed");
          console.warn(`WebMCP: failed to register tool "${tool.name}"`, e);
        },
      );
  }

  // `<html data-shiny-webmcp>`: pending, registered, partial (some tools
  // failed), failed, or unavailable (no `document.modelContext`).
  #setStatus(name: string, status: RegisterStatus | undefined): void {
    if (status) this.#status.set(name, status);
    else this.#status.delete(name);

    const all = [...this.#status.values()];

    if (all.length === 0) {
      document.documentElement.removeAttribute("data-shiny-webmcp");
      return;
    }
    setStatusAttr(
      all.includes("pending")
        ? "pending"
        : !all.includes("failed")
          ? "registered"
          : all.includes("registered")
            ? "partial"
            : "failed",
    );
  }
}

function setStatusAttr(status: string): void {
  document.documentElement.setAttribute("data-shiny-webmcp", status);
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

// MCP-style CallToolResult. "data" entries in `images` (e.g. plots) become
// image content, in order, so multimodal agents can see them; their base64
// is dropped from the JSON.
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

  const imageContent: ToolContent[] = [];
  const { images } = value as { images?: OutputImage[] };
  const structured = !Array.isArray(images)
    ? value
    : {
        ...value,
        images: images.map((img) => {
          const match =
            img.type === "data" &&
            img.value.match(/^data:(image\/[^;]+);base64,(.*)$/);

          if (!match) return img;
          imageContent.push({
            type: "image",
            mimeType: match[1],
            data: match[2],
          });
          return { index: img.index, type: img.type };
        }),
      };

  return {
    content: [
      { type: "text", text: JSON.stringify(structured) },
      ...imageContent,
    ],
    structuredContent: structured,
  };
}

export { ShinyWebMCP };
export type { AgentTool, ToolResult };
