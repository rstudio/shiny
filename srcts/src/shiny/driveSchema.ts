import $ from "jquery";
import type { InputBinding } from "../bindings";

// JSON Schemas for input values, and a check against them. Kept apart from
// drive.ts (which listens on `document` at load) so it can be unit tested.

type JSONSchema = { [key: string]: unknown };
type Selectize = { options: { [value: string]: unknown } };

const maxChoicesInError = 50;

// Strict-enough check of the JSON Schemas produced by valueSchema(), so the
// agent gets an actionable error instead of a widget silently ignoring it.
function checkValue(schema: JSONSchema, value: unknown, path = "value"): void {
  const show = (x: unknown) => JSON.stringify(x);

  if (Array.isArray(schema.enum) && !schema.enum.includes(value)) {
    const choices = schema.enum.slice(0, maxChoicesInError).map(show);
    const more = schema.enum.length > maxChoicesInError ? ", ..." : "";

    throw new Error(
      `${path} must be one of ${choices.join(", ")}${more}; got ${show(value)}`,
    );
  }
  if (schema.type === "array") {
    if (!Array.isArray(value)) {
      throw new Error(`${path} must be an array; got ${show(value)}`);
    }
    const { minItems, maxItems } = schema as {
      minItems?: number;
      maxItems?: number;
    };

    if (
      (minItems !== undefined && value.length < minItems) ||
      (maxItems !== undefined && value.length > maxItems)
    ) {
      throw new Error(`${path} must have ${minItems ?? 0}-${maxItems} items`);
    }
    value.forEach((v, i) =>
      checkValue((schema.items ?? {}) as JSONSchema, v, `${path}[${i}]`),
    );
    return;
  }
  if (typeof schema.type === "string" && typeof value !== schema.type) {
    throw new Error(`${path} must be a ${schema.type}; got ${show(value)}`);
  }
  const { minimum, maximum } = schema as { minimum?: number; maximum?: number };

  if (
    typeof value === "number" &&
    ((minimum !== undefined && value < minimum) ||
      (maximum !== undefined && value > maximum))
  ) {
    throw new Error(`${path} must be between ${minimum} and ${maximum}`);
  }
}

function stateOf(el: HTMLElement, binding: InputBinding): JSONSchema {
  try {
    const state = binding.getState(el);

    if (state && typeof state === "object") {
      // Some getState()s return jQuery objects (e.g. select label)
      return JSON.parse(
        JSON.stringify({ ...(state as JSONSchema), label: undefined }),
      );
    }
  } catch {
    // Base/unimplemented getState() throws
  }
  return {};
}

// JSON Schema for the `value` argument of a set_input tool. `null` means the
// input is not settable by value (action buttons are clicked instead).
function valueSchema(
  el: HTMLElement,
  binding: InputBinding,
): JSONSchema | null {
  const state = stateOf(el, binding);
  const choices = Array.isArray(state.options)
    ? (state.options as Array<{ value: string }>).map((o) => o.value)
    : undefined;

  switch (binding.name) {
    case "shiny.actionButtonInput":
      return null;
    case "shiny.checkboxInput":
      return { type: "boolean" };
    case "shiny.textInput":
    case "shiny.textareaInput":
    case "shiny.passwordInput":
      return { type: "string" };
    case "shiny.numberInput": {
      const { min, max } = el as HTMLInputElement;

      return {
        type: "number",
        minimum: min === "" ? undefined : Number(min),
        maximum: max === "" ? undefined : Number(max),
      };
    }
    case "shiny.dateInput":
      return { type: "string", format: "date" };
    case "shiny.dateRangeInput":
      return {
        type: "array",
        items: { type: "string", format: "date" },
        minItems: 2,
        maxItems: 2,
      };
    case "shiny.radioInput":
      return { enum: choices };
    case "shiny.checkboxGroupInput":
      return { type: "array", items: { enum: choices } };
    case "shiny.selectInput": {
      // Selectize keeps only the selected <option>s in the <select>.
      // ponytail: only the currently loaded choices; server-side selectize
      // needs a custom tool (or a free-form string) to reach the rest.
      const selectize = (el as HTMLSelectElement & { selectize?: Selectize })
        .selectize;
      const opts = selectize ? Object.keys(selectize.options) : choices;

      return (el as HTMLSelectElement).multiple
        ? { type: "array", items: { enum: opts } }
        : { enum: opts };
    }
    case "shiny.bootstrapTabInput":
      return {
        enum: $(el)
          .find("a[data-value]")
          .toArray()
          .map((a) => a.getAttribute("data-value")),
      };
    case "shiny.sliderInput": {
      // Date/time sliders take epoch milliseconds; leave them unconstrained
      if (binding.getType(el)) return {};
      const opts = $(el).data("ionRangeSlider")?.options ?? {};
      const num = { type: "number", minimum: opts.min, maximum: opts.max };

      return opts.type === "double"
        ? { type: "array", items: num, minItems: 2, maxItems: 2 }
        : num;
    }
    default:
      // Custom bindings: passed through as `receiveMessage(el, {value})`
      return {};
  }
}

// Tool args come from the agent: accept an object or a JSON string of one
// (hosts differ), then check the keys against the tool's input schema. The
// error carries the schema and an example call so the agent can retry
// rather than give up on the tool.
function parseToolArgs(
  schema: JSONSchema | undefined,
  args: unknown,
): { [key: string]: unknown } {
  const problems: string[] = [];
  let value = args ?? {};

  if (typeof value === "string") {
    try {
      value = value.trim() === "" ? {} : JSON.parse(value);
    } catch {
      problems.push("input is a string but not valid JSON");
    }
  }
  const obj = value as { [key: string]: unknown };

  if (problems.length === 0) {
    if (typeof value !== "object" || value === null || Array.isArray(value)) {
      problems.push(`input must be an object; got ${JSON.stringify(value)}`);
    } else if (schema) {
      const props = (schema.properties ?? {}) as { [key: string]: JSONSchema };

      if (schema.additionalProperties === false) {
        Object.keys(obj)
          .filter((k) => !(k in props))
          .forEach((k) => problems.push(`unknown parameter "${k}"`));
      }
      ((schema.required ?? []) as string[])
        .filter((k) => obj[k] === undefined)
        .forEach((k) => problems.push(`missing required parameter "${k}"`));
    }
  }
  if (problems.length === 0) return obj;

  throw new Error(
    `Invalid input: ${problems.join("; ")}.` +
      (schema
        ? ` Expected input schema: ${JSON.stringify(schema)}. Example: ${JSON.stringify(exampleArgs(schema))}`
        : ""),
  );
}

function exampleArgs(schema: JSONSchema): { [key: string]: unknown } {
  const props = (schema.properties ?? {}) as { [key: string]: JSONSchema };

  return Object.fromEntries(
    Object.entries(props).map(([key, p]) => [
      key,
      Array.isArray(p.enum)
        ? p.enum[0]
        : p.type === "integer" || p.type === "number"
          ? 0
          : p.type === "boolean"
            ? false
            : p.type === "array"
              ? []
              : "...",
    ]),
  );
}

export { checkValue, parseToolArgs, valueSchema };
export type { JSONSchema };
