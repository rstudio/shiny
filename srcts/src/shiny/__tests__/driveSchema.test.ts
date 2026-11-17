import assert from "node:assert/strict";
import test from "node:test";

import { checkValue, valueSchema } from "../driveSchema";

// Fake binding: valueSchema() only reads `name`, `getState()`, `getType()`.
function binding(name: string, state?: unknown): never {
  return {
    name,
    getState: () => {
      if (state === undefined) throw new Error("not implemented");
      return state;
    },
    getType: () => null,
  } as never;
}
const el = (props: object = {}) => props as never;

void test("valueSchema() describes built-in inputs", () => {
  assert.equal(valueSchema(el(), binding("shiny.actionButtonInput")), null);
  assert.deepEqual(valueSchema(el(), binding("shiny.checkboxInput")), {
    type: "boolean",
  });
  assert.deepEqual(
    valueSchema(el({ min: "1", max: "" }), binding("shiny.numberInput")),
    { type: "number", minimum: 1, maximum: undefined },
  );
  assert.deepEqual(valueSchema(el(), binding("shiny.dateRangeInput")), {
    type: "array",
    items: { type: "string", format: "date" },
    minItems: 2,
    maxItems: 2,
  });
  assert.deepEqual(valueSchema(el(), binding("my.customInput")), {});
});

void test("valueSchema() reads choices from getState()", () => {
  const state = {
    options: [{ value: "a" }, { value: "b" }],
    label: { jquery: "not JSON-safe" },
  };

  assert.deepEqual(valueSchema(el(), binding("shiny.radioInput", state)), {
    enum: ["a", "b"],
  });
  assert.deepEqual(
    valueSchema(el({ multiple: true }), binding("shiny.selectInput", state)),
    { type: "array", items: { enum: ["a", "b"] } },
  );
  // Selectize keeps only the selected <option>s; its own option list wins
  assert.deepEqual(
    valueSchema(
      el({ selectize: { options: { x: {}, y: {} } } }),
      binding("shiny.selectInput", state),
    ),
    { enum: ["x", "y"] },
  );
});

void test("checkValue() accepts valid values", () => {
  assert.doesNotThrow(() => checkValue({ type: "boolean" }, true));
  assert.doesNotThrow(() => checkValue({ enum: ["a", "b"] }, "b"));
  assert.doesNotThrow(() =>
    checkValue({ type: "number", minimum: 0, maximum: 10 }, 10),
  );
  assert.doesNotThrow(() =>
    checkValue({ type: "array", items: { enum: ["a", "b"] }, minItems: 0 }, [
      "a",
      "b",
    ]),
  );
  assert.doesNotThrow(() => checkValue({}, { anything: 1 }));
});

void test("checkValue() rejects invalid values with a useful message", () => {
  assert.throws(
    () => checkValue({ type: "boolean" }, "true"),
    /value must be a boolean; got "true"/,
  );
  assert.throws(
    () => checkValue({ enum: ["a", "b"] }, "c"),
    /value must be one of "a", "b"; got "c"/,
  );
  assert.throws(
    () => checkValue({ type: "number", minimum: 0, maximum: 10 }, 11),
    /between 0 and 10/,
  );
  assert.throws(
    () => checkValue({ type: "array", minItems: 2, maxItems: 2 }, ["x"]),
    /must have 2-2 items/,
  );
  assert.throws(
    () => checkValue({ type: "array", items: { enum: ["a"] } }, ["a", "z"]),
    /value\[1\] must be one of "a"/,
  );
});

void test("checkValue() caps the choices listed in errors", () => {
  const choices = [...Array(60).keys()].map(String);

  assert.throws(
    () => checkValue({ enum: choices }, "nope"),
    (e: Error) =>
      e.message.includes('"49", ...') && !e.message.includes('"50"'),
  );
});
