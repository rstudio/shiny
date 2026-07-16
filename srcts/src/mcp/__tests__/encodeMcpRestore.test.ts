import assert from "node:assert";
import test from "node:test";
import { encodeMcpRestore } from "../restore";

test("encodeMcpRestore encodes args to bookmark restore format", () => {
  const result = encodeMcpRestore({ n: 200, note: "hi" });
  assert.strictEqual(result, "_inputs_&n=200&note=%22hi%22");
});

test("encodeMcpRestore handles empty args", () => {
  const result = encodeMcpRestore({});
  assert.strictEqual(result, "_inputs_&");
});

test("encodeMcpRestore handles special characters in keys and values", () => {
  const result = encodeMcpRestore({ "a&b": "x=y" });
  // key "a&b" -> "a%26b", value "x=y" JSON-stringified -> '"x=y"' -> '%22x%3Dy%22'
  assert.strictEqual(result, "_inputs_&a%26b=%22x%3Dy%22");
});

test("encodeMcpRestore handles arrays and objects", () => {
  const result = encodeMcpRestore({ items: [1, 2, 3] });
  assert.strictEqual(result, "_inputs_&items=%5B1%2C2%2C3%5D");
});

test("encodeMcpRestore handles boolean and null values", () => {
  const result = encodeMcpRestore({ flag: true, empty: null });
  assert.strictEqual(result, "_inputs_&flag=true&empty=null");
});
