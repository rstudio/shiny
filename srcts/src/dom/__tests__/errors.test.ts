import assert from "node:assert/strict";
import test from "node:test";
import { JQueryRequiredError } from "../errors";

void test("JQueryRequiredError carries feature and binding fields", () => {
  const err = new JQueryRequiredError("dom/widgets:datepicker", "MyDateInput");
  assert.equal(err.name, "JQueryRequiredError");
  assert.equal(err.feature, "dom/widgets:datepicker");
  assert.equal(err.binding, "MyDateInput");
  assert.ok(err.message.includes("dom/widgets:datepicker"));
  assert.ok(err.message.includes("MyDateInput"));
  assert.ok(err.message.includes("jQuery"));
});

void test("JQueryRequiredError works without a binding name", () => {
  const err = new JQueryRequiredError("dom/widgets:slider");
  assert.equal(err.binding, undefined);
  assert.ok(err.message.includes("dom/widgets:slider"));
  assert.ok(!err.message.includes("used by binding"));
});

void test("JQueryRequiredError is an Error", () => {
  const err = new JQueryRequiredError("x");
  assert.ok(err instanceof Error);
  assert.ok(err instanceof JQueryRequiredError);
});
