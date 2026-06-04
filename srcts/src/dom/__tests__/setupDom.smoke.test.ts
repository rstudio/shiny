import assert from "node:assert/strict";
import test from "node:test";
import { setupDom } from "../testing/setupDom";

void test("setupDom installs window.document with a body", () => {
  const teardown = setupDom("<!doctype html><html><body><div id=x></div></body></html>");
  try {
    assert.equal((globalThis as any).document.getElementById("x")?.id, "x");
  } finally {
    teardown();
  }
});

void test("setupDom teardown restores prior globals", () => {
  const before = (globalThis as any).document;
  const teardown = setupDom();
  teardown();
  assert.equal((globalThis as any).document, before);
});
