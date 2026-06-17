import assert from "node:assert/strict";
import test from "node:test";
import { setupDom } from "../testing/setupDom";
import { withJQuery } from "../testing/withJQuery";

void test("withJQuery(true, fn) makes window.jQuery a function during fn", () => {
  const teardown = setupDom();
  try {
    let observed: unknown;
    withJQuery(true, () => {
      observed = typeof (globalThis as any).window.jQuery;
    });
    assert.equal(observed, "function");
  } finally {
    teardown();
  }
});

void test("withJQuery(false, fn) sets window.jQuery to undefined during fn", () => {
  const teardown = setupDom();
  try {
    (globalThis as any).window.jQuery = () => undefined; // pretend it was loaded
    let observed: unknown = "not-set";
    withJQuery(false, () => {
      observed = (globalThis as any).window.jQuery;
    });
    assert.equal(observed, undefined);
  } finally {
    teardown();
  }
});

void test("withJQuery restores prior value after fn", () => {
  const teardown = setupDom();
  try {
    const sentinel = () => "sentinel";
    (globalThis as any).window.jQuery = sentinel;
    withJQuery(false, () => undefined);
    assert.equal((globalThis as any).window.jQuery, sentinel);
  } finally {
    teardown();
  }
});

void test("withJQuery restores prior value even if fn throws", () => {
  const teardown = setupDom();
  try {
    const sentinel = () => "sentinel";
    (globalThis as any).window.jQuery = sentinel;
    assert.throws(() => {
      withJQuery(false, () => {
        throw new Error("boom");
      });
    });
    assert.equal((globalThis as any).window.jQuery, sentinel);
  } finally {
    teardown();
  }
});
