import assert from "node:assert/strict";
import test from "node:test";

import { unloadOnPageHide } from "../resumeUnload";

void test("a page going away for good tells the server to drop its saved state", () => {
  assert.equal(
    unloadOnPageHide(false, "0123456789abcdef0123456789abcdef"),
    true,
  );
});

void test("a page entering the back/forward cache keeps its saved state", () => {
  assert.equal(
    unloadOnPageHide(true, "0123456789abcdef0123456789abcdef"),
    false,
  );
});

void test("a page with resume off has nothing to drop", () => {
  assert.equal(unloadOnPageHide(false, null), false);
});
