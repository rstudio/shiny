import assert from "node:assert/strict";
import test from "node:test";

import { retryDecision } from "../reconnectGate";

void test("allowReconnect(TRUE) retries on any socket until the attempts run out", () => {
  assert.equal(retryDecision({ allow: true, exhausted: false }), "retry");
  assert.equal(retryDecision({ allow: true, exhausted: true }), "stop");
});

void test("force is bounded too", () => {
  assert.equal(retryDecision({ allow: "force", exhausted: false }), "retry");
  assert.equal(retryDecision({ allow: "force", exhausted: true }), "stop");
});

void test("allowReconnect(FALSE), the default, never retries", () => {
  assert.equal(retryDecision({ allow: false, exhausted: false }), "never");
  assert.equal(retryDecision({ allow: false, exhausted: true }), "never");
});
