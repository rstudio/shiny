import assert from "node:assert/strict";
import test from "node:test";

import { retryDecision } from "../reconnectGate";

const base = {
  allow: true as boolean | "force",
  holdsToken: false,
  shimAllows: false,
  exhausted: false,
};

void test("with a resume token the client retries on any socket, up to the bound", () => {
  assert.equal(retryDecision({ ...base, holdsToken: true }), "retry");
  assert.equal(
    retryDecision({ ...base, holdsToken: true, exhausted: true }),
    "stop",
  );
});

void test("without a token, allowReconnect(TRUE) retries only behind the shim, unbounded (main)", () => {
  assert.equal(retryDecision(base), "never");
  assert.equal(retryDecision({ ...base, shimAllows: true }), "retry");
  assert.equal(
    retryDecision({ ...base, shimAllows: true, exhausted: true }),
    "retry",
  );
});

void test('"force" retries on any socket; it is bounded only when a token is held', () => {
  assert.equal(retryDecision({ ...base, allow: "force" }), "retry");
  assert.equal(
    retryDecision({ ...base, allow: "force", exhausted: true }),
    "retry",
  );
  assert.equal(
    retryDecision({
      ...base,
      allow: "force",
      holdsToken: true,
      exhausted: true,
    }),
    "stop",
  );
});

void test("allowReconnect: false (ended for good) never retries, token and shim notwithstanding", () => {
  assert.equal(
    retryDecision({
      ...base,
      allow: false,
      holdsToken: true,
      shimAllows: true,
    }),
    "never",
  );
});
