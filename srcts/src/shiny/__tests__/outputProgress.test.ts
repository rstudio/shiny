import assert from "node:assert/strict";
import test from "node:test";

import { OutputProgressReporter } from "../outputProgress";

// Drive an output through one full render cycle: INITIAL -> RUNNING -> IDLE ->
// VALUE.
function renderOnce(reporter: OutputProgressReporter, name: string): void {
  reporter.updateStateFromMessage({
    recalculating: { name, status: "recalculating" },
  });
  reporter.updateStateFromMessage({
    recalculating: { name, status: "recalculated" },
  });
  reporter.updateStateFromMessage({
    values: { [name]: "a value" },
    errors: {},
    inputMessages: [],
  });
}

void test("a second render cycle without an invalidation is an illegal transition", () => {
  const reporter = new OutputProgressReporter();

  renderOnce(reporter, "out");

  // The output is now in the VALUE state, so `recalculating` has no legal edge.
  assert.throws(
    () => renderOnce(reporter, "out"),
    /output 'out' is recalculating[\s\S]*unexpected state of: 'value'/
  );
});

void test("reset() lets an output start a fresh render cycle", () => {
  const reporter = new OutputProgressReporter();

  renderOnce(reporter, "out");
  reporter.reset();

  // Back at INITIAL, so the cycle a reconnected session sends is legal again.
  assert.doesNotThrow(() => renderOnce(reporter, "out"));
});

void test("reset() clears pending progress changes", () => {
  const reporter = new OutputProgressReporter();

  reporter.updateStateFromMessage({
    recalculating: { name: "out", status: "recalculating" },
  });
  reporter.reset();

  assert.equal(reporter.takeChanges().size, 0);
  assert.equal(reporter.isRecalculating("out"), true); // INITIAL
});
