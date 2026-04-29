import assert from "node:assert/strict";
import { describe, test } from "node:test";

import { OutputProgressReporter } from "../outputProgress";

// Helper to build the messages that the state machine consumes.
function recalculating(name: string) {
  return { recalculating: { name, status: "recalculating" as const } };
}
function recalculated(name: string) {
  return { recalculating: { name, status: "recalculated" as const } };
}
function flush(
  values: { [key: string]: unknown } = {},
  errors: { [key: string]: unknown } = {},
) {
  return { values, errors, inputMessages: [] as never[] };
}
function progressBinding(id: string, persistent = false) {
  return {
    progress: { type: "binding" as const, message: { id, persistent } },
  };
}

// Drive an output through a full recalculate→flush→value cycle.
function runFullCycle(
  r: OutputProgressReporter,
  name: string,
  outcome: "value" | "error" = "value",
) {
  r.updateStateFromMessage(recalculating(name));
  r.updateStateFromMessage(recalculated(name));
  if (outcome === "value") {
    r.updateStateFromMessage(flush({ [name]: 1 }));
  } else {
    r.updateStateFromMessage(flush({}, { [name]: "err" }));
  }
}

// ---------------------------------------------------------------------------
// isRecalculating
// ---------------------------------------------------------------------------
void describe("isRecalculating", () => {
  void test("unknown output starts in Initial (recalculating)", () => {
    const r = new OutputProgressReporter();
    assert.equal(r.isRecalculating("x"), true);
  });

  void test("Running state is recalculating", () => {
    const r = new OutputProgressReporter();
    r.updateStateFromMessage(recalculating("x"));
    assert.equal(r.isRecalculating("x"), true);
  });

  void test("Idle state is recalculating", () => {
    const r = new OutputProgressReporter();
    r.updateStateFromMessage(recalculating("x"));
    r.updateStateFromMessage(recalculated("x"));
    assert.equal(r.isRecalculating("x"), true);
  });

  void test("Value state is not recalculating", () => {
    const r = new OutputProgressReporter();
    runFullCycle(r, "x", "value");
    assert.equal(r.isRecalculating("x"), false);
  });

  void test("Error state is not recalculating", () => {
    const r = new OutputProgressReporter();
    runFullCycle(r, "x", "error");
    assert.equal(r.isRecalculating("x"), false);
  });

  void test("Cancel state is not recalculating", () => {
    const r = new OutputProgressReporter();
    r.updateStateFromMessage(recalculating("x"));
    r.updateStateFromMessage(recalculated("x"));
    // Flush with no value for "x" → cancel
    r.updateStateFromMessage(flush());
    assert.equal(r.isRecalculating("x"), false);
  });

  void test("Persistent state is recalculating", () => {
    const r = new OutputProgressReporter();
    r.updateStateFromMessage(recalculating("x"));
    r.updateStateFromMessage(progressBinding("x", true));
    assert.equal(r.isRecalculating("x"), true);
  });

  void test("Invalidated state is recalculating", () => {
    const r = new OutputProgressReporter();
    runFullCycle(r, "x", "value");
    r.updateStateFromMessage(progressBinding("x"));
    assert.equal(r.isRecalculating("x"), true);
  });
});

// ---------------------------------------------------------------------------
// isInvalidated
// ---------------------------------------------------------------------------
void describe("isInvalidated", () => {
  void test("unknown output is not invalidated", () => {
    const r = new OutputProgressReporter();
    assert.equal(r.isInvalidated("x"), false);
  });

  void test("Initial state is not invalidated", () => {
    const r = new OutputProgressReporter();
    assert.equal(r.isInvalidated("x"), false);
    assert.equal(r.isRecalculating("x"), true);
  });

  void test("output that received a value then was invalidated", () => {
    const r = new OutputProgressReporter();
    runFullCycle(r, "x", "value");
    assert.equal(r.isInvalidated("x"), false);
    r.updateStateFromMessage(progressBinding("x"));
    assert.equal(r.isInvalidated("x"), true);
  });

  void test("output that received an error then was invalidated", () => {
    const r = new OutputProgressReporter();
    runFullCycle(r, "x", "error");
    r.updateStateFromMessage(progressBinding("x"));
    assert.equal(r.isInvalidated("x"), true);
  });

  void test("cancelled output then invalidated", () => {
    const r = new OutputProgressReporter();
    r.updateStateFromMessage(recalculating("x"));
    r.updateStateFromMessage(recalculated("x"));
    r.updateStateFromMessage(flush()); // cancel
    r.updateStateFromMessage(progressBinding("x"));
    assert.equal(r.isInvalidated("x"), true);
  });

  void test("persistent output then invalidated", () => {
    const r = new OutputProgressReporter();
    r.updateStateFromMessage(recalculating("x"));
    r.updateStateFromMessage(progressBinding("x", true));
    r.updateStateFromMessage(progressBinding("x"));
    assert.equal(r.isInvalidated("x"), true);
  });

  void test("invalidated output moves out of invalidated on recalculating", () => {
    const r = new OutputProgressReporter();
    runFullCycle(r, "x", "value");
    r.updateStateFromMessage(progressBinding("x"));
    assert.equal(r.isInvalidated("x"), true);
    r.updateStateFromMessage(recalculating("x"));
    assert.equal(r.isInvalidated("x"), false);
  });
});

// ---------------------------------------------------------------------------
// State machine transitions (full lifecycle)
// ---------------------------------------------------------------------------
void describe("state transitions", () => {
  void test("full happy path: Initial → Running → Idle → Value → Invalidated → Running", () => {
    const r = new OutputProgressReporter();

    // Initial
    assert.equal(r.isRecalculating("x"), true);
    assert.equal(r.isInvalidated("x"), false);

    // → Running
    r.updateStateFromMessage(recalculating("x"));
    assert.equal(r.isRecalculating("x"), true);
    assert.equal(r.isInvalidated("x"), false);

    // → Idle
    r.updateStateFromMessage(recalculated("x"));
    assert.equal(r.isRecalculating("x"), true);
    assert.equal(r.isInvalidated("x"), false);

    // → Value
    r.updateStateFromMessage(flush({ x: 1 }));
    assert.equal(r.isRecalculating("x"), false);
    assert.equal(r.isInvalidated("x"), false);

    // → Invalidated
    r.updateStateFromMessage(progressBinding("x"));
    assert.equal(r.isRecalculating("x"), true);
    assert.equal(r.isInvalidated("x"), true);

    // → Running again
    r.updateStateFromMessage(recalculating("x"));
    assert.equal(r.isRecalculating("x"), true);
    assert.equal(r.isInvalidated("x"), false);
  });

  void test("error path: Initial → Running → Idle → Error → Invalidated", () => {
    const r = new OutputProgressReporter();

    r.updateStateFromMessage(recalculating("x"));
    r.updateStateFromMessage(recalculated("x"));
    r.updateStateFromMessage(flush({}, { x: "err" }));
    assert.equal(r.isRecalculating("x"), false);

    r.updateStateFromMessage(progressBinding("x"));
    assert.equal(r.isInvalidated("x"), true);
  });

  void test("cancel path: Idle with no value in flush → Cancel → Invalidated", () => {
    const r = new OutputProgressReporter();

    r.updateStateFromMessage(recalculating("x"));
    r.updateStateFromMessage(recalculated("x"));
    // Flush with no value/error for x → cancel
    r.updateStateFromMessage(flush());
    assert.equal(r.isRecalculating("x"), false);

    r.updateStateFromMessage(progressBinding("x"));
    assert.equal(r.isInvalidated("x"), true);
  });

  void test("persistent progress path", () => {
    const r = new OutputProgressReporter();

    r.updateStateFromMessage(recalculating("x"));
    // Running → Persistent
    r.updateStateFromMessage(progressBinding("x", true));
    assert.equal(r.isRecalculating("x"), true);
    assert.equal(r.isInvalidated("x"), false);

    // Persistent → Invalidated
    r.updateStateFromMessage(progressBinding("x"));
    assert.equal(r.isInvalidated("x"), true);
  });

  void test("idle output can be directly invalidated", () => {
    const r = new OutputProgressReporter();

    r.updateStateFromMessage(recalculating("x"));
    r.updateStateFromMessage(recalculated("x"));
    // Idle → Invalidated (edge 5 from Idle)
    r.updateStateFromMessage(progressBinding("x"));
    assert.equal(r.isInvalidated("x"), true);
    assert.equal(r.isRecalculating("x"), true);
  });

  void test("suspended output stays Invalidated through flush", () => {
    const r = new OutputProgressReporter();
    runFullCycle(r, "x", "value");
    r.updateStateFromMessage(progressBinding("x"));
    assert.equal(r.isInvalidated("x"), true);

    // Run another output through a cycle; x should stay Invalidated
    runFullCycle(r, "y", "value");
    assert.equal(r.isInvalidated("x"), true);
  });
});

// ---------------------------------------------------------------------------
// takeChanges
// ---------------------------------------------------------------------------
void describe("takeChanges", () => {
  void test("empty reporter has no changes", () => {
    const r = new OutputProgressReporter();
    const changes = r.takeChanges();
    assert.equal(changes.size, 0);
  });

  void test("tracks transition from recalculating to not-recalculating", () => {
    const r = new OutputProgressReporter();
    runFullCycle(r, "x", "value");

    const changes = r.takeChanges();
    assert.equal(changes.get("x"), false);
  });

  void test("tracks transition from not-recalculating to recalculating", () => {
    const r = new OutputProgressReporter();
    runFullCycle(r, "x", "value");
    r.takeChanges(); // drain

    r.updateStateFromMessage(progressBinding("x"));
    const changes = r.takeChanges();
    assert.equal(changes.get("x"), true);
  });

  void test("transitions within recalculating states do not produce changes", () => {
    const r = new OutputProgressReporter();
    // Initial → Running (both recalculating)
    r.updateStateFromMessage(recalculating("x"));
    const changes = r.takeChanges();
    assert.equal(changes.has("x"), false);
  });

  void test("takeChanges clears the change set", () => {
    const r = new OutputProgressReporter();
    runFullCycle(r, "x", "value");
    r.takeChanges();
    const changes = r.takeChanges();
    assert.equal(changes.size, 0);
  });

  void test("multiple outputs tracked independently", () => {
    const r = new OutputProgressReporter();
    runFullCycle(r, "a", "value");
    runFullCycle(r, "b", "error");

    const changes = r.takeChanges();
    assert.equal(changes.get("a"), false);
    assert.equal(changes.get("b"), false);
  });
});

// ---------------------------------------------------------------------------
// Invalid transitions throw
// ---------------------------------------------------------------------------
void describe("invalid transitions throw", () => {
  void test("recalculating from Value throws", () => {
    const r = new OutputProgressReporter();
    runFullCycle(r, "x", "value");
    assert.throws(() => r.updateStateFromMessage(recalculated("x")));
  });

  void test("recalculated from Initial throws", () => {
    const r = new OutputProgressReporter();
    assert.throws(() => r.updateStateFromMessage(recalculated("x")));
  });

  void test("value from Running throws", () => {
    const r = new OutputProgressReporter();
    r.updateStateFromMessage(recalculating("x"));
    assert.throws(() => r.updateStateFromMessage(flush({ x: 1 })));
  });

  void test("persistent progress from Idle throws", () => {
    const r = new OutputProgressReporter();
    r.updateStateFromMessage(recalculating("x"));
    r.updateStateFromMessage(recalculated("x"));
    assert.throws(() => r.updateStateFromMessage(progressBinding("x", true)));
  });

  void test("progress binding from Initial throws", () => {
    const r = new OutputProgressReporter();
    assert.throws(() => r.updateStateFromMessage(progressBinding("x")));
  });

  void test("progress binding from Running throws", () => {
    const r = new OutputProgressReporter();
    r.updateStateFromMessage(recalculating("x"));
    assert.throws(() => r.updateStateFromMessage(progressBinding("x")));
  });
});
