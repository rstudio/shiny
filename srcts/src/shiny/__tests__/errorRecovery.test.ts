import assert from "node:assert/strict";
import test from "node:test";

import {
  ErrorRecovery,
  recoveryBody,
  recoveryTitle,
  recoveryView,
  type RecoveryView,
} from "../errorRecovery";

function harness(canResume = true) {
  const log: string[] = [];
  let shown: RecoveryView | null = null;
  let choose: ((c: "resume" | "fresh") => void) | null = null;
  const r = new ErrorRecovery({
    show: (view, c) => {
      shown = view;
      choose = c;
      log.push("show");
    },
    greyOut: () => log.push("grey"),
    resume: () => log.push("resume"),
    startOver: () => log.push("start-over"),
    canResume: () => canResume,
  });
  return {
    r,
    log,
    shown: () => shown,
    choose: (c: "resume" | "fresh") => choose!(c),
  };
}

void test("the view carries the message and offers Resume only when something was saved", () => {
  assert.deepEqual(recoveryView({ message: "boom", saved: true }), {
    message: "boom",
    saved: true,
  });
  assert.deepEqual(recoveryView({ saved: false }), {
    message: null,
    saved: false,
  });
  assert.deepEqual(recoveryView({ message: 42, saved: "yes" }), {
    message: null,
    saved: false,
  });
  assert.equal(recoveryTitle, "Something went wrong");
  assert.equal(
    recoveryBody,
    "Resume returns to the state saved just before the error. Your last change may already have taken effect.",
  );
});

void test("a fatal error greys the page and shows the dialog; Resume and Start over run their actions", () => {
  const h = harness();
  h.r.fatalError({ message: "boom", saved: true });
  assert.deepEqual(h.log, ["grey", "show"]);
  assert.deepEqual(h.shown(), { message: "boom", saved: true });
  h.choose("resume");
  assert.deepEqual(h.log, ["grey", "show", "resume"]);
  const h2 = harness();
  h2.r.fatalError({ saved: false });
  h2.choose("fresh");
  assert.deepEqual(h2.log, ["grey", "show", "start-over"]);
});

void test("Resume is offered only when the tab can resume", () => {
  const h = harness(false);
  h.r.fatalError({ message: "boom", saved: true });
  assert.deepEqual(h.shown(), { message: "boom", saved: false });
});

void test("the error flow owns the close: no retry, and a second fatalError changes nothing", () => {
  const h = harness();
  assert.equal(h.r.ownsClose(), false);
  h.r.fatalError({ saved: true });
  assert.equal(h.r.ownsClose(), true);
  h.r.fatalError({ saved: true });
  assert.deepEqual(h.log, ["grey", "show"]);
});
