import assert from "node:assert/strict";
import test from "node:test";

import { handleVisualChange } from "../outputInfoObserver";

void test("visual change handling refreshes theme info for theme-reporting outputs", () => {
  const calls: string[] = [];
  const el = {} as HTMLElement;

  handleVisualChange(el, {
    doTriggerResize: () => calls.push("resize"),
    doSendHiddenState: () => calls.push("hidden"),
    doSendSize: () => calls.push("size"),
    doSendTheme: () => calls.push("theme"),
    reportsSize: () => true,
    reportsTheme: () => true,
  });

  assert.deepEqual(calls, ["resize", "hidden", "size", "theme"]);
});
