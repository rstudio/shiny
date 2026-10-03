import assert from "node:assert/strict";
import test from "node:test";

import { InputNoResendDecorator } from "../inputNoResendDecorator";
import type { InputPolicy } from "../inputPolicy";

void test("remember() makes the next identical value a no-op without forgetting other inputs", () => {
  const sent: string[] = [];
  const target = {
    setInput: (nameType: string) => {
      sent.push(nameType);
    },
  } as unknown as InputPolicy;
  const filter = new InputNoResendDecorator(target, {
    // eslint-disable-next-line @typescript-eslint/naming-convention
    "a:shiny.action": 0,
    b: "x",
  });
  filter.remember("a:shiny.action", 3);
  filter.setInput("a:shiny.action", 3, { priority: "immediate" });
  filter.setInput("b", "x", { priority: "immediate" });
  assert.deepEqual(sent, []);
  filter.setInput("a:shiny.action", 0, { priority: "immediate" });
  assert.deepEqual(sent, ["a:shiny.action"]);
});
