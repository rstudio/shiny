import assert from "node:assert/strict";
import test from "node:test";

import type { InputBinding } from "../../bindings";
import {
  adaptPushedValue,
  applyPushedInputs,
  type PushDeps,
} from "../resumePush";

type Call = { id: string; via: string; value: unknown };

function binding(
  name: string,
  calls: Call[],
  opts: { setValue?: boolean; throws?: boolean; type?: string } = {},
) {
  const b: { [key: string]: unknown } = {
    name,
    getType: () => opts.type ?? null,
    receiveMessage: (el: { id: string }, data: unknown) => {
      calls.push({ id: el.id, via: "receiveMessage", value: data });
    },
  };
  if (opts.setValue !== false) {
    b.setValue = (el: { id: string }, value: unknown) => {
      if (opts.throws) throw new Error("boom");
      calls.push({ id: el.id, via: "setValue", value });
    };
  }
  return b as unknown as InputBinding;
}

function deps(
  bindings: { [id: string]: InputBinding },
  dataTypes: { [id: string]: string } = {},
) {
  const log: string[] = [];
  const order: string[] = [];
  let resent = 0;
  const d: PushDeps = {
    lookup: (id) =>
      bindings[id]
        ? {
            binding: bindings[id],
            el: { id } as unknown as HTMLElement,
            dataType: dataTypes[id],
          }
        : null,
    remember: (nameType) => {
      order.push("remember " + nameType);
    },
    resendAll: () => {
      resent += 1;
    },
    log: (message) => {
      log.push(message);
    },
  };
  return { d, log, order, resent: () => resent };
}

void test("a pushed value goes through setValue, or receiveMessage({value}) when there is none", async () => {
  const calls: Call[] = [];
  const { d, resent } = deps({
    a: binding("shiny.textInput", calls),
    b: binding("custom.widget", calls, { setValue: false }),
  });
  await applyPushedInputs({ a: "typed", b: 7, gone: 1 }, d);
  assert.deepEqual(calls, [
    { id: "a", via: "setValue", value: "typed" },
    { id: "b", via: "receiveMessage", value: { value: 7 } },
  ]);
  assert.equal(resent(), 1);
});

void test("a binding that throws is logged and the rest are still applied", async () => {
  const calls: Call[] = [];
  const { d, log } = deps({
    a: binding("x", calls, { throws: true }),
    b: binding("y", calls),
  });
  await applyPushedInputs({ a: 1, b: 2 }, d);
  assert.equal(log.length, 1);
  assert.match(log[0], /'a'/);
  assert.deepEqual(calls, [{ id: "b", via: "setValue", value: 2 }]);
});

void test("the filter learns the pushed value, with the binding's type, before the binding is touched", async () => {
  const calls: Call[] = [];
  const { d, order } = deps({
    plus: binding("shiny.actionButtonInput", calls, { type: "shiny.action" }),
  });
  d.remember = (nameType) => {
    order.push("remember " + nameType + " after " + calls.length + " calls");
  };
  await applyPushedInputs({ plus: 3 }, d);
  assert.deepEqual(order, ["remember plus:shiny.action after 0 calls"]);
});

void test("an input whose lookup or getType throws is logged and skipped; the rest are applied and re-sent", async () => {
  const calls: Call[] = [];
  const badType = binding("x", calls);
  badType.getType = () => {
    throw new Error("boom");
  };
  const { d, log, resent } = deps({ a: badType, c: binding("y", calls) });
  const lookup = d.lookup;
  d.lookup = (id) => {
    if (id === "b") throw new Error("boom");
    return lookup(id);
  };
  await applyPushedInputs({ a: 1, b: 2, c: 3 }, d);
  assert.equal(log.length, 2);
  assert.match(log[0], /'a'/);
  assert.match(log[1], /'b'/);
  assert.deepEqual(calls, [{ id: "c", via: "setValue", value: 3 }]);
  assert.equal(resent(), 1);
});

void test("a re-send that throws is logged, not raised", async () => {
  const calls: Call[] = [];
  const { d, log } = deps({ a: binding("x", calls) });
  d.resendAll = () => {
    throw new Error("boom");
  };
  await applyPushedInputs({ a: 1 }, d);
  assert.deepEqual(calls, [{ id: "a", via: "setValue", value: 1 }]);
  assert.equal(log.length, 1);
});

void test("date ranges and date sliders get the shape their setValue takes", () => {
  assert.deepEqual(
    adaptPushedValue("shiny.dateRangeInput", undefined, [
      "2024-01-02",
      "2024-01-05",
    ]),
    {
      start: "2024-01-02",
      end: "2024-01-05",
    },
  );
  assert.equal(
    adaptPushedValue("shiny.sliderInput", "date", "2024-01-02"),
    Date.UTC(2024, 0, 2),
  );
  assert.deepEqual(
    adaptPushedValue("shiny.sliderInput", "date", ["2024-01-02", "2024-01-03"]),
    [Date.UTC(2024, 0, 2), Date.UTC(2024, 0, 3)],
  );
  assert.equal(
    adaptPushedValue("shiny.sliderInput", "datetime", "2024-01-01T12:00:00Z"),
    Date.UTC(2024, 0, 1, 12),
  );
  assert.equal(adaptPushedValue("shiny.sliderInput", undefined, 5), 5);
  assert.deepEqual(adaptPushedValue("shiny.selectInput", undefined, ["a"]), [
    "a",
  ]);
});
