import assert from "node:assert/strict";
import test from "node:test";
import { setupDom } from "../../testing/setupDom";
import { withJQuery } from "../../testing/withJQuery";
import { closest, matches, select } from "../index";

const FIXTURE = `<!doctype html><html><body>
  <div id="root">
    <div class="a outer">
      <span class="b inner" data-x="1">one</span>
      <span class="b inner" data-x="2">two</span>
    </div>
    <div class="a outer">
      <span class="b inner" data-x="3">three</span>
    </div>
  </div>
</body></html>`;

function runDualPath(name: string, fn: () => void): void {
  void test(`${name} (jquery adapter)`, () => {
    const teardown = setupDom(FIXTURE);
    try {
      withJQuery(true, fn);
    } finally {
      teardown();
    }
  });
  void test(`${name} (native adapter)`, () => {
    const teardown = setupDom(FIXTURE);
    try {
      withJQuery(false, fn);
    } finally {
      teardown();
    }
  });
}

runDualPath("select returns all matching descendants of scope", () => {
  const scope = (globalThis as any).document.getElementById("root") as Element;
  const out = select(scope, ".inner");
  assert.equal(out.length, 3);
  assert.deepEqual(
    out.map((el) => el.getAttribute("data-x")),
    ["1", "2", "3"],
  );
});

runDualPath("select returns [] when nothing matches", () => {
  const scope = (globalThis as any).document.getElementById("root") as Element;
  const out = select(scope, ".does-not-exist");
  assert.deepEqual(out, []);
});

runDualPath("closest returns the nearest ancestor (or self) matching selector", () => {
  const span = (globalThis as any).document.querySelector("span[data-x='2']") as Element;
  const outer = closest(span, ".outer");
  assert.ok(outer);
  assert.equal(outer!.tagName, "DIV");
  assert.ok(outer!.classList.contains("a"));
});

runDualPath("closest returns null when no ancestor matches", () => {
  const span = (globalThis as any).document.querySelector("span[data-x='2']") as Element;
  const out = closest(span, ".no-such-class");
  assert.equal(out, null);
});

runDualPath("matches returns true when element matches selector", () => {
  const span = (globalThis as any).document.querySelector("span[data-x='1']") as Element;
  assert.equal(matches(span, ".inner"), true);
  assert.equal(matches(span, ".outer"), false);
});
