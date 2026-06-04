import assert from "node:assert/strict";
import test from "node:test";
import { setupDom } from "../../testing/setupDom";
import { closest, matches, select } from "../index";

void test("dom/query works without window.jQuery", () => {
  const teardown = setupDom(
    `<!doctype html><html><body>
       <div id="root">
         <span class="x" data-v="1"></span>
         <span class="x" data-v="2"></span>
       </div>
     </body></html>`,
  );
  try {
    // Explicitly assert no jQuery present.
    assert.equal(typeof (globalThis as any).window.jQuery, "undefined");

    const root = (globalThis as any).document.getElementById("root") as Element;

    const all = select(root, ".x");
    assert.equal(all.length, 2);
    assert.deepEqual(
      all.map((el) => el.getAttribute("data-v")),
      ["1", "2"],
    );

    assert.equal(closest(all[0], "#root")?.id, "root");
    assert.equal(matches(all[0], ".x"), true);
  } finally {
    teardown();
  }
});
