import assert from "node:assert/strict";
import test from "node:test";

import { initPanelScales } from "../initPanelScales";

void test("initPanelScales respects clip = false in scaleImgToData and scaleDataToImg", () => {
  const [panel] = initPanelScales([
    {
      domain: { left: 0, right: 10, bottom: 0, top: 10 },
      range: { left: 100, right: 200, bottom: 200, top: 100 },
      mapping: { x: "x", y: "y" },
    },
  ]);

  // Default (clip = true) clamps outside coordinates to domain/range
  assert.deepEqual(panel.scaleImgToData({ x: 50, y: 250 }), { x: 0, y: 0 });
  assert.deepEqual(
    panel.scaleDataToImg({ xmin: -5, xmax: 15, ymin: -5, ymax: 15 }),
    { xmin: 100, xmax: 200, ymin: 200, ymax: 100 },
  );

  // Explicit clip = false preserves out-of-range coordinates
  assert.deepEqual(panel.scaleImgToData({ x: 50, y: 250 }, false), {
    x: -5,
    y: -5,
  });
  assert.deepEqual(
    panel.scaleDataToImg({ xmin: -5, xmax: 15, ymin: -5, ymax: 15 }, false),
    { xmin: 50, xmax: 250, ymin: 250, ymax: 50 },
  );
});
