// Given two sets of x/y coordinates, return an object representing the min
// and max x and y values. (This could be generalized to any number of

import type { Bounds } from "./createBrush";

type Offset = {
  x: number;
  y: number;
};

// points).
function findBox(offset1: Offset, offset2: Offset): Bounds {
  return {
    xmin: Math.min(offset1.x, offset2.x),
    xmax: Math.max(offset1.x, offset2.x),
    ymin: Math.min(offset1.y, offset2.y),
    ymax: Math.max(offset1.y, offset2.y),
  };
}

export { findBox };
export type { Offset };
