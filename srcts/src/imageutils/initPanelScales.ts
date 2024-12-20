// Map a value x from a domain to a range. If clip is true, clip it to the

import { mapValues } from "../utils";
import type { Bounds } from "./createBrush";
import type { Offset } from "./findbox";

// range.
function mapLinear(
  x: number,
  domainMin: number,
  domainMax: number,
  rangeMin: number,
  rangeMax: number,
  clip = true // By default, clip to range
) {
  const factor = (rangeMax - rangeMin) / (domainMax - domainMin);
  const val = x - domainMin;
  let newval = val * factor + rangeMin;

  if (clip) {
    const max = Math.max(rangeMax, rangeMin);
    const min = Math.min(rangeMax, rangeMin);

    if (newval > max) newval = max;
    else if (newval < min) newval = min;
  }
  return newval;
}

// Create scale and inverse-scale functions for a single direction (x or y).
function scaler1D(
  domainMin: number,
  domainMax: number,
  rangeMin: number,
  rangeMax: number,
  logbase: number | null
) {
  return {
    scale: function (val: number, clip?: boolean) {
      if (logbase) val = Math.log(val) / Math.log(logbase);
      return mapLinear(val, domainMin, domainMax, rangeMin, rangeMax, clip);
    },

    scaleInv: function (val: number, clip?: boolean) {
      let res = mapLinear(val, rangeMin, rangeMax, domainMin, domainMax, clip);

      if (logbase) res = Math.pow(logbase, res);
      return res;
    },
  };
}

type PanelInit = {
  domain: {
    // Data coordinates
    top: number;
    bottom: number;
    left: number;
    right: number;
  };
  range: {
    // Pixel coordinates
    top: number;
    bottom: number;
    left: number;
    right: number;
  };
  log?: {
    x?: number;
    y?: number;
  };
  mapping: { [key: string]: string };
  // eslint-disable-next-line @typescript-eslint/naming-convention
  panel_vars?: { [key: string]: number | string };
};
type Panel = PanelInit & {
  scaleDataToImg: {
    (val: Bounds, clip?: boolean): Bounds;
  };
  scaleImgToData: {
    (val: Offset, clip?: boolean): Offset;
  };

  clipImg: (offsetImg: { x: number; y: number }) => { x: number; y: number };
};

// Modify panel, adding scale and inverse-scale functions that take objects
// like {x:1, y:3}, and also add clip function.
function addScaleFuns(panel_: PanelInit): Panel {
  const panel = panel_ as Panel;
  const d = panel.domain;
  const r = panel.range;
  const xlog = panel.log && panel.log.x ? panel.log.x : null;
  const ylog = panel.log && panel.log.y ? panel.log.y : null;
  const xscaler = scaler1D(d.left, d.right, r.left, r.right, xlog);
  const yscaler = scaler1D(d.bottom, d.top, r.bottom, r.top, ylog);

  // Given an object of form {x:1, y:2}, or {x:1, xmin:2:, ymax: 3}, convert
  // from data coordinates to img. Whether a value is converted as x or y
  // depends on the first character of the key.
  // (val: Offset, clip?: boolean): Offset;
  // (val: Bounds, clip?: boolean): Bounds;
  // (val: { [key: `${"x" | "y"}${string}`]: number }, clip?: boolean): { [key: `${"x" | "y"}${string}`]: number }
  // (val: { [key: string]: number | null }, clip?: boolean): {
  //   [key: string]: number | null;
  // };
  function scaleDataToImg(
    val: Bounds,
    clip?: Parameters<typeof xscaler.scale>[1]
  ): Bounds {
    return mapValues(val, (value, key) => {
      const prefix = key.substring(0, 1);

      if (prefix === "x") {
        return xscaler.scale(value, clip);
      } else if (prefix === "y") {
        return yscaler.scale(value, clip);
      }
      // TODO-future; If we know the input is a valid input (starts with x/y), why do we still have this value?
      return null;
    }) as Bounds;
  }
  panel.scaleDataToImg = scaleDataToImg;

  function scaleImgToData(val: Offset, clip?: boolean): Offset {
    return mapValues(val, (value, key) => {
      const prefix = key.substring(0, 1);

      if (prefix === "x") {
        return xscaler.scaleInv(value, clip);
      } else if (prefix === "y") {
        return yscaler.scaleInv(value, clip);
      }
      // TODO-future; If we know the input is a valid input (starts with x/y), why do we still have this value?
      return null;
    }) as Offset;
  }
  panel.scaleImgToData = scaleImgToData;

  // Given a scaled offset (in img pixels), clip it to the nearest panel region.
  panel.clipImg = function (offsetImg) {
    const newOffset = {
      x: offsetImg.x,
      y: offsetImg.y,
    };

    const bounds = panel.range;

    if (offsetImg.x > bounds.right) newOffset.x = bounds.right;
    else if (offsetImg.x < bounds.left) newOffset.x = bounds.left;

    if (offsetImg.y > bounds.bottom) newOffset.y = bounds.bottom;
    else if (offsetImg.y < bounds.top) newOffset.y = bounds.top;

    return newOffset;
  };

  return panel;
}

// Modifies the panel objects in a coordmap, adding scaleImgToData(),
// scaleDataToImg(), and clipImg() functions to each one. The panel objects
// use img and data coordinates only; they do not use css coordinates. The
// domain is in data coordinates; the range is in img coordinates.
function initPanelScales(panels: PanelInit[]): Panel[] {
  // Add the functions to each panel object.
  return panels.map((panel) => addScaleFuns(panel));
}

export type { Panel, PanelInit };
export { initPanelScales };
