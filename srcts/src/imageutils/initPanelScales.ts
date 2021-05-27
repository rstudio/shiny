// Map a value x from a domain to a range. If clip is true, clip it to the

import { OffsetType } from "./findbox";
import { mapValues } from "../utils";

// range.
function mapLinear(
  x: number,
  domainMin: number,
  domainMax: number,
  rangeMin: number,
  rangeMax: number,
  clip = true
) {
  // By default, clip to range
  clip = clip || true;

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
  logbase: number
) {
  return {
    scale: function (val: number, clip: boolean) {
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

type PanelType = {
  domain: {
    top: number;
    bottom: number;
    left: number;
    right: number;
  };
  range: {
    top: number;
    bottom: number;
    left: number;
    right: number;
  };
  log?: {
    x?: number;
    y?: number;
  };
  mapping: Record<string, string>;
  // eslint-disable-next-line camelcase
  panel_vars?: Record<string, number | string>;

  scaleDataToImg?: (
    val: Record<string, number>,
    clip?: boolean
  ) => Record<string, number>;
  scaleImgToData?: {
    (val: OffsetType, clip?: boolean): OffsetType;
    (val: Record<string, number>, clip?: boolean): Record<string, number>;
  };

  clipImg?: (offsetImg: { x: number; y: number }) => { x: number; y: number };
};

// Modify panel, adding scale and inverse-scale functions that take objects
// like {x:1, y:3}, and also add clip function.
function addScaleFuns(panel: PanelType) {
  const d = panel.domain;
  const r = panel.range;
  const xlog = panel.log && panel.log.x ? panel.log.x : null;
  const ylog = panel.log && panel.log.y ? panel.log.y : null;
  const xscaler = scaler1D(d.left, d.right, r.left, r.right, xlog);
  const yscaler = scaler1D(d.bottom, d.top, r.bottom, r.top, ylog);

  // Given an object of form {x:1, y:2}, or {x:1, xmin:2:, ymax: 3}, convert
  // from data coordinates to img. Whether a value is converted as x or y
  // depends on the first character of the key.
  panel.scaleDataToImg = function (val, clip) {
    return mapValues(val, (value, key) => {
      const prefix = key.substring(0, 1);

      if (prefix === "x") {
        return xscaler.scale(value, clip);
      } else if (prefix === "y") {
        return yscaler.scale(value, clip);
      }
      return null;
    });
  };

  function scaleImgToData(val: OffsetType, clip?: boolean);
  function scaleImgToData(val: Record<string, number>, clip?: boolean) {
    return mapValues(val, (value, key) => {
      const prefix = key.substring(0, 1);

      if (prefix === "x") {
        return xscaler.scaleInv(value, clip);
      } else if (prefix === "y") {
        return yscaler.scaleInv(value, clip);
      }
      return null;
    });
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
}

// Modifies the panel objects in a coordmap, adding scaleImgToData(),
// scaleDataToImg(), and clipImg() functions to each one. The panel objects
// use img and data coordinates only; they do not use css coordinates. The
// domain is in data coordinates; the range is in img coordinates.
function initPanelScales(panels: Array<PanelType>): void {
  // Add the functions to each panel object.
  for (let i = 0; i < panels.length; i++) {
    const panel = panels[i];

    addScaleFuns(panel);
  }
}

export type { PanelType };
export { initPanelScales };
