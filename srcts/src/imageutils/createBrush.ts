import $ from "jquery";
import { equal, isnan } from "../utils";
import type { Coordmap } from "./initCoordmap";
import { findOrigin } from "./initCoordmap";
import type { Panel } from "./initPanelScales";

import type { Offset } from "./findbox";
import { findBox } from "./findbox";
import { shiftToRange } from "./shiftToRange";

type Bounds = {
  xmin: number;
  xmax: number;
  ymin: number;
  ymax: number;
};
type BoundsCss = Bounds;
type BoundsData = Bounds;

type ImageState = {
  brushing: boolean;
  dragging: boolean;
  resizing: boolean;

  // Offset of last mouse down and up events (in CSS pixels)
  down: Offset;
  up: Offset;

  // Which side(s) we're currently resizing
  resizeSides: {
    left: boolean;
    right: boolean;
    top: boolean;
    bottom: boolean;
  };

  boundsCss: BoundsCss;
  boundsData: BoundsData;

  // Panel object that the brush is in
  panel: Panel | null;

  // The bounds at the start of a drag/resize (in CSS pixels)
  changeStartBounds: Bounds;
};

type BrushOpts = {
  brushDirection: "x" | "xy" | "y";
  brushClip: boolean;
  brushFill: string;
  brushOpacity: string;
  brushStroke: string;
  brushDelayType?: "debounce" | "throttle";
  brushDelay?: number;
  brushResetOnNew?: boolean;
};

type Brush = {
  updateCoordmap: (newCoordmap: Coordmap) => void;
  reset: () => void;

  isInsideBrush: (offsetCss: Offset) => boolean;
  isInResizeArea: (offsetCss: Offset) => boolean;
  whichResizeSides: (offsetCss: Offset) => ImageState["resizeSides"];

  // A callback when the wrapper div or img is resized. Only called if the
  // brush is on a cached plot.
  onImgResize: () => void;

  // These functions will get or set the bounds of the brush based on CSS or
  // data coordinates, respectively. The setter versions will visually update
  // the div as required to match.
  boundsCss: {
    (boxCss: BoundsCss): void;
    (): BoundsCss;
  };
  boundsData: {
    (boxData: BoundsData): void;
    (): BoundsData;
  };

  getPanel: () => ImageState["panel"];

  setPanelIdx: (idx: number) => void;

  down: {
    (): ImageState["down"];
    (offsetCss: Offset): void;
  };
  up: {
    (): ImageState["up"];
    (offsetCss: Offset): void;
  };

  isBrushing: () => ImageState["brushing"];
  startBrushing: () => void;
  brushTo: (offsetCss: Offset) => void;
  stopBrushing: () => void;

  isDragging: () => ImageState["dragging"];
  startDragging: () => void;
  dragTo: (offsetCss: Offset) => void;
  stopDragging: () => void;

  isResizing: () => ImageState["resizing"];
  startResizing: () => void;
  resizeTo: (offsetCss: Offset) => void;
  stopResizing: () => void;
};

// Returns an object that represents the state of the brush. This gets wrapped
// in a brushHandler, which provides various event listeners.
function createBrush(
  $el: JQuery<HTMLElement>,
  opts: BrushOpts,
  coordmap: Coordmap,
  expandPixels: number
): Brush {
  // Number of pixels outside of brush to allow start resizing
  const resizeExpand = 10;

  const el = $el[0];
  let $div: JQuery<HTMLElement> | null = null; // The div representing the brush

  const state = {} as ImageState;

  // Aliases for conciseness
  let cssToImg = coordmap.scaleCssToImg;
  let imgToCss = coordmap.scaleImgToCss;

  reset();

  function reset() {
    // Current brushing/dragging/resizing state
    state.brushing = false;
    state.dragging = false;
    state.resizing = false;

    // Offset of last mouse down and up events (in CSS pixels)
    state.down = { x: NaN, y: NaN };
    state.up = { x: NaN, y: NaN };

    // Which side(s) we're currently resizing
    state.resizeSides = {
      left: false,
      right: false,
      top: false,
      bottom: false,
    };

    // Bounding rectangle of the brush, in CSS pixel and data dimensions. We
    // need to record data dimensions along with pixel dimensions so that when
    // a new plot is sent, we can re-draw the brush div with the appropriate
    // coords.
    state.boundsCss = {
      xmin: NaN,
      xmax: NaN,
      ymin: NaN,
      ymax: NaN,
    };
    state.boundsData = {
      xmin: NaN,
      xmax: NaN,
      ymin: NaN,
      ymax: NaN,
    };

    // Panel object that the brush is in
    state.panel = null;

    // The bounds at the start of a drag/resize (in CSS pixels)
    state.changeStartBounds = {
      xmin: NaN,
      xmax: NaN,
      ymin: NaN,
      ymax: NaN,
    };

    if ($div) {
      $div.remove();
      $div = null; // let's not have a floating variable hanging around
    }
  }

  // When provided a new coordmap from a redrawn image, reposition the brush
  // to keep the same coordinates in the data space.
  function updateCoordmap(newCoordmap: Coordmap) {
    coordmap = newCoordmap;
    cssToImg = coordmap.scaleCssToImg;
    imgToCss = coordmap.scaleImgToCss;

    const oldBoundsData = boundsData();
    const oldPanel = state.panel;

    // TODO are all of these checks necessary?
    if (!oldBoundsData || !oldPanel) {
      reset();
      return;
    }

    // Check to see if we have valid boundsData
    if (Object.values(oldBoundsData).some(isnan)) {
      reset();
      return;
    }

    let foundPanel = false;

    // TODO: Could store panel index and just use panel with same index
    // Would be faster/simpler, but could be buggy if server updates us with
    // an entirely different set of panels
    for (let i = 0; i < coordmap.panels.length; i++) {
      // Look through the coordmap for a panel whose mapping and panel_vars
      // match our previous panel
      const curPanel = coordmap.panels[i];

      if (
        equal(oldPanel.mapping, curPanel.mapping) &&
        equal(oldPanel.panel_vars, curPanel.panel_vars)
      ) {
        // We've found a matching panel--save it and set the boundsData
        state.panel = coordmap.panels[i];
        boundsData(oldBoundsData); // This will recalculate the CSS coords
        foundPanel = true;
        break;
      }
    }
    if (!foundPanel) {
      reset();
    }
  }

  // This will reposition the brush div when the image is resized, maintaining
  // the same data coordinates. Note that the "resize" here refers to the
  // wrapper div/img being resized; elsewhere, "resize" refers to the brush
  // div being resized. This is only necessary (and only called) for cached
  // plots; all other plots are reloaded whenever they're resized, and the
  // brush repositioning is handled by updateCoordmap().
  function onImgResize() {
    const boundsDataVal = boundsData();

    // Check to see if we have valid boundsData
    if (Object.values(boundsDataVal).some(isnan)) return;

    boundsData(boundsDataVal);
  }

  // Return true if the offset is inside min/max coords
  function isInsideBrush(offsetCss: Offset) {
    const bounds = state.boundsCss;

    return (
      offsetCss.x <= bounds.xmax &&
      offsetCss.x >= bounds.xmin &&
      offsetCss.y <= bounds.ymax &&
      offsetCss.y >= bounds.ymin
    );
  }

  // Return true if offset is inside a region to start a resize
  function isInResizeArea(offsetCss: Offset) {
    const sides = whichResizeSides(offsetCss);

    return sides.left || sides.right || sides.top || sides.bottom;
  }

  // Return an object representing which resize region(s) the cursor is in.
  function whichResizeSides(offsetCss: Offset) {
    const b = state.boundsCss;
    // Bounds with expansion
    const e = {
      xmin: b.xmin - resizeExpand,
      xmax: b.xmax + resizeExpand,
      ymin: b.ymin - resizeExpand,
      ymax: b.ymax + resizeExpand,
    };
    const res = {
      left: false,
      right: false,
      top: false,
      bottom: false,
    };

    if (
      (opts.brushDirection === "xy" || opts.brushDirection === "x") &&
      offsetCss.y <= e.ymax &&
      offsetCss.y >= e.ymin
    ) {
      if (offsetCss.x < b.xmin && offsetCss.x >= e.xmin) res.left = true;
      else if (offsetCss.x > b.xmax && offsetCss.x <= e.xmax) res.right = true;
    }

    if (
      (opts.brushDirection === "xy" || opts.brushDirection === "y") &&
      offsetCss.x <= e.xmax &&
      offsetCss.x >= e.xmin
    ) {
      if (offsetCss.y < b.ymin && offsetCss.y >= e.ymin) res.top = true;
      else if (offsetCss.y > b.ymax && offsetCss.y <= e.ymax) res.bottom = true;
    }

    return res;
  }

  // Sets the bounds of the brush (in CSS pixels), given a box and optional
  // panel. This will fit the box bounds into the panel, so we don't brush
  // outside of it. This knows whether we're brushing in the x, y, or xy
  // directions, and sets bounds accordingly. If no box is passed in, just
  // return current bounds. For new bounds, creates or updates the div.
  function boundsCss(): ImageState["boundsCss"];
  function boundsCss(boxCss: BoundsCss): void;
  function boundsCss(boxCss?: BoundsCss) {
    if (boxCss === undefined) {
      return { ...state.boundsCss };
    }

    // Give up and reset if the panel is null
    if (!state.panel) {
      reset();
      return;
    }
    const panel = state.panel;
    const panelBoundsImg = panel.range;

    let minCss: Offset = { x: boxCss.xmin, y: boxCss.ymin };
    let maxCss: Offset = { x: boxCss.xmax, y: boxCss.ymax };

    if (opts.brushClip) {
      minCss = imgToCss(panel.clipImg(cssToImg(minCss)));
      maxCss = imgToCss(panel.clipImg(cssToImg(maxCss)));
    }

    if (opts.brushDirection === "xy") {
      // No change
    } else if (opts.brushDirection === "x") {
      // Extend top and bottom of plotting area
      minCss.y = imgToCss({ y: panelBoundsImg.top }).y;
      maxCss.y = imgToCss({ y: panelBoundsImg.bottom }).y;
    } else if (opts.brushDirection === "y") {
      minCss.x = imgToCss({ x: panelBoundsImg.left }).x;
      maxCss.x = imgToCss({ x: panelBoundsImg.right }).x;
    }

    state.boundsCss = {
      xmin: minCss.x,
      xmax: maxCss.x,
      ymin: minCss.y,
      ymax: maxCss.y,
    };

    // Positions in data space
    const minData = panel.scaleImgToData(cssToImg(minCss), opts.brushClip);
    const maxData = panel.scaleImgToData(cssToImg(maxCss), opts.brushClip);

    // For reversed scales, the min and max can be reversed, so use findBox
    // to ensure correct order.
    state.boundsData = findBox(minData, maxData);

    if (!$div) {
      addDiv();
    }
    updateDiv();

    return undefined;
  }

  // Get or set the bounds of the brush using coordinates in the data space.
  function boundsData(): BoundsData;
  function boundsData(boxData: BoundsData): void;
  function boundsData(boxData?: BoundsData | undefined): BoundsData | void {
    if (typeof boxData === "undefined") {
      return { ...state.boundsData };
    }

    // Give up and reset if the panel is null
    if (!state.panel) {
      reset();
      return;
    }

    // Converting to CSS coordinates (and then eventually back to data
    // coordinates) is a bit roundabout, but the checks for clipping and brush
    // direction are done in the CSS coordinate space. This introduces slight
    // floating-point rounding errors, which are smoothed over by rounding
    // data and CSS coordinates in createHandlers.ts before sending them off to
    // the server.
    const boxCss = imgToCss(
      state.panel.scaleDataToImg(boxData, opts.brushClip)
    );

    // The scaling function can reverse the direction of the axes, so we need to
    // find the min and max again.
    boundsCss({
      xmin: Math.min(boxCss.xmin, boxCss.xmax),
      xmax: Math.max(boxCss.xmin, boxCss.xmax),
      ymin: Math.min(boxCss.ymin, boxCss.ymax),
      ymax: Math.max(boxCss.ymin, boxCss.ymax),
    });
    return undefined;
  }

  function getPanel() {
    return state.panel;
  }

  function setPanelIdx(idx: number) {
    state.panel = coordmap.panels[idx];
  }

  // Add a new div representing the brush.
  function addDiv() {
    // Don't bother starting hidden, this only is added when we want to
    // draw it now.
    $div = $(document.createElement("div"))
      .attr("id", el.id + "_brush")
      .css({
        //eslint-disable-next-line @typescript-eslint/naming-convention
        "background-color": opts.brushFill,
        opacity: opts.brushOpacity,
        //eslint-disable-next-line @typescript-eslint/naming-convention
        "pointer-events": "none",
        position: "absolute",
      });

    const borderStyle = "1px solid " + opts.brushStroke;

    if (opts.brushDirection === "xy") {
      $div.css({
        border: borderStyle,
      });
    } else if (opts.brushDirection === "x") {
      $div.css({
        //eslint-disable-next-line @typescript-eslint/naming-convention
        "border-left": borderStyle,
        //eslint-disable-next-line @typescript-eslint/naming-convention
        "border-right": borderStyle,
      });
    } else if (opts.brushDirection === "y") {
      $div.css({
        //eslint-disable-next-line @typescript-eslint/naming-convention
        "border-top": borderStyle,
        //eslint-disable-next-line @typescript-eslint/naming-convention
        "border-bottom": borderStyle,
      });
    }

    $el.append($div);
    $div.offset({ top: 0, left: 0 }).width(0).outerHeight(0);
  }

  // Update the brush div to reflect the current brush bounds.
  function updateDiv() {
    // Need parent offset relative to page to calculate mouse offset
    // relative to page.
    const imgOffsetCss = findOrigin($el.find("img"));
    const b = state.boundsCss;

    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    $div!
      .offset({
        top: imgOffsetCss.y + b.ymin,
        left: imgOffsetCss.x + b.xmin,
      })
      .outerWidth(b.xmax - b.xmin + 1)
      .outerHeight(b.ymax - b.ymin + 1);
  }

  function down(): ImageState["down"];
  function down(offsetCss: Offset): void;
  function down(offsetCss?: Offset | undefined) {
    if (offsetCss === undefined) return state.down;

    state.down = offsetCss;
    return undefined;
  }

  function up(): ImageState["up"];
  function up(offsetCss: Offset): void;
  function up(offsetCss?: Offset | undefined) {
    if (offsetCss === undefined) return state.up;

    state.up = offsetCss;
    return undefined;
  }

  function isBrushing() {
    return state.brushing;
  }

  function startBrushing() {
    state.brushing = true;
    if ($div) {
      $div.remove(); // Remove previous div
      $div = null;
    }
    state.panel = coordmap.getPanelCss(state.down, expandPixels);
  }

  function brushTo(offsetCss: Offset) {
    boundsCss(findBox(state.down, offsetCss));
  }

  function stopBrushing() {
    state.brushing = false;
    // Save the final bounding box of the brush
    boundsCss(findBox(state.down, state.up));
  }

  function isDragging() {
    return state.dragging;
  }

  function startDragging() {
    state.dragging = true;
    state.changeStartBounds = { ...state.boundsCss };
  }

  function dragTo(offsetCss: Offset) {
    // How far the brush was dragged
    const dx = offsetCss.x - state.down.x;
    const dy = offsetCss.y - state.down.y;

    // Calculate what new positions would be, before clipping.
    const start = state.changeStartBounds;
    let newBoundsCss = {
      xmin: start.xmin + dx,
      xmax: start.xmax + dx,
      ymin: start.ymin + dy,
      ymax: start.ymax + dy,
    };

    // Clip to the plotting area
    if (opts.brushClip) {
      const panel = state.panel as Panel;
      const panelBoundsImg = panel.range;
      const newBoundsImg = cssToImg(newBoundsCss);

      // Convert to format for shiftToRange
      let xvalsImg = [newBoundsImg.xmin, newBoundsImg.xmax];
      let yvalsImg = [newBoundsImg.ymin, newBoundsImg.ymax];

      xvalsImg = shiftToRange(
        xvalsImg,
        panelBoundsImg.left,
        panelBoundsImg.right
      );
      yvalsImg = shiftToRange(
        yvalsImg,
        panelBoundsImg.top,
        panelBoundsImg.bottom
      );

      // Convert back to bounds format
      newBoundsCss = imgToCss({
        xmin: xvalsImg[0],
        xmax: xvalsImg[1],
        ymin: yvalsImg[0],
        ymax: yvalsImg[1],
      });
    }

    boundsCss(newBoundsCss);
  }

  function stopDragging() {
    state.dragging = false;
  }

  function isResizing() {
    return state.resizing;
  }

  function startResizing() {
    state.resizing = true;
    state.changeStartBounds = { ...state.boundsCss };
    state.resizeSides = whichResizeSides(state.down);
  }

  function resizeTo(offsetCss: Offset) {
    // How far the brush was dragged
    const dCss = {
      x: offsetCss.x - state.down.x,
      y: offsetCss.y - state.down.y,
    };

    const dImg = cssToImg(dCss);

    // Calculate what new positions would be, before clipping.
    const bImg = cssToImg(state.changeStartBounds);
    const panel = state.panel as Panel;
    const panelBoundsImg = panel.range;

    if (state.resizeSides.left) {
      const xminImg = shiftToRange(
        bImg.xmin + dImg.x,
        panelBoundsImg.left,
        bImg.xmax
      )[0];

      bImg.xmin = xminImg;
    } else if (state.resizeSides.right) {
      const xmaxImg = shiftToRange(
        bImg.xmax + dImg.x,
        bImg.xmin,
        panelBoundsImg.right
      )[0];

      bImg.xmax = xmaxImg;
    }

    if (state.resizeSides.top) {
      const yminImg = shiftToRange(
        bImg.ymin + dImg.y,
        panelBoundsImg.top,
        bImg.ymax
      )[0];

      bImg.ymin = yminImg;
    } else if (state.resizeSides.bottom) {
      const ymaxImg = shiftToRange(
        bImg.ymax + dImg.y,
        bImg.ymin,
        panelBoundsImg.bottom
      )[0];

      bImg.ymax = ymaxImg;
    }

    boundsCss(imgToCss(bImg));
  }

  function stopResizing() {
    state.resizing = false;
  }

  return {
    updateCoordmap: updateCoordmap,
    reset: reset,

    isInsideBrush: isInsideBrush,
    isInResizeArea: isInResizeArea,
    whichResizeSides: whichResizeSides,

    // Only used for cached plots
    onImgResize: onImgResize,

    boundsCss: boundsCss,
    boundsData: boundsData,
    getPanel: getPanel,
    setPanelIdx: setPanelIdx,

    down: down,
    up: up,

    isBrushing: isBrushing,
    startBrushing: startBrushing,
    brushTo: brushTo,
    stopBrushing: stopBrushing,

    isDragging: isDragging,
    startDragging: startDragging,
    dragTo: dragTo,
    stopDragging: stopDragging,

    isResizing: isResizing,
    startResizing: startResizing,
    resizeTo: resizeTo,
    stopResizing: stopResizing,
  };
}

export { createBrush };
export type { Bounds, BrushOpts, BoundsCss };
