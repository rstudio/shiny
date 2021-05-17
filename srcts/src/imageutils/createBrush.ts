import $ from "jquery";
import { findOrigin } from "../imageutils/initCoordmap";
import { OffsetType } from ".";
import { isnan, roundSignif } from "../utils";
import { Offset } from "bootstrap";
import { StatsBase } from "fs";

type BoundsType = {
  xmin: number;
  xmax: number;
  ymin: number;
  ymax: number;
};
type BoundsCss = BoundsType;
type BoundsData = BoundsType;

type BrushType = {
  reset: () => void;

  importOldBrush: () => void;
  isInsideBrush: (offsetCss: OffsetType) => boolean;
  isInResizeArea: (offsetCss: OffsetType) => boolean;
  whichResizeSides: (
    offsetCss: OffsetType
  ) => { left: boolean; right: boolean; top: boolean; bottom: boolean };

  // A callback when the wrapper div or img is resized.
  onResize: () => void;

  // TODO define this type as both a getter and a setter interfaces.
  // boundsCss: (boxCss: BoundsCss) => void;
  // boundsCss: () => BoundsCss;
  boundsCss: (boxCss: BoundsCss) => BoundsCss | void;
  boundsData: (boxData: BoundsData) => BoundsData | void;

  getPanel: () => State.panel;

  down: down;
  up: up;

  isBrushing: isBrushing;
  startBrushing: startBrushing;
  brushTo: brushTo;
  stopBrushing: stopBrushing;

  isDragging: isDragging;
  startDragging: startDragging;
  dragTo: dragTo;
  stopDragging: stopDragging;

  isResizing: isResizing;
  startResizing: startResizing;
  resizeTo: resizeTo;
  stopResizing: stopResizing;
};

// Returns an object that represents the state of the brush. This gets wrapped
// in a brushHandler, which provides various event listeners.
function createBrush($el, opts, coordmap, expandPixels) {
  // Number of pixels outside of brush to allow start resizing
  const resizeExpand = 10;

  const el = $el[0];
  let $div = null; // The div representing the brush

  const state = {};

  // Aliases for conciseness
  const cssToImg = coordmap.scaleCssToImg;
  const imgToCss = coordmap.scaleImgToCss;

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

    if ($div) $div.remove();
  }

  // If there's an existing brush div, use that div to set the new brush's
  // settings, provided that the x, y, and panel variables have the same names,
  // and there's a panel with matching panel variable values.
  function importOldBrush() {
    const oldDiv = $el.find("#" + el.id + "_brush");

    if (oldDiv.length === 0) return;

    const oldBoundsData = oldDiv.data("bounds-data");
    const oldPanel = oldDiv.data("panel");

    if (!oldBoundsData || !oldPanel) return;

    // Find a panel that has matching vars; if none found, we can't restore.
    // The oldPanel and new panel must match on their mapping vars, and the
    // values.
    for (let i = 0; i < coordmap.panels.length; i++) {
      const curPanel = coordmap.panels[i];

      if (
        equal(oldPanel.mapping, curPanel.mapping) &&
        equal(oldPanel.panel_vars, curPanel.panel_vars)
      ) {
        // We've found a matching panel
        state.panel = coordmap.panels[i];
        break;
      }
    }

    // If we didn't find a matching panel, remove the old div and return
    if (state.panel === null) {
      oldDiv.remove();
      return;
    }

    $div = oldDiv;

    boundsData(oldBoundsData);
    updateDiv();
  }

  // This will reposition the brush div when the image is resized, maintaining
  // the same data coordinates. Note that the "resize" here refers to the
  // wrapper div/img being resized; elsewhere, "resize" refers to the brush
  // div being resized.
  function onResize() {
    const boundsData = boundsData();
    // Check to see if we have valid boundsData

    for (const val in boundsData) {
      if (isnan(boundsData[val])) return;
    }

    boundsData(boundsData);
    updateDiv();
  }

  // Return true if the offset is inside min/max coords
  function isInsideBrush(offsetCss) {
    const bounds = state.boundsCss;

    return (
      offsetCss.x <= bounds.xmax &&
      offsetCss.x >= bounds.xmin &&
      offsetCss.y <= bounds.ymax &&
      offsetCss.y >= bounds.ymin
    );
  }

  // Return true if offset is inside a region to start a resize
  function isInResizeArea(offsetCss) {
    const sides = whichResizeSides(offsetCss);

    return sides.left || sides.right || sides.top || sides.bottom;
  }

  // Return an object representing which resize region(s) the cursor is in.
  function whichResizeSides(offsetCss) {
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
  // return current bounds.
  function boundsCss(boxCss) {
    if (boxCss === undefined) {
      return $.extend({}, state.boundsCss);
    }

    let minCss = { x: boxCss.xmin, y: boxCss.ymin };
    let maxCss = { x: boxCss.xmax, y: boxCss.ymax };

    const panel = state.panel;
    const panelBounds_img = panel.range;

    if (opts.brushClip) {
      minCss = imgToCss(panel.clipImg(cssToImg(minCss)));
      maxCss = imgToCss(panel.clipImg(cssToImg(maxCss)));
    }

    if (opts.brushDirection === "xy") {
      // No change
    } else if (opts.brushDirection === "x") {
      // Extend top and bottom of plotting area
      minCss.y = imgToCss({ y: panelBounds_img.top }).y;
      maxCss.y = imgToCss({ y: panelBounds_img.bottom }).y;
    } else if (opts.brushDirection === "y") {
      minCss.x = imgToCss({ x: panelBounds_img.left }).x;
      maxCss.x = imgToCss({ x: panelBounds_img.right }).x;
    }

    state.boundsCss = {
      xmin: minCss.x,
      xmax: maxCss.x,
      ymin: minCss.y,
      ymax: maxCss.y,
    };

    // Positions in data space
    const minData = state.panel.scaleImgToData(cssToImg(minCss));
    const maxData = state.panel.scaleImgToData(cssToImg(maxCss));
    // For reversed scales, the min and max can be reversed, so use findBox
    // to ensure correct order.

    state.boundsData = imageutils.findBox(minData, maxData);
    // Round to 14 significant digits to avoid spurious changes in FP values
    // (#1634).
    state.boundsData = mapValues(state.boundsData, (val) =>
      roundSignif(val, 14)
    );

    // We also need to attach the data bounds and panel as data attributes, so
    // that if the image is re-sent, we can grab the data bounds to create a new
    // brush. This should be fast because it doesn't actually modify the DOM.
    $div.data("bounds-data", state.boundsData);
    $div.data("panel", state.panel);
    return undefined;
  }

  // Get or set the bounds of the brush using coordinates in the data space.
  function boundsData(boxData) {
    if (boxData === undefined) {
      return $.extend({}, state.boundsData);
    }

    let boxCss = imgToCss(state.panel.scaleDataToImg(boxData));
    // Round to 13 significant digits to avoid spurious changes in FP values
    // (#2197).

    boxCss = mapValues(boxCss, (val) => roundSignif(val, 13));

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

  // Add a new div representing the brush.
  function addDiv() {
    if ($div) $div.remove();

    // Start hidden; we'll show it when movement occurs
    $div = $(document.createElement("div"))
      .attr("id", el.id + "_brush")
      .css({
        "background-color": opts.brushFill,
        opacity: opts.brushOpacity,
        "pointer-events": "none",
        position: "absolute",
      })
      .hide();

    const borderStyle = "1px solid " + opts.brushStroke;

    if (opts.brushDirection === "xy") {
      $div.css({
        border: borderStyle,
      });
    } else if (opts.brushDirection === "x") {
      $div.css({
        "border-left": borderStyle,
        "border-right": borderStyle,
      });
    } else if (opts.brushDirection === "y") {
      $div.css({
        "border-top": borderStyle,
        "border-bottom": borderStyle,
      });
    }

    $el.append($div);
    $div.offset({ x: 0, y: 0 }).width(0).outerHeight(0);
  }

  // Update the brush div to reflect the current brush bounds.
  function updateDiv() {
    // Need parent offset relative to page to calculate mouse offset
    // relative to page.
    const imgOffsetCss = findOrigin($el.find("img"));
    const b = state.boundsCss;

    $div
      .offset({
        top: imgOffsetCss.y + b.ymin,
        left: imgOffsetCss.x + b.xmin,
      })
      .outerWidth(b.xmax - b.xmin + 1)
      .outerHeight(b.ymax - b.ymin + 1);
  }

  function down(offset_css) {
    if (offset_css === undefined) return state.down;

    state.down = offset_css;
    return undefined;
  }

  function up(offset_css) {
    if (offset_css === undefined) return state.up;

    state.up = offset_css;
    return undefined;
  }

  function isBrushing() {
    return state.brushing;
  }

  function startBrushing() {
    state.brushing = true;
    addDiv();
    state.panel = coordmap.getPanelCss(state.down, expandPixels);

    boundsCss(imageutils.findBox(state.down, state.down));
    updateDiv();
  }

  function brushTo(offset_css) {
    boundsCss(imageutils.findBox(state.down, offset_css));
    $div.show();
    updateDiv();
  }

  function stopBrushing() {
    state.brushing = false;
    // Save the final bounding box of the brush
    boundsCss(imageutils.findBox(state.down, state.up));
  }

  function isDragging() {
    return state.dragging;
  }

  function startDragging() {
    state.dragging = true;
    state.changeStartBounds = $.extend({}, state.boundsCss);
  }

  function dragTo(offset_css) {
    // How far the brush was dragged
    const dx = offset_css.x - state.down.x;
    const dy = offset_css.y - state.down.y;

    // Calculate what new positions would be, before clipping.
    const start = state.changeStartBounds;
    let newBounds_css = {
      xmin: start.xmin + dx,
      xmax: start.xmax + dx,
      ymin: start.ymin + dy,
      ymax: start.ymax + dy,
    };

    // Clip to the plotting area
    if (opts.brushClip) {
      const panelBounds_img = state.panel.range;
      const newBounds_img = cssToImg(newBounds_css);

      // Convert to format for shiftToRange
      let xvals_img = [newBounds_img.xmin, newBounds_img.xmax];
      let yvals_img = [newBounds_img.ymin, newBounds_img.ymax];

      xvals_img = imageutils.shiftToRange(
        xvals_img,
        panelBounds_img.left,
        panelBounds_img.right
      );
      yvals_img = imageutils.shiftToRange(
        yvals_img,
        panelBounds_img.top,
        panelBounds_img.bottom
      );

      // Convert back to bounds format
      newBounds_css = imgToCss({
        xmin: xvals_img[0],
        xmax: xvals_img[1],
        ymin: yvals_img[0],
        ymax: yvals_img[1],
      });
    }

    boundsCss(newBounds_css);
    updateDiv();
  }

  function stopDragging() {
    state.dragging = false;
  }

  function isResizing() {
    return state.resizing;
  }

  function startResizing() {
    state.resizing = true;
    state.changeStartBounds = $.extend({}, state.boundsCss);
    state.resizeSides = whichResizeSides(state.down);
  }

  function resizeTo(offset_css) {
    // How far the brush was dragged
    const d_css = {
      x: offset_css.x - state.down.x,
      y: offset_css.y - state.down.y,
    };

    const d_img = cssToImg(d_css);

    // Calculate what new positions would be, before clipping.
    const b_img = cssToImg(state.changeStartBounds);
    const panelBounds_img = state.panel.range;

    if (state.resizeSides.left) {
      const xmin_img = imageutils.shiftToRange(
        b_img.xmin + d_img.x,
        panelBounds_img.left,
        b_img.xmax
      )[0];

      b_img.xmin = xmin_img;
    } else if (state.resizeSides.right) {
      const xmax_img = imageutils.shiftToRange(
        b_img.xmax + d_img.x,
        b_img.xmin,
        panelBounds_img.right
      )[0];

      b_img.xmax = xmax_img;
    }

    if (state.resizeSides.top) {
      const ymin_img = imageutils.shiftToRange(
        b_img.ymin + d_img.y,
        panelBounds_img.top,
        b_img.ymax
      )[0];

      b_img.ymin = ymin_img;
    } else if (state.resizeSides.bottom) {
      const ymax_img = imageutils.shiftToRange(
        b_img.ymax + d_img.y,
        b_img.ymin,
        panelBounds_img.bottom
      )[0];

      b_img.ymax = ymax_img;
    }

    boundsCss(imgToCss(b_img));
    updateDiv();
  }

  function stopResizing() {
    state.resizing = false;
  }

  return {
    reset: reset,

    importOldBrush: importOldBrush,
    isInsideBrush: isInsideBrush,
    isInResizeArea: isInResizeArea,
    whichResizeSides: whichResizeSides,

    onResize: onResize, // A callback when the wrapper div or img is resized.

    boundsCss: boundsCss,
    boundsData: boundsData,
    getPanel: getPanel,

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
