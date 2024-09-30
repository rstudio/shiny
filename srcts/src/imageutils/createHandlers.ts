import $ from "jquery";
import { imageOutputBinding } from "../bindings/output/image";
import type { InputRatePolicy } from "../inputPolicies";
import { shinySetInputValue } from "../shiny/initedMethods";
import { Debouncer, Throttler } from "../time";
import { mapValues, roundDigits, roundSignif } from "../utils";
import type { Bounds, BoundsCss, BrushOpts } from "./createBrush";
import { createBrush } from "./createBrush";
import type { Offset } from "./findbox";
import type { Coordmap } from "./initCoordmap";
import type { Panel } from "./initPanelScales";

// ----------------------------------------------------------
// Handler creators for click, hover, brush.
// Each of these returns an object with a few public members. These public
// members are callbacks that are meant to be bound to events on $el with
// the same name (like 'mousedown').
// ----------------------------------------------------------

type CreateHandler = {
  mousemove?: (e: JQuery.MouseMoveEvent) => void;
  mouseout?: (e: JQuery.MouseOutEvent) => void;
  mousedown?: (e: JQuery.MouseDownEvent) => void;
  onResetImg: () => void;
  onResize: ((e: JQuery.ResizeEvent) => void) | null; // Only used for brushes on cached plots
  // TODO maybe also used if image size changes without window changing?
  updateCoordmap?: (newMap: Coordmap) => void;
};

type BrushInfo = {
  xmin: number;
  xmax: number;
  ymin: number;
  ymax: number;
  // eslint-disable-next-line @typescript-eslint/naming-convention
  coords_css?: BoundsCss;
  // eslint-disable-next-line @typescript-eslint/naming-convention
  coords_img?: Bounds;
  x?: number;
  y?: number;
  // eslint-disable-next-line @typescript-eslint/naming-convention
  img_css_ratio?: Offset;
  mapping?: Panel["mapping"];
  domain?: Panel["domain"];
  range?: Panel["range"];
  log?: Panel["log"];
  direction?: BrushOpts["brushDirection"];
  brushId?: string;
  outputId?: string;
};

type InputId = Parameters<Coordmap["mouseCoordinateSender"]>[0];
type Clip = Parameters<Coordmap["mouseCoordinateSender"]>[1];
type NullOutside = Parameters<Coordmap["mouseCoordinateSender"]>[2];

function createClickHandler(
  inputId: InputId,
  clip: Clip,
  coordmap: Coordmap
): CreateHandler {
  let clickInfoSender = coordmap.mouseCoordinateSender(inputId, clip);

  // Send initial (null) value on creation.
  clickInfoSender(null);

  return {
    mousedown: function (e) {
      // Listen for left mouse button only
      if (e.which !== 1) return;
      clickInfoSender(e);
    },
    onResetImg: function () {
      // TODO: consider making this a no-op or adding an option, see #2153
      clickInfoSender(null);
    },
    updateCoordmap: function (newMap) {
      clickInfoSender = newMap.mouseCoordinateSender(inputId, clip);
    },
    onResize: null,
  };
}

function createHoverHandler(
  inputId: InputId,
  delay: number,
  delayType: string | "throttle",
  clip: Clip,
  nullOutside: NullOutside,
  coordmap: Coordmap
): CreateHandler {
  let hoverInfoSender: InputRatePolicy<
    (e: JQuery.MouseDownEvent | JQuery.MouseMoveEvent | null) => void
  >;

  function updateHoverInfoSender(newCoordmap: Coordmap) {
    const sendHoverInfo = newCoordmap.mouseCoordinateSender(
      inputId,
      clip,
      nullOutside
    );

    // TODO: should we support Invoker as an option? (i.e. spam the server
    // with every mousemove) Or is it better not to?
    if (delayType === "throttle")
      hoverInfoSender = new Throttler(null, sendHoverInfo, delay);
    else hoverInfoSender = new Debouncer(null, sendHoverInfo, delay);
  }

  // Initialize the sender with the starting coordmap
  updateHoverInfoSender(coordmap);

  // Send initial (null) value on creation.
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  hoverInfoSender!.immediateCall(null);

  // What to do when mouse exits the image
  let mouseout: () => void;

  if (nullOutside)
    mouseout = function () {
      hoverInfoSender.normalCall(null);
    };
  else
    mouseout = function () {
      // do nothing
    };

  return {
    mousemove: function (e) {
      hoverInfoSender.normalCall(e);
    },
    mouseout: mouseout,
    onResetImg: function () {
      // TODO: consider making this a no-op or adding an option, see #2153
      hoverInfoSender.immediateCall(null);
    },
    updateCoordmap: updateHoverInfoSender,
    onResize: null,
  };
}

// Returns a brush handler object. This has three public functions:
// mousedown, mousemove, and onResetImg.
function createBrushHandler(
  inputId: InputId,
  $el: JQuery<HTMLElement>,
  opts: BrushOpts,
  initCoordmap: Coordmap,
  outputId: BrushInfo["outputId"]
): CreateHandler {
  // Parameter: expand the area in which a brush can be started, by this
  // many pixels in all directions. (This should probably be a brush option)
  const expandPixels = 20;

  let coordmap = initCoordmap;

  // Represents the state of the brush
  const brush = createBrush($el, opts, coordmap, expandPixels);

  // This is called by the image binding when the image is redrawn for any reason.
  function updateCoordmap(newCoordmap: Coordmap) {
    coordmap = newCoordmap;
    brush.updateCoordmap(coordmap);
    // Made sure to send new coords if the new map changed the pixel scale or
    // clipped us off the side, and we were the most recent brush with our id
    if ($el.data("mostRecentBrush")) {
      brushInfoSender.normalCall(); // Don't jump the queue--see #1642
    }
  }

  // Brush IDs can span multiple image/plot outputs. When an output is brushed,
  // if a brush with the same ID is active on a different image/plot, it must
  // be dismissed (but without sending any data to the server). We implement
  // this by sending the shiny-internal:brushed event to all plots, and letting
  // each plot decide for itself what to do.
  //
  // The decision to have the event sent to each plot (as opposed to a single
  // event triggered on, say, the document) was made to make cleanup easier;
  // listening on an event on the document would prevent garbage collection
  // of plot outputs that are removed from the document.
  $el.on("shiny-internal:brushed.image_output", function (e, coords) {
    // If the new brush shares our ID but not our output element ID, we
    // need to clear our brush (if any).
    if (coords.brushId === inputId && coords.outputId !== outputId) {
      $el.data("mostRecentBrush", false);
      // Remove mousemove and mouseup handlers if necessary
      if (brush.isBrushing() || brush.isDragging() || brush.isResizing()) {
        $(document).off("mousemove.image_brush").off("mouseup.image_brush");
      }
      brush.reset();
    }
  });

  // Listen for the event generated by the `setBrush()` function
  $el.on("shiny-internal:setBrush.image_output", function (e, data) {
    if (data.brushId !== inputId) return; // ignore if message wasn't for us
    // Check outputId only if provided
    if (data.outputId && data.outputId !== outputId) return;

    // Cancel any current manual brushing by removing listeners
    if (brush.isBrushing() || brush.isDragging() || brush.isResizing()) {
      $(document).off("mousemove.image_brush").off("mouseup.image_brush");
    }

    brush.setPanelIdx(data.panelIdx);
    // boundData will now check for a valid panel and reset if invalid
    brush.boundsData(data.imgCoords);
    brushInfoSender.immediateCall();
    // This is a race condition if multiple plots share the same brushId
    // and outputId isn't specified; documentation should warn about that.
    // I think that's acceptable, since there's no way for a brush to know
    // if it's unique.
  });

  // Set cursor to one of 7 styles. We need to set the cursor on the whole
  // el instead of the brush div, because the brush div has
  // 'pointer-events:none' so that it won't intercept pointer events.
  // If `style` is null, don't add a cursor style.
  function setCursorStyle(
    style:
      | "crosshair"
      | "ew-resize"
      | "grabbable"
      | "grabbing"
      | "nesw-resize"
      | "ns-resize"
      | "nwse-resize"
      | null
  ) {
    $el.removeClass(
      "crosshair grabbable grabbing ns-resize ew-resize nesw-resize nwse-resize"
    );

    if (style) $el.addClass(style);
  }

  function sendBrushInfo() {
    // Round to 13 significant digits *here* to prevent FP-rounding-induced
    // resends of almost-but-not-quite-exactly-the-same data.
    // This fixes #1634, #1642, #2197, and #2344 more reliably.
    // Values are still sporadic near zero, because 1.23456789e-20
    // and 1.98765432e-20 both get reported with all their "significant"
    // digits and cause a resend--this issue is fixed with the further
    // rounding below.
    const coords: BrushInfo = mapValues(brush.boundsData(), (val) =>
      roundSignif(val, 13)
    ) as Bounds;

    // We're in a new or reset state
    if (isNaN(coords.xmin)) {
      shinySetInputValue(inputId, null);
      // Must tell other brushes to clear.
      imageOutputBinding
        .find(document.documentElement)
        .trigger("shiny-internal:brushed", {
          brushId: inputId,
          outputId: null,
        });
      return;
    }

    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const panel = brush.getPanel()!;

    // Round values near zero more agressively to fix the problem mentioned above
    // Specifically, round values to the same absolute precision achieved by
    // rounding the range of values to 13 digits.
    const dataDomainX = Math.abs(panel.domain.right - panel.domain.left);
    const xDigits = 13 - Math.floor(Math.log10(dataDomainX));
    const dataDomainY = Math.abs(panel.domain.top - panel.domain.bottom);
    const yDigits = 13 - Math.floor(Math.log10(dataDomainY));

    coords.xmin = roundDigits(coords.xmin, xDigits);
    coords.xmax = roundDigits(coords.xmax, xDigits);
    coords.ymin = roundDigits(coords.ymin, yDigits);
    coords.ymax = roundDigits(coords.ymax, yDigits);

    // Add the panel (facet) variables, if present
    $.extend(coords, panel.panel_vars);

    // Round *here* to prevent FP-rounding-induced
    // resends of almost-but-not-quite-exactly-the-same data.
    // Rounding more aggressively, to an eigth of a pixel, since less than
    // that is not meaningful at reasonable zoom levels (≤ 8X) and the
    // power of two makes the actual floating-point value a round number
    // in binary.
    // eslint-disable-next-line camelcase
    coords.coords_css = mapValues(
      brush.boundsCss(),
      (val) => Math.round(val * 8) / 8
    ) as Bounds;

    // eslint-disable-next-line camelcase
    coords.coords_img = coordmap.scaleCssToImg(coords.coords_css);

    // eslint-disable-next-line camelcase
    coords.img_css_ratio = coordmap.cssToImgScalingRatio();

    // Add variable name mappings
    coords.mapping = panel.mapping;

    // Add scaling information
    coords.domain = panel.domain;
    coords.range = panel.range;
    coords.log = panel.log;

    coords.direction = opts.brushDirection;

    coords.brushId = inputId;
    coords.outputId = outputId;

    // Send data to server
    shinySetInputValue(inputId, coords);

    $el.data("mostRecentBrush", true);
    imageOutputBinding
      .find(document.documentElement)
      .trigger("shiny-internal:brushed", coords);
  }

  let brushInfoSender:
    | Debouncer<typeof sendBrushInfo>
    | Throttler<typeof sendBrushInfo>;

  // TODO: should we support Invoker as an option? (i.e. spam the server
  // with every mousemove) Or is it better not to
  if (opts.brushDelayType === "throttle") {
    brushInfoSender = new Throttler(null, sendBrushInfo, opts.brushDelay);
  } else {
    brushInfoSender = new Debouncer(null, sendBrushInfo, opts.brushDelay);
  }

  // Send initial (null) value on creation.
  brushInfoSender.immediateCall();

  function mousedown(e: JQuery.MouseDownEvent) {
    // This can happen when mousedown inside the graphic, then mouseup
    // outside, then mousedown inside. Just ignore the second
    // mousedown.
    if (brush.isBrushing() || brush.isDragging() || brush.isResizing()) return;

    // Listen for left mouse button only
    // TODO: should we change this to `event.button !== 0`?
    // e.which is technically deprecated, and every modern browser
    // (including IE9+, but not IE8) supports e.button (the actual standard)
    if (e.which !== 1) return;

    // In general, brush uses css pixels, and coordmap uses img pixels.
    const offsetCss = coordmap.mouseOffsetCss(e);

    // Ignore mousedown events outside of plotting region, expanded by
    // a number of pixels specified in expandPixels.
    if (opts.brushClip && !coordmap.isInPanelCss(offsetCss, expandPixels))
      return;

    brush.up({ x: NaN, y: NaN });
    brush.down(offsetCss);

    if (brush.isInResizeArea(offsetCss)) {
      brush.startResizing();

      // Attach the move and up handlers to the window so that they respond
      // even when the mouse is moved outside of the image.
      $(document)
        .on("mousemove.image_brush", mousemoveResizing)
        .on("mouseup.image_brush", mouseupResizing);
    } else if (brush.isInsideBrush(offsetCss)) {
      brush.startDragging();
      setCursorStyle("grabbing");

      // Attach the move and up handlers to the window so that they respond
      // even when the mouse is moved outside of the image.
      $(document)
        .on("mousemove.image_brush", mousemoveDragging)
        .on("mouseup.image_brush", mouseupDragging);
    } else {
      brush.startBrushing();

      // Attach the move and up handlers to the window so that they respond
      // even when the mouse is moved outside of the image.
      $(document)
        .on("mousemove.image_brush", mousemoveBrushing)
        .on("mouseup.image_brush", mouseupBrushing);
    }
  }

  // This sets the cursor style when it's in the el
  function mousemove(e: JQuery.MouseMoveEvent) {
    // In general, brush uses css pixels, and coordmap uses img pixels.
    const offsetCss = coordmap.mouseOffsetCss(e);

    if (!(brush.isBrushing() || brush.isDragging() || brush.isResizing())) {
      // Set the cursor depending on where it is
      if (brush.isInResizeArea(offsetCss)) {
        const r = brush.whichResizeSides(offsetCss);

        if ((r.left && r.top) || (r.right && r.bottom)) {
          setCursorStyle("nwse-resize");
        } else if ((r.left && r.bottom) || (r.right && r.top)) {
          setCursorStyle("nesw-resize");
        } else if (r.left || r.right) {
          setCursorStyle("ew-resize");
        } else if (r.top || r.bottom) {
          setCursorStyle("ns-resize");
        }
      } else if (brush.isInsideBrush(offsetCss)) {
        setCursorStyle("grabbable");
      } else if (coordmap.isInPanelCss(offsetCss, expandPixels)) {
        setCursorStyle("crosshair");
      } else {
        setCursorStyle(null);
      }
    }
  }

  // mousemove handlers while brushing or dragging
  function mousemoveBrushing(e: JQuery.MouseMoveEvent) {
    brush.brushTo(coordmap.mouseOffsetCss(e));
    brushInfoSender.normalCall();
  }

  function mousemoveDragging(e: JQuery.MouseMoveEvent) {
    brush.dragTo(coordmap.mouseOffsetCss(e));
    brushInfoSender.normalCall();
  }

  function mousemoveResizing(e: JQuery.MouseMoveEvent) {
    brush.resizeTo(coordmap.mouseOffsetCss(e));
    brushInfoSender.normalCall();
  }

  // mouseup handlers while brushing or dragging
  function mouseupBrushing(e: JQuery.MouseUpEvent) {
    // Listen for left mouse button only
    if (e.which !== 1) return;

    $(document).off("mousemove.image_brush").off("mouseup.image_brush");

    brush.up(coordmap.mouseOffsetCss(e));

    brush.stopBrushing();
    setCursorStyle("crosshair");

    // If the brush didn't go anywhere, hide the brush, clear value,
    // and return.
    if (brush.down().x === brush.up().x && brush.down().y === brush.up().y) {
      brush.reset();
      brushInfoSender.immediateCall();
      return;
    }

    // Send info immediately on mouseup, since shinySetInputValue will already
    // filter out any duplicate sends and we want the brush to be responsive
    // when the user completes their action.
    brushInfoSender.immediateCall();
  }

  function mouseupDragging(e: JQuery.MouseUpEvent) {
    // Listen for left mouse button only
    if (e.which !== 1) return;

    $(document).off("mousemove.image_brush").off("mouseup.image_brush");

    brush.up(coordmap.mouseOffsetCss(e));

    brush.stopDragging();
    setCursorStyle("grabbable");

    // if (brushInfoSender.isPending()) brushInfoSender.immediateCall();
    brushInfoSender.immediateCall();
  }

  function mouseupResizing(e: JQuery.MouseUpEvent) {
    // Listen for left mouse button only
    if (e.which !== 1) return;

    $(document).off("mousemove.image_brush").off("mouseup.image_brush");

    brush.up(coordmap.mouseOffsetCss(e));
    brush.stopResizing();

    // if (brushInfoSender.isPending()) brushInfoSender.immediateCall();
    brushInfoSender.immediateCall();
  }

  // Brush maintenance: When an image is re-rendered, the brush must either
  // be removed (if brushResetOnNew) or resized (if !brushResetOnNew). The
  // "mostRecentBrush" bit is to ensure that when multiple outputs share the
  // same brush ID, inactive brushes don't send null values up to the server.
  // If opts.brushResetOnNew is false, then updateCoordmap() above will take care
  // of all necessary resizing.

  // This should be called when the img (not the el) is reset
  function onResetImg() {
    // Reset the brush only if the reset_on_new option is TRUE,
    // or if we are in an error state
    if (opts.brushResetOnNew || $el.data("errorState")) {
      // Remove mousemove and mouseup handlers if necessary
      if (brush.isBrushing() || brush.isDragging() || brush.isResizing()) {
        $(document).off("mousemove.image_brush").off("mouseup.image_brush");
      }
      brush.reset();
      if ($el.data("mostRecentBrush")) {
        brushInfoSender.immediateCall();
      }
    }
  }

  // This lets the brush know to resize if the image is resized without being
  // redrawn. This is only possible for cached plots, so this function is only
  // triggered in image.ts if the plot is a cached plot.
  function onResize() {
    brush.onImgResize();
    brushInfoSender.immediateCall();
  }

  return {
    mousedown: mousedown,
    mousemove: mousemove,
    onResetImg: onResetImg,
    updateCoordmap: updateCoordmap,
    onResize: onResize,
  };
}

export { createClickHandler, createHoverHandler, createBrushHandler };
export type { BrushInfo };
