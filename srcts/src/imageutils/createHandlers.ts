import $ from "jquery";
import { imageOutputBinding } from "../bindings/output/image";
import type { InputRatePolicy } from "../inputPolicies";
import { shinySetInputValue } from "../shiny/initedMethods";
import { Debouncer, Throttler } from "../time";
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
  onResize: ((e: JQuery.ResizeEvent) => void) | null;
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
  const clickInfoSender = coordmap.mouseCoordinateSender(inputId, clip);

  // Send initial (null) value on creation.
  clickInfoSender(null);

  return {
    mousedown: function (e) {
      // Listen for left mouse button only
      if (e.which !== 1) return;
      clickInfoSender(e);
    },
    onResetImg: function () {
      clickInfoSender(null);
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
  const sendHoverInfo = coordmap.mouseCoordinateSender(
    inputId,
    clip,
    nullOutside
  );

  let hoverInfoSender: InputRatePolicy<typeof sendHoverInfo>;

  if (delayType === "throttle")
    hoverInfoSender = new Throttler(null, sendHoverInfo, delay);
  else hoverInfoSender = new Debouncer(null, sendHoverInfo, delay);

  // Send initial (null) value on creation.
  hoverInfoSender.immediateCall(null);

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
      hoverInfoSender.immediateCall(null);
    },
    onResize: null,
  };
}

// Returns a brush handler object. This has three public functions:
// mousedown, mousemove, and onResetImg.
function createBrushHandler(
  inputId: InputId,
  $el: JQuery<HTMLElement>,
  opts: BrushOpts,
  coordmap: Coordmap,
  outputId: BrushInfo["outputId"]
): CreateHandler {
  // Parameter: expand the area in which a brush can be started, by this
  // many pixels in all directions. (This should probably be a brush option)
  const expandPixels = 20;

  // Represents the state of the brush
  const brush = createBrush($el, opts, coordmap, expandPixels);

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
      brush.reset();
    }
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
    const coords: BrushInfo = brush.boundsData();

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

    // Add the panel (facet) variables, if present
    $.extend(coords, panel.panel_vars);

    // eslint-disable-next-line camelcase
    coords.coords_css = brush.boundsCss();
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

  if (opts.brushDelayType === "throttle") {
    brushInfoSender = new Throttler(null, sendBrushInfo, opts.brushDelay);
  } else {
    brushInfoSender = new Debouncer(null, sendBrushInfo, opts.brushDelay);
  }

  // Send initial (null) value on creation.
  if (!brush.hasOldBrush()) {
    brushInfoSender.immediateCall();
  }

  function mousedown(e: JQuery.MouseDownEvent) {
    // This can happen when mousedown inside the graphic, then mouseup
    // outside, then mousedown inside. Just ignore the second
    // mousedown.
    if (brush.isBrushing() || brush.isDragging() || brush.isResizing()) return;

    // Listen for left mouse button only
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
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-expect-error; TODO-barret; Remove the variable? it is not used
      brush.startResizing(offsetCss);

      // Attach the move and up handlers to the window so that they respond
      // even when the mouse is moved outside of the image.
      $(document)
        .on("mousemove.image_brush", mousemoveResizing)
        .on("mouseup.image_brush", mouseupResizing);
    } else if (brush.isInsideBrush(offsetCss)) {
      // @ts-expect-error; TODO-barret this variable is not respected
      brush.startDragging(offsetCss);
      setCursorStyle("grabbing");

      // Attach the move and up handlers to the window so that they respond
      // even when the mouse is moved outside of the image.
      $(document)
        .on("mousemove.image_brush", mousemoveDragging)
        .on("mouseup.image_brush", mouseupDragging);
    } else {
      const panel = coordmap.getPanelCss(offsetCss, expandPixels);

      // @ts-expect-error; TODO-barret start brushing does not take any args; Either change the function to ignore, or do not send to function;
      brush.startBrushing(panel.clipImg(coordmap.scaleCssToImg(offsetCss)));

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

    // Send info immediately on mouseup, but only if needed. If we don't
    // do the pending check, we might send the same data twice (with
    // with difference nonce).
    if (brushInfoSender.isPending()) brushInfoSender.immediateCall();
  }

  function mouseupDragging(e: JQuery.MouseUpEvent) {
    // Listen for left mouse button only
    if (e.which !== 1) return;

    $(document).off("mousemove.image_brush").off("mouseup.image_brush");

    brush.up(coordmap.mouseOffsetCss(e));

    brush.stopDragging();
    setCursorStyle("grabbable");

    if (brushInfoSender.isPending()) brushInfoSender.immediateCall();
  }

  function mouseupResizing(e: JQuery.MouseUpEvent) {
    // Listen for left mouse button only
    if (e.which !== 1) return;

    $(document).off("mousemove.image_brush").off("mouseup.image_brush");

    brush.up(coordmap.mouseOffsetCss(e));
    brush.stopResizing();

    if (brushInfoSender.isPending()) brushInfoSender.immediateCall();
  }

  // Brush maintenance: When an image is re-rendered, the brush must either
  // be removed (if brushResetOnNew) or imported (if !brushResetOnNew). The
  // "mostRecentBrush" bit is to ensure that when multiple outputs share the
  // same brush ID, inactive brushes don't send null values up to the server.

  // This should be called when the img (not the el) is reset
  function onResetImg() {
    if (opts.brushResetOnNew) {
      if ($el.data("mostRecentBrush")) {
        brush.reset();
        brushInfoSender.immediateCall();
      }
    }
  }

  if (!opts.brushResetOnNew) {
    if ($el.data("mostRecentBrush")) {
      // Importing an old brush must happen after the image data has loaded
      // and the <img> DOM element has the updated size. If importOldBrush()
      // is called before this happens, then the css-img coordinate mappings
      // will give the wrong result, and the brush will have the wrong
      // position.
      //
      // jcheng 09/26/2018: This used to happen in img.onLoad, but recently
      // we moved to all brush initialization moving to img.onLoad so this
      // logic can be executed inline.
      brush.importOldBrush();
      brushInfoSender.immediateCall();
    }
  }

  function onResize() {
    brush.onResize();
    brushInfoSender.immediateCall();
  }

  return {
    mousedown: mousedown,
    mousemove: mousemove,
    onResetImg: onResetImg,
    onResize: onResize,
  };
}

export { createClickHandler, createHoverHandler, createBrushHandler };
export type { BrushInfo };
