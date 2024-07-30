import $ from "jquery";
import { shinySetInputValue } from "../shiny/initedMethods";
import { mapValues } from "../utils";
import type { Bounds } from "./createBrush";
import type { Offset } from "./findbox";
import type { Panel, PanelInit } from "./initPanelScales";
import { initPanelScales } from "./initPanelScales";

// -----------------------------------------------------------------------
// Utility functions for finding dimensions and locations of DOM elements
// -----------------------------------------------------------------------

// Returns the ratio that an element has been scaled (for example, by CSS
// transforms) in the x and y directions.
function findScalingRatio($el: JQuery<HTMLElement>) {
  const boundingRect = $el[0].getBoundingClientRect();

  return {
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    x: boundingRect.width / $el.outerWidth()!,
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    y: boundingRect.height / $el.outerHeight()!,
  };
}

function findOrigin($el: JQuery<HTMLElement>): Offset {
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  const offset = $el.offset()!;
  const scalingRatio = findScalingRatio($el);

  // Find the size of the padding and border, for the top and left. This is
  // before any transforms.
  const paddingBorder = {
    left:
      parseInt($el.css("border-left-width")) +
      parseInt($el.css("padding-left")),
    top:
      parseInt($el.css("border-top-width")) + parseInt($el.css("padding-top")),
  };

  // offset() returns the upper left corner of the element relative to the
  // page, but it includes padding and border. Here we find the upper left
  // of the element, not including padding and border.
  return {
    x: offset.left + scalingRatio.x * paddingBorder.left,
    y: offset.top + scalingRatio.y * paddingBorder.top,
  };
}

// Find the dimensions of a tag, after transforms, and without padding and
// border.
function findDims($el: JQuery<HTMLElement>) {
  // If there's any padding/border, we need to find the ratio of the actual
  // element content compared to the element plus padding and border.
  const contentRatio = {
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    x: $el.width()! / $el.outerWidth()!,
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    y: $el.height()! / $el.outerHeight()!,
  };

  // Get the dimensions of the element _after_ any CSS transforms. This
  // includes the padding and border.
  const boundingRect = $el[0].getBoundingClientRect();

  // Dimensions of the element after any CSS transforms, and without
  // padding/border.
  return {
    x: contentRatio.x * boundingRect.width,
    y: contentRatio.y * boundingRect.height,
  };
}

type OffsetCss = { [key: string]: number };
type OffsetImg = { [key: string]: number };

type Coords = {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  coords_css: Offset;
  // eslint-disable-next-line @typescript-eslint/naming-convention
  coords_img: Offset;
  x?: number;
  y?: number;
  // eslint-disable-next-line @typescript-eslint/naming-convention
  img_css_ratio?: Offset;
  mapping?: Panel["mapping"];
  domain?: Panel["domain"];
  range?: Panel["range"];
  log?: Panel["log"];
};

type CoordmapInit = {
  panels: PanelInit[];
  dims:
    | {
        height: number;
        width: number;
      }
    | { height: null; width: null };
};
type Coordmap = {
  panels: Panel[];
  dims: {
    height: number;
    width: number;
  };
  mouseOffsetCss: (evt: JQuery.MouseEventBase) => Offset;
  scaleCssToImg: {
    (offsetCss: Bounds): Bounds;
    (offsetCss: Offset): Offset;
    (offsetCss: OffsetCss): OffsetImg;
  };
  scaleImgToCss: {
    (offsetImg: Bounds): Bounds;
    (offsetImg: Offset): Offset;
    (offsetImg: OffsetImg): OffsetCss;
    (offsetImg: { [key: string]: number }): { [key: string]: number | null };
  };
  imgToCssScalingRatio: () => Offset;
  cssToImgScalingRatio: () => Offset;

  getPanelCss: (offsetCss: OffsetCss, expand?: number) => Panel | null;
  isInPanelCss: (offsetCss: OffsetCss, expand?: number) => boolean;

  mouseCoordinateSender: (
    inputId: string,
    clip?: boolean,
    nullOutside?: boolean
  ) => (e: JQuery.MouseDownEvent | JQuery.MouseMoveEvent | null) => void;
};

// This adds functions to the coordmap object to handle various
// coordinate-mapping tasks, and send information to the server. The input
// coordmap is an array of objects, each of which represents a panel. coordmap
// must be an array, even if empty, so that it can be modified in place; when
// empty, we add a dummy panel to the array. It also calls initPanelScales,
// which modifies each panel object to have scaleImgToData, scaleDataToImg,
// and clip functions.
//
// There are three coordinate spaces which we need to translate between:
//
// 1. css: The pixel coordinates in the web browser, also known as CSS pixels.
//    The origin is the upper-left corner of the <img> (not including padding
//    and border).
// 2. img: The pixel coordinates of the image data. A common case is on a
//    HiDPI device, where the source PNG image could be 1000 pixels wide but
//    be displayed in 500 CSS pixels. Another case is when the image has
//    additional scaling due to CSS transforms or width.
// 3. data: The coordinates in the data space. This is a bit more complicated
//    than the other two, because there can be multiple panels (as in facets).
function initCoordmap(
  $el: JQuery<HTMLElement>,
  coordmap_: CoordmapInit
): Coordmap {
  const $img = $el.find("img");
  const img = $img[0];

  // If we didn't get any panels, create a dummy one where the domain and range
  // are simply the pixel dimensions.
  // that we modify.
  if (coordmap_.panels.length === 0) {
    const bounds = {
      top: 0,
      left: 0,
      right: img.naturalWidth - 1,
      bottom: img.naturalHeight - 1,
    };

    coordmap_.panels[0] = {
      domain: bounds,
      range: bounds,
      mapping: {},
    };
  }

  const coordmap = coordmap_ as Coordmap;
  // If no dim height and width values are found, set them to the raw image height and width
  // These values should be the same...
  // This is only done to initialize an image output, whose height and width are unknown until the image is retrieved

  coordmap.dims.height = coordmap.dims.height || img.naturalHeight;
  coordmap.dims.width = coordmap.dims.width || img.naturalWidth;

  // Add scaling functions to each panel
  coordmap.panels = initPanelScales(coordmap_.panels);

  // This returns the offset of the mouse in CSS pixels relative to the img,
  // but not including the  padding or border, if present.
  coordmap.mouseOffsetCss = function (mouseEvent) {
    const imgOrigin = findOrigin($img);

    // The offset of the mouse from the upper-left corner of the img, in
    // pixels.
    return {
      x: mouseEvent.pageX - imgOrigin.x,
      y: mouseEvent.pageY - imgOrigin.y,
    };
  };

  // Given an offset in an img in CSS pixels, return the corresponding offset
  // in source image pixels. The offsetCss can have properties like "x",
  // "xmin", "y", and "ymax" -- anything that starts with "x" and "y". If the
  // img content is 1000 pixels wide, but is scaled to 400 pixels on screen,
  // and the input is x:400, then this will return x:1000.
  function scaleCssToImg(offsetCss: Bounds): Bounds;
  function scaleCssToImg(offsetCss: Offset): Offset;
  function scaleCssToImg(offsetCss: OffsetCss): OffsetImg;
  function scaleCssToImg(offsetCss: OffsetCss) {
    const pixelScaling = coordmap.imgToCssScalingRatio();

    const result = mapValues(offsetCss, (value, key) => {
      const prefix = key.substring(0, 1);

      if (prefix === "x") {
        return offsetCss[key] / pixelScaling.x;
      } else if (prefix === "y") {
        return offsetCss[key] / pixelScaling.y;
      }
      return null;
    });

    return result;
  }
  coordmap.scaleCssToImg = scaleCssToImg;

  // Given an offset in an img, in source image pixels, return the
  // corresponding offset in CSS pixels. If the img content is 1000 pixels
  // wide, but is scaled to 400 pixels on screen, and the input is x:1000,
  // then this will return x:400.
  function scaleImgToCss(offsetImg: Bounds): Bounds;
  function scaleImgToCss(offsetImg: Offset): Offset;
  function scaleImgToCss(offsetImg: OffsetImg): OffsetCss;
  function scaleImgToCss(offsetImg: { [key: string]: number }): {
    [key: string]: number | null;
  } {
    const pixelScaling = coordmap.imgToCssScalingRatio();

    const result = mapValues(offsetImg, (value, key) => {
      const prefix = key.substring(0, 1);

      if (prefix === "x") {
        return offsetImg[key] * pixelScaling.x;
      } else if (prefix === "y") {
        return offsetImg[key] * pixelScaling.y;
      }
      return null;
    });

    return result;
  }
  coordmap.scaleImgToCss = scaleImgToCss;

  // Returns the x and y ratio the image content is scaled to on screen. If
  // the image data is 1000 pixels wide and is scaled to 300 pixels on screen,
  // then this returns 0.3. (Note the 300 pixels refers to CSS pixels.)
  coordmap.imgToCssScalingRatio = function () {
    const imgDims = findDims($img);

    return {
      x: imgDims.x / coordmap.dims.width,
      y: imgDims.y / coordmap.dims.height,
    };
  };

  coordmap.cssToImgScalingRatio = function () {
    const res = coordmap.imgToCssScalingRatio();

    return {
      x: 1 / res.x,
      y: 1 / res.y,
    };
  };

  // Given an offset in css pixels, return an object representing which panel
  // it's in. The `expand` argument tells it to expand the panel area by that
  // many pixels. It's possible for an offset to be within more than one
  // panel, because of the `expand` value. If that's the case, find the
  // nearest panel.
  coordmap.getPanelCss = function (offsetCss, expand = 0) {
    const offsetImg = coordmap.scaleCssToImg(offsetCss);
    const x = offsetImg.x;
    const y = offsetImg.y;

    // Convert expand from css pixels to img pixels
    const cssToImgRatio = coordmap.cssToImgScalingRatio();
    const expandImg = {
      x: expand * cssToImgRatio.x,
      y: expand * cssToImgRatio.y,
    };

    const matches = []; // Panels that match
    const dists = []; // Distance of offset to each matching panel
    let i;

    for (i = 0; i < coordmap.panels.length; i++) {
      const panelRange = coordmap.panels[i].range;
      const b = {
        top: panelRange.top * cssToImgRatio.y,
        bottom: panelRange.bottom * cssToImgRatio.y,
        left: panelRange.left * cssToImgRatio.x,
        right: panelRange.right * cssToImgRatio.x,
      };

      if (
        x <= b.right + expandImg.x &&
        x >= b.left - expandImg.x &&
        y <= b.bottom + expandImg.y &&
        y >= b.top - expandImg.y
      ) {
        matches.push(coordmap.panels[i]);

        // Find distance from edges for x and y
        let xdist = 0;
        let ydist = 0;

        if (x > b.right && x <= b.right + expandImg.x) {
          xdist = x - b.right;
        } else if (x < b.left && x >= b.left - expandImg.x) {
          xdist = x - b.left;
        }
        if (y > b.bottom && y <= b.bottom + expandImg.y) {
          ydist = y - b.bottom;
        } else if (y < b.top && y >= b.top - expandImg.y) {
          ydist = y - b.top;
        }

        // Cartesian distance
        dists.push(Math.sqrt(Math.pow(xdist, 2) + Math.pow(ydist, 2)));
      }
    }

    if (matches.length) {
      // Find shortest distance
      const minDist = Math.min.apply(null, dists);

      for (i = 0; i < matches.length; i++) {
        if (dists[i] === minDist) {
          return matches[i];
        }
      }
    }

    return null;
  };

  // Is an offset (in css pixels) in a panel? If supplied, `expand` tells us
  // to expand the panels by that many pixels in all directions.
  coordmap.isInPanelCss = function (offsetCss, expand = 0) {
    if (coordmap.getPanelCss(offsetCss, expand)) return true;

    return false;
  };

  // Returns a function that sends mouse coordinates, scaled to data space.
  // If that function is passed a null event, it will send null.
  coordmap.mouseCoordinateSender = function (
    inputId,
    clip = true,
    nullOutside = false
  ) {
    return function (e) {
      if (e === null) {
        shinySetInputValue(inputId, null);
        return;
      }
      const coordsCss = coordmap.mouseOffsetCss(e);
      // If outside of plotting region

      if (!coordmap.isInPanelCss(coordsCss)) {
        if (nullOutside) {
          shinySetInputValue(inputId, null);
          return;
        }
        if (clip) return;

        const coords: Coords = {
          // eslint-disable-next-line @typescript-eslint/naming-convention
          coords_css: coordsCss,
          // eslint-disable-next-line @typescript-eslint/naming-convention
          coords_img: coordmap.scaleCssToImg(coordsCss),
        };

        shinySetInputValue(inputId, coords, { priority: "event" });
        return;
      }
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      const panel = coordmap.getPanelCss(coordsCss)!;

      const coordsImg = coordmap.scaleCssToImg(coordsCss);
      const coordsData = panel.scaleImgToData(coordsImg);

      const coords: Coords = {
        x: coordsData?.x,
        y: coordsData?.y,
        // eslint-disable-next-line @typescript-eslint/naming-convention
        coords_css: coordsCss,
        // eslint-disable-next-line @typescript-eslint/naming-convention
        coords_img: coordsImg,
        // eslint-disable-next-line @typescript-eslint/naming-convention
        img_css_ratio: coordmap.cssToImgScalingRatio(),
      };

      // Add the panel (facet) variables, if present
      $.extend(coords, panel.panel_vars);

      // Add variable name mappings
      coords.mapping = panel.mapping;

      // Add scaling information
      coords.domain = panel.domain;
      coords.range = panel.range;
      coords.log = panel.log;

      shinySetInputValue(inputId, coords, { priority: "event" });
    };
  };

  return coordmap;
}

export { findOrigin, initCoordmap };
export type { Coordmap, CoordmapInit };
