import $ from "jquery";
import {
  createBrushHandler,
  createClickHandler,
  createClickInfo,
  createHoverHandler,
  disableDrag,
  initCoordmap,
} from "../../imageutils";
import type { CoordmapInit } from "../../imageutils/initCoordmap";
import type { ErrorsMessageValue } from "../../shiny/shinyapp";
import {
  getComputedLinkColor,
  getStyle,
  hasOwnProperty,
  strToBool,
} from "../../utils";
import { IEVersion, isIE } from "../../utils/browser";
import { ifUndefined } from "../../utils/object";
import { OutputBinding } from "./outputBinding";

class ImageOutputBinding extends OutputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find(".shiny-image-output, .shiny-plot-output");
  }

  renderValue(
    el: HTMLElement,
    data: {
      coordmap: CoordmapInit;
      error?: string;
    } & { [key: string]: string }
  ): void {
    // The overall strategy:
    // * Clear out existing image and event handlers.
    // * Create new image.
    // * Create various event handlers.
    // * Bind those event handlers to events.
    // * Insert the new image.

    const outputId = this.getId(el);

    const $el = $(el);
    let img: HTMLImageElement;

    // Get existing img element if present.
    let $img = $el.find("img");

    if ($img.length === 0) {
      // If a img element is not already present, that means this is either
      // the first time renderValue() has been called, or this is after an
      // error.
      img = document.createElement("img");
      $el.append(img);
      $img = $(img);
    } else {
      // Trigger custom 'reset' event for any existing images in the div
      img = $img[0];
      $img.trigger("reset");
    }

    if (!data) {
      $el.empty();
      return;
    }

    // If value is undefined, return alternate. Sort of like ||, except it won't
    // return alternate for other falsy values (0, false, null).

    const opts = {
      clickId: $el.data("click-id"),
      clickClip: ifUndefined(strToBool($el.data("click-clip")), true),

      dblclickId: $el.data("dblclick-id"),
      dblclickClip: ifUndefined(strToBool($el.data("dblclick-clip")), true),
      dblclickDelay: ifUndefined($el.data("dblclick-delay"), 400),

      hoverId: $el.data("hover-id"),
      hoverClip: ifUndefined(strToBool($el.data("hover-clip")), true),
      hoverDelayType: ifUndefined($el.data("hover-delay-type"), "debounce"),
      hoverDelay: ifUndefined($el.data("hover-delay"), 300),
      hoverNullOutside: ifUndefined(
        strToBool($el.data("hover-null-outside")),
        false
      ),

      brushId: $el.data("brush-id"),
      brushClip: ifUndefined(strToBool($el.data("brush-clip")), true),
      brushDelayType: ifUndefined($el.data("brush-delay-type"), "debounce"),
      brushDelay: ifUndefined($el.data("brush-delay"), 300),
      brushFill: ifUndefined($el.data("brush-fill"), "#666"),
      brushStroke: ifUndefined($el.data("brush-stroke"), "#000"),
      brushOpacity: ifUndefined($el.data("brush-opacity"), 0.3),
      brushDirection: ifUndefined($el.data("brush-direction"), "xy"),
      brushResetOnNew: ifUndefined(
        strToBool($el.data("brush-reset-on-new")),
        false
      ),

      coordmap: data.coordmap,
    };

    if (opts.brushFill === "auto") {
      opts.brushFill = getComputedLinkColor($el[0]);
    }
    if (opts.brushStroke === "auto") {
      opts.brushStroke = getStyle($el[0], "color");
    }

    // Copy items from data to img. Don't set the coordmap as an attribute.
    $.each(data, function (key: string, value) {
      if (value === null || key === "coordmap") {
        return;
      }
      // this checks only against base64 encoded src values
      // images put here are only from renderImage and renderPlot
      if (key === "src" && value === img.getAttribute("src")) {
        // Ensure the browser actually fires an onLoad event, which doesn't
        // happen on WebKit if the value we set on src is the same as the
        // value it already has
        // https://github.com/rstudio/shiny/issues/2197
        // https://stackoverflow.com/questions/5024111/javascript-image-onload-doesnt-fire-in-webkit-if-loading-same-image
        img.removeAttribute("src");
      }
      img.setAttribute(key, value);
    });

    // Unset any attributes in the current img that were not provided in the
    // new data.
    for (let i = 0; i < img.attributes.length; i++) {
      const attrib = img.attributes[i];
      // Need to check attrib.specified on IE because img.attributes contains
      // all possible attributes on IE.

      if (attrib.specified && !hasOwnProperty(data, attrib.name)) {
        img.removeAttribute(attrib.name);
      }
    }

    if (!opts.coordmap) {
      opts.coordmap = {
        panels: [],
        dims: {
          // These values be set to the naturalWidth and naturalHeight once the image has loaded
          height: null,
          width: null,
        },
      };
    }

    // When the image loads, initialize all the interaction handlers. When the
    // value of src is set, the browser may not load the image immediately,
    // even if it's a data URL. If we try to initialize this stuff
    // immediately, it can cause problems because we use we need the raw image
    // height and width
    $img.off("load.shiny_image_interaction");
    $img.one("load.shiny_image_interaction", function () {
      // Use a local variable so the type check is happy
      const optsCoordmap = (opts.coordmap = initCoordmap($el, opts.coordmap));

      // This object listens for mousedowns, and triggers mousedown2 and dblclick2
      // events as appropriate.
      const clickInfo = createClickInfo(
        $el,
        opts.dblclickId,
        opts.dblclickDelay
      );

      $el.on("mousedown.image_output", clickInfo.mousedown);

      if (isIE() && IEVersion() === 8) {
        $el.on("dblclick.image_output", clickInfo.dblclickIE8);
      }

      // ----------------------------------------------------------
      // Register the various event handlers
      // ----------------------------------------------------------
      // New strategy: Keep the old handlers around when the image reloads,
      // simply updating them with a new coordmap. This is accomplished by
      // storing a reference to their updateCoordmap functions in $el.data,
      // and calling it with the new coordmap when the image reloads.
      // This solves #1642
      if (opts.clickId) {
        disableDrag($el, $img);

        // Check to see if a click handler already exists
        if ($el.data("clickHandler")) {
          // If so, update the old handler with the new coordmap
          $el.data("clickHandler").updateCoordmap(optsCoordmap);
        } else {
          // If not, this is the first load, create a new handler
          const clickHandler = createClickHandler(
            opts.clickId,
            opts.clickClip,
            optsCoordmap
          );

          $el.on("mousedown2.image_output", clickHandler.mousedown);

          // $el.on("resize.image_output", clickHandler.onResize); // currently unused

          // When img is reset, do housekeeping: update $el's mouse listener
          // with the new coordmap and call the handler's onResetImg callback.
          // These should listen on $el rather than $img so they can persist even
          // after an error.
          $el.on("reset.image_output", clickHandler.onResetImg);
          $el.data("clickHandler", clickHandler);
        }
      }

      if (opts.dblclickId) {
        disableDrag($el, $img);

        // Check to see if a double-click handler already exists
        if ($el.data("dblclickHandler")) {
          // If so, update the old handler with the new coordmap
          $el.data("dblclickHandler").updateCoordmap(optsCoordmap);
        } else {
          // We'll use the clickHandler's mousedown function, but register it to
          // our custom 'dblclick2' event.
          const dblclickHandler = createClickHandler(
            opts.dblclickId,
            opts.clickClip,
            optsCoordmap
          );

          $el.on("dblclick2.image_output", dblclickHandler.mousedown);

          // $el.on("resize.image_output", dblclickHandler.onResize); // currently unused
          $el.on("reset.image_output", dblclickHandler.onResetImg);
          $el.data("dblclickHandler", dblclickHandler);
        }
      }

      if (opts.hoverId) {
        disableDrag($el, $img);

        if ($el.data("hoverHandler")) {
          $el.data("hoverHandler").updateCoordmap(optsCoordmap);
        } else {
          const hoverHandler = createHoverHandler(
            opts.hoverId,
            opts.hoverDelay,
            opts.hoverDelayType,
            opts.hoverClip,
            opts.hoverNullOutside,
            optsCoordmap
          );

          $el.on("mousemove.image_output", hoverHandler.mousemove);
          $el.on("mouseout.image_output", hoverHandler.mouseout);

          // $el.on("resize.image_output", hoverHandler.onResize); // currently unused
          $el.on("reset.image_output", hoverHandler.onResetImg);
          $el.data("hoverHandler", hoverHandler);
        }
      }

      if (opts.brushId) {
        disableDrag($el, $img);

        if ($el.data("brushHandler")) {
          $el.data("brushHandler").updateCoordmap(optsCoordmap);
        } else {
          const brushHandler = createBrushHandler(
            opts.brushId,
            $el,
            opts,
            optsCoordmap,
            outputId
          );

          $el.on("mousedown.image_output", brushHandler.mousedown);
          $el.on("mousemove.image_output", brushHandler.mousemove);

          // resize handler is used for cached plots!
          $el.on("resize.image_output", brushHandler.onResize);
          $el.on("reset.image_output", brushHandler.onResetImg);
          $el.data("brushHandler", brushHandler);
        }
      }

      if (opts.clickId || opts.dblclickId || opts.hoverId || opts.brushId) {
        $el.addClass("crosshair");
      }

      if (data.error)
        console.log("Error on server extracting coordmap: " + data.error);
    });
  }

  renderError(el: HTMLElement, err: ErrorsMessageValue): void {
    $(el).data("errorState", true);
    $(el).find("img").trigger("reset");
    OutputBinding.prototype.renderError.call(this, el, err);
  }

  clearError(el: HTMLElement): void {
    // Remove all elements except img and the brush; this is usually just
    // error messages.
    $(el)
      .contents()
      .filter(function () {
        return !(
          this instanceof HTMLElement &&
          (this.tagName === "IMG" || this.id === el.id + "_brush")
        );
      })
      .remove();

    $(el).data("errorState", false);
    // TODO-barret does this work?: `super.clearError(el)`
    OutputBinding.prototype.clearError.call(this, el);
  }

  // For non-cached plots, this is useless, since it will always be followed by
  // a reload. However, cached plots need this code path to update brushes
  // visually when a small, reload-less resize occurs.
  resize(
    el: HTMLElement,
    width: number | string,
    height: number | string
  ): void {
    const img = $(el).find("img");

    // Only trigger resize handlers if this is a cached plot--resizes of
    // non-cached plots are always immediately followed by reloads
    if (img.hasClass("shiny-scalable")) {
      img.trigger("resize");
    }
    return;
    width;
    height;
  }
}

const imageOutputBinding = new ImageOutputBinding();

export { imageOutputBinding, ImageOutputBinding };
