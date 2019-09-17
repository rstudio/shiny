var imageOutputBinding = new OutputBinding();
$.extend(imageOutputBinding, {
  find: function(scope) {
    return $(scope).find('.shiny-image-output, .shiny-plot-output');
  },
  renderValue: function(el, data) {
    // The overall strategy:
    // * Clear out existing image and event handlers.
    // * Create new image.
    // * Create various event handlers.
    // * Bind those event handlers to events.
    // * Insert the new image.

    var outputId = this.getId(el);

    var $el = $(el);
    var img;

    // Get existing img element if present.
    var $img = $el.find('img');

    if ($img.length === 0) {
      // If a img element is not already present, that means this is either
      // the first time renderValue() has been called, or this is after an
      // error.
      img = document.createElement('img');
      $el.append(img);
      $img = $(img);
    } else {
      // Trigger custom 'reset' event for any existing images in the div
      img = $img[0];
      $img.trigger('reset');
    }

    if (!data) {
      $el.empty();
      return;
    }

    // If value is undefined, return alternate. Sort of like ||, except it won't
    // return alternate for other falsy values (0, false, null).
    function OR(value, alternate) {
      if (value === undefined) return alternate;
      return value;
    }

    var opts = {
      clickId: $el.data('click-id'),
      clickClip: OR(strToBool($el.data('click-clip')), true),

      dblclickId: $el.data('dblclick-id'),
      dblclickClip: OR(strToBool($el.data('dblclick-clip')), true),
      dblclickDelay: OR($el.data('dblclick-delay'), 400),

      hoverId: $el.data('hover-id'),
      hoverClip: OR(strToBool($el.data('hover-clip')), true),
      hoverDelayType: OR($el.data('hover-delay-type'), 'debounce'),
      hoverDelay: OR($el.data('hover-delay'), 300),
      hoverNullOutside: OR(strToBool($el.data('hover-null-outside')), false),

      brushId: $el.data('brush-id'),
      brushClip: OR(strToBool($el.data('brush-clip')), true),
      brushDelayType: OR($el.data('brush-delay-type'), 'debounce'),
      brushDelay: OR($el.data('brush-delay'), 300),
      brushFill: OR($el.data('brush-fill'), '#666'),
      brushStroke: OR($el.data('brush-stroke'), '#000'),
      brushOpacity: OR($el.data('brush-opacity'), 0.3),
      brushDirection: OR($el.data('brush-direction'), 'xy'),
      brushResetOnNew: OR(strToBool($el.data('brush-reset-on-new')), false),

      coordmap: data.coordmap
    };

    // Copy items from data to img. Don't set the coordmap as an attribute.
    $.each(data, function(key, value) {
      if (value === null || key === 'coordmap') {
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
    for (var i=0; i<img.attributes.length; i++) {
      var attrib = img.attributes[i];
      // Need to check attrib.specified on IE because img.attributes contains
      // all possible attributes on IE.
      if (attrib.specified && !data.hasOwnProperty(attrib.name)) {
        img.removeAttribute(attrib.name);
      }
    }

    if (!opts.coordmap) {
      opts.coordmap = {
        panels: [],
        dims: {
          // These values be set to the naturalWidth and naturalHeight once the image has loaded
          height: null,
          width: null
        }
      };
    }

    // Remove event handlers that were added in previous runs of this function.
    $el.off('.image_output');
    $img.off('.image_output');

    // When the image loads, initialize all the interaction handlers. When the
    // value of src is set, the browser may not load the image immediately,
    // even if it's a data URL. If we try to initialize this stuff
    // immediately, it can cause problems because we use we need the raw image
    // height and width
    $img.off("load.shiny_image_interaction");
    $img.one("load.shiny_image_interaction", function() {

      imageutils.initCoordmap($el, opts.coordmap);

      // This object listens for mousedowns, and triggers mousedown2 and dblclick2
      // events as appropriate.
      var clickInfo = imageutils.createClickInfo($el, opts.dblclickId, opts.dblclickDelay);

      $el.on('mousedown.image_output', clickInfo.mousedown);

      if (browser.isIE && browser.IEVersion === 8) {
        $el.on('dblclick.image_output', clickInfo.dblclickIE8);
      }

      // ----------------------------------------------------------
      // Register the various event handlers
      // ----------------------------------------------------------
      if (opts.clickId) {
        imageutils.disableDrag($el, $img);

        var clickHandler = imageutils.createClickHandler(opts.clickId,
          opts.clickClip, opts.coordmap);
        $el.on('mousedown2.image_output', clickHandler.mousedown);

        $el.on('resize.image_output', clickHandler.onResize);

        // When img is reset, do housekeeping: clear $el's mouse listener and
        // call the handler's onResetImg callback.
        $img.on('reset.image_output', clickHandler.onResetImg);
      }

      if (opts.dblclickId) {
        imageutils.disableDrag($el, $img);

        // We'll use the clickHandler's mousedown function, but register it to
        // our custom 'dblclick2' event.
        var dblclickHandler = imageutils.createClickHandler(opts.dblclickId,
          opts.clickClip, opts.coordmap);
        $el.on('dblclick2.image_output', dblclickHandler.mousedown);

        $el.on('resize.image_output', dblclickHandler.onResize);
        $img.on('reset.image_output', dblclickHandler.onResetImg);
      }

      if (opts.hoverId) {
        imageutils.disableDrag($el, $img);

        var hoverHandler = imageutils.createHoverHandler(opts.hoverId,
          opts.hoverDelay, opts.hoverDelayType, opts.hoverClip,
          opts.hoverNullOutside, opts.coordmap);
        $el.on('mousemove.image_output', hoverHandler.mousemove);
        $el.on('mouseout.image_output', hoverHandler.mouseout);

        $el.on('resize.image_output', hoverHandler.onResize);
        $img.on('reset.image_output', hoverHandler.onResetImg);
      }

      if (opts.brushId) {
        imageutils.disableDrag($el, $img);

        var brushHandler = imageutils.createBrushHandler(opts.brushId, $el, opts,
          opts.coordmap, outputId);
        $el.on('mousedown.image_output', brushHandler.mousedown);
        $el.on('mousemove.image_output', brushHandler.mousemove);

        $el.on('resize.image_output', brushHandler.onResize);
        $img.on('reset.image_output', brushHandler.onResetImg);
      }

      if (opts.clickId || opts.dblclickId || opts.hoverId || opts.brushId) {
        $el.addClass('crosshair');
      }

      if (data.error)
        console.log('Error on server extracting coordmap: ' + data.error);

    });
  },

  renderError: function(el, err) {
    $(el).find('img').trigger('reset');
    OutputBinding.prototype.renderError.call(this, el, err);
  },

  clearError: function(el) {
    // Remove all elements except img and the brush; this is usually just
    // error messages.
    $(el).contents().filter(function() {
      return this.tagName !== "IMG" &&
             this.id !== el.id + '_brush';
    }).remove();

    OutputBinding.prototype.clearError.call(this, el);
  },

  resize: function(el, width, height) {
    $(el).find("img").trigger("resize");
  }
});
outputBindings.register(imageOutputBinding, 'shiny.imageOutput');


var imageutils = {};

imageutils.disableDrag = function($el, $img) {
  // Make image non-draggable (Chrome, Safari)
  $img.css('-webkit-user-drag', 'none');

  // Firefox, IE<=10
  // First remove existing handler so we don't keep adding handlers.
  $img.off('dragstart.image_output');
  $img.on('dragstart.image_output', function() { return false; });

  // Disable selection of image and text when dragging in IE<=10
  $el.off('selectstart.image_output');
  $el.on('selectstart.image_output', function() { return false; });
};

// Modifies the panel objects in a coordmap, adding scaleImgToData(),
// scaleDataToImg(), and clipImg() functions to each one. The panel objects
// use img and data coordinates only; they do not use css coordinates. The
// domain is in data coordinates; the range is in img coordinates.
imageutils.initPanelScales = function(panels) {
  // Map a value x from a domain to a range. If clip is true, clip it to the
  // range.
  function mapLinear(x, domainMin, domainMax, rangeMin, rangeMax, clip) {
    // By default, clip to range
    clip = clip || true;

    var factor = (rangeMax - rangeMin) / (domainMax - domainMin);
    var val = x - domainMin;
    var newval = (val * factor) + rangeMin;

    if (clip) {
      var max = Math.max(rangeMax, rangeMin);
      var min = Math.min(rangeMax, rangeMin);
      if (newval > max)
        newval = max;
      else if (newval < min)
        newval = min;
    }
    return newval;
  }

  // Create scale and inverse-scale functions for a single direction (x or y).
  function scaler1D(domainMin, domainMax, rangeMin, rangeMax, logbase) {
    return {
      scale: function(val, clip) {
        if (logbase)
          val = Math.log(val) / Math.log(logbase);
        return mapLinear(val, domainMin, domainMax, rangeMin, rangeMax, clip);
      },

      scaleInv: function(val, clip) {
        var res = mapLinear(val, rangeMin, rangeMax, domainMin, domainMax, clip);
        if (logbase)
          res = Math.pow(logbase, res);
        return res;
      }
    };
  }

  // Modify panel, adding scale and inverse-scale functions that take objects
  // like {x:1, y:3}, and also add clip function.
  function addScaleFuns(panel) {
    var d = panel.domain;
    var r = panel.range;
    var xlog = (panel.log && panel.log.x) ? panel.log.x : null;
    var ylog = (panel.log && panel.log.y) ? panel.log.y : null;
    var xscaler = scaler1D(d.left, d.right, r.left, r.right, xlog);
    var yscaler = scaler1D(d.bottom, d.top, r.bottom, r.top, ylog);

    // Given an object of form {x:1, y:2}, or {x:1, xmin:2:, ymax: 3}, convert
    // from data coordinates to img. Whether a value is converted as x or y
    // depends on the first character of the key.
    panel.scaleDataToImg = function(val, clip) {
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

    panel.scaleImgToData = function(val, clip) {
      return mapValues(val, (value, key) => {
        const prefix = key.substring(0, 1);
        if (prefix === "x") {
          return xscaler.scaleInv(value, clip);
        } else if (prefix === "y") {
          return yscaler.scaleInv(value, clip);
        }
        return null;
      });
    };

    // Given a scaled offset (in img pixels), clip it to the nearest panel region.
    panel.clipImg = function(offset_img) {
      var newOffset = {
        x: offset_img.x,
        y: offset_img.y
      };

      var bounds = panel.range;

      if      (offset_img.x > bounds.right)  newOffset.x = bounds.right;
      else if (offset_img.x < bounds.left)   newOffset.x = bounds.left;

      if      (offset_img.y > bounds.bottom) newOffset.y = bounds.bottom;
      else if (offset_img.y < bounds.top)    newOffset.y = bounds.top;

      return newOffset;
    };
  }

  // Add the functions to each panel object.
  for (var i=0; i<panels.length; i++) {
    var panel = panels[i];
    addScaleFuns(panel);
  }
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
imageutils.initCoordmap = function($el, coordmap) {
  const $img = $el.find("img");
  const img = $img[0];

  // If we didn't get any panels, create a dummy one where the domain and range
  // are simply the pixel dimensions.
  // that we modify.
  if (coordmap.panels.length === 0) {
    let bounds = {
      top: 0,
      left: 0,
      right: img.clientWidth - 1,
      bottom: img.clientHeight - 1
    };

    coordmap.panels[0] = {
      domain: bounds,
      range: bounds,
      mapping: {}
    };
  }

  // If no dim height and width values are found, set them to the raw image height and width
  // These values should be the same...
  // This is only done to initialize an image output, whose height and width are unknown until the image is retrieved
  coordmap.dims.height = coordmap.dims.height || img.naturalHeight;
  coordmap.dims.width = coordmap.dims.width || img.naturalWidth;

  // Add scaling functions to each panel
  imageutils.initPanelScales(coordmap.panels);


  // This returns the offset of the mouse in CSS pixels relative to the img,
  // but not including the  padding or border, if present.
  coordmap.mouseOffsetCss = function(mouseEvent) {
    const img_origin = findOrigin($img);

    // The offset of the mouse from the upper-left corner of the img, in
    // pixels.
    return {
      x: mouseEvent.pageX - img_origin.x,
      y: mouseEvent.pageY - img_origin.y
    };
  };

  // Given an offset in an img in CSS pixels, return the corresponding offset
  // in source image pixels. The offset_css can have properties like "x",
  // "xmin", "y", and "ymax" -- anything that starts with "x" and "y". If the
  // img content is 1000 pixels wide, but is scaled to 400 pixels on screen,
  // and the input is x:400, then this will return x:1000.
  coordmap.scaleCssToImg = function(offset_css) {
    const pixel_scaling = coordmap.imgToCssScalingRatio();

    const result = mapValues(offset_css, (value, key) => {
      const prefix = key.substring(0, 1);

      if (prefix === "x") {
        return offset_css[key] / pixel_scaling.x;
      } else if (prefix === "y") {
        return offset_css[key] / pixel_scaling.y;
      }
      return null;
    });

    return result;
  };

  // Given an offset in an img, in source image pixels, return the
  // corresponding offset in CSS pixels. If the img content is 1000 pixels
  // wide, but is scaled to 400 pixels on screen, and the input is x:1000,
  // then this will return x:400.
  coordmap.scaleImgToCss = function(offset_img) {
    const pixel_scaling = coordmap.imgToCssScalingRatio();

    const result = mapValues(offset_img, (value, key) => {
      const prefix = key.substring(0, 1);

      if (prefix === "x") {
        return offset_img[key] * pixel_scaling.x;
      } else if (prefix === "y") {
        return offset_img[key] * pixel_scaling.y;
      }
      return null;
    });

    return result;
  };

  // Returns the x and y ratio the image content is scaled to on screen. If
  // the image data is 1000 pixels wide and is scaled to 300 pixels on screen,
  // then this returns 0.3. (Note the 300 pixels refers to CSS pixels.)
  coordmap.imgToCssScalingRatio = function() {
    const img_dims = findDims($img);
    return {
      x: img_dims.x / coordmap.dims.width,
      y: img_dims.y / coordmap.dims.height
    };
  };

  coordmap.cssToImgScalingRatio = function() {
    const res = coordmap.imgToCssScalingRatio();
    return {
      x: 1 / res.x,
      y: 1 / res.y
    };
  };

  // Given an offset in css pixels, return an object representing which panel
  // it's in. The `expand` argument tells it to expand the panel area by that
  // many pixels. It's possible for an offset to be within more than one
  // panel, because of the `expand` value. If that's the case, find the
  // nearest panel.
  coordmap.getPanelCss = function(offset_css, expand = 0) {
    const offset_img = coordmap.scaleCssToImg(offset_css);
    const x = offset_img.x;
    const y = offset_img.y;

    // Convert expand from css pixels to img pixels
    const cssToImgRatio = coordmap.cssToImgScalingRatio();
    const expand_img = {
      x: expand * cssToImgRatio.x,
      y: expand * cssToImgRatio.y
    };

    const matches = []; // Panels that match
    const dists = [];   // Distance of offset to each matching panel
    let b;
    for (var i=0; i<coordmap.panels.length; i++) {
      b = coordmap.panels[i].range;

      if (x <= b.right  + expand_img.x &&
          x >= b.left   - expand_img.x &&
          y <= b.bottom + expand_img.y &&
          y >= b.top    - expand_img.y)
      {
        matches.push(coordmap.panels[i]);

        // Find distance from edges for x and y
        var xdist = 0;
        var ydist = 0;
        if (x > b.right && x <= b.right + expand_img.x) {
          xdist = x - b.right;
        } else if (x < b.left && x >= b.left - expand_img.x) {
          xdist = x - b.left;
        }
        if (y > b.bottom && y <= b.bottom + expand_img.y) {
          ydist = y - b.bottom;
        } else if (y < b.top && y >= b.top - expand_img.y) {
          ydist = y - b.top;
        }

        // Cartesian distance
        dists.push(Math.sqrt( Math.pow(xdist, 2) + Math.pow(ydist, 2) ));
      }
    }

    if (matches.length) {
      // Find shortest distance
      var min_dist = Math.min.apply(null, dists);
      for (i=0; i<matches.length; i++) {
        if (dists[i] === min_dist) {
          return matches[i];
        }
      }
    }

    return null;
  };

  // Is an offset (in css pixels) in a panel? If supplied, `expand` tells us
  // to expand the panels by that many pixels in all directions.
  coordmap.isInPanelCss = function(offset_css, expand = 0) {
    if (coordmap.getPanelCss(offset_css, expand))
      return true;

    return false;
  };

  // Returns a function that sends mouse coordinates, scaled to data space.
  // If that function is passed a null event, it will send null.
  coordmap.mouseCoordinateSender = function(inputId, clip, nullOutside) {
    if (clip === undefined) clip = true;
    if (nullOutside === undefined) nullOutside = false;

    return function(e) {
      if (e === null) {
        exports.setInputValue(inputId, null);
        return;
      }
      const coords = {};
      const coords_css = coordmap.mouseOffsetCss(e);
      // If outside of plotting region
      if (!coordmap.isInPanelCss(coords_css)) {
        if (nullOutside) {
          exports.setInputValue(inputId, null);
          return;
        }
        if (clip)
          return;

        coords.coords_css = coords_css;
        coords.coords_img = coordmap.scaleCssToImg(coords_css);

        exports.setInputValue(inputId, coords, {priority: "event"});
        return;
      }
      const panel = coordmap.getPanelCss(coords_css);

      const coords_img = coordmap.scaleCssToImg(coords_css);
      const coords_data = panel.scaleImgToData(coords_img);
      coords.x = coords_data.x;
      coords.y = coords_data.y;
      coords.coords_css = coords_css;
      coords.coords_img = coords_img;

      coords.img_css_ratio = coordmap.cssToImgScalingRatio();

      // Add the panel (facet) variables, if present
      $.extend(coords, panel.panel_vars);

      // Add variable name mappings
      coords.mapping = panel.mapping;

      // Add scaling information
      coords.domain = panel.domain;
      coords.range  = panel.range;
      coords.log    = panel.log;

      exports.setInputValue(inputId, coords, {priority: "event"});
    };
  };
};


// Given two sets of x/y coordinates, return an object representing the min
// and max x and y values. (This could be generalized to any number of
// points).
imageutils.findBox = function(offset1, offset2) {
  return {
    xmin: Math.min(offset1.x, offset2.x),
    xmax: Math.max(offset1.x, offset2.x),
    ymin: Math.min(offset1.y, offset2.y),
    ymax: Math.max(offset1.y, offset2.y)
  };
};

// Shift an array of values so that they are within a min and max. The vals
// will be shifted so that they maintain the same spacing internally. If the
// range in vals is larger than the range of min and max, the result might not
// make sense.
imageutils.shiftToRange = function(vals, min, max) {
  if (!(vals instanceof Array))
    vals = [vals];

  var maxval = Math.max.apply(null, vals);
  var minval = Math.min.apply(null, vals);
  var shiftAmount = 0;
  if (maxval > max) {
    shiftAmount = max - maxval;
  } else if (minval < min) {
    shiftAmount = min - minval;
  }

  var newvals = [];
  for (var i=0; i<vals.length; i++) {
    newvals[i] = vals[i] + shiftAmount;
  }
  return newvals;
};



// This object provides two public event listeners: mousedown, and
// dblclickIE8.
// We need to make sure that, when the image is listening for double-
// clicks, that a double-click doesn't trigger two click events. We'll
// trigger custom mousedown2 and dblclick2 events with this mousedown
// listener.
imageutils.createClickInfo = function($el, dblclickId, dblclickDelay) {
  var clickTimer = null;
  var pending_e = null;    // A pending mousedown2 event

  // Create a new event of type eventType (like 'mousedown2'), and trigger
  // it with the information stored in this.e.
  function triggerEvent(newEventType, e) {
    // Extract important info from e and construct a new event with type
    // eventType.
    var e2 = $.Event(newEventType, {
      which:   e.which,
      pageX:   e.pageX,
      pageY:   e.pageY
    });

    $el.trigger(e2);
  }

  function triggerPendingMousedown2() {
    // It's possible that between the scheduling of a mousedown2 and the
    // time this callback is executed, someone else triggers a
    // mousedown2, so check for that.
    if (pending_e) {
      triggerEvent('mousedown2', pending_e);
      pending_e = null;
    }
  }

  // Set a timer to trigger a mousedown2 event, using information from the
  // last recorded mousdown event.
  function scheduleMousedown2(e) {
    pending_e = e;

    clickTimer = setTimeout(function() {
      triggerPendingMousedown2();
    }, dblclickDelay);
  }

  function mousedown(e) {
    // Listen for left mouse button only
    if (e.which !== 1) return;

    // If no dblclick listener, immediately trigger a mousedown2 event.
    if (!dblclickId) {
      triggerEvent('mousedown2', e);
      return;
    }

    // If there's a dblclick listener, make sure not to count this as a
    // click on the first mousedown; we need to wait for the dblclick
    // delay before we can be sure this click was a single-click.
    if (pending_e === null) {
      scheduleMousedown2(e);

    } else {
      clearTimeout(clickTimer);

      // If second click is too far away, it doesn't count as a double
      // click. Instead, immediately trigger a mousedown2 for the previous
      // click, and set this click as a new first click.
      if (pending_e &&
          Math.abs(pending_e.pageX - e.pageX) > 2 ||
          Math.abs(pending_e.pageY - e.pageY) > 2) {

        triggerPendingMousedown2();
        scheduleMousedown2(e);

      } else {
        // The second click was close to the first one. If it happened
        // within specified delay, trigger our custom 'dblclick2' event.
        pending_e = null;
        triggerEvent('dblclick2', e);
      }
    }
  }

  // IE8 needs a special hack because when you do a double-click it doesn't
  // trigger the click event twice - it directly triggers dblclick.
  function dblclickIE8(e) {
    e.which = 1;   // In IE8, e.which is 0 instead of 1. ???
    triggerEvent('dblclick2', e);
  }

  return {
    mousedown: mousedown,
    dblclickIE8: dblclickIE8
  };
};


// ----------------------------------------------------------
// Handler creators for click, hover, brush.
// Each of these returns an object with a few public members. These public
// members are callbacks that are meant to be bound to events on $el with
// the same name (like 'mousedown').
// ----------------------------------------------------------

imageutils.createClickHandler = function(inputId, clip, coordmap) {
  var clickInfoSender = coordmap.mouseCoordinateSender(inputId, clip);

  return {
    mousedown: function(e) {
      // Listen for left mouse button only
      if (e.which !== 1) return;
      clickInfoSender(e);
    },
    onResetImg: function() { clickInfoSender(null); },
    onResize: null
  };
};


imageutils.createHoverHandler = function(inputId, delay, delayType, clip,
  nullOutside, coordmap)
{
  var sendHoverInfo = coordmap.mouseCoordinateSender(inputId, clip, nullOutside);

  var hoverInfoSender;
  if (delayType === 'throttle')
    hoverInfoSender = new Throttler(null, sendHoverInfo, delay);
  else
    hoverInfoSender = new Debouncer(null, sendHoverInfo, delay);

  // What to do when mouse exits the image
  var mouseout;
  if (nullOutside)
    mouseout = function() { hoverInfoSender.normalCall(null); };
  else
    mouseout = function() {};

  return {
    mousemove:   function(e) { hoverInfoSender.normalCall(e); },
    mouseout: mouseout,
    onResetImg: function()  { hoverInfoSender.immediateCall(null); },
    onResize: null
  };
};


// Returns a brush handler object. This has three public functions:
// mousedown, mousemove, and onResetImg.
imageutils.createBrushHandler = function(inputId, $el, opts, coordmap, outputId) {
  // Parameter: expand the area in which a brush can be started, by this
  // many pixels in all directions. (This should probably be a brush option)
  var expandPixels = 20;

  // Represents the state of the brush
  var brush = imageutils.createBrush($el, opts, coordmap, expandPixels);

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
  $el.on("shiny-internal:brushed.image_output", function(e, coords) {
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
  function setCursorStyle(style) {
    $el.removeClass('crosshair grabbable grabbing ns-resize ew-resize nesw-resize nwse-resize');

    if (style) $el.addClass(style);
  }

  function sendBrushInfo() {
    var coords = brush.boundsData();

    // We're in a new or reset state
    if (isNaN(coords.xmin)) {
      exports.setInputValue(inputId, null);
      // Must tell other brushes to clear.
      imageOutputBinding.find(document).trigger("shiny-internal:brushed", {
        brushId: inputId, outputId: null
      });
      return;
    }

    var panel = brush.getPanel();

    // Add the panel (facet) variables, if present
    $.extend(coords, panel.panel_vars);

    coords.coords_css = brush.boundsCss();
    coords.coords_img = coordmap.scaleCssToImg(coords.coords_css);

    coords.img_css_ratio = coordmap.cssToImgScalingRatio();

    // Add variable name mappings
    coords.mapping = panel.mapping;

    // Add scaling information
    coords.domain = panel.domain;
    coords.range  = panel.range;
    coords.log    = panel.log;

    coords.direction = opts.brushDirection;

    coords.brushId = inputId;
    coords.outputId = outputId;

    // Send data to server
    exports.setInputValue(inputId, coords);

    $el.data("mostRecentBrush", true);
    imageOutputBinding.find(document).trigger("shiny-internal:brushed", coords);
  }

  var brushInfoSender;
  if (opts.brushDelayType === 'throttle') {
    brushInfoSender = new Throttler(null, sendBrushInfo, opts.brushDelay);
  } else {
    brushInfoSender = new Debouncer(null, sendBrushInfo, opts.brushDelay);
  }

  function mousedown(e) {
    // This can happen when mousedown inside the graphic, then mouseup
    // outside, then mousedown inside. Just ignore the second
    // mousedown.
    if (brush.isBrushing() || brush.isDragging() || brush.isResizing()) return;

    // Listen for left mouse button only
    if (e.which !== 1) return;

    // In general, brush uses css pixels, and coordmap uses img pixels.
    const offset_css = coordmap.mouseOffsetCss(e);

    // Ignore mousedown events outside of plotting region, expanded by
    // a number of pixels specified in expandPixels.
    if (opts.brushClip && !coordmap.isInPanelCss(offset_css, expandPixels))
      return;

    brush.up({ x: NaN, y: NaN });
    brush.down(offset_css);


    if (brush.isInResizeArea(offset_css)) {
      brush.startResizing(offset_css);

      // Attach the move and up handlers to the window so that they respond
      // even when the mouse is moved outside of the image.
      $(document)
        .on('mousemove.image_brush', mousemoveResizing)
        .on('mouseup.image_brush', mouseupResizing);

    } else if (brush.isInsideBrush(offset_css)) {
      brush.startDragging(offset_css);
      setCursorStyle('grabbing');

      // Attach the move and up handlers to the window so that they respond
      // even when the mouse is moved outside of the image.
      $(document)
        .on('mousemove.image_brush', mousemoveDragging)
        .on('mouseup.image_brush', mouseupDragging);

    } else {
      const panel = coordmap.getPanelCss(offset_css, expandPixels);
      brush.startBrushing(panel.clipImg(coordmap.scaleCssToImg(offset_css)));

      // Attach the move and up handlers to the window so that they respond
      // even when the mouse is moved outside of the image.
      $(document)
        .on('mousemove.image_brush', mousemoveBrushing)
        .on('mouseup.image_brush', mouseupBrushing);
    }
  }

  // This sets the cursor style when it's in the el
  function mousemove(e) {
    // In general, brush uses css pixels, and coordmap uses img pixels.
    const offset_css = coordmap.mouseOffsetCss(e);

    if (!(brush.isBrushing() || brush.isDragging() || brush.isResizing())) {
      // Set the cursor depending on where it is
      if (brush.isInResizeArea(offset_css)) {
        const r = brush.whichResizeSides(offset_css);

        if ((r.left && r.top) || (r.right && r.bottom)) {
          setCursorStyle('nwse-resize');
        } else if ((r.left && r.bottom) || (r.right && r.top)) {
          setCursorStyle('nesw-resize');
        } else if (r.left || r.right) {
          setCursorStyle('ew-resize');
        } else if (r.top || r.bottom) {
          setCursorStyle('ns-resize');
        }
      } else if (brush.isInsideBrush(offset_css)) {
        setCursorStyle('grabbable');
      } else if (coordmap.isInPanelCss(offset_css, expandPixels)) {
        setCursorStyle('crosshair');
      } else {
        setCursorStyle(null);
      }
    }
  }

  // mousemove handlers while brushing or dragging
  function mousemoveBrushing(e) {
    brush.brushTo(coordmap.mouseOffsetCss(e));
    brushInfoSender.normalCall();
  }

  function mousemoveDragging(e) {
    brush.dragTo(coordmap.mouseOffsetCss(e));
    brushInfoSender.normalCall();
  }

  function mousemoveResizing(e) {
    brush.resizeTo(coordmap.mouseOffsetCss(e));
    brushInfoSender.normalCall();
  }

  // mouseup handlers while brushing or dragging
  function mouseupBrushing(e) {
    // Listen for left mouse button only
    if (e.which !== 1) return;

    $(document)
      .off('mousemove.image_brush')
      .off('mouseup.image_brush');

    brush.up(coordmap.mouseOffsetCss(e));

    brush.stopBrushing();
    setCursorStyle('crosshair');

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
    if (brushInfoSender.isPending())
      brushInfoSender.immediateCall();
  }

  function mouseupDragging(e) {
    // Listen for left mouse button only
    if (e.which !== 1) return;

    $(document)
      .off('mousemove.image_brush')
      .off('mouseup.image_brush');

    brush.up(coordmap.mouseOffsetCss(e));

    brush.stopDragging();
    setCursorStyle('grabbable');

    if (brushInfoSender.isPending())
      brushInfoSender.immediateCall();
  }

  function mouseupResizing(e) {
    // Listen for left mouse button only
    if (e.which !== 1) return;

    $(document)
      .off('mousemove.image_brush')
      .off('mouseup.image_brush');

    brush.up(coordmap.mouseOffsetCss(e));
    brush.stopResizing();

    if (brushInfoSender.isPending())
      brushInfoSender.immediateCall();

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
    mousedown:  mousedown,
    mousemove:  mousemove,
    onResetImg: onResetImg,
    onResize:   onResize
  };
};

// Returns an object that represents the state of the brush. This gets wrapped
// in a brushHandler, which provides various event listeners.
imageutils.createBrush = function($el, opts, coordmap, expandPixels) {
  // Number of pixels outside of brush to allow start resizing
  var resizeExpand = 10;

  var el = $el[0];
  var $div = null;  // The div representing the brush

  var state = {};

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
    state.up   = { x: NaN, y: NaN };

    // Which side(s) we're currently resizing
    state.resizeSides = {
      left: false,
      right: false,
      top: false,
      bottom: false
    };

    // Bounding rectangle of the brush, in CSS pixel and data dimensions. We
    // need to record data dimensions along with pixel dimensions so that when
    // a new plot is sent, we can re-draw the brush div with the appropriate
    // coords.
    state.boundsCss = {
      xmin: NaN,
      xmax: NaN,
      ymin: NaN,
      ymax: NaN
    };
    state.boundsData = {
      xmin: NaN,
      xmax: NaN,
      ymin: NaN,
      ymax: NaN
    };

    // Panel object that the brush is in
    state.panel = null;

    // The bounds at the start of a drag/resize (in CSS pixels)
    state.changeStartBounds = {
      xmin: NaN,
      xmax: NaN,
      ymin: NaN,
      ymax: NaN
    };

    if ($div)
      $div.remove();
  }

  // If there's an existing brush div, use that div to set the new brush's
  // settings, provided that the x, y, and panel variables have the same names,
  // and there's a panel with matching panel variable values.
  function importOldBrush() {
    var oldDiv = $el.find('#' + el.id + '_brush');
    if (oldDiv.length === 0)
      return;

    var oldBoundsData = oldDiv.data('bounds-data');
    var oldPanel = oldDiv.data('panel');

    if (!oldBoundsData || !oldPanel)
      return;

    // Find a panel that has matching vars; if none found, we can't restore.
    // The oldPanel and new panel must match on their mapping vars, and the
    // values.
    for (var i=0; i<coordmap.panels.length; i++){
      var curPanel = coordmap.panels[i];

      if (equal(oldPanel.mapping, curPanel.mapping) &&
          equal(oldPanel.panel_vars, curPanel.panel_vars)) {
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
    const bounds_data = boundsData();
    // Check to see if we have valid boundsData
    for (let val in bounds_data) {
      if (isnan(bounds_data[val])) return;
    }

    boundsData(bounds_data);
    updateDiv();
  }

  // Return true if the offset is inside min/max coords
  function isInsideBrush(offset_css) {
    var bounds = state.boundsCss;
    return offset_css.x <= bounds.xmax && offset_css.x >= bounds.xmin &&
           offset_css.y <= bounds.ymax && offset_css.y >= bounds.ymin;
  }

  // Return true if offset is inside a region to start a resize
  function isInResizeArea(offset_css) {
    var sides = whichResizeSides(offset_css);
    return sides.left || sides.right || sides.top || sides.bottom;
  }

  // Return an object representing which resize region(s) the cursor is in.
  function whichResizeSides(offset_css) {
    const b = state.boundsCss;
    // Bounds with expansion
    const e = {
      xmin: b.xmin - resizeExpand,
      xmax: b.xmax + resizeExpand,
      ymin: b.ymin - resizeExpand,
      ymax: b.ymax + resizeExpand
    };
    const res = {
      left:   false,
      right:  false,
      top:    false,
      bottom: false
    };

    if ((opts.brushDirection === 'xy' || opts.brushDirection === 'x') &&
        (offset_css.y <= e.ymax && offset_css.y >= e.ymin))
    {
      if (offset_css.x < b.xmin && offset_css.x >= e.xmin)
        res.left = true;
      else if (offset_css.x > b.xmax && offset_css.x <= e.xmax)
        res.right = true;
    }

    if ((opts.brushDirection === 'xy' || opts.brushDirection === 'y') &&
        (offset_css.x <= e.xmax && offset_css.x >= e.xmin))
    {
      if (offset_css.y < b.ymin && offset_css.y >= e.ymin)
        res.top = true;
      else if (offset_css.y > b.ymax && offset_css.y <= e.ymax)
        res.bottom = true;
    }

    return res;
  }


  // Sets the bounds of the brush (in CSS pixels), given a box and optional
  // panel. This will fit the box bounds into the panel, so we don't brush
  // outside of it. This knows whether we're brushing in the x, y, or xy
  // directions, and sets bounds accordingly. If no box is passed in, just
  // return current bounds.
  function boundsCss(box_css) {
    if (box_css === undefined) {
      return $.extend({}, state.boundsCss);
    }

    let min_css = { x: box_css.xmin, y: box_css.ymin };
    let max_css = { x: box_css.xmax, y: box_css.ymax };

    const panel = state.panel;
    const panelBounds_img = panel.range;

    if (opts.brushClip) {
      min_css = imgToCss(panel.clipImg(cssToImg(min_css)));
      max_css = imgToCss(panel.clipImg(cssToImg(max_css)));
    }

    if (opts.brushDirection === 'xy') {
      // No change

    } else if (opts.brushDirection === 'x') {
      // Extend top and bottom of plotting area
      min_css.y = imgToCss({y: panelBounds_img.top   }).y;
      max_css.y = imgToCss({y: panelBounds_img.bottom}).y;

    } else if (opts.brushDirection === 'y') {
      min_css.x = imgToCss({x: panelBounds_img.left }).x;
      max_css.x = imgToCss({x: panelBounds_img.right}).x;
    }

    state.boundsCss = {
      xmin: min_css.x,
      xmax: max_css.x,
      ymin: min_css.y,
      ymax: max_css.y
    };

    // Positions in data space
    const min_data = state.panel.scaleImgToData(cssToImg(min_css));
    const max_data = state.panel.scaleImgToData(cssToImg(max_css));
    // For reversed scales, the min and max can be reversed, so use findBox
    // to ensure correct order.
    state.boundsData = imageutils.findBox(min_data, max_data);
    // Round to 14 significant digits to avoid spurious changes in FP values
    // (#1634).
    state.boundsData = mapValues(state.boundsData, val => roundSignif(val, 14));

    // We also need to attach the data bounds and panel as data attributes, so
    // that if the image is re-sent, we can grab the data bounds to create a new
    // brush. This should be fast because it doesn't actually modify the DOM.
    $div.data('bounds-data', state.boundsData);
    $div.data('panel', state.panel);
    return undefined;
  }

  // Get or set the bounds of the brush using coordinates in the data space.
  function boundsData(box_data) {
    if (box_data === undefined) {
      return $.extend({}, state.boundsData);
    }

    let box_css = imgToCss(state.panel.scaleDataToImg(box_data));
    // Round to 13 significant digits to avoid spurious changes in FP values
    // (#2197).
    box_css = mapValues(box_css, val => roundSignif(val, 13));

    // The scaling function can reverse the direction of the axes, so we need to
    // find the min and max again.
    boundsCss({
      xmin: Math.min(box_css.xmin, box_css.xmax),
      xmax: Math.max(box_css.xmin, box_css.xmax),
      ymin: Math.min(box_css.ymin, box_css.ymax),
      ymax: Math.max(box_css.ymin, box_css.ymax)
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
    $div = $(document.createElement('div'))
      .attr('id', el.id + '_brush')
      .css({
        'background-color': opts.brushFill,
        'opacity': opts.brushOpacity,
        'pointer-events': 'none',
        'position': 'absolute'
      })
      .hide();

    var borderStyle = '1px solid ' + opts.brushStroke;
    if (opts.brushDirection === 'xy') {
      $div.css({
        'border': borderStyle
      });
    } else if (opts.brushDirection === 'x') {
      $div.css({
        'border-left': borderStyle,
        'border-right': borderStyle
      });
    } else if (opts.brushDirection === 'y') {
      $div.css({
        'border-top': borderStyle,
        'border-bottom': borderStyle
      });
    }

    $el.append($div);
    $div.offset({x:0, y:0}).width(0).outerHeight(0);
  }

  // Update the brush div to reflect the current brush bounds.
  function updateDiv() {
    // Need parent offset relative to page to calculate mouse offset
    // relative to page.
    const img_offset_css = findOrigin($el.find("img"));
    const b = state.boundsCss;

    $div.offset({
        top:  img_offset_css.y + b.ymin,
        left: img_offset_css.x + b.xmin
      })
      .outerWidth(b.xmax - b.xmin + 1)
      .outerHeight(b.ymax - b.ymin + 1);
  }

  function down(offset_css) {
    if (offset_css === undefined)
      return state.down;

    state.down = offset_css;
    return undefined;
  }

  function up(offset_css) {
    if (offset_css === undefined)
      return state.up;

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
      ymax: start.ymax + dy
    };

    // Clip to the plotting area
    if (opts.brushClip) {
      const panelBounds_img = state.panel.range;
      const newBounds_img = cssToImg(newBounds_css);

      // Convert to format for shiftToRange
      let xvals_img = [ newBounds_img.xmin, newBounds_img.xmax ];
      let yvals_img = [ newBounds_img.ymin, newBounds_img.ymax ];

      xvals_img = imageutils.shiftToRange(xvals_img, panelBounds_img.left, panelBounds_img.right);
      yvals_img = imageutils.shiftToRange(yvals_img, panelBounds_img.top,  panelBounds_img.bottom);

      // Convert back to bounds format
      newBounds_css = imgToCss({
        xmin: xvals_img[0],
        xmax: xvals_img[1],
        ymin: yvals_img[0],
        ymax: yvals_img[1]
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
      y: offset_css.y - state.down.y
    };

    const d_img = cssToImg(d_css);

    // Calculate what new positions would be, before clipping.
    const b_img = cssToImg(state.changeStartBounds);
    const panelBounds_img = state.panel.range;

    if (state.resizeSides.left) {
      const xmin_img = imageutils.shiftToRange(b_img.xmin + d_img.x, panelBounds_img.left, b_img.xmax)[0];
      b_img.xmin = xmin_img;
    } else if (state.resizeSides.right) {
      const xmax_img = imageutils.shiftToRange(b_img.xmax + d_img.x, b_img.xmin, panelBounds_img.right)[0];
      b_img.xmax = xmax_img;
    }

    if (state.resizeSides.top) {
      const ymin_img = imageutils.shiftToRange(b_img.ymin + d_img.y, panelBounds_img.top, b_img.ymax)[0];
      b_img.ymin = ymin_img;
    } else if (state.resizeSides.bottom) {
      const ymax_img = imageutils.shiftToRange(b_img.ymax + d_img.y, b_img.ymin, panelBounds_img.bottom)[0];
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

    onResize: onResize,  // A callback when the wrapper div or img is resized.

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
    stopResizing: stopResizing
  };
};

exports.resetBrush = function(brushId) {
  exports.setInputValue(brushId, null);
  imageOutputBinding.find(document).trigger("shiny-internal:brushed", {
    brushId: brushId, outputId: null
  });
};


// -----------------------------------------------------------------------
// Utility functions for finding dimensions and locations of DOM elements
// -----------------------------------------------------------------------

// Returns the ratio that an element has been scaled (for example, by CSS
// transforms) in the x and y directions.
function findScalingRatio($el) {
  const boundingRect = $el[0].getBoundingClientRect();
  return {
    x: boundingRect.width  / $el.outerWidth(),
    y: boundingRect.height / $el.outerHeight()
  };
}

function findOrigin($el) {
  const offset = $el.offset();
  const scaling_ratio = findScalingRatio($el);

  // Find the size of the padding and border, for the top and left. This is
  // before any transforms.
  const paddingBorder = {
    left: parseInt($el.css("border-left-width")) + parseInt($el.css("padding-left")),
    top:  parseInt($el.css("border-top-width"))  + parseInt($el.css("padding-top"))
  };

  // offset() returns the upper left corner of the element relative to the
  // page, but it includes padding and border. Here we find the upper left
  // of the element, not including padding and border.
  return {
    x: offset.left + scaling_ratio.x * paddingBorder.left,
    y: offset.top  + scaling_ratio.y * paddingBorder.top
  };
}

// Find the dimensions of a tag, after transforms, and without padding and
// border.
function findDims($el) {
  // If there's any padding/border, we need to find the ratio of the actual
  // element content compared to the element plus padding and border.
  const content_ratio = {
    x: $el.width()  / $el.outerWidth(),
    y: $el.height() / $el.outerHeight()
  };

  // Get the dimensions of the element _after_ any CSS transforms. This
  // includes the padding and border.
  const bounding_rect = $el[0].getBoundingClientRect();

  // Dimensions of the element after any CSS transforms, and without
  // padding/border.
  return {
    x: content_ratio.x * bounding_rect.width,
    y: content_ratio.y * bounding_rect.height
  };
}
