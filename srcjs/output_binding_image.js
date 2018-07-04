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

    // Remove event handlers that were added in previous renderValue()
    $el.off('.image_output');

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

    if (!opts.coordmap)
      opts.coordmap = [];

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
      var clickHandler = imageutils.createClickHandler(opts.clickId,
        opts.clickClip, opts.coordmap);
      $el.on('mousedown2.image_output', clickHandler.mousedown);

      // When img is reset, do housekeeping: clear $el's mouse listener and
      // call the handler's onResetImg callback.
      $img.on('reset', clickHandler.onResetImg);
    }

    if (opts.dblclickId) {
      // We'll use the clickHandler's mousedown function, but register it to
      // our custom 'dblclick2' event.
      var dblclickHandler = imageutils.createClickHandler(opts.dblclickId,
        opts.clickClip, opts.coordmap);
      $el.on('dblclick2.image_output', dblclickHandler.mousedown);

      $img.on('reset', dblclickHandler.onResetImg);
    }

    if (opts.hoverId) {
      var hoverHandler = imageutils.createHoverHandler(opts.hoverId,
        opts.hoverDelay, opts.hoverDelayType, opts.hoverClip,
        opts.hoverNullOutside, opts.coordmap);
      $el.on('mousemove.image_output', hoverHandler.mousemove);
      $el.on('mouseout.image_output', hoverHandler.mouseout);

      $img.on('reset', hoverHandler.onResetImg);
    }

    if (opts.brushId) {
      // Make image non-draggable (Chrome, Safari)
      $img.css('-webkit-user-drag', 'none');
      // Firefox, IE<=10
      $img.on('dragstart', function() { return false; });

      // Disable selection of image and text when dragging in IE<=10
      $el.on('selectstart.image_output', function() { return false; });

      var brushHandler = imageutils.createBrushHandler(opts.brushId, $el, opts,
        opts.coordmap, outputId);
      $el.on('mousedown.image_output', brushHandler.mousedown);
      $el.on('mousemove.image_output', brushHandler.mousemove);

      $img.on('reset', brushHandler.onResetImg);
    }

    if (opts.clickId || opts.dblclickId || opts.hoverId || opts.brushId) {
      $el.addClass('crosshair');
    }

    if (data.error)
      console.log('Error on server extracting coordmap: ' + data.error);
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
  }
});
outputBindings.register(imageOutputBinding, 'shiny.imageOutput');


var imageutils = {};


// Modifies the panel objects in a coordmap, adding scale(), scaleInv(),
// and clip() functions to each one.
imageutils.initPanelScales = function(coordmap) {
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

    panel.scale = function(val, clip) {
      return {
        x: xscaler.scale(val.x, clip),
        y: yscaler.scale(val.y, clip)
      };
    };

    panel.scaleInv = function(val, clip) {
      return {
        x: xscaler.scaleInv(val.x, clip),
        y: yscaler.scaleInv(val.y, clip)
      };
    };

    // Given a scaled offset (in pixels), clip it to the nearest panel region.
    panel.clip = function(offset) {
      var newOffset = {
        x: offset.x,
        y: offset.y
      };

      var bounds = panel.range;

      if      (offset.x > bounds.right)  newOffset.x = bounds.right;
      else if (offset.x < bounds.left)   newOffset.x = bounds.left;

      if      (offset.y > bounds.bottom) newOffset.y = bounds.bottom;
      else if (offset.y < bounds.top)    newOffset.y = bounds.top;

      return newOffset;
    };
  }

  // Add the functions to each panel object.
  for (var i=0; i<coordmap.length; i++) {
    var panel = coordmap[i];
    addScaleFuns(panel);
  }
};


// This adds functions to the coordmap object to handle various
// coordinate-mapping tasks, and send information to the server.
// The input coordmap is an array of objects, each of which represents a panel.
// coordmap must be an array, even if empty, so that it can be modified in
// place; when empty, we add a dummy panel to the array.
// It also calls initPanelScales, which modifies each panel object to have
// scale, scaleInv, and clip functions.
imageutils.initCoordmap = function($el, coordmap) {
  var el = $el[0];

  // If we didn't get any panels, create a dummy one where the domain and range
  // are simply the pixel dimensions.
  // that we modify.
  if (coordmap.length === 0) {
    var bounds = {
      top: 0,
      left: 0,
      right: el.clientWidth - 1,
      bottom: el.clientHeight - 1
    };

    coordmap[0] = {
      domain: bounds,
      range: bounds,
      mapping: {}
    };
  }

  // Add scaling functions to each panel
  imageutils.initPanelScales(coordmap);


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
      left: parseInt($el.css("border-left")) + parseInt($el.css("padding-left")),
      top:  parseInt($el.css("border-top"))  + parseInt($el.css("padding-top"))
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

  // Returns the x and y ratio that image content (like a PNG) is scaled to on
  // screen. If the image data is 1000 pixels wide and is scaled to 300 pixels
  // on screen, then this returns 0.3. (Note the 300 pixels refers to CSS
  // pixels.)
  function findImgPixelScalingRatio($img) {
    const img_dims = findDims($img);
    return {
      x: img_dims.x / $img[0].naturalWidth,
      y: img_dims.y / $img[0].naturalHeight
    };
  }

  // This returns the offset of the mouse, relative to the img, but with some
  // extra sauce. First, it returns the offset in the pixel dimensions of the
  // image as if it were scaled to 100%. If the img content is 1000 pixels
  // wide, but is scaled to 400 pixels on screen, and the mouse is on the far
  // right side, then this will return x:1000. Second, if there is any padding
  // or border around the img, it handles that. Third, if there are any
  // scaling transforms on the image, it handles that as well.
  coordmap.mouseOffset = function(mouseEvent) {
    const $img = $el.find("img");
    const img_origin = findOrigin($img);

    // The offset of the mouse from the upper-left corner of the img, in
    // pixels.
    const offset_raw = {
      x: mouseEvent.pageX - img_origin.x,
      y: mouseEvent.pageY - img_origin.y
    };

    const pixel_scaling = findImgPixelScalingRatio($img);

    return {
      x: offset_raw.x / pixel_scaling.x,
      y: offset_raw.y / pixel_scaling.y
    };
  };

  // Given two sets of x/y coordinates, return an object representing the
  // min and max x and y values. (This could be generalized to any number
  // of points).
  coordmap.findBox = function(offset1, offset2) {
    return {
      xmin: Math.min(offset1.x, offset2.x),
      xmax: Math.max(offset1.x, offset2.x),
      ymin: Math.min(offset1.y, offset2.y),
      ymax: Math.max(offset1.y, offset2.y)
    };
  };


  // Shift an array of values so that they are within a min and max.
  // The vals will be shifted so that they maintain the same spacing
  // internally. If the range in vals is larger than the range of
  // min and max, the result might not make sense.
  coordmap.shiftToRange = function(vals, min, max) {
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

  // Given an offset, return an object representing which panel it's in. The
  // `expand` argument tells it to expand the panel area by that many pixels.
  // It's possible for an offset to be within more than one panel, because
  // of the `expand` value. If that's the case, find the nearest panel.
  coordmap.getPanel = function(offset, expand) {
    expand = expand || 0;

    var x = offset.x;
    var y = offset.y;

    var matches = []; // Panels that match
    var dists = [];   // Distance of offset to each matching panel
    var b;
    for (var i=0; i<coordmap.length; i++) {
      b = coordmap[i].range;

      if (x <= b.right  + expand &&
          x >= b.left   - expand &&
          y <= b.bottom + expand &&
          y >= b.top    - expand)
      {
        matches.push(coordmap[i]);

        // Find distance from edges for x and y
        var xdist = 0;
        var ydist = 0;
        if (x > b.right && x <= b.right + expand) {
          xdist = x - b.right;
        } else if (x < b.left && x >= b.left - expand) {
          xdist = x - b.left;
        }
        if (y > b.bottom && y <= b.bottom + expand) {
          ydist = y - b.bottom;
        } else if (y < b.top && y >= b.top - expand) {
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

  // Is an offset in a panel? If supplied, `expand` tells us to expand the
  // panels by that many pixels in all directions.
  coordmap.isInPanel = function(offset, expand) {
    expand = expand || 0;

    if (coordmap.getPanel(offset, expand))
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

      var offset = coordmap.mouseOffset(e);
      // If outside of plotting region
      if (!coordmap.isInPanel(offset)) {
        if (nullOutside) {
          exports.setInputValue(inputId, null);
          return;
        }
        if (clip)
          return;
      }
      if (clip && !coordmap.isInPanel(offset)) return;

      var panel = coordmap.getPanel(offset);
      var coords = panel.scaleInv(offset);

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
      pageY:   e.pageY,
      offsetX: e.offsetX,
      offsetY: e.offsetY
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
          Math.abs(pending_e.offsetX - e.offsetX) > 2 ||
          Math.abs(pending_e.offsetY - e.offsetY) > 2) {

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
    onResetImg: function() { clickInfoSender(null); }
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
    onResetImg: function()  { hoverInfoSender.immediateCall(null); }
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

    var offset = coordmap.mouseOffset(e);

    // Ignore mousedown events outside of plotting region, expanded by
    // a number of pixels specified in expandPixels.
    if (opts.brushClip && !coordmap.isInPanel(offset, expandPixels))
      return;

    brush.up({ x: NaN, y: NaN });
    brush.down(offset);


    if (brush.isInResizeArea(offset)) {
      brush.startResizing(offset);

      // Attach the move and up handlers to the window so that they respond
      // even when the mouse is moved outside of the image.
      $(document)
        .on('mousemove.image_brush', mousemoveResizing)
        .on('mouseup.image_brush', mouseupResizing);

    } else if (brush.isInsideBrush(offset)) {
      brush.startDragging(offset);
      setCursorStyle('grabbing');

      // Attach the move and up handlers to the window so that they respond
      // even when the mouse is moved outside of the image.
      $(document)
        .on('mousemove.image_brush', mousemoveDragging)
        .on('mouseup.image_brush', mouseupDragging);

    } else {
      var panel = coordmap.getPanel(offset, expandPixels);
      brush.startBrushing(panel.clip(offset));

      // Attach the move and up handlers to the window so that they respond
      // even when the mouse is moved outside of the image.
      $(document)
        .on('mousemove.image_brush', mousemoveBrushing)
        .on('mouseup.image_brush', mouseupBrushing);
    }
  }

  // This sets the cursor style when it's in the el
  function mousemove(e) {
    var offset = coordmap.mouseOffset(e);

    if (!(brush.isBrushing() || brush.isDragging() || brush.isResizing())) {
      // Set the cursor depending on where it is
      if (brush.isInResizeArea(offset)) {
        var r = brush.whichResizeSides(offset);

        if ((r.left && r.top) || (r.right && r.bottom)) {
          setCursorStyle('nwse-resize');
        } else if ((r.left && r.bottom) || (r.right && r.top)) {
          setCursorStyle('nesw-resize');
        } else if (r.left || r.right) {
          setCursorStyle('ew-resize');
        } else if (r.top || r.bottom) {
          setCursorStyle('ns-resize');
        }
      } else if (brush.isInsideBrush(offset)) {
        setCursorStyle('grabbable');
      } else if (coordmap.isInPanel(offset, expandPixels)) {
        setCursorStyle('crosshair');
      } else {
        setCursorStyle(null);
      }
    }
  }

  // mousemove handlers while brushing or dragging
  function mousemoveBrushing(e) {
    brush.brushTo(coordmap.mouseOffset(e));
    brushInfoSender.normalCall();
  }

  function mousemoveDragging(e) {
    brush.dragTo(coordmap.mouseOffset(e));
    brushInfoSender.normalCall();
  }

  function mousemoveResizing(e) {
    brush.resizeTo(coordmap.mouseOffset(e));
    brushInfoSender.normalCall();
  }

  // mouseup handlers while brushing or dragging
  function mouseupBrushing(e) {
    // Listen for left mouse button only
    if (e.which !== 1) return;

    $(document)
      .off('mousemove.image_brush')
      .off('mouseup.image_brush');

    brush.up(coordmap.mouseOffset(e));

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

    brush.up(coordmap.mouseOffset(e));

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

    brush.up(coordmap.mouseOffset(e));
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
      brush.importOldBrush();
      brushInfoSender.immediateCall();
    }
  }

  return {
    mousedown: mousedown,
    mousemove: mousemove,
    onResetImg: onResetImg
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
  reset();

  function reset() {
    // Current brushing/dragging/resizing state
    state.brushing = false;
    state.dragging = false;
    state.resizing = false;

    // Offset of last mouse down and up events
    state.down = { x: NaN, y: NaN };
    state.up   = { x: NaN, y: NaN };

    // Which side(s) we're currently resizing
    state.resizeSides = {
      left: false,
      right: false,
      top: false,
      bottom: false
    };

    // Bounding rectangle of the brush, in pixel and data dimensions. We need to
    // record data dimensions along with pixel dimensions so that when a new
    // plot is sent, we can re-draw the brush div with the appropriate coords.
    state.boundsPx = {
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

    // The bounds at the start of a drag/resize
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

    // Compare two objects. This checks that objects a and b have the same est
    // of keys, and that each key has the same value. This function isn't
    // perfect, but it's good enough for comparing variable mappings, below.
    function isEquivalent(a, b) {
      if (a === undefined) {
        if (b === undefined)
          return true;
        else
          return false;
      }
      if (a === null) {
        if (b === null)
          return true;
        else
          return false;
      }

      var aProps = Object.getOwnPropertyNames(a);
      var bProps = Object.getOwnPropertyNames(b);

      if (aProps.length !== bProps.length)
        return false;

      for (var i=0; i<aProps.length; i++) {
        var propName = aProps[i];
        if (a[propName] !== b[propName]) {
          return false;
        }
      }
      return true;
    }

    // Find a panel that has matching vars; if none found, we can't restore.
    // The oldPanel and new panel must match on their mapping vars, and the
    // values.
    for (var i=0; i<coordmap.length; i++){
      var curPanel = coordmap[i];

      if (isEquivalent(oldPanel.mapping, curPanel.mapping) &&
          isEquivalent(oldPanel.panel_vars, curPanel.panel_vars)) {
        // We've found a matching panel
        state.panel = coordmap[i];
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

  // Return true if the offset is inside min/max coords
  function isInsideBrush(offset) {
    var bounds = state.boundsPx;
    return offset.x <= bounds.xmax && offset.x >= bounds.xmin &&
           offset.y <= bounds.ymax && offset.y >= bounds.ymin;
  }

  // Return true if offset is inside a region to start a resize
  function isInResizeArea(offset) {
    var sides = whichResizeSides(offset);
    return sides.left || sides.right || sides.top || sides.bottom;
  }

  // Return an object representing which resize region(s) the cursor is in.
  function whichResizeSides(offset) {
    var b = state.boundsPx;
    // Bounds with expansion
    var e = {
      xmin: b.xmin - resizeExpand,
      xmax: b.xmax + resizeExpand,
      ymin: b.ymin - resizeExpand,
      ymax: b.ymax + resizeExpand
    };
    var res = {
      left: false,
      right: false,
      top: false,
      bottom: false
    };

    if ((opts.brushDirection === 'xy' || opts.brushDirection === 'x') &&
        (offset.y <= e.ymax && offset.y >= e.ymin))
    {
      if (offset.x < b.xmin && offset.x >= e.xmin)
        res.left = true;
      else if (offset.x > b.xmax && offset.x <= e.xmax)
        res.right = true;
    }

    if ((opts.brushDirection === 'xy' || opts.brushDirection === 'y') &&
        (offset.x <= e.xmax && offset.x >= e.xmin))
    {
      if (offset.y < b.ymin && offset.y >= e.ymin)
        res.top = true;
      else if (offset.y > b.ymax && offset.y <= e.ymax)
        res.bottom = true;
    }

    return res;
  }


  // Sets the bounds of the brush, given a box and optional panel. This
  // will fit the box bounds into the panel, so we don't brush outside of it.
  // This knows whether we're brushing in the x, y, or xy directions, and sets
  // bounds accordingly.
  // If no box is passed in, just return current bounds.
  function boundsPx(box) {
    if (box === undefined)
      return state.boundsPx;

    var min = { x: box.xmin, y: box.ymin };
    var max = { x: box.xmax, y: box.ymax };

    var panel = state.panel;
    var panelBounds = panel.range;

    if (opts.brushClip) {
      min = panel.clip(min);
      max = panel.clip(max);
    }

    if (opts.brushDirection === 'xy') {
      // No change

    } else if (opts.brushDirection === 'x') {
      // Extend top and bottom of plotting area
      min.y = panelBounds.top;
      max.y = panelBounds.bottom;

    } else if (opts.brushDirection === 'y') {
      min.x = panelBounds.left;
      max.x = panelBounds.right;
    }

    state.boundsPx = {
      xmin: min.x,
      xmax: max.x,
      ymin: min.y,
      ymax: max.y
    };

    // Positions in data space
    var minData = state.panel.scaleInv(min);
    var maxData = state.panel.scaleInv(max);
    // For reversed scales, the min and max can be reversed, so use findBox
    // to ensure correct order.
    state.boundsData = coordmap.findBox(minData, maxData);
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
  function boundsData(box) {
    if (box === undefined) {
      return state.boundsData;
    }

    var min = { x: box.xmin, y: box.ymin };
    var max = { x: box.xmax, y: box.ymax };

    var minPx = state.panel.scale(min);
    var maxPx = state.panel.scale(max);

    // The scaling function can reverse the direction of the axes, so we need to
    // find the min and max again.
    boundsPx({
      xmin: Math.min(minPx.x, maxPx.x),
      xmax: Math.max(minPx.x, maxPx.x),
      ymin: Math.min(minPx.y, maxPx.y),
      ymax: Math.max(minPx.y, maxPx.y)
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
    var imgOffset = $el.offset();
    var b = state.boundsPx;
    $div.offset({
        top: imgOffset.top + b.ymin,
        left: imgOffset.left + b.xmin
      })
      .outerWidth(b.xmax - b.xmin + 1)
      .outerHeight(b.ymax - b.ymin + 1);
  }

  function down(offset) {
    if (offset === undefined)
      return state.down;

    state.down = offset;
    return undefined;
  }

  function up(offset) {
    if (offset === undefined)
      return state.up;

    state.up = offset;
    return undefined;
  }

  function isBrushing() {
    return state.brushing;
  }

  function startBrushing() {
    state.brushing = true;
    addDiv();
    state.panel = coordmap.getPanel(state.down, expandPixels);

    boundsPx(coordmap.findBox(state.down, state.down));
    updateDiv();
  }

  function brushTo(offset) {
    boundsPx(coordmap.findBox(state.down, offset));
    $div.show();
    updateDiv();
  }

  function stopBrushing() {
    state.brushing = false;

    // Save the final bounding box of the brush
    boundsPx(coordmap.findBox(state.down, state.up));
  }

  function isDragging() {
    return state.dragging;
  }

  function startDragging() {
    state.dragging = true;
    state.changeStartBounds = $.extend({}, state.boundsPx);
  }

  function dragTo(offset) {
    // How far the brush was dragged
    var dx = offset.x - state.down.x;
    var dy = offset.y - state.down.y;

    // Calculate what new positions would be, before clipping.
    var start = state.changeStartBounds;
    var newBounds = {
      xmin: start.xmin + dx,
      xmax: start.xmax + dx,
      ymin: start.ymin + dy,
      ymax: start.ymax + dy
    };

    // Clip to the plotting area
    if (opts.brushClip) {
      var panelBounds = state.panel.range;

      // Convert to format for shiftToRange
      var xvals = [ newBounds.xmin, newBounds.xmax ];
      var yvals = [ newBounds.ymin, newBounds.ymax ];

      xvals = coordmap.shiftToRange(xvals, panelBounds.left, panelBounds.right);
      yvals = coordmap.shiftToRange(yvals, panelBounds.top,  panelBounds.bottom);

      // Convert back to bounds format
      newBounds = {
        xmin: xvals[0],
        xmax: xvals[1],
        ymin: yvals[0],
        ymax: yvals[1]
      };
    }

    boundsPx(newBounds);
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
    state.changeStartBounds = $.extend({}, state.boundsPx);
    state.resizeSides = whichResizeSides(state.down);
  }

  function resizeTo(offset) {
    // How far the brush was dragged
    var dx = offset.x - state.down.x;
    var dy = offset.y - state.down.y;

    // Calculate what new positions would be, before clipping.
    var b = $.extend({}, state.changeStartBounds);
    var panelBounds = state.panel.range;

    if (state.resizeSides.left) {
      b.xmin = coordmap.shiftToRange([b.xmin + dx], panelBounds.left, b.xmax)[0];
    } else if (state.resizeSides.right) {
      b.xmax = coordmap.shiftToRange([b.xmax + dx], b.xmin, panelBounds.right)[0];
    }

    if (state.resizeSides.top) {
      b.ymin = coordmap.shiftToRange([b.ymin + dy], panelBounds.top, b.ymax)[0];
    } else if (state.resizeSides.bottom) {
      b.ymax = coordmap.shiftToRange([b.ymax + dy], b.ymin, panelBounds.bottom)[0];
    }

    boundsPx(b);
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

    boundsPx: boundsPx,
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
