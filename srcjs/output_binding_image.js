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

    var $el = $(el);
    // Load the image before emptying, to minimize flicker
    var img = null;

    // Remove event handlers that were added in previous renderValue()
    $el.off('.image_output');
    // Trigger custom 'remove' event for any existing images in the div
    $el.find('img').trigger('remove');

    if (!data) {
      $el.empty();
      return;
    }

    var opts = {
      clickId: $el.data('click-id'),
      clickClip: strToBool($el.data('click-clip')) || true,

      dblclickId: $el.data('dblclick-id'),
      dblclickClip: strToBool($el.data('dblclick-clip')) || true,
      dblclickDelay: $el.data('dblclick-delay') || 400,

      hoverId: $el.data('hover-id'),
      hoverClip: $el.data('hover-clip') || true,
      hoverDelayType: $el.data('hover-delay-type') || 'debounce',
      hoverDelay: $el.data('hover-delay') || 300,

      brushId: $el.data('brush-id'),
      brushClip: strToBool($el.data('brush-clip')) || true,
      brushDelayType: $el.data('brush-delay-type') || 'debounce',
      brushDelay: $el.data('brush-delay') || 300,
      brushFill: $el.data('brush-fill') || '#666',
      brushStroke: $el.data('brush-stroke') || '#000',
      brushOpacity: $el.data('brush-opacity') || 0.3,
      brushDirection: $el.data('brush-direction') || 'xy',
      brushResetOnNew: strToBool($el.data('brush-reset-on-new')) || false,

      coordmap: data.coordmap
    };

    img = document.createElement('img');
    // Copy items from data to img. This should include 'src'
    $.each(data, function(key, value) {
      if (value !== null)
        img[key] = value;
    });

    var $img = $(img);

    // Create object that handles mapping coordinates to data space, and which
    // sends coordinates to the server.
    var mapper = imageutils.createCoordMapper($el, opts.coordmap);

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
      var clickHandler = imageutils.createClickHandler(opts.clickId, opts, mapper);
      $el.on('mousedown2.image_output', clickHandler.mousedown);

      // When img is removed, do housekeeping: clear $el's mouse listener and
      // call the handler's onRemoveImg callback.
      $img.on('remove', clickHandler.onRemoveImg);
    }

    if (opts.dblclickId) {
      // We'll use the clickHandler's mousedown function, but register it to
      // our custom 'dblclick2' event.
      var dblclickHandler = imageutils.createClickHandler(opts.dblclickId, opts, mapper);
      $el.on('dblclick2.image_output', dblclickHandler.mousedown);

      $img.on('remove', dblclickHandler.onRemoveImg);
    }

    if (opts.hoverId) {
      var hoverHandler = imageutils.createHoverHandler(opts.hoverId, opts, mapper);
      $el.on('mousemove.image_output', hoverHandler.mousemove);

      $img.on('remove', hoverHandler.onRemoveImg);
    }

    if (opts.brushId) {
      // Make image non-draggable (Chrome, Safari)
      $img.css('-webkit-user-drag', 'none');
      // Firefox, IE<=10
      $img.on('dragstart', function() { return false; });

      // Disable selection of image and text when dragging in IE<=10
      $el.on('selectstart.image_output', function() { return false; });

      var brushHandler = imageutils.createBrushHandler(opts.brushId, $el, opts, mapper);
      $el.on('mousedown.image_output', brushHandler.mousedown);
      $el.on('mousemove.image_output', brushHandler.mousemove);

      $img.on('remove', brushHandler.onRemoveImg);
    }

    if (opts.clickId || opts.dblclickId || opts.hoverId || opts.brushId) {
      $el.addClass('crosshair');
    }

    // Remove all elements except brush, usually image plus error messages.
    // These extra contortions are needed to select the bare text of error
    // message.
    $el.contents().filter(function() {
      return this.id !== el.id + '_brush';
    }).remove();

    if (img)
      $el.append(img);
  }
});
outputBindings.register(imageOutputBinding, 'shiny.imageOutput');


var imageutils = {};


// This object handles various coordinate-mapping tasks, and sends information
// to the server.
imageutils.createCoordMapper = function($el, coordmap) {
  var el = $el[0];

  // If we didn't get a coordmap, create a dummy one where the domain and range
  // are simply the pixel dimensions.
  if (!coordmap) {
    var bounds = {
      top: 0,
      left: 0,
      right: el.clientWidth - 1,
      bottom: el.clientHeight - 1
    };

    coordmap = [{ domain: bounds, range: bounds }];
  }

  // Firefox doesn't have offsetX/Y, so we need to use an alternate
  // method of calculation for it. Even though other browsers do have
  // offsetX/Y, we need to calculate relative to $el, because sometimes the
  // mouse event can come with offset relative to other elements on the
  // page. This happens when the event listener is bound to, say, window.
  function mouseOffset(mouseEvent) {
    var offset = $el.offset();
    return {
      x: mouseEvent.pageX - offset.left,
      y: mouseEvent.pageY - offset.top
    };
  }

  // Given two sets of x/y coordinates, return an object representing the
  // min and max x and y values. (This could be generalized to any number
  // of points).
  function findBox(offset1, offset2) {
    return {
      xmin: Math.min(offset1.x, offset2.x),
      xmax: Math.max(offset1.x, offset2.x),
      ymin: Math.min(offset1.y, offset2.y),
      ymax: Math.max(offset1.y, offset2.y)
    };
  }

  // Map a value x from a domain to a range. If the domain is in data space and
  // range is in px, this is scaling. If the reverse, then this is inverse
  // scaling.
  function _map(x, domainMin, domainMax, rangeMin, rangeMax, clip) {
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

  // Shift an array of values so that they are within a min and max.
  // The vals will be shifted so that they maintain the same spacing
  // internally. If the range in vals is larger than the range of
  // min and max, the result might not make sense.
  function shiftToRange(vals, min, max) {
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
  }

  // Transform offset coordinates to data space coordinates
  function scaleInv(offset, panel, clip) {
    // By default, clip to plotting region
    clip = clip || true;

    var d = panel.domain;
    var r = panel.range;
    var userX = _map(offset.x, r.left, r.right, d.left, d.right, clip);
    var userY = _map(offset.y, r.bottom, r.top, d.bottom, d.top, clip);

    if (panel.log) {
      if (panel.log.x)
        userX = Math.pow(panel.log.x, userX);
      if (panel.log.y)
        userY = Math.pow(panel.log.x, userY);
    }

    return {
      x: userX,
      y: userY
    };
  }

  // Transform offset coordinates to data space coordinates, for a box
  function scaleInvBox(box, panel, clip) {
    var min = scaleInv({ x: box.xmin, y: box.ymin }, panel, clip);
    var max = scaleInv({ x: box.xmax, y: box.ymax }, panel, clip);
    return {
      xmin: min.x,
      xmax: max.x,
      ymin: min.y,
      ymax: max.y
    };
  }

  // Transform data space coordinates to offset coordinates
  function scale(offset, panel, clip) {
    // By default, clip to plotting region
    clip = clip || true;



  }

  // Given an offset, return an object representing which panel it's in.
  function getPanel(offset, expand) {
    expand = expand || 0;
    var bounds;
    for (var i=0; i<coordmap.length; i++) {
      bounds = coordmap[i].range;
      if (offset.x <= bounds.right + expand &&
          offset.x >= bounds.left - expand &&
          offset.y <= bounds.bottom + expand &&
          offset.y >= bounds.top - expand) {

        return coordmap[i];
      }
    }

    return null;
  }

  // Is an offset in a panel? If supplied, `expand` tells us to expand the
  // panels by that many pixels in all directions.
  function isInPanel(offset, expand) {
    expand = expand || 0;

    // TODO: use expand
    if (getPanel(offset))
      return true;

    return false;
  }

  // Given an offset, clip it to the nearest panel region as specified by
  // coordmap. If there is no coordmap, clip it to bounds of the DOM
  // element.
  function clipToBounds(offset, bounds) {
    var newOffset = {
      x: offset.x,
      y: offset.y
    };

    if (offset.x > bounds.right)
      newOffset.x = bounds.right;
    else if (offset.x < bounds.left)
      newOffset.x = bounds.left;

    if (offset.y > bounds.bottom)
      newOffset.y = bounds.bottom;
    else if (offset.y < bounds.top)
      newOffset.y = bounds.top;

    return newOffset;
  }

  // Returns a function that sends mouse coordinates, scaled to data space.
  // If that function is passed a null event, it will send null.
  function mouseCoordinateSender(inputId, clip) {
    clip = clip || true;

    return function(e) {
      if (e === null) {
        exports.onInputChange(inputId, null);
        return;
      }

      var offset = mouseOffset(e);
      // Ignore events outside of plotting region
      if (clip && !isInPanel(offset)) return;

      var panel = getPanel(offset);
      var coords = scaleInv(offset, panel);

      // Add the panel (facet) variables, if present
      if (panel.vars) {
        var v;
        for (var i=0; i<panel.vars.length; i++) {
          v = panel.vars[i];
          coords[v.name] = v.value;
        }
      }

      coords[".nonce"] = Math.random();
      exports.onInputChange(inputId, coords);
    };
  }

  return {
    mouseOffset: mouseOffset,
    findBox: findBox,
    shiftToRange: shiftToRange,
    scaleInv: scaleInv,
    scaleInvBox: scaleInvBox,
    getPanel: getPanel,
    isInPanel: isInPanel,
    clipToBounds: clipToBounds,
    mouseCoordinateSender: mouseCoordinateSender
  };
};


// This object provides two public event listeners: mousedown, and
// dblclickIE8.
// We need to make sure that, when the image is listening for double-
// clicks, that a double-click doesn't trigger two click events. We'll
// trigger custom mousedown2 and dblclick2 events with this mousedown
// listener.
imageutils.createClickInfo = function($el, dblclickId, dblclickDelay) {
  var clickCount = 0;
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

imageutils.createClickHandler = function(inputId, opts, mapper) {
  var clickInfoSender = mapper.mouseCoordinateSender(inputId, opts.clickClip);

  return {
    mousedown: function(e) {
      // Listen for left mouse button only
      if (e.which !== 1) return;
      clickInfoSender(e);
    },
    onRemoveImg: function() { clickInfoSender(null); }
  };
};


imageutils.createHoverHandler = function(inputId, opts, mapper) {
  var sendHoverInfo = mapper.mouseCoordinateSender(inputId, opts.hoverClip);

  var hoverInfoSender;
  if (opts.hoverDelayType === 'throttle')
    hoverInfoSender = new Throttler(null, sendHoverInfo, opts.hoverDelay);
  else
    hoverInfoSender = new Debouncer(null, sendHoverInfo, opts.hoverDelay);

  return {
    mousemove:   function(e) { hoverInfoSender.normalCall(e); },
    onRemoveImg: function()  { hoverInfoSender.immediateCall(null); }
  };
};


// Returns a brush handler object. This has three public functions:
// mousedown, mousemove, and onRemoveImg.
imageutils.createBrushHandler = function(inputId, $el, opts, mapper) {
  // Parameter: expand the area in which a brush can be started, by this
  // many pixels in all directions.
  var expandPixels = 20;

  // Represents the state of the brush
  var brush = imageutils.createBrush($el, opts, mapper);

  // Set cursor to one of 3 styles. We need to set the cursor on the whole
  // el instead of the brush div, because the brush div has
  // 'pointer-events:none' so that it won't intercept pointer events.
  // If `style` is null, don't add a cursor style.
  function setCursorStyle(style) {
    $el.removeClass('crosshair');
    $el.removeClass('grabbable');
    $el.removeClass('grabbing');

    if (style) $el.addClass(style);
  }

  function sendBrushInfo() {
    var bounds = brush.boundsPx();

    // We're in a new or reset state
    if (isNaN(bounds.xmin)) {
      exports.onInputChange(inputId, null);
      return;
    }

    // Transform coordinates of brush to data space
    // TODO: remove this - use brush API
    var panel = mapper.getPanel({ x: bounds.xmin, y: bounds.ymin });
    var min = mapper.scaleInv({ x: bounds.xmin, y: bounds.ymin }, panel, opts.brushClip);
    var max = mapper.scaleInv({ x: bounds.xmax, y: bounds.ymax }, panel, opts.brushClip);

    // Because the x and y directions of the pixel space may differ from
    // the x and y directions of the data space, we need to recalculate
    // the min and max.
    var coords = {
      xmin: Math.min(min.x, max.x),
      xmax: Math.max(min.x, max.x),
      ymin: Math.min(min.y, max.y),
      ymax: Math.max(min.y, max.y)
    };

    // Add the panel (facet) variables, if present
    if (panel.vars) {
      var v;
      for (var i=0; i<panel.vars.length; i++) {
        v = panel.vars[i];
        coords[v.name] = v.value;
      }
    }

    // Send data to server
    exports.onInputChange(inputId, coords);
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
    if (brush.isBrushing() || brush.isDragging()) return;

    // Listen for left mouse button only
    if (e.which !== 1) return;

    var offset = mapper.mouseOffset(e);

    // Ignore mousedown events outside of plotting region, expanded by
    // a number of pixels specified in expandPixels.
    if (opts.brushClip && !mapper.isInPanel(offset, expandPixels))
      return;

    brush.up({ x: NaN, y: NaN });
    brush.down(offset);

    if (brush.isInsideBrush(offset)) {
      brush.startDragging(offset);
      setCursorStyle('grabbing');

      // Attach the move and up handlers to the window so that they respond
      // even when the mouse is moved outside of the image.
      $(document)
        .on('mousemove.image_brush', mousemoveDragging)
        .on('mouseup.image_brush', mouseupDragging);

    } else {
      var panel = mapper.getPanel(offset);
      brush.startBrushing(mapper.clipToBounds(offset, panel.range));

      // Attach the move and up handlers to the window so that they respond
      // even when the mouse is moved outside of the image.
      $(document)
        .on('mousemove.image_brush', mousemoveBrushing)
        .on('mouseup.image_brush', mouseupBrushing);
    }
  }

  // This sets the cursor style when it's in the el
  function mousemove(e) {
    var offset = mapper.mouseOffset(e);

    if (!(brush.isBrushing() || brush.isDragging())) {
      // Set the cursor depending on where it is
      if (brush.isInsideBrush(offset)) {
        setCursorStyle('grabbable');
      } else if (mapper.isInPanel(offset, expandPixels)) {
        setCursorStyle('crosshair');
      } else {
        setCursorStyle(null);
      }
    }
  }

  // mousemove handlers while brushing or dragging
  function mousemoveBrushing(e) {
    brush.brushTo(mapper.mouseOffset(e));
    brushInfoSender.normalCall();
  }

  function mousemoveDragging(e) {
    brush.dragTo(mapper.mouseOffset(e));
    brushInfoSender.normalCall();
  }

  // mouseup handlers while brushing or dragging
  function mouseupBrushing(e) {
    // Listen for left mouse button only
    if (e.which !== 1) return;

    $(document)
      .off('mousemove.image_brush')
      .off('mouseup.image_brush');

    brush.up(mapper.mouseOffset(e));

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

    brush.up(mapper.mouseOffset(e));

    brush.stopDragging();
    setCursorStyle('grabbable');

    if (brushInfoSender.isPending())
      brushInfoSender.immediateCall();
  }

  // This should be called when the img (not the el) is removed
  function onRemoveImg() {
    if (opts.brushResetOnNew) {
      brush.reset();
      brushInfoSender.immediateCall();
    }
  }

  if (!opts.brushResetOnNew) {
    brush.importOldBrush();
  }

  return {
    mousedown: mousedown,
    mousemove: mousemove,
    onRemoveImg: onRemoveImg
  };
};

// Returns an object that represents the state of the brush. This gets wrapped
// in a brushHandler, which provides various event listeners.
imageutils.createBrush = function($el, opts, mapper) {
  var el = $el[0];
  var $div = null;  // The div representing the brush

  var state = {};
  reset();

  function reset() {
    // Current brushing and dragging state
    state.brushing = false;
    state.dragging = false;

    // Offset of last mouse down and up events
    state.down = { x: NaN, y: NaN };
    state.up   = { x: NaN, y: NaN };

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

    // The bounds at the start of a drag
    state.dragStartBounds = {
      xmin: NaN,
      xmax: NaN,
      ymin: NaN,
      ymax: NaN
    };

    if ($div)
      $div.remove();
  }

  // If there's an existing brush div, use that div to set the new
  // brush's settings.
  function importOldBrush() {
    var oldDiv = $el.find('#' + el.id + '_brush');
    if (oldDiv.length === 0)
      return;

    var elOffset = $el.offset();
    var divOffset = oldDiv.offset();
    var min = {
      x: divOffset.left - elOffset.left,
      y: divOffset.top - elOffset.top
    };
    var max = {
      x: divOffset.left - elOffset.left + oldDiv.width(),
      y: divOffset.top - elOffset.top + oldDiv.height()
    };

    // Check that the min and max are in the same panel. This is needed because
    // then panel dimensions could change.
    // minPanel mapper.getPanel(min);
    // mapper

    state.boundsPx = {
      xmin: min.x,
      xmax: max.x,
      ymin: min.y,
      ymax: max.y
    };

    $div = oldDiv;
  }

  // Return true if the offset is inside min/max coords
  function isInsideBrush(offset) {
    var bounds = state.boundsPx;
    return offset.x <= bounds.xmax && offset.x >= bounds.xmin &&
           offset.y <= bounds.ymax && offset.y >= bounds.ymin;
  }

  // Sets the bounds of the brush, given a box and optional panel. This
  // will fit the box bounds into the panel, so we don't brush outside of it.
  // This knows whether we're brushing in the x, y, or xy directions, and sets
  // bounds accordingly.
  // If no box is passed in, return current bounds.
  function boundsPx(box) {
    if (box === undefined)
      return state.boundsPx;

    var min = { x: box.xmin, y: box.ymin };
    var max = { x: box.xmax, y: box.ymax };

    var panelBounds = state.panel.range;

    if (opts.brushClip) {
      min = mapper.clipToBounds(min, panelBounds);
      max = mapper.clipToBounds(max, panelBounds);
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
  }

  // Get or set the bounds of the brush using coordinates in the data space.
  function boundsScaled(box) {
    if (box === undefined) {
      mapper.scaleInvBox(boundsPx());
    }

    mapper.scaleInvBox(boundsPx());
  }

  // Add a new div representing the brush.
  function addDiv() {
    if ($div) $div.remove();

    $div = $(document.createElement('div'))
      .attr('id', el.id + '_brush')
      .css({
        'background-color': opts.brushFill,
        'opacity': opts.brushOpacity,
        'pointer-events': 'none',
        'position': 'absolute'
      });

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
    $div.offset({x:0, y:0}).width(0).outerHeight(0).show();
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
      .outerWidth(b.xmax - b.xmin)
      .outerHeight(b.ymax - b.ymin)
      .show();
  }

  function down(offset) {
    if (offset === undefined)
      return state.down;

    state.down = offset;
  }

  function up(offset) {
    if (offset === undefined)
      return state.up;

    state.up = offset;
  }

  function isBrushing() {
    return state.brushing;
  }

  function startBrushing() {
    state.brushing = true;
    addDiv();
    state.panel = mapper.getPanel(state.down);

    boundsPx(mapper.findBox(state.down, state.down));
    updateDiv();
  }

  function brushTo(offset) {
    boundsPx(mapper.findBox(state.down, offset));
    updateDiv();
  }

  function stopBrushing() {
    state.brushing = false;

    // Save the final bounding box of the brush
    boundsPx(mapper.findBox(state.down, state.up));
  }

  function isDragging() {
    return state.dragging;
  }

  function startDragging() {
    state.dragging = true;
    state.dragStartBounds = $.extend({}, state.boundsPx);
  }

  function dragTo(offset) {
    // How far the brush was dragged
    var dx = offset.x - state.down.x;
    var dy = offset.y - state.down.y;

    // Calculate what new start/end positions would be, before clipping.
    var start = state.dragStartBounds;
    var newBounds = {
      xmin: start.xmin + dx,
      xmax: start.xmax + dx,
      ymin: start.ymin + dy,
      ymax: start.ymax + dy
    };

    // Clip to the plotting area
    if (opts.brushClip) {
      // Get the panel that we started dragging in
      var panelBounds = mapper.getPanel(state.down).range;

      // Convert to format for shiftToRange
      var xvals = [ newBounds.xmin, newBounds.xmax ];
      var yvals = [ newBounds.ymin, newBounds.ymax ];

      xvals = mapper.shiftToRange(xvals, panelBounds.left, panelBounds.right);
      yvals = mapper.shiftToRange(yvals, panelBounds.top,  panelBounds.bottom);

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

  return {
    reset: reset,

    importOldBrush: importOldBrush,
    isInsideBrush: isInsideBrush,

    boundsPx: boundsPx,

    down: down,
    up: up,

    isBrushing: isBrushing,
    startBrushing: startBrushing,
    brushTo: brushTo,
    stopBrushing: stopBrushing,

    isDragging: isDragging,
    startDragging: startDragging,
    dragTo: dragTo,
    stopDragging: stopDragging
  };
};
