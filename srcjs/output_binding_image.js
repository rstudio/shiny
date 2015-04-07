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

    // Transform offset coordinates to data space coordinates
    function offsetToScaledCoords(offset, clip) {
      // By default, clip to plotting region
      clip = clip || true;

      var coordmap = opts.coordmap;
      if (!coordmap) return offset;

      function devToDataX(deviceX) {
        var x = deviceX - coordmap.range.left;
        var factor = (coordmap.domain.right - coordmap.domain.left) /
                     (coordmap.range.right - coordmap.range.left);
        var newx = (x * factor) + coordmap.domain.left;

        if (clip) {
          var max = Math.max(coordmap.domain.right, coordmap.domain.left);
          var min = Math.min(coordmap.domain.right, coordmap.domain.left);
          if (newx > max)
            newx = max;
          else if (newx < min)
            newx = min;
        }

        return newx;
      }
      function devToDataY(deviceY) {
        var y = deviceY - coordmap.range.bottom;
        var factor = (coordmap.domain.top - coordmap.domain.bottom) /
                     (coordmap.range.top - coordmap.range.bottom);
        var newy = (y * factor) + coordmap.domain.bottom;

        if (clip) {
          var max = Math.max(coordmap.domain.top, coordmap.domain.bottom);
          var min = Math.min(coordmap.domain.top, coordmap.domain.bottom);
          if (newy > max)
            newy = max;
          else if (newy < min)
            newy = min;
        }

        return newy;
      }

      var userX = devToDataX(offset.x);
      if (coordmap.log.x)
        userX = Math.pow(coordmap.log.x, userX);

      var userY = devToDataY(offset.y);
      if (coordmap.log.y)
        userY = Math.pow(coordmap.log.x, userY);

      return {
        x: userX,
        y: userY
      };
    }

    // Get the pixel bounds of the coordmap; if there's no coordmap, return
    // the bounds of the image.
    function getPlotBounds() {
      if (opts.coordmap) {
        return opts.coordmap.range;
      } else {
        return {
          top: 0,
          left: 0,
          right: img.clientWidth - 1,
          bottom: img.clientHeight - 1
        };
      }
    }

    // Is an offset in the plotting region? If supplied, `expand` tells us to
    // expand the region by that many pixels in all directions.
    function isInPlottingRegion(offset, expand) {
      expand = expand || 0;
      var bounds = getPlotBounds();
      return offset.x < bounds.right + expand &&
             offset.x > bounds.left - expand &&
             offset.y < bounds.bottom + expand &&
             offset.y > bounds.top - expand;
    }

    // Given an offset, clip it to the plotting region as specified by
    // coordmap. If there is no coordmap, clip it to bounds of the DOM
    // element.
    function clipToPlottingRegion(offset) {
      var bounds = getPlotBounds();

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
        if (clip && !isInPlottingRegion(offset)) return;

        var coords = offsetToScaledCoords(offset);
        coords[".nonce"] = Math.random();
        exports.onInputChange(inputId, coords);
      };
    }

    // ----------------------------------------------------------
    // Handler creators for click, hover, brush.
    // Each of these returns an object with a few public members. These public
    // members are callbacks that are meant to be bound to events on $el with
    // the same name (like 'mousedown').
    // ----------------------------------------------------------

    function createClickHandler(inputId) {
      var clickInfoSender =  mouseCoordinateSender(inputId, opts.clickClip);

      return {
        mousedown: function(e) {
          // Listen for left mouse button only
          if (e.which !== 1) return;
          clickInfoSender(e);
        },
        onRemoveImg: function() { clickInfoSender(null); }
      };
    }

    function createHoverHandler(inputId) {
      var sendHoverInfo = mouseCoordinateSender(inputId, opts.hoverClip);

      var hoverInfoSender;
      if (opts.hoverDelayType === 'throttle')
        hoverInfoSender = new Throttler(null, sendHoverInfo, opts.hoverDelay);
      else
        hoverInfoSender = new Debouncer(null, sendHoverInfo, opts.hoverDelay);

      return {
        mousemove:   function(e) { hoverInfoSender.normalCall(e); },
        onRemoveImg: function()  { hoverInfoSender.immediateCall(null); }
      };
    }

    // Returns a brush handler object. This has three public functions:
    // mousedown, mousemove, and onRemoveImg.
    function createBrushHandler(inputId) {
      // Parameter: expand the area in which a brush can be started, by this
      // many pixels in all directions.
      var expandPixels = 20;

      // Object that encapsulates brush state
      var brush = {
        // Current brushing and dragging state
        brushing: false,
        dragging: false,

        // Offset of last mouse down and up events
        down: { x: NaN, y: NaN },
        up: { x: NaN, y: NaN },

        // Bounding rectangle of the brush
        bounds: {
          xmin: NaN,
          xmax: NaN,
          ymin: NaN,
          ymax: NaN
        },

        // The bounds at the start of a drag
        dragStartBounds: {
          xmin: NaN,
          xmax: NaN,
          ymin: NaN,
          ymax: NaN
        },

        // div that displays the brush
        $div: null,

        reset: function() {
          this.brushing = false;
          this.dragging = false;
          this.down = { x: NaN, y: NaN };
          this.up   = { x: NaN, y: NaN };
          this.bounds = {
            xmin: NaN,
            xmax: NaN,
            ymin: NaN,
            ymax: NaN
          };
          this.dragStartBounds = {
            xmin: NaN,
            xmax: NaN,
            ymin: NaN,
            ymax: NaN
          };

          if (this.$div)
            this.$div.remove();

          return this;
        },

        // If there's an existing brush div, use that div to set the new
        // brush's settings.
        importOldBrush: function() {
          var oldDiv = $el.find('#' + el.id + '_brush');
          if (oldDiv.length === 0)
            return;

          var elOffset = $el.offset();
          var divOffset = oldDiv.offset();
          this.bounds = {
            xmin: divOffset.left - elOffset.left,
            xmax: divOffset.left - elOffset.left + oldDiv.width(),
            ymin: divOffset.top - elOffset.top,
            ymax: divOffset.top - elOffset.top + oldDiv.height()
          };

          this.$div = oldDiv;
        },

        // Return true if the offset is inside min/max coords
        isInsideBrush: function(offset) {
          var bounds = this.bounds;
          return offset.x <= bounds.xmax && offset.x >= bounds.xmin &&
                 offset.y <= bounds.ymax && offset.y >= bounds.ymin;
        },

        // Sets the bounds of the brush, given a bounding box. This knows
        // whether we're brushing in the x, y, or xy directions and sets
        // bounds accordingly.
        setBounds: function(box) {
          var plotBounds = getPlotBounds();

          var min = { x: box.xmin, y: box.ymin };
          var max = { x: box.xmax, y: box.ymax };

          if (opts.brushClip) {
            min = clipToPlottingRegion(min);
            max = clipToPlottingRegion(max);
          }

          if (opts.brushDirection === 'xy') {
            // No change

          } else if (opts.brushDirection === 'x') {
            // Extend top and bottom of plotting area
            min.y = plotBounds.top;
            max.y = plotBounds.bottom;

          } else if (opts.brushDirection === 'y') {
            min.x = plotBounds.left;
            max.x = plotBounds.right;
          }

          this.bounds = {
            xmin: min.x,
            xmax: max.x,
            ymin: min.y,
            ymax: max.y
          };
        },

        // Add a new div representing the brush.
        addDiv: function() {
          if (this.$div) this.$div.remove();

          this.$div = $(document.createElement('div'))
            .attr('id', el.id + '_brush')
            .css({
              'background-color': opts.brushFill,
              'opacity': opts.brushOpacity,
              'pointer-events': 'none',
              'position': 'absolute'
            });

          var borderStyle = '1px solid ' + opts.brushStroke;
          if (opts.brushDirection === 'xy') {
            this.$div.css({
              'border': borderStyle
            });
          } else if (opts.brushDirection === 'x') {
            this.$div.css({
              'border-left': borderStyle,
              'border-right': borderStyle
            });
          } else if (opts.brushDirection === 'y') {
            this.$div.css({
              'border-top': borderStyle,
              'border-bottom': borderStyle
            });
          }


          $el.append(this.$div);
          this.$div.offset({x:0, y:0}).width(0).height(0).show();
        },

        // Update the brush div to reflect the current brush bounds.
        updateDiv: function() {
          // Need parent offset relative to page to calculate mouse offset
          // relative to page.
          var imgOffset = $el.offset();
          var b = this.bounds;
          this.$div.offset({
              top: imgOffset.top + b.ymin,
              left: imgOffset.left + b.xmin
            })
            .width(b.xmax - b.xmin)
            .height(b.ymax - b.ymin)
            .show();
        },

        startBrushing: function() {
          this.brushing = true;
          this.addDiv();

          this.setBounds(findBox(this.down, this.down));
          this.updateDiv();
        },

        brushTo: function(offset) {
          this.setBounds(findBox(this.down, offset));
          this.updateDiv();
        },

        stopBrushing: function() {
          this.brushing = false;

          // Save the final bounding box of the brush
          this.setBounds(findBox(this.down, this.up));
        },

        startDragging: function() {
          this.dragging = true;
          this.dragStartBounds = $.extend({}, this.bounds);
        },

        dragTo: function(offset) {
          // How far the brush was dragged
          var dx = offset.x - this.down.x;
          var dy = offset.y - this.down.y;

          // Calculate what new start/end positions would be, before clipping.
          var start = this.dragStartBounds;
          var newBounds = {
            xmin: start.xmin + dx,
            xmax: start.xmax + dx,
            ymin: start.ymin + dy,
            ymax: start.ymax + dy
          };

          // Clip to the plotting area
          if (opts.brushClip) {
            var plotBounds = getPlotBounds();

            // Convert to format for shiftToRange
            var xvals = [ newBounds.xmin, newBounds.xmax ];
            var yvals = [ newBounds.ymin, newBounds.ymax ];

            xvals = shiftToRange(xvals, plotBounds.left, plotBounds.right); 
            yvals = shiftToRange(yvals, plotBounds.top, plotBounds.bottom);

            // Convert back to bounds format
            newBounds = {
              xmin: xvals[0],
              xmax: xvals[1],
              ymin: yvals[0],
              ymax: yvals[1]
            };
          }

          this.setBounds(newBounds);
          this.updateDiv();
        },

        stopDragging: function() {
          this.dragging = false;
        }
      };


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
        var bounds = brush.bounds;

        // We're in a new or reset state
        if (isNaN(bounds.xmin)) {
          exports.onInputChange(inputId, null);
          return;
        }

        // Transform coordinates of brush to data space
        var min = offsetToScaledCoords({ x: bounds.xmin, y: bounds.ymin }, opts.brushClip);
        var max = offsetToScaledCoords({ x: bounds.xmax, y: bounds.ymax }, opts.brushClip);

        // Because the x and y directions of the pixel space may differ from
        // the x and y directions of the data space, we need to recalculate
        // the min and max.
        var coords = {
          xmin: Math.min(min.x, max.x),
          xmax: Math.max(min.x, max.x),
          ymin: Math.min(min.y, max.y),
          ymax: Math.max(min.y, max.y)
        };

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
        if (brush.brushing || brush.dragging) return;

        // Listen for left mouse button only
        if (e.which !== 1) return;

        var offset = mouseOffset(e);

        // Ignore mousedown events outside of plotting region, expanded by
        // a number of pixels specified in expandPixels.
        if (opts.brushClip && !isInPlottingRegion(offset, expandPixels))
          return;

        brush.up = { x: NaN, y: NaN };
        brush.down = offset;

        if (brush.isInsideBrush(offset)) {
          brush.startDragging(offset);
          setCursorStyle('grabbing');

          // Attach the move and up handlers to the window so that they respond
          // even when the mouse is moved outside of the image.
          $(document)
            .on('mousemove.image_brush', mousemoveDragging)
            .on('mouseup.image_brush', mouseupDragging );

        } else {
          brush.startBrushing(clipToPlottingRegion(offset));

          // Attach the move and up handlers to the window so that they respond
          // even when the mouse is moved outside of the image.
          $(document)
            .on('mousemove.image_brush', mousemoveBrushing)
            .on('mouseup.image_brush', mouseupBrushing);
        }
      }

      // This sets the cursor style when it's in the el
      function mousemove(e) {
        var offset = mouseOffset(e);

        if (!(brush.brushing || brush.dragging)) {
          // Set the cursor depending on where it is
          if (brush.isInsideBrush(offset)) {
            setCursorStyle('grabbable');
          } else if (isInPlottingRegion(offset, expandPixels)) {
            setCursorStyle('crosshair');
          } else {
            setCursorStyle(null);
          }
        }
      }

      // mousemove handlers while brushing or dragging
      function mousemoveBrushing(e) {
        brush.brushTo(mouseOffset(e));
        brushInfoSender.normalCall();
      }

      function mousemoveDragging(e) {
        brush.dragTo(mouseOffset(e));
        brushInfoSender.normalCall();
      }

      // mouseup handlers while brushing or dragging
      function mouseupBrushing(e) {
        // Listen for left mouse button only
        if (e.which !== 1) return;

        $(document)
          .off('mousemove.image_brush')
          .off('mouseup.image_brush');

        brush.up = mouseOffset(e);

        brush.stopBrushing();
        setCursorStyle('crosshair');

        // If the brush didn't go anywhere, hide the brush, clear value,
        // and return.
        if (brush.down.x === brush.up.x && brush.down.y === brush.up.y) {
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

        brush.up = mouseOffset(e);

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
    }


    // This object provides two public event listeners: mousedown, and
    // dblclickIE8.
    // We need to make sure that, when the image is listening for double-
    // clicks, that a double-click doesn't trigger two click events. We'll
    // trigger custom mousedown2 and dblclick2 events with this mousedown
    // listener.
    var clickInfo = {
      clickCount: 0,
      clickTimer: null,
      pending_e: null,    // A pending mousedown2 event

      // Create a new event of type eventType (like 'mousedown2'), and trigger
      // it with the information stored in this.e.
      triggerEvent: function(newEventType, e) {
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
      },

      triggerPendingMousedown2: function() {
        // It's possible that between the scheduling of a mousedown2 and the
        // time this callback is executed, someone else triggers a
        // mousedown2, so check for that.
        if (this.pending_e) {
          this.triggerEvent('mousedown2', this.pending_e);
          this.pending_e = null;
        }
      },

      // Set a timer to trigger a mousedown2 event, using information from the
      // last recorded mousdown event.
      scheduleMousedown2: function(e) {
        this.pending_e = e;

        var self = this;
        this.clickTimer = setTimeout(function() {
          self.triggerPendingMousedown2();
        }, opts.dblclickDelay);
      },

      mousedown: function(e) {
        // Listen for left mouse button only
        if (e.which !== 1) return;

        // If no dblclick listener, immediately trigger a mousedown2 event.
        if (!opts.dblclickId) {
          this.triggerEvent('mousedown2', e);
          return;
        }

        // If there's a dblclick listener, make sure not to count this as a
        // click on the first mousedown; we need to wait for the dblclick
        // delay before we can be sure this click was a single-click.
        if (this.pending_e === null) {
          this.scheduleMousedown2(e);

        } else {
          clearTimeout(this.clickTimer);

          // If second click is too far away, it doesn't count as a double
          // click. Instead, immediately trigger a mousedown2 for the previous
          // click, and set this click as a new first click.
          if (this.pending_e &&
              Math.abs(this.pending_e.offsetX - e.offsetX) > 2 ||
              Math.abs(this.pending_e.offsetY - e.offsetY) > 2) {

            this.triggerPendingMousedown2();
            this.scheduleMousedown2(e);

          } else {
            // The second click was close to the first one. If it happened
            // within specified delay, trigger our custom 'dblclick2' event.
            this.pending_e = null;
            this.triggerEvent('dblclick2', e);
          }
        }
      },

      // IE8 needs a special hack because when you do a double-click it doesn't
      // trigger the click event twice - it directly triggers dblclick.
      dblclickIE8: function(e) {
        e.which = 1;   // In IE8, e.which is 0 instead of 1. ???
        this.triggerEvent('dblclick2', e);
      }
    };

    $el.on('mousedown.image_output', function(e) { clickInfo.mousedown(e); });

    if (browser.isIE && browser.IEVersion === 8) {
      $el.on('dblclick.image_output', function(e) { clickInfo.dblclickIE8(e); });
    }

    // ----------------------------------------------------------
    // Register the various event handlers
    // ----------------------------------------------------------
    if (opts.clickId) {
      var clickHandler = createClickHandler(opts.clickId);
      $el.on('mousedown2.image_output', clickHandler.mousedown);

      // When img is removed, do housekeeping: clear $el's mouse listener and
      // call the handler's onRemoveImg callback.
      $img.on('remove', clickHandler.onRemoveImg);
    }

    if (opts.dblclickId) {
      // We'll use the clickHandler's mousedown function, but register it to
      // our custom 'dblclick2' event.
      var dblclickHandler = createClickHandler(opts.dblclickId);
      $el.on('dblclick2.image_output', dblclickHandler.mousedown);

      $img.on('remove', dblclickHandler.onRemoveImg);
    }

    if (opts.hoverId) {
      var hoverHandler = createHoverHandler(opts.hoverId);
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

      var brushHandler = createBrushHandler(opts.brushId);
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
