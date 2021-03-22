// Necessary to get hidden sliders to send their updated values
function forceIonSliderUpdate(slider) {
  if (slider.$cache && slider.$cache.input)
    slider.$cache.input.trigger('change');
  else
    console.log("Couldn't force ion slider to update");
}

function getTypePrettifyer(dataType, timeFormat, timezone) {
  var timeFormatter;
  var prettify;
  if (dataType === 'date') {
    timeFormatter = strftime.utc();
    prettify = function(num) {
      return timeFormatter(timeFormat, new Date(num));
    };

  } else if (dataType === 'datetime') {
    if (timezone)
      timeFormatter = strftime.timezone(timezone);
    else
      timeFormatter = strftime;

    prettify = function(num) {
      return timeFormatter(timeFormat, new Date(num));
    };

  } else {
    // The default prettify function for ion.rangeSlider adds thousands
    // separators after the decimal mark, so we have our own version here.
    // (#1958)
    prettify = function(num) {
      // When executed, `this` will refer to the `IonRangeSlider.options`
      // object.
      return formatNumber(num, this.prettify_separator);
    };
  }
  return prettify;
}

var sliderInputBinding = {};
$.extend(sliderInputBinding, textInputBinding, {
  find: function(scope) {
    // Check if ionRangeSlider plugin is loaded
    if (!$.fn.ionRangeSlider)
      return [];

    return $(scope).find('input.js-range-slider');
  },
  getType: function(el) {
    var dataType = $(el).data('data-type');
    if (dataType === 'date')
      return 'shiny.date';
    else if (dataType === 'datetime')
      return 'shiny.datetime';
    else
      return false;
  },
  getValue: function(el) {
    var $el = $(el);
    var result = $(el).data('ionRangeSlider').result;

    // Function for converting numeric value from slider to appropriate type.
    var convert;
    var dataType = $el.data('data-type');
    if (dataType === 'date') {
      convert = function(val) {
        return formatDateUTC(new Date(+val));
      };
    } else if (dataType === 'datetime') {
      convert = function(val) {
        // Convert ms to s
        return +val / 1000;
      };
    } else {
      convert = function(val) { return +val; };
    }

    if (this._numValues(el) === 2) {
      return [convert(result.from), convert(result.to)];
    }
    else {
      return convert(result.from);
    }

  },
  setValue: function(el, value) {
    var $el = $(el);
    var slider = $el.data('ionRangeSlider');

    $el.data('immediate', true);
    try {
      if (this._numValues(el) === 2 && value instanceof Array) {
        slider.update({ from: value[0], to: value[1] });
      } else {
        slider.update({ from: value });
      }

      forceIonSliderUpdate(slider);
    } finally {
      $el.data('immediate', false);
    }
  },
  subscribe: function(el, callback) {
    $(el).on('change.sliderInputBinding', function(event) {
      callback(!$(el).data('immediate') && !$(el).data('animating'));
    });
  },
  unsubscribe: function(el) {
    $(el).off('.sliderInputBinding');
  },
  receiveMessage: function(el, data) {
    var $el = $(el);
    var slider = $el.data('ionRangeSlider');
    var msg = {};

    if (data.hasOwnProperty('value')) {
      if (this._numValues(el) === 2 && data.value instanceof Array) {
        msg.from = data.value[0];
        msg.to = data.value[1];
      } else {
        msg.from = data.value;
      }
    }
    var sliderFeatures = ['min', 'max', 'step'];
    for (var i = 0; i < sliderFeatures.length; i++) {
      var feats = sliderFeatures[i];
      if (data.hasOwnProperty(feats)) {
        msg[feats] = data[feats];
      }
    }

    updateLabel(data.label, this._getLabelNode(el));

    var domElements = ['data-type', 'time-format', 'timezone'];
    for (var i = 0; i < domElements.length; i++) {
      var elem = domElements[i];
      if (data.hasOwnProperty(elem)) {
        $el.data(elem, data[elem]);
      }
    }

    var dataType = $el.data('data-type');
    var timeFormat = $el.data('time-format');
    var timezone = $el.data('timezone');

    msg.prettify = getTypePrettifyer(dataType, timeFormat, timezone);

    $el.data('immediate', true);
    try {
      slider.update(msg);
      forceIonSliderUpdate(slider);
    } finally {
      $el.data('immediate', false);
    }
  },
  getRatePolicy: function() {
    return {
      policy: 'debounce',
      delay: 250
    };
  },
  getState: function(el) {
  },
  initialize: function(el) {
    var opts = {};
    var $el = $(el);
    var dataType = $el.data('data-type');
    var timeFormat = $el.data('time-format');
    var timezone = $el.data('timezone');

    opts.prettify = getTypePrettifyer(dataType, timeFormat, timezone);

    $el.ionRangeSlider(opts);
  },
  _getLabelNode: function(el) {
    return $(el).parent().find('label[for="' + $escape(el.id) + '"]');
  },
  // Number of values; 1 for single slider, 2 for range slider
  _numValues: function(el) {
    if ($(el).data('ionRangeSlider').options.type === 'double')
      return 2;
    else
      return 1;
  }
});
inputBindings.register(sliderInputBinding, 'shiny.sliderInput');


// Format numbers for nicer output.
// formatNumber(1234567.12345)           === "1,234,567.12345"
// formatNumber(1234567.12345, ".", ",") === "1.234.567,12345"
// formatNumber(1000, " ")               === "1 000"
// formatNumber(20)                      === "20"
// formatNumber(1.2345e24)               === "1.2345e+24"
function formatNumber(num, thousand_sep = ",", decimal_sep = ".") {
  let parts = num.toString().split(".");

  // Add separators to portion before decimal mark.
  parts[0] = parts[0].replace(/(\d{1,3}(?=(?:\d\d\d)+(?!\d)))/g, "$1" + thousand_sep);

  if (parts.length === 1)
    return parts[0];
  else if (parts.length === 2)
    return parts[0] + decimal_sep + parts[1];
  else
    return "";
};

$(document).on('click', '.slider-animate-button', function(evt) {
  evt.preventDefault();
  var self = $(this);
  var target = $('#' + $escape(self.attr('data-target-id')));
  var startLabel = 'Play';
  var stopLabel = 'Pause';
  var loop = self.attr('data-loop') !== undefined &&
             !/^\s*false\s*$/i.test(self.attr('data-loop'));
  var animInterval = self.attr('data-interval');
  if (isNaN(animInterval))
    animInterval = 1500;
  else
    animInterval = +animInterval;

  if (!target.data('animTimer')) {
    var slider;
    var timer;

    // Separate code paths:
    // Backward compatible code for old-style jsliders (Shiny <= 0.10.2.2),
    // and new-style ionsliders.
    if (target.hasClass('jslider')) {
      slider = target.slider();

      // If we're currently at the end, restart
      if (!slider.canStepNext())
        slider.resetToStart();

      timer = setInterval(function() {
        if (loop && !slider.canStepNext()) {
          slider.resetToStart();
        }
        else {
          slider.stepNext();
          if (!loop && !slider.canStepNext()) {
            self.click(); // stop the animation
          }
        }
      }, animInterval);

    } else {
      slider = target.data('ionRangeSlider');
      // Single sliders have slider.options.type == "single", and only the
      // `from` value is used. Double sliders have type == "double", and also
      // use the `to` value for the right handle.
      var sliderCanStep = function() {
        if (slider.options.type === "double")
          return slider.result.to < slider.result.max;
        else
          return slider.result.from < slider.result.max;
      };
      var sliderReset = function() {
        var val = { from: slider.result.min };
        // Preserve the current spacing for double sliders
        if (slider.options.type === "double")
          val.to = val.from + (slider.result.to - slider.result.from);

        slider.update(val);
        forceIonSliderUpdate(slider);
      };
      var sliderStep = function() {
        // Don't overshoot the end
        var val = {
          from: Math.min(slider.result.max, slider.result.from + slider.options.step)
        };
        if (slider.options.type === "double")
          val.to = Math.min(slider.result.max, slider.result.to + slider.options.step);

        slider.update(val);
        forceIonSliderUpdate(slider);
      };

      // If we're currently at the end, restart
      if (!sliderCanStep())
        sliderReset();

      timer = setInterval(function() {
        if (loop && !sliderCanStep()) {
          sliderReset();
        }
        else {
          sliderStep();
          if (!loop && !sliderCanStep()) {
            self.click(); // stop the animation
          }
        }
      }, animInterval);
    }

    target.data('animTimer', timer);
    self.attr('title', stopLabel);
    self.addClass('playing');
    target.data('animating', true);
  }
  else {
    clearTimeout(target.data('animTimer'));
    target.removeData('animTimer');
    self.attr('title', startLabel);
    self.removeClass('playing');
    target.removeData('animating');
  }
});
