// Necessary to get hidden sliders to send their updated values
function forceIonSliderUpdate(slider) {
  if (slider.$cache && slider.$cache.input)
    slider.$cache.input.trigger('change');
  else
    console.log("Couldn't force ion slider to update");
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

    if (this._numValues(el) == 2) {
      return [convert(result.from), convert(result.to)];
    }
    else {
      return convert(result.from);
    }

  },
  setValue: function(el, value) {
    var slider = $(el).data('ionRangeSlider');

    if (this._numValues(el) == 2 && value instanceof Array) {
      slider.update({ from: value[0], to: value[1] });
    } else {
      slider.update({ from: value });
    }
    forceIonSliderUpdate(slider);
  },
  subscribe: function(el, callback) {
    $(el).on('change.sliderInputBinding', function(event) {
      callback(!$(el).data('updating') && !$(el).data('animating'));
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
      if (this._numValues(el) == 2 && data.value instanceof Array) {
        msg.from = data.value[0];
        msg.to = data.value[1];
      } else {
        msg.from = data.value;
      }
    }
    if (data.hasOwnProperty('min'))  msg.min   = data.min;
    if (data.hasOwnProperty('max'))  msg.max   = data.max;
    if (data.hasOwnProperty('step')) msg.step  = data.step;

    if (data.hasOwnProperty('label'))
      $el.parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

    $el.data('updating', true);
    try {
      slider.update(msg);
      forceIonSliderUpdate(slider);
    } finally {
      $el.data('updating', false);
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
    var timeFormatter;

    // Set up formatting functions
    if (dataType === 'date') {
      timeFormatter = strftime.utc();
      opts.prettify = function(num) {
        return timeFormatter(timeFormat, new Date(num));
      };

    } else if (dataType === 'datetime') {
      var timezone = $el.data('timezone');
      if (timezone)
        timeFormatter = strftime.timezone(timezone);
      else
        timeFormatter = strftime;

      opts.prettify = function(num) {
        return timeFormatter(timeFormat, new Date(num));
      };
    }

    $el.ionRangeSlider(opts);
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
