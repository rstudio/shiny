var dateRangeInputBinding = {};
$.extend(dateRangeInputBinding, dateInputBinding, {
  find: function(scope) {
    return $(scope).find('.shiny-date-range-input');
  },
  // Return the date in an unambiguous format, yyyy-mm-dd (as opposed to a
  // format like mm/dd/yyyy)
  getValue: function(el) {
    var $inputs = $(el).find('input');
    var start = $inputs.eq(0).bsDatepicker('getUTCDate');
    var end   = $inputs.eq(1).bsDatepicker('getUTCDate');

    return [formatDateUTC(start), formatDateUTC(end)];
  },
  // value must be an object, with optional fields `start` and `end`. These
  // should be unambiguous strings like '2001-01-01', or Date objects.
  setValue: function(el, value) {
    if (!(value instanceof Object)) {
      return;
    }

    // Get the start and end input objects
    var $inputs = $(el).find('input');

    // If value is undefined, don't try to set
    // null will remove the current value
    if (value.start !== undefined) {
      if (value.start === null) {
        $inputs.eq(0).val('').bsDatepicker('update');
      } else {
        var start = this._newDate(value.start);
        $inputs.eq(0).bsDatepicker('setUTCDate', start);
      }
    }
    if (value.end !== undefined) {
      if (value.end === null) {
        $inputs.eq(1).val('').bsDatepicker('update');
      } else {
        var end = this._newDate(value.end);
        $inputs.eq(1).bsDatepicker('setUTCDate', end);
      }
    }
  },
  getState: function(el) {
    var $el = $(el);
    var $inputs     = $el.find('input');
    var $startinput = $inputs.eq(0);
    var $endinput   = $inputs.eq(1);

    // For many of the properties, assume start and end have the same values
    var min = $startinput.bsDatepicker('getStartDate');
    var max = $startinput.bsDatepicker('getEndDate');

    // Stringify min and max. If min and max aren't set, they will be
    // -Infinity and Infinity; replace these with null.
    min = (min === -Infinity) ? null : formatDateUTC(min);
    max = (max ===  Infinity) ? null : formatDateUTC(max);

    // startViewMode is stored as a number; convert to string
    var startview = $startinput.data('datepicker').startView;
    if      (startview === 2)  startview = 'decade';
    else if (startview === 1)  startview = 'year';
    else if (startview === 0)  startview = 'month';

    return {
      label:       this._getLabelNode(el).text(),
      value:       this.getValue(el),
      valueString: [ $startinput.val(), $endinput.val() ],
      min:         min,
      max:         max,
      weekstart:   $startinput.data('datepicker').weekStart,
      format:      this._formatToString($startinput.data('datepicker').format),
      language:    $startinput.data('datepicker').language,
      startview:   startview
    };
  },
  receiveMessage: function(el, data) {
    var $el = $(el);
    var $inputs     = $el.find('input');
    var $startinput = $inputs.eq(0);
    var $endinput   = $inputs.eq(1);

    updateLabel(data.label, this._getLabelNode(el));

    if (data.hasOwnProperty('min')) {
      this._setMin($startinput[0], data.min);
      this._setMin($endinput[0],   data.min);
    }

    if (data.hasOwnProperty('max')) {
      this._setMax($startinput[0], data.max);
      this._setMax($endinput[0],   data.max);
    }

    // Must set value only after min and max have been set. If new value is
    // outside the bounds of the previous min/max, then the result will be a
    // blank input.
    if (data.hasOwnProperty('value'))
      this.setValue(el, data.value);

    $el.trigger('change');
  },
  initialize: function(el) {
    var $el = $(el);
    var $inputs     = $el.find('input');
    var $startinput = $inputs.eq(0);
    var $endinput   = $inputs.eq(1);

    var start = $startinput.data('initial-date');
    var end   = $endinput.data('initial-date');

    // If empty/null, use local date, but as UTC
    if (start === undefined || start === null)
      start = this._dateAsUTC(new Date());

    if (end === undefined || end === null)
      end = this._dateAsUTC(new Date());

    this.setValue(el, { "start": start, "end": end });

    // // Set the start and end dates, from min-date and max-date. These always
    // // use yyyy-mm-dd format, instead of bootstrap-datepicker's built-in
    // // support for date-startdate and data-enddate, which use the current
    // // date format.
    this._setMin($startinput[0], $startinput.data('min-date'));
    this._setMin($endinput[0],   $startinput.data('min-date'));
    this._setMax($startinput[0], $endinput.data('max-date'));
    this._setMax($endinput[0],   $endinput.data('max-date'));
  },
  subscribe: function(el, callback) {
    $(el).on('keyup.dateRangeInputBinding input.dateRangeInputBinding', function(event) {
      // Use normal debouncing policy when typing
      callback(true);
    });
    $(el).on('changeDate.dateRangeInputBinding change.dateRangeInputBinding', function(event) {
      // Send immediately when clicked
      callback(false);
    });
  },
  unsubscribe: function(el) {
    $(el).off('.dateRangeInputBinding');
  },
  _getLabelNode: function(el) {
    return $(el).find('label[for="' + $escape(el.id) + '"]');
  },
});
inputBindings.register(dateRangeInputBinding, 'shiny.dateRangeInput');
