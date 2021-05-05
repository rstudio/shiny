var dateInputBinding = new InputBinding();
$.extend(dateInputBinding, {
  find: function(scope) {
    return $(scope).find('.shiny-date-input');
  },
  getType: function(el) {
    return "shiny.date";
  },
  // Return the date in an unambiguous format, yyyy-mm-dd (as opposed to a
  // format like mm/dd/yyyy)
  getValue: function(el) {
    var date = $(el).find('input').bsDatepicker('getUTCDate');
    return formatDateUTC(date);
  },
  // value must be an unambiguous string like '2001-01-01', or a Date object.
  setValue: function(el, value) {
    // R's NA, which is null here will remove current value
    if (value === null) {
      $(el).find('input').val('').bsDatepicker('update');
      return;
    }

    var date = this._newDate(value);
    // If date is invalid, do nothing
    if (isNaN(date))
      return;

    $(el).find('input').bsDatepicker('setUTCDate', date);
  },
  getState: function(el) {
    var $el = $(el);
    var $input = $el.find('input');

    var min = $input.data('datepicker').startDate;
    var max = $input.data('datepicker').endDate;

    // Stringify min and max. If min and max aren't set, they will be
    // -Infinity and Infinity; replace these with null.
    min = (min === -Infinity) ? null : formatDateUTC(min);
    max = (max ===  Infinity) ? null : formatDateUTC(max);

    // startViewMode is stored as a number; convert to string
    var startview = $input.data('datepicker').startViewMode;
    if      (startview === 2)  startview = 'decade';
    else if (startview === 1)  startview = 'year';
    else if (startview === 0)  startview = 'month';

    return {
      label:       this._getLabelNode(el).text(),
      value:       this.getValue(el),
      valueString: $input.val(),
      min:         min,
      max:         max,
      language:    $input.data('datepicker').language,
      weekstart:   $input.data('datepicker').weekStart,
      format:      this._formatToString($input.data('datepicker').format),
      startview:   startview
    };
  },
  receiveMessage: function(el, data) {
    var $input = $(el).find('input');

    updateLabel(data.label, this._getLabelNode(el));

    if (data.hasOwnProperty('min'))
      this._setMin($input[0], data.min);

    if (data.hasOwnProperty('max'))
      this._setMax($input[0], data.max);

    // Must set value only after min and max have been set. If new value is
    // outside the bounds of the previous min/max, then the result will be a
    // blank input.
    if (data.hasOwnProperty('value'))
      this.setValue(el, data.value);

    $(el).trigger('change');
  },
  subscribe: function(el, callback) {
    $(el).on('keyup.dateInputBinding input.dateInputBinding', function(event) {
      // Use normal debouncing policy when typing
      callback(true);
    });
    $(el).on('changeDate.dateInputBinding change.dateInputBinding', function(event) {
      // Send immediately when clicked
      callback(false);
    });
  },
  unsubscribe: function(el) {
    $(el).off('.dateInputBinding');
  },
  getRatePolicy: function() {
    return {
      policy: 'debounce',
      delay: 250
    };
  },
  initialize: function(el) {
    var $input = $(el).find('input');

    // The challenge with dates is that we want them to be at 00:00 in UTC so
    // that we can do comparisons with them. However, the Date object itself
    // does not carry timezone information, so we should call _floorDateTime()
    // on Dates as soon as possible so that we know we're always working with
    // consistent objects.

    var date = $input.data('initial-date');
    // If initial_date is null, set to current date
    if (date === undefined || date === null) {
      // Get local date, but normalized to beginning of day in UTC.
      date = this._floorDateTime(this._dateAsUTC(new Date()));
    }

    this.setValue(el, date);

    // Set the start and end dates, from min-date and max-date. These always
    // use yyyy-mm-dd format, instead of bootstrap-datepicker's built-in
    // support for date-startdate and data-enddate, which use the current
    // date format.
    if ($input.data('min-date') !== undefined) {
      this._setMin($input[0], $input.data('min-date'));
    }
    if ($input.data('max-date') !== undefined) {
      this._setMax($input[0], $input.data('max-date'));
    }
  },
  _getLabelNode: function(el) {
    return $(el).find('label[for="' + $escape(el.id) + '"]');
  },
  // Given a format object from a date picker, return a string
  _formatToString: function(format) {
    // Format object has structure like:
    // { parts: ['mm', 'dd', 'yy'], separators: ['', '/', '/' ,''] }
    var str = '';
    for (var i = 0; i < format.parts.length; i++) {
      str += format.separators[i] + format.parts[i];
    }
    str += format.separators[i];
    return str;
  },
  // Given an unambiguous date string or a Date object, set the min (start) date.
  // null will unset. undefined will result in no change,
  _setMin: function(el, date) {
    if (date === undefined)
      return;
    if (date === null) {
      $(el).bsDatepicker('setStartDate', null);
      return;
    }

    date = this._newDate(date);
    // If date parsing fails, do nothing
    if (date === null)
      return;

    if (isNaN(date))
      return;
    // Workarounds for
    // https://github.com/rstudio/shiny/issues/2335
    var curValue = $(el).bsDatepicker('getUTCDate');

    // Note that there's no `setUTCStartDate`, so we need to convert this Date.
    // It starts at 00:00 UTC, and we convert it to 00:00 in local time, which
    // is what's needed for `setStartDate`.
    $(el).bsDatepicker('setStartDate', this._UTCDateAsLocal(date));

    // If the new min is greater than the current date, unset the current date.
    if (date && curValue && date.getTime() > curValue.getTime()) {
      $(el).bsDatepicker('clearDates');
    } else {
      // Setting the date needs to be done AFTER `setStartDate`, because the
      // datepicker has a bug where calling `setStartDate` will clear the date
      // internally (even though it will still be visible in the UI) when a
      // 2-digit year format is used.
      // https://github.com/eternicode/bootstrap-datepicker/issues/2010
      $(el).bsDatepicker('setUTCDate', curValue);
    }
  },
  // Given an unambiguous date string or a Date object, set the max (end) date
  // null will unset.
  _setMax: function(el, date) {
    if (date === undefined)
      return;
    if (date === null) {
      $(el).bsDatepicker('setEndDate', null);
      return;
    }

    date = this._newDate(date);
    // If date parsing fails, do nothing
    if (date === null)
      return;

    if (isNaN(date))
      return;

    // Workaround for same issue as in _setMin.
    var curValue = $(el).bsDatepicker('getUTCDate');

    $(el).bsDatepicker('setEndDate', this._UTCDateAsLocal(date));

    // If the new min is greater than the current date, unset the current date.
    if (date && curValue && date.getTime() < curValue.getTime()) {
      $(el).bsDatepicker('clearDates');
    } else {
      $(el).bsDatepicker('setUTCDate', curValue);
    }
  },
  // Given a date string of format yyyy-mm-dd, return a Date object with
  // that date at 12AM UTC.
  // If date is a Date object, return it unchanged.
  _newDate: function(date) {
    if (date instanceof Date)
      return date;
    if (!date)
      return null;

    // Get Date object - this will be at 12AM in UTC, but may print
    // differently at the Javascript console.
    var d = parseDate(date);

    // If invalid date, return null
    if (isNaN(d))
      return null;

    return d;
  },
  // A Date can have any time during a day; this will return a new Date object
  // set to 00:00 in UTC.
  _floorDateTime: function(date) {
    date = new Date(date.getTime());
    date.setUTCHours(0, 0, 0, 0);
    return date;
  },
  // Given a Date object, return a Date object which has the same "clock time"
  // in UTC. For example, if input date is 2013-02-01 23:00:00 GMT-0600 (CST),
  // output will be 2013-02-01 23:00:00 UTC. Note that the JS console may
  // print this in local time, as "Sat Feb 02 2013 05:00:00 GMT-0600 (CST)".
  _dateAsUTC: function(date) {
    return new Date(date.getTime() - date.getTimezoneOffset() * 60000);
  },
  // The inverse of _dateAsUTC. This is needed to adjust time zones because
  // some bootstrap-datepicker methods only take local dates as input, and not
  // UTC.
  _UTCDateAsLocal: function(date) {
    return new Date(date.getTime() + date.getTimezoneOffset() * 60000);
  }
});
inputBindings.register(dateInputBinding, 'shiny.dateInput');
