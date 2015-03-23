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
    var date = $(el).find('input').data('datepicker').getUTCDate();
    return this._formatDate(date);
  },
  // value must be an unambiguous string like '2001-01-01', or a Date object.
  setValue: function(el, value) {
    var date = this._newDate(value);
    // If date is invalid, do nothing
    if (isNaN(date))
      return;

    $(el).find('input').datepicker('update', date);
  },
  getState: function(el) {
    var $el = $(el);
    var $input = $el.find('input');

    var min = $input.data('datepicker').startDate;
    var max = $input.data('datepicker').endDate;

    // Stringify min and max. If min and max aren't set, they will be
    // -Infinity and Infinity; replace these with null.
    min = (min === -Infinity) ? null : this._formatDate(min);
    max = (max ===  Infinity) ? null : this._formatDate(max);

    // startViewMode is stored as a number; convert to string
    var startview = $input.data('datepicker').startViewMode;
    if      (startview === 2)  startview = 'decade';
    else if (startview === 1)  startview = 'year';
    else if (startview === 0)  startview = 'month';

    return {
      label:       $el.find('label[for="' + $escape(el.id) + '"]').text(),
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

    if (data.hasOwnProperty('value'))
      this.setValue(el, data.value);

    if (data.hasOwnProperty('label'))
      $(el).find('label[for="' + $escape(el.id) + '"]').text(data.label);

    if (data.hasOwnProperty('min'))
      this._setMin($input[0], data.min);

    if (data.hasOwnProperty('max'))
      this._setMax($input[0], data.max);

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

    var date = $input.data('initial-date');
    // If initial_date is null, set to current date
    if (date === undefined || date === null) {
      // Get local date, but as UTC
      date = this._dateAsUTC(new Date());
    }

    this.setValue(el, date);

    // Set the start and end dates, from min-date and max-date. These always
    // use yyyy-mm-dd format, instead of bootstrap-datepicker's built-in
    // support for date-startdate and data-enddate, which use the current
    // date format.
    this._setMin($input[0], $input.data('min-date'));
    this._setMax($input[0], $input.data('max-date'));
  },
  // Given a Date object, return a string in yyyy-mm-dd format, using the
  // UTC date. This may be a day off from the date in the local time zone.
  _formatDate: function(date) {
    if (date instanceof Date) {
      return date.getUTCFullYear() + '-' +
             padZeros(date.getUTCMonth()+1, 2) + '-' +
             padZeros(date.getUTCDate(), 2);

    } else {
      return null;
    }
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
  // null will unset.
  _setMin: function(el, date) {
    if (date === null) {
      $(el).datepicker('setStartDate', null);

    } else {
      date = this._newDate(date);
      if (!isNaN(date))
        $(el).datepicker('setStartDate', date);
    }
  },
  // Given an unambiguous date string or a Date object, set the max (end) date
  // null will unset.
  _setMax: function(el, date) {
    if (date === null) {
      $(el).datepicker('setEndDate', null);

    } else {
      date = this._newDate(date);
      if (!isNaN(date))
        $(el).datepicker('setEndDate', date);
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

    return new Date(d.getTime());
  },
  // Given a Date object, return a Date object which has the same "clock time"
  // in UTC. For example, if input date is 2013-02-01 23:00:00 GMT-0600 (CST),
  // output will be 2013-02-01 23:00:00 UTC. Note that the JS console may
  // print this in local time, as "Sat Feb 02 2013 05:00:00 GMT-0600 (CST)".
  _dateAsUTC: function(date) {
    return new Date(date.getTime() - date.getTimezoneOffset() * 60000);
  }
});
inputBindings.register(dateInputBinding, 'shiny.dateInput');
