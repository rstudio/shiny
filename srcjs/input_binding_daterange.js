  var dateRangeInputBinding = {};
  $.extend(dateRangeInputBinding, dateInputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-date-range-input');
    },
    // Return the date in an unambiguous format, yyyy-mm-dd (as opposed to a
    // format like mm/dd/yyyy)
    getValue: function(el) {
      var $inputs = $(el).find('input');
      var start = $inputs.eq(0).data('datepicker').getUTCDate();
      var end   = $inputs.eq(1).data('datepicker').getUTCDate();

      return [this._formatDate(start), this._formatDate(end)];
    },
    // value must be an array of unambiguous strings like '2001-01-01', or
    // Date objects.
    setValue: function(el, value) {
      if (!(value instanceof Array)) {
        return;
      }

      // Get the start and end input objects
      var $inputs = $(el).find('input');

      // If value is undefined, don't try to set
      if (value[0] !== undefined) {
        var start = this._newDate(value[0]);
        $inputs.eq(0).datepicker('update', start);
      }
      if (value[1] !== undefined) {
        var end = this._newDate(value[1]);
        $inputs.eq(1).datepicker('update', end);
      }
    },
    getState: function(el) {
      var $el = $(el);
      var $inputs     = $el.find('input');
      var $startinput = $inputs.eq(0);
      var $endinput   = $inputs.eq(1);

      // For many of the properties, assume start and end have the same values
      var min = $startinput.data('datepicker').startDate;
      var max = $startinput.data('datepicker').endDate;

      // Stringify min and max. If min and max aren't set, they will be
      // -Infinity and Infinity; replace these with null.
      min = (min === -Infinity) ? null : this._formatDate(min);
      max = (max ===  Infinity) ? null : this._formatDate(max);

      // startViewMode is stored as a number; convert to string
      var startview = $startinput.data('datepicker').startViewMode;
      if      (startview === 2)  startview = 'decade';
      else if (startview === 1)  startview = 'year';
      else if (startview === 0)  startview = 'month';

      return {
        label:       $el.find('label[for="' + $escape(el.id) + '"]').text(),
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

      if (data.hasOwnProperty('value'))
        this.setValue(el, data.value);

      if (data.hasOwnProperty('label'))
        $el.find('label[for="' + $escape(el.id) + '"]').text(data.label);

      if (data.hasOwnProperty('min')) {
        this._setMin($startinput[0], data.min);
        this._setMin($endinput[0],   data.min);
      }

      if (data.hasOwnProperty('max')) {
        this._setMax($startinput[0], data.max);
        this._setMax($endinput[0],   data.max);
      }

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

      this.setValue(el, [start, end]);

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
    }
  });
  inputBindings.register(dateRangeInputBinding, 'shiny.dateRangeInput');
