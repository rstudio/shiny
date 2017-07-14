var OutputBinding = exports.OutputBinding = function() {};
(function() {
  // Returns a jQuery object or element array that contains the
  // descendants of scope that match this binding
  this.find = function(scope) { throw "Not implemented"; };

  this.getId = function(el) {
    return el['data-input-id'] || el.id;
  };

  this.onValueChange = function(el, data) {
    this.clearError(el);
    this.renderValue(el, data);
  };
  this.onValueError = function(el, err) {
    this.renderError(el, err);
  };
  this.renderError = function(el, err) {
    this.clearError(el);
    if (err.message === '') {
      // not really error, but we just need to wait (e.g. action buttons)
      $(el).empty();
      return;
    }
    var errClass = 'shiny-output-error';
    if (err.type !== null) {
      // use the classes of the error condition as CSS class names
      errClass = errClass + ' ' + $.map(asArray(err.type), function(type) {
        return errClass + '-' + type;
      }).join(' ');
    }
    $(el).addClass(errClass).text(err.message);
  };
  this.clearError = function(el) {
    $(el).attr('class', function(i, c) {
      return c.replace(/(^|\s)shiny-output-error\S*/g, '');
    });
  };
  this.showProgress = function(el, show) {
    var RECALC_CLASS = 'recalculating';
    if (show)
      $(el).addClass(RECALC_CLASS);
    else
      $(el).removeClass(RECALC_CLASS);
  };
}).call(OutputBinding.prototype);
