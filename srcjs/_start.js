(function() {
  var $ = jQuery;

  var exports = window.Shiny = window.Shiny || {};

  exports.version = "{{ VERSION }}";  // Version number inserted by Grunt

  if (!exports.hasOwnProperty("bindGenericInputs")) {
    // Setting Shiny.bindGenericInputs=false during page load prevents inputs
    // that don't have Shiny-specific classnames from being bound. See
    // https://github.com/rstudio/shiny/issues/2956 for context.
    exports.bindGenericInputs = true;
  }

  var origPushState = window.history.pushState;
  window.history.pushState = function() {
    var result = origPushState.apply(this, arguments);
    $(document).trigger("pushstate");
    return result;
  };

  $(document).on('submit', 'form:not([action])', function(e) {
    e.preventDefault();
  });
