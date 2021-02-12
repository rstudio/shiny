(function() {
  var $ = jQuery;

  var exports = window.Shiny = window.Shiny || {};

  exports.version = "{{ VERSION }}";  // Version number inserted by Grunt

  var origPushState = window.history.pushState;
  window.history.pushState = function() {
    var result = origPushState.apply(this, arguments);
    $(document).trigger("pushstate");
    return result;
  };

  $(document).on('submit', 'form:not([action])', function(e) {
    e.preventDefault();
  });
