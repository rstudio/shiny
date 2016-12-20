(function() {
  var $ = jQuery;

  var exports = window.Shiny = window.Shiny || {};

  var origPushState = window.history.pushState;
  window.history.pushState = function() {
    var result = origPushState.apply(this, arguments);
    $(document).trigger("pushstate");
    return result;
  };

  $(document).on('submit', 'form:not([action])', function(e) {
    e.preventDefault();
  });
