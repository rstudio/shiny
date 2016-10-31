(function() {
  var $ = jQuery;

  var exports = window.Shiny = window.Shiny || {};

  $(document).on('submit', 'form:not([action])', function(e) {
    e.preventDefault();
  });
