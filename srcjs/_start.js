/*jshint
  undef:true,
  browser:true,
  devel: true,
  jquery:true,
  strict:false,
  curly:false,
  indent:2
*/

(function() {
  var $ = jQuery;

  var exports = window.Shiny = window.Shiny || {};


  $(document).on('submit', 'form:not([action])', function(e) {
    e.preventDefault();
  });
