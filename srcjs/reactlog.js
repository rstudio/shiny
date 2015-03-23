$(document).on('keydown', function(e) {
  if (e.which !== 114 || (!e.ctrlKey && !e.metaKey) || (e.shiftKey || e.altKey))
    return;
  var url = 'reactlog?w=' + exports.shinyapp.config.workerId;
  window.open(url);
  e.preventDefault();
});
