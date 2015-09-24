$(document).on('keydown', function(e) {
  if (e.which !== 114 || (!e.ctrlKey && !e.metaKey) || (e.shiftKey || e.altKey))
    return;
  var url = 'reactlog?w=' + window.escape(exports.shinyapp.config.workerId) +
    "&s=" + window.escape(exports.shinyapp.config.sessionId);
  window.open(url);
  e.preventDefault();
});
