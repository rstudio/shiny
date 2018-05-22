// initialize all log entries to have a step value
(function() {
  for (var i = 0; i < window.log.length; i++) {
    window.log[i].step = i;
  }
})();
