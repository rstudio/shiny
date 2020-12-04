(function() {
  var protocol = 'ws:';
  if (window.location.protocol === 'https:')
    protocol = 'wss:';

  var defaultPath = window.location.pathname;
  if (!/\/$/.test(defaultPath))
    defaultPath += '/';
  defaultPath += 'autoreload/';

  var ws = new WebSocket(protocol + '//' + window.location.host + defaultPath);
  ws.onmessage = function(event) {
    if (event.data === "autoreload") {
      window.location.reload()
    }
  }
})();
