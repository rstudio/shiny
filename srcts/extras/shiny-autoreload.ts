/* eslint-disable unicorn/filename-case */
const protocol = (window.location.protocol === "https:") ? "wss:" : "ws:";
// Add trailing slash to path, if necessary, before appending "autoreload"
const defaultPath = window.location.pathname.replace(/\/?$/, "/") + "autoreload/";
const defaultUrl = `${protocol}//${window.location.host}${defaultPath}`;

// By default, use the defaultUrl. But if there's a data-ws-url attribute on our
// <script> tag, use that instead.
const wsUrl = document.currentScript.dataset.wsUrl || defaultUrl;
const ws = new WebSocket(wsUrl);

enum Status {
  // Trying to reach the autoreload server
  Connecting = "connecting",
  // Connected to the autoreload server, just waiting for it to tell us to
  // reload
  Ready = "ready",
  // The server has asked us to reload and we're in the process of doing so
  Reloading = "reloading",
  // The connection to the server was broken. TODO: should we try reconnecting?
  Closed = "closed",
}

// Put a class on <html> indicating the current state of the autoreload channel
function setAutoreloadStatus(status: Status) {
  for (const s of Object.values(Status)) {
    if (status === s) {
      document.documentElement.classList.add(`autoreload-${s}`);
    } else {
      document.documentElement.classList.remove(`autoreload-${s}`);
    }
  }
}

// Also unconditionally add the "autoreload" class to <html>, so it's easy to
// tell whether autoreload is even enabled
document.documentElement.classList.add("autoreload");

setAutoreloadStatus(Status.Connecting);

ws.onopen = function(event) {
  setAutoreloadStatus(Status.Ready);
};

ws.onclose = function(event) {
  setAutoreloadStatus(Status.Closed);
};

ws.onmessage = function (event) {
  if (event.data === "autoreload") {
    window.location.reload();
    setAutoreloadStatus(Status.Reloading);
  }
};

export {};
