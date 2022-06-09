/* eslint-disable unicorn/filename-case */
const protocol = (window.location.protocol === "https:") ? "wss:" : "ws:";
// Add trailing slash to path, if necessary, before appending "autoreload"
const defaultPath = window.location.pathname.replace(/\/?$/, "/") + "autoreload/";
const defaultUrl = `${protocol}//${window.location.host}${defaultPath}`;

// By default, use the defaultUrl. But if there's a data-ws-url attribute on our
// <script> tag, use that instead.
const wsUrl = document.currentScript.dataset.wsUrl || defaultUrl;
const ws = new WebSocket(wsUrl);

ws.onmessage = function (event) {
  if (event.data === "autoreload") {
    window.location.reload();
  }
};

export {};
