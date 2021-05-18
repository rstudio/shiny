import $ from "jquery";

import {
  show as showNotification,
  remove as removeNotification,
} from "./notifications";

function updateTime(reconnectTime): void {
  const $time = $("#shiny-reconnect-time");
  // If the time has been removed, exit and don't reschedule this function.

  if ($time.length === 0) return;

  const seconds = Math.floor((reconnectTime - new Date().getTime()) / 1000);

  if (seconds > 0) {
    $time.text(" in " + seconds + "s");
  } else {
    $time.text("...");
  }

  // Reschedule this function after 1 second
  setTimeout(function () {
    updateTime(reconnectTime);
  }, 1000);
}

function showReconnectDialog(delay): void {
  const reconnectTime = new Date().getTime() + delay;

  // If there's already a reconnect dialog, don't add another
  if ($("#shiny-reconnect-text").length > 0) return;

  const html =
    '<span id="shiny-reconnect-text">Attempting to reconnect</span>' +
    '<span id="shiny-reconnect-time"></span>';
  const action =
    '<a id="shiny-reconnect-now" href="#" onclick="Shiny.shinyapp.reconnect();">Try now</a>';

  showNotification({
    id: "reconnect",
    html: html,
    action: action,
    duration: null,
    closeButton: false,
    type: "warning",
  });

  updateTime(reconnectTime);
}

function hideReconnectDialog(): void {
  removeNotification("reconnect");
}

export { showReconnectDialog, hideReconnectDialog };
