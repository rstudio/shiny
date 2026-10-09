import $ from "jquery";

import { removeNotification, showNotification } from "./notifications";

function updateTime(reconnectTime: number): void {
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

// `stillWanted` is checked after the notice renders, which is async: a stop
// that arrived meanwhile must not leave a retry notice on screen.
async function showReconnectDialog(
  delay: number,
  stillWanted: () => boolean = () => true,
): Promise<void> {
  const reconnectTime = new Date().getTime() + delay;

  // If there's already a reconnect dialog, don't add another
  if ($("#shiny-reconnect-text").length > 0) return;

  const html =
    '<span id="shiny-reconnect-text">Attempting to reconnect</span>' +
    '<span id="shiny-reconnect-time"></span>';
  const action =
    '<a id="shiny-reconnect-now" href="#" onclick="Shiny.shinyapp.reconnect();">Try now</a>';

  await showNotification({
    id: "reconnect",
    html: html,
    action: action,
    duration: null,
    closeButton: false,
    type: "warning",
  });

  if (!stillWanted()) {
    hideReconnectDialog();
    return;
  }

  updateTime(reconnectTime);
}

function hideReconnectDialog(): void {
  removeNotification("reconnect");
}

// Shown when the server ended a session the page was trying to resume.
// Nothing can be retried; the reason is in the server log. It has its own id:
// the retry notice is still fading out when the server's answer arrives, and
// reusing "reconnect" would let that fade remove this one.
async function showResumeEndedDialog(): Promise<void> {
  await showNotification({
    id: "resume-ended",
    html: '<span id="shiny-resume-ended">This session could not be resumed.</span>',
    action:
      '<a id="shiny-reload-link" href="#" onclick="window.location.reload(); return false;">Reload to start fresh</a>',
    duration: null,
    closeButton: false,
    type: "warning",
  });
}

export { hideReconnectDialog, showReconnectDialog, showResumeEndedDialog };
