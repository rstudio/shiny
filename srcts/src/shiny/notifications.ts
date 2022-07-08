import $ from "jquery";

import { $escape, randomId } from "../utils";
import { shinyUnbindAll } from "./initedMethods";
import type { HtmlDep } from "./render";
import { renderContent } from "./render";

// Milliseconds to fade in or out
const fadeDuration = 250;

async function show({
  html = "",
  action = "",
  deps = [],
  duration = 5000,
  id = null,
  closeButton = true,
  type = null,
}: {
  html?: string;
  action?: string;
  deps?: HtmlDep[];
  duration?: number | null;
  id?: string | null;
  closeButton?: boolean;
  type?: string | null;
} = {}): Promise<ReturnType<typeof randomId>> {
  if (!id) id = randomId();

  // Create panel if necessary
  createPanel();

  // Get existing DOM element for this ID, or create if needed.
  let $notificationInit = get(id);

  if ($notificationInit?.length === 0) $notificationInit = create(id);
  const $notification = $notificationInit as JQuery<HTMLElement>;

  // Render html and dependencies
  const newHtml =
    `<div class="shiny-notification-content-text">${html}</div>` +
    `<div class="shiny-notification-content-action">${action}</div>`;
  const $content = $notification.find(".shiny-notification-content");

  await renderContent($content, { html: newHtml, deps: deps });

  // Remove any existing classes of the form 'shiny-notification-xxxx'.
  // The xxxx would be strings like 'warning'.
  const classes = $notification?.attr("class");

  if (classes) {
    const classVal = classes
      .split(/\s+/)
      .filter((cls: string) => cls.match(/^shiny-notification-/))
      .join(" ");

    $notification.removeClass(classVal);
  }

  // Add class. 'default' means no additional CSS class.
  if (type && type !== "default")
    $notification.addClass("shiny-notification-" + type);

  // Make sure that the presence/absence of close button matches with value
  // of `closeButton`.
  const $close = $notification.find(".shiny-notification-close");

  if (closeButton && $close.length === 0) {
    $notification.append('<div class="shiny-notification-close">&times;</div>');
  } else if (!closeButton && $close.length !== 0) {
    $close.remove();
  }

  // If duration was provided, schedule removal. If not, clear existing
  // removal callback (this happens if a message was first added with
  // a duration, and then updated with no duration).
  if (duration) addRemovalCallback(id, duration);
  else clearRemovalCallback(id);

  return id;
}

function remove(id: string): void {
  get(id)?.fadeOut(fadeDuration, function () {
    shinyUnbindAll(this);
    $(this).remove();

    // If no more notifications, remove the panel from the DOM.
    if (ids().length === 0) {
      getPanel().remove();
    }
  });
}

// Returns an individual notification DOM object (wrapped in jQuery).
function get(id: string | null | undefined) {
  if (!id) return null;
  return getPanel().find("#shiny-notification-" + $escape(id));
}

// Return array of all notification IDs
function ids() {
  return getPanel()
    .find(".shiny-notification")
    .map(function () {
      return this.id.replace(/shiny-notification-/, "");
    })
    .get();
}

// Returns the notification panel DOM object (wrapped in jQuery).
function getPanel() {
  return $("#shiny-notification-panel");
}

// Create notifications panel and return the jQuery object. If the DOM
// element already exists, just return it.
function createPanel() {
  const $panel = getPanel();

  if ($panel.length > 0) return $panel;

  $(document.body).append('<div id="shiny-notification-panel">');

  return $panel;
}

// Create a notification DOM element and return the jQuery object. If the
// DOM element already exists for the ID, just return it without creating.
function create(id: string): JQuery<HTMLElement> {
  let $notification = get(id);

  if ($notification?.length === 0) {
    $notification = $(
      `<div id="shiny-notification-${id}" class="shiny-notification">` +
        '<div class="shiny-notification-close">&times;</div>' +
        '<div class="shiny-notification-content"></div>' +
        "</div>"
    );

    $notification.find(".shiny-notification-close").on("click", (e) => {
      e.preventDefault();
      e.stopPropagation();
      remove(id);
    });

    getPanel().append($notification);
  }

  return $notification as JQuery<HTMLElement>;
}

// Add a callback to remove a notification after a delay in ms.
function addRemovalCallback(id: string, delay: number) {
  // If there's an existing removalCallback, clear it before adding the new
  // one.
  clearRemovalCallback(id);

  // Attach new removal callback
  const removalCallback = setTimeout(function () {
    remove(id);
  }, delay);

  get(id)?.data("removalCallback", removalCallback);
}

// Clear a removal callback from a notification, if present.
function clearRemovalCallback(id: string) {
  const $notification = get(id);
  const oldRemovalCallback: ReturnType<typeof setTimeout> =
    $notification?.data("removalCallback");

  if (oldRemovalCallback) {
    clearTimeout(oldRemovalCallback);
  }
}

export { show as showNotification, remove as removeNotification };
