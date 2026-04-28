import $ from "jquery";

import { OutputBinding } from "./outputBinding";

class DownloadLinkOutputBinding extends OutputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find("a.shiny-download-link");
  }
  renderValue(el: HTMLElement, data: string): void {
    el.setAttribute("href", data);
    // If we or shinyjs have marked this element as disabled (via shinyjs::disabled()),
    // skip the auto-enable behavior so that the intentional disabled state is
    // preserved. See https://github.com/rstudio/shiny/issues/4119.
    if (
      !el.hasAttribute("data-shiny-disable-auto-enable") &&
      !el.classList.contains("shinyjs-disabled")
    ) {
      el.classList.remove("disabled");
      el.removeAttribute("aria-disabled");
      el.removeAttribute("tabindex");
    }
  }
  // Progress shouldn't be shown on the download button
  // (progress will be shown as a page level pulse instead)
  showProgress(el: HTMLElement, show: boolean): void {
    return;
    el; // eslint-disable-line @typescript-eslint/no-unused-expressions
    show; // eslint-disable-line @typescript-eslint/no-unused-expressions
  }
}

interface FileDownloadEvent extends JQuery.Event {
  name: string;
  href: string;
}

// TODO-barret should this be in an init method?
// Trigger shiny:filedownload event whenever a downloadButton/Link is clicked
$(document).on(
  "click.shinyDownloadLink",
  "a.shiny-download-link",
  function (e: Event) {
    const el = e.currentTarget as HTMLAnchorElement;

    // Prevent clicks when the button is disabled.
    if (el.classList.contains("disabled")) {
      e.preventDefault();
      return;
    }

    const evt: FileDownloadEvent = $.Event("shiny:filedownload");

    evt.name = el.id;
    evt.href = el.href;
    $(document).trigger(evt);

    return;
    e; // eslint-disable-line @typescript-eslint/no-unused-expressions
  },
);

export { DownloadLinkOutputBinding };
