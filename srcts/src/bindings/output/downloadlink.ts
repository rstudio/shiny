import $ from "jquery";

import { OutputBinding } from "./outputBinding";

class DownloadLinkOutputBinding extends OutputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find("a.shiny-download-link");
  }
  renderValue(el: HTMLElement, data: string): void {
    el.setAttribute("href", data);
    el.classList.remove("disabled");
    el.removeAttribute("aria-disabled");
    el.removeAttribute("tabindex");
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
    const evt: FileDownloadEvent = $.Event("shiny:filedownload");

    evt.name = this.id;
    evt.href = this.href;
    $(document).trigger(evt);

    return;
    e; // eslint-disable-line @typescript-eslint/no-unused-expressions
  },
);

export { DownloadLinkOutputBinding };
