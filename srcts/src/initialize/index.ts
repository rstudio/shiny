import { determineBrowserInfo } from "./browser";
import { disableFormSubmission } from "./disableForm";
import { trackHistory } from "./history";

import { ShinyClass } from "../shiny";
import { setUserAgent } from "../utils/userAgent";
import { windowUserAgent } from "../window/userAgent";

import { initReactlog } from "../shiny/reactlog";

function init(): void {
  window.Shiny = window.Shiny || new ShinyClass();
  setUserAgent(windowUserAgent()); // before determineBrowserInfo()

  determineBrowserInfo();
  trackHistory();

  disableFormSubmission();

  initReactlog();
}

export { init, type ShinyClass };
