import { determineBrowserInfo } from "./browser";
import { disableFormSubmission } from "./disableForm";
import { trackHistory } from "./history";

import { ShinyClass } from "../shiny";
import { setUserAgent } from "../utils/userAgent";
import { windowUserAgent } from "../window/userAgent";

import { initReactlog } from "../shiny/reactlog";

// eslint-disable-next-line @typescript-eslint/naming-convention
let Shiny: ShinyClass;

function init(): void {
  if (window.Shiny) {
    throw new Error("Trying to create window.Shiny, but it already exists!");
  }
  Shiny = window.Shiny = new ShinyClass();
  setUserAgent(windowUserAgent()); // before determineBrowserInfo()

  determineBrowserInfo();
  trackHistory();

  disableFormSubmission();

  initReactlog();
}

export { init, Shiny, type ShinyClass };
