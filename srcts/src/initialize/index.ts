import { disableFormSubmission } from "./disableForm";
import { trackHistory } from "./history";
import { determineBrowserInfo } from "./browser";

import { windowShiny } from "../window/libraries";
import { setShiny } from "../shiny";
import { setUserAgent } from "../utils/userAgent";
import { windowUserAgent } from "../window/userAgent";

import { initReactlog } from "../shiny/reactlog";

function init(): void {
  setShiny(windowShiny());
  setUserAgent(windowUserAgent()); // before determineBrowserInfo()

  determineBrowserInfo();
  trackHistory();

  disableFormSubmission();

  initReactlog();
}

export { init };
