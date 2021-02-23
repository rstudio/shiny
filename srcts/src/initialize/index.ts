import { disableFormSubmission } from "./disableForm";
import { trackHistory } from "./history";
import { determineBrowserInfo } from "./browser";

import { windowShiny, windowJQuery } from "../window/libraries";
import { setJQuery } from "../jquery";
import { setShiny } from "../shiny";
import { setBlobBuilder } from "../utils/blob";
import { windowBlobBuilder } from "../window/blobBuilder";
import { setUserAgent } from "../utils/userAgent";
import { windowUserAgent } from "../window/userAgent";

function init(): void {
  setJQuery(windowJQuery());
  setShiny(windowShiny());
  setUserAgent(windowUserAgent()); // before determineBrowserInfo()

  determineBrowserInfo();
  trackHistory();

  disableFormSubmission();

  setBlobBuilder(windowBlobBuilder());
}

export { init };
