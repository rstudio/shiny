import { disableFormSubmission } from "./disableForm";
import { trackHistory } from "./history";
import { determineBrowserInfo } from "./browser";
import { initBlobBuilder } from "./blobBuilder";

function init(): void {
  determineBrowserInfo();
  trackHistory();
  disableFormSubmission();
  initBlobBuilder();
}

export { init };
