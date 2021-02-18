import { disable_form } from "./disableForm";
import { track_history } from "./history";
import { determine_browser_info } from "./browser";
import { initBlobBuilder } from "./blobBuilder";

function init(): void {
  determine_browser_info();
  track_history();
  disable_form();
  initBlobBuilder();
}

export { init };
