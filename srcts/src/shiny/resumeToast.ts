import $ from "jquery";

import { isShinyInDevMode } from "../utils";
import { showNotification } from "./notifications";

// Under enableResume(reload = "resume") only: after a reload resumed from
// saved state, say so and offer a clean session instead (spec 2.3).
async function showResumedToast(onStartFresh: () => void): Promise<void> {
  await showNotification({
    id: "resumed",
    html: "<span>Restored your session.</span>",
    action: '<a href="#" id="shiny-resume-start-fresh">Start fresh instead</a>',
    duration: 10000,
    closeButton: true,
  });
  $("#shiny-resume-start-fresh").one("click", (e) => {
    e.preventDefault();
    try {
      onStartFresh();
    } catch (error) {
      if (isShinyInDevMode())
        console.warn("[shiny] Could not start fresh", error);
    }
  });
}

export { showResumedToast };
