// Under enableResume(reload = "ask"): before any session starts, the user
// decides (spec 2.3). Nothing is restored into the page until they do.
import { hideBlockingDialog, showBlockingDialog } from "./blockingDialog";

const askDialogId = "shiny-resume-ask";

function showResumeAskDialog(deps: {
  onPickUp: () => void;
  onStartFresh: () => void;
}): void {
  const choose = (f: () => void) => (): void => {
    hideBlockingDialog(askDialogId);
    f();
  };
  showBlockingDialog({
    id: askDialogId,
    title: "Pick up where you left off?",
    body: "This page has saved state from your last visit.",
    buttons: [
      {
        label: "Pick up where you left off",
        style: "primary",
        choice: "pickup",
        onClick: choose(deps.onPickUp),
      },
      {
        label: "Start fresh",
        style: "link",
        choice: "fresh",
        onClick: choose(deps.onStartFresh),
      },
    ],
  });
}

export { askDialogId, showResumeAskDialog };
