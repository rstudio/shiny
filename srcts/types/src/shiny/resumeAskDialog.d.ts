declare const askDialogId = "shiny-resume-ask";
declare function showResumeAskDialog(deps: {
    onPickUp: () => void;
    onStartFresh: () => void;
}): void;
export { askDialogId, showResumeAskDialog };
