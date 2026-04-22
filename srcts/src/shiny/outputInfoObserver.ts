type VisualChangeHandlers = {
  doTriggerResize: (el: HTMLElement) => void;
  doSendHiddenState: (el: HTMLElement) => void;
  doSendSize: (el: HTMLElement) => void;
  doSendTheme: (el: HTMLElement) => void;
  reportsSize: (el: HTMLElement) => boolean;
  reportsTheme: (el: HTMLElement) => boolean;
};

function shouldObserveThemeMutations(reportsTheme: boolean): boolean {
  return reportsTheme;
}

function handleVisualChange(
  el: HTMLElement,
  {
    doTriggerResize,
    doSendHiddenState,
    doSendSize,
    doSendTheme,
    reportsSize,
    reportsTheme,
  }: VisualChangeHandlers,
): void {
  doTriggerResize(el);
  doSendHiddenState(el);

  if (reportsSize(el)) {
    doSendSize(el);
  }

  if (reportsTheme(el)) {
    doSendTheme(el);
  }
}

export { handleVisualChange };
export { shouldObserveThemeMutations };
export type { VisualChangeHandlers };
