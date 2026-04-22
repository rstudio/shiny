type VisualChangeHandlers = {
    doTriggerResize: (el: HTMLElement) => void;
    doSendHiddenState: (el: HTMLElement) => void;
    doSendSize: (el: HTMLElement) => void;
    doSendTheme: (el: HTMLElement) => void;
    reportsSize: (el: HTMLElement) => boolean;
    reportsTheme: (el: HTMLElement) => boolean;
};
declare function shouldObserveThemeMutations(reportsTheme: boolean): boolean;
declare function handleVisualChange(el: HTMLElement, { doTriggerResize, doSendHiddenState, doSendSize, doSendTheme, reportsSize, reportsTheme, }: VisualChangeHandlers): void;
export { handleVisualChange };
export { shouldObserveThemeMutations };
export type { VisualChangeHandlers };
