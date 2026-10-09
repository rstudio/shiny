declare function showReconnectDialog(delay: number, stillWanted?: () => boolean): Promise<void>;
declare function hideReconnectDialog(): void;
declare function showResumeEndedDialog(): Promise<void>;
export { hideReconnectDialog, showReconnectDialog, showResumeEndedDialog };
