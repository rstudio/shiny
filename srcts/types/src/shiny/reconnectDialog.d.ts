declare function showReconnectDialog(delay: number, stillWanted?: () => boolean): Promise<void>;
declare function hideReconnectDialog(): void;
declare function showResumeEndedDialog(): Promise<void>;
declare function showResumeLostNotice(): Promise<void>;
declare function hideResumeLostNotice(): void;
export { hideReconnectDialog, hideResumeLostNotice, showReconnectDialog, showResumeEndedDialog, showResumeLostNotice, };
