declare function showReconnectDialog(delay: number, stillWanted?: () => boolean): Promise<void>;
declare function hideReconnectDialog(): void;
declare function showReconnectFailedDialog(): Promise<void>;
export { hideReconnectDialog, showReconnectDialog, showReconnectFailedDialog };
