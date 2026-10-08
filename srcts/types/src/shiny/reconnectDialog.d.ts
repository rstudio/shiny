declare function showReconnectDialog(delay: number): Promise<void>;
declare function hideReconnectDialog(): void;
declare function showReconnectFailedDialog(): Promise<void>;
export { hideReconnectDialog, showReconnectDialog, showReconnectFailedDialog };
