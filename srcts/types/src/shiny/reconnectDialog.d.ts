declare function showReconnectDialog(delay: number, stillWanted?: () => boolean): Promise<void>;
declare function hideReconnectDialog(): void;
export { hideReconnectDialog, showReconnectDialog };
