type FatalErrorMessage = {
    message?: unknown;
    saved?: unknown;
};
type RecoveryView = {
    message: string | null;
    saved: boolean;
};
type RecoveryDeps = {
    show: (view: RecoveryView, choose: (choice: "resume" | "fresh") => void) => void;
    greyOut: () => void;
    resume: () => void;
    startOver: () => void;
    canResume: () => boolean;
};
declare const recoveryTitle = "Something went wrong";
declare const recoveryBody = "Resume returns to the state saved just before the error. Your last change may already have taken effect.";
declare function recoveryView(msg: FatalErrorMessage): RecoveryView;
declare class ErrorRecovery {
    private deps;
    private seen;
    constructor(deps: RecoveryDeps);
    fatalError(msg: FatalErrorMessage): void;
    ownsClose(): boolean;
}
export { ErrorRecovery, recoveryBody, recoveryTitle, recoveryView };
export type { FatalErrorMessage, RecoveryDeps, RecoveryView };
