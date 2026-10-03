// What the page does after the server reports a fatal observer error
// (`fatalError`). Pure, with injected dependencies; shinyapp.ts
// wires it to blockingDialog.ts.

type FatalErrorMessage = { message?: unknown; saved?: unknown };

type RecoveryView = { message: string | null; saved: boolean };

type RecoveryDeps = {
  show: (
    view: RecoveryView,
    choose: (choice: "resume" | "fresh") => void,
  ) => void;
  // The disconnected overlay: the session is over, whatever the user picks.
  greyOut: () => void;
  // Marks the stash server-initiated and reloads.
  resume: () => void;
  // Drops the stash and reloads.
  startOver: () => void;
  // Whether the tab has a stash to resume with; without one, Resume would
  // quietly start over.
  canResume: () => boolean;
};

const recoveryTitle = "Something went wrong";
const recoveryBody =
  "Resume returns to the state saved just before the error. Your last change may already have taken effect.";

function recoveryView(msg: FatalErrorMessage): RecoveryView {
  return {
    message: typeof msg.message === "string" ? msg.message : null,
    saved: msg.saved === true,
  };
}

class ErrorRecovery {
  private deps: RecoveryDeps;
  private seen = false;

  constructor(deps: RecoveryDeps) {
    this.deps = deps;
  }

  fatalError(msg: FatalErrorMessage): void {
    // The server sends one; a duplicate (a second error before the close) is noise.
    if (this.seen) return;
    this.seen = true;
    this.deps.greyOut();
    const view = recoveryView(msg);
    if (view.saved && !this.deps.canResume()) view.saved = false;
    this.deps.show(view, (choice) => {
      if (choice === "resume") this.deps.resume();
      else this.deps.startOver();
    });
  }

  // True once a fatalError arrived: the socket close that follows belongs to
  // the error flow, so the caller neither retries nor removes the dialog, and
  // the stash is kept for Resume.
  ownsClose(): boolean {
    return this.seen;
  }
}

export { ErrorRecovery, recoveryBody, recoveryTitle, recoveryView };
export type { FatalErrorMessage, RecoveryDeps, RecoveryView };
