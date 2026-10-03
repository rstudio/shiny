// The per-tab stash that ties a page to the session it can resume.
// Everything above "Browser glue" is pure and unit-tested.

type ReloadMode = "ask" | "resume" | "fresh";

type ResumeStash = {
  token: string;
  // location.href when written; a pasted bookmark URL differs and wins.
  url: string;
  // The app's enableResume(reload =), from `config`.
  reload: ReloadMode;
  // Set before a reload Shiny made (UI changed, autoreload, the error
  // dialog's Resume): the next page resumes without asking.
  serverInitiated: boolean;
  // Fresh-page resumes that died young.
  failures: number;
};

type LoadDecision =
  | { kind: "fresh"; discard: "url" | "crash-loop" | "reload-fresh" | null }
  | { kind: "ask"; token: string }
  | { kind: "resume"; token: string };

const crashLoopWindowMs = 10000;
const crashLoopLimit = 2;
const reloadModes: readonly ReloadMode[] = ["ask", "resume", "fresh"];

function decideOnLoad(stash: ResumeStash | null, href: string): LoadDecision {
  if (stash === null) return { kind: "fresh", discard: null };
  if (href !== stash.url) return { kind: "fresh", discard: "url" };
  if (stash.failures >= crashLoopLimit)
    return { kind: "fresh", discard: "crash-loop" };
  // The user did not reload: the saved state can only arrive through a reload.
  if (stash.serverInitiated) return { kind: "resume", token: stash.token };
  if (stash.reload === "resume") return { kind: "resume", token: stash.token };
  if (stash.reload === "fresh")
    return { kind: "fresh", discard: "reload-fresh" };
  return { kind: "ask", token: stash.token };
}

// The stash a session's `config` calls for, or null with resume off.
function nextStash(
  old: ResumeStash | null,
  token: string | null,
  reload: ReloadMode,
  href: string,
): ResumeStash | null {
  if (token === null) return null;
  return {
    token,
    url: href,
    reload,
    serverInitiated: false,
    failures: old?.failures ?? 0,
  };
}

function readStash(storage: Storage, pathname: string): ResumeStash | null {
  try {
    const raw = storage.getItem(stashKey(pathname));
    if (raw === null) return null;
    const parsed: unknown = JSON.parse(raw);
    if (typeof parsed !== "object" || parsed === null) return null;
    const s = parsed as Partial<ResumeStash>;
    if (typeof s.token !== "string" || typeof s.url !== "string") return null;
    return {
      token: s.token,
      url: s.url,
      reload: reloadModes.includes(s.reload as ReloadMode)
        ? (s.reload as ReloadMode)
        : "ask",
      serverInitiated: s.serverInitiated === true,
      failures: typeof s.failures === "number" ? s.failures : 0,
    };
  } catch {
    return null;
  }
}

function writeStash(
  storage: Storage,
  pathname: string,
  stash: ResumeStash,
): void {
  try {
    storage.setItem(stashKey(pathname), JSON.stringify(stash));
  } catch {
    // Storage full or disabled: the next reload starts fresh.
  }
}

function removeStash(storage: Storage, pathname: string): void {
  try {
    storage.removeItem(stashKey(pathname));
  } catch {
    // As in writeStash().
  }
}

function stashKey(pathname: string): string {
  return "shiny-resume:" + pathname;
}

// "Start fresh instead": ask the server to discard the record, then drop the
// stash and reload, whether or not the server answers within timeoutMs.
function startFresh(
  deps: {
    request: (done: () => void) => void;
    discardStash: () => void;
    reload: () => void;
    setTimer: (f: () => void, ms: number) => void;
  },
  timeoutMs = 2000,
): void {
  let finished = false;
  const finish = (): void => {
    if (finished) return;
    finished = true;
    deps.discardStash();
    deps.reload();
  };
  deps.setTimer(finish, timeoutMs);
  try {
    deps.request(finish);
  } catch {
    finish();
  }
}

// Tracks one fresh-page resume at a time.
class CrashLoopTracker {
  private now: () => number;
  private resumedAt: number | null = null;

  constructor(now: () => number) {
    this.now = now;
  }
  resumed(): void {
    this.resumedAt = this.now();
  }
  // True when this close (or fatalError) is a failed resume.
  closed(): boolean {
    const at = this.resumedAt;

    this.resumedAt = null;
    return at !== null && this.now() - at < crashLoopWindowMs;
  }
  // Asked by a timer crashLoopWindowMs after resumed(). Only closed() ends
  // the watch, so elapsed time is not checked again: the timer may fire a
  // millisecond before the clock agrees.
  survived(): boolean {
    return this.resumedAt !== null;
  }
}

// When a session's `config` may point the stash at its token. After a
// `resume`, the stash keeps the token being resumed until `resumed`: a
// `reload` that comes first (the UI changed) is from a session with no saved
// state of its own, and the reloaded page must resume the old record.
class StashSync {
  private pending = false;
  private reloadPending = false;

  // A socket opened; `resuming` when it sent `resume` with a token.
  opened(resuming: boolean): void {
    this.pending = resuming;
  }
  // `reloadCancelled`: a `reload` was asked for and the page is still here,
  // so the reload did not happen (a beforeunload handler cancelled it).
  config(hasToken: boolean): { sync: boolean; reloadCancelled: boolean } {
    const reloadCancelled = this.reloadPending;

    this.reloadPending = false;
    return { sync: !hasToken || !this.pending, reloadCancelled };
  }
  reload(): void {
    this.reloadPending = true;
  }
  // True when `resumed` should sync the stash.
  resumed(): boolean {
    const sync = this.pending && !this.reloadPending;

    this.pending = false;
    return sync;
  }
}

// What `resumed` sets off. Only the first socket of a reloaded page (`fresh`)
// restored from saved state is watched for a crash loop, and only under
// enableResume(reload = "resume") does it say so in a toast.
function resumedEffects(
  fresh: boolean,
  resumed: "snapshot" | "inputs",
  reload: ReloadMode,
): { watchCrashLoop: boolean; toast: boolean } {
  const restored = fresh && resumed === "snapshot";
  return { watchCrashLoop: restored, toast: restored && reload === "resume" };
}

// A page leaving for good tells the server, which then keeps the record for
// minutes rather than a day. A page entering the back/forward cache
// (`persisted`) may come back and resume.
function unloadOnPageHide(persisted: boolean, token: string | null): boolean {
  return !persisted && token !== null;
}

// ---- Browser glue ----------------------------------------------------------

let pageDecision: LoadDecision | null = null;
let resumeNotice: string | null = null;

// Runs at bundle evaluation, before the body renders.
function initResumeOnLoad(): void {
  try {
    const storage = browserStorage();
    const pathname = window.location.pathname;
    const stash = storage ? readStash(storage, pathname) : null;
    const decision = decideOnLoad(stash, window.location.href);
    window.addEventListener("popstate", refreshResumeStashUrl);
    window.addEventListener("hashchange", refreshResumeStashUrl);
    if (storage && stash) {
      if (decision.kind === "fresh" && decision.discard !== null) {
        if (decision.discard === "crash-loop") {
          resumeNotice =
            "This page's resumed session ended within seconds twice, so it was loaded fresh instead.";
        }
        removeStash(storage, pathname);
      } else if (stash.serverInitiated) {
        writeStash(storage, pathname, { ...stash, serverInitiated: false });
      }
    }
    pageDecision = decision;
  } catch {
    pageDecision = { kind: "fresh", discard: null };
  }
}

function takeLoadDecision(): LoadDecision | null {
  const taken = pageDecision;

  pageDecision = null;
  return taken;
}

// A devmode console message the load decision wants shown; read once Shiny
// knows whether it is in devmode.
function takeResumeNotice(): string | null {
  const notice = resumeNotice;

  resumeNotice = null;
  return notice;
}

function syncResumeStash(token: string | null, reload: ReloadMode): void {
  const storage = browserStorage();
  if (!storage) return;
  const pathname = window.location.pathname;
  const stash = nextStash(
    readStash(storage, pathname),
    token,
    reload,
    window.location.href,
  );
  if (stash === null) removeStash(storage, pathname);
  else writeStash(storage, pathname, stash);
}

function updateStash(update: (stash: ResumeStash) => ResumeStash): void {
  const storage = browserStorage();
  if (!storage) return;
  const pathname = window.location.pathname;
  const stash = readStash(storage, pathname);
  if (stash !== null) writeStash(storage, pathname, update(stash));
}

// Before a reload Shiny makes.
function markResumeStashServerInitiated(): void {
  updateStash((s) => ({ ...s, serverInitiated: true }));
}

// After a reload Shiny asked for did not happen.
function clearResumeStashServerInitiated(): void {
  updateStash((s) => ({ ...s, serverInitiated: false }));
}

function hasResumeStash(): boolean {
  const storage = browserStorage();
  return (
    storage !== null && readStash(storage, window.location.pathname) !== null
  );
}

// The stash follows the page's own URL changes.
function refreshResumeStashUrl(): void {
  updateStash((s) => ({ ...s, url: window.location.href }));
}

function updateResumeFailures(update: (failures: number) => number): void {
  updateStash((s) => ({ ...s, failures: update(s.failures) }));
}

function discardResumeStash(): void {
  const storage = browserStorage();
  if (storage) removeStash(storage, window.location.pathname);
}

function browserStorage(): Storage | null {
  try {
    return window.sessionStorage;
  } catch {
    // Sandboxed iframes and disabled storage throw on access.
    return null;
  }
}

export {
  CrashLoopTracker,
  StashSync,
  clearResumeStashServerInitiated,
  crashLoopWindowMs,
  decideOnLoad,
  discardResumeStash,
  hasResumeStash,
  initResumeOnLoad,
  markResumeStashServerInitiated,
  nextStash,
  readStash,
  refreshResumeStashUrl,
  removeStash,
  resumedEffects,
  startFresh,
  syncResumeStash,
  takeLoadDecision,
  takeResumeNotice,
  unloadOnPageHide,
  updateResumeFailures,
  writeStash,
};
export type { LoadDecision, ReloadMode, ResumeStash };
