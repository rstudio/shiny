type ReloadMode = "ask" | "resume" | "fresh";
type ResumeStash = {
    token: string;
    url: string;
    reload: ReloadMode;
    serverInitiated: boolean;
    failures: number;
};
type LoadDecision = {
    kind: "fresh";
    discard: "url" | "crash-loop" | "reload-fresh" | null;
} | {
    kind: "ask";
    token: string;
} | {
    kind: "resume";
    token: string;
};
declare const crashLoopWindowMs = 10000;
declare function decideOnLoad(stash: ResumeStash | null, href: string): LoadDecision;
declare function nextStash(old: ResumeStash | null, token: string | null, reload: ReloadMode, href: string): ResumeStash | null;
declare function readStash(storage: Storage, pathname: string): ResumeStash | null;
declare function writeStash(storage: Storage, pathname: string, stash: ResumeStash): void;
declare function removeStash(storage: Storage, pathname: string): void;
declare function startFresh(deps: {
    request: (done: () => void) => void;
    discardStash: () => void;
    reload: () => void;
    setTimer: (f: () => void, ms: number) => void;
}, timeoutMs?: number): void;
declare class CrashLoopTracker {
    private now;
    private resumedAt;
    constructor(now: () => number);
    resumed(): void;
    closed(): boolean;
    survived(): boolean;
}
declare function initResumeOnLoad(): void;
declare function takeLoadDecision(): LoadDecision | null;
declare function takeResumeNotice(): string | null;
declare function syncResumeStash(token: string | null, reload: ReloadMode): void;
declare function markResumeStashServerInitiated(): void;
declare function refreshResumeStashUrl(): void;
declare function updateResumeFailures(update: (failures: number) => number): void;
declare function discardResumeStash(): void;
export { CrashLoopTracker, crashLoopWindowMs, decideOnLoad, discardResumeStash, initResumeOnLoad, markResumeStashServerInitiated, nextStash, readStash, refreshResumeStashUrl, removeStash, startFresh, syncResumeStash, takeLoadDecision, takeResumeNotice, updateResumeFailures, writeStash, };
export type { LoadDecision, ReloadMode, ResumeStash };
