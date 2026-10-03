type RetryDecision = "never" | "retry" | "stop";
declare function retryDecision({ allow, exhausted, }: {
    allow: boolean | "force";
    exhausted: boolean;
}): RetryDecision;
export { retryDecision };
export type { RetryDecision };
