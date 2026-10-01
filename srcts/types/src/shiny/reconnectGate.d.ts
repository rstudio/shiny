export type RetryDecision = "never" | "retry" | "stop";
export declare function retryDecision({ allow, holdsToken, shimAllows, exhausted, }: {
    allow: boolean | "force";
    holdsToken: boolean;
    shimAllows: boolean;
    exhausted: boolean;
}): RetryDecision;
