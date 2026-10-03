// The client retry rule (srcts/PROTOCOL.md, "Client retry rule"): retry
// whenever the app allowed it, on any socket, until the attempts run out.
type RetryDecision = "never" | "retry" | "stop";

function retryDecision({
  allow,
  exhausted,
}: {
  allow: boolean | "force";
  exhausted: boolean;
}): RetryDecision {
  if (!(allow === true || allow === "force")) return "never";
  return exhausted ? "stop" : "retry";
}

export { retryDecision };
export type { RetryDecision };
