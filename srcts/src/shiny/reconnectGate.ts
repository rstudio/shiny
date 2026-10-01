// The client retry rule (srcts/PROTOCOL.md, "Client retry rule"). A client
// holding a resume token retries on any socket, because the server will
// resume the session; without one it follows main's rule: only behind
// shiny-server-client's shim, or on "force". Only resuming clients give up.
export type RetryDecision = "never" | "retry" | "stop";

export function retryDecision({
  allow,
  holdsToken,
  shimAllows,
  exhausted,
}: {
  allow: boolean | "force";
  holdsToken: boolean;
  shimAllows: boolean;
  exhausted: boolean;
}): RetryDecision {
  const allowed =
    allow === "force" || (allow === true && (holdsToken || shimAllows));
  if (!allowed) return "never";
  return holdsToken && exhausted ? "stop" : "retry";
}
