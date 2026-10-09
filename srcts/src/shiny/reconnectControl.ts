type ReconnectPermission = boolean | "force";

// The page's reconnect permission and its pending reconnect, kept together so
// turning retries off also cancels a reconnect already scheduled.
class ReconnectControl {
  private permission: ReconnectPermission = false;
  private timer: unknown = undefined;
  // Bumped by cancel(); a timer callback from an older generation is stale.
  private generation = 0;

  constructor(
    private setTimer: (fn: () => void, delay: number) => unknown = (
      fn,
      delay,
    ) => setTimeout(fn, delay),
    private clearTimer: (handle: unknown) => void = (handle) =>
      clearTimeout(handle as ReturnType<typeof setTimeout>),
  ) {}

  get allow(): ReconnectPermission {
    return this.permission;
  }

  setAllow(value: ReconnectPermission): void {
    this.permission = value;
    if (value === false) this.cancel();
  }

  // `socketAllows` is the connection layer's own flag (Shiny Server and
  // Posit Connect set it on their socket); "force" ignores it.
  shouldRetry(socketAllows: boolean): boolean {
    return (
      (this.permission === true && socketAllows) || this.permission === "force"
    );
  }

  schedule(delay: number, reconnect: () => void): void {
    this.cancel();
    const generation = this.generation;
    this.timer = this.setTimer(() => {
      this.timer = undefined;
      if (generation !== this.generation || this.permission === false) return;
      reconnect();
    }, delay);
  }

  cancel(): void {
    this.generation++;
    if (this.timer !== undefined) this.clearTimer(this.timer);
    this.timer = undefined;
  }

  permitsManualRetry(): boolean {
    return this.permission !== false;
  }
}

// The top-level `allowReconnect` of a text message, read as it arrives rather
// than in queue order: a stop must win over a socket close that happens while
// an earlier message's handler is still running.
function peekAllowReconnect(data: unknown): ReconnectPermission | undefined {
  if (typeof data !== "string" || !data.includes('"allowReconnect"')) {
    return undefined;
  }
  let msg: unknown;
  try {
    msg = JSON.parse(data);
  } catch {
    return undefined;
  }
  if (
    typeof msg !== "object" ||
    msg === null ||
    !Object.prototype.hasOwnProperty.call(msg, "allowReconnect")
  ) {
    return undefined;
  }
  const value = (msg as { allowReconnect: unknown }).allowReconnect;
  return value === true || value === false || value === "force"
    ? value
    : undefined;
}

export { ReconnectControl, peekAllowReconnect };
export type { ReconnectPermission };
