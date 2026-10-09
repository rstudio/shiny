type InputValues = { [key: string]: unknown };

type FirstMessage =
  | { method: "init"; data: InputValues }
  | {
      method: "resume";
      data: { token?: string; inputs: InputValues; current: InputValues };
    };

// What a page carries across a dropped connection (srcts/PROTOCOL.md,
// "resume" and "Saved state"): the inputs as of the disconnect, which the
// `resume` payload restores as state; the current inputs, which seed
// restoreInput(); and the saved-state token. A fresh page adopts the token
// from `config` at once. A reconnecting page keeps the token it reconnected
// with until the server answers `resumed`: if the connection drops between
// `config` and the server processing `resume`, the next attempt must still
// name the record that exists.
class ReconnectState {
  private token: string | null = null;
  private pendingToken: string | null = null;
  private snapshot: InputValues | null = null;
  private isReconnecting = false;
  private isAnswered = false;

  get resumeToken(): string | null {
    return this.token;
  }

  get reconnecting(): boolean {
    return this.isReconnecting;
  }

  get answered(): boolean {
    return this.isAnswered;
  }

  onConfig(token: string | null): void {
    if (this.isReconnecting) this.pendingToken = token;
    else this.token = token;
  }

  onResumed(): void {
    this.token = this.pendingToken;
    this.pendingToken = null;
    this.isAnswered = true;
  }

  onAnswered(): void {
    this.isAnswered = true;
  }

  // `hadOpened`: the socket had opened, so `inputs` are what the server last
  // had a chance to see. A failed attempt leaves the snapshot alone.
  onSocketClosed(hadOpened: boolean, inputs: InputValues): void {
    this.pendingToken = null;
    // The first close of an opened socket in an outage fixes the snapshot;
    // retries that opened and died again keep it. onReconnected() clears it.
    if (hadOpened && (!this.isReconnecting || this.snapshot === null)) {
      this.snapshot = { ...inputs };
    }
  }

  beginReconnect(): void {
    this.isReconnecting = true;
    this.isAnswered = false;
  }

  // Called when a reconnect succeeded end to end, so the next drop takes a
  // fresh snapshot.
  onReconnected(): void {
    this.snapshot = null;
  }

  firstMessage(current: InputValues): FirstMessage {
    if (!this.isReconnecting) return { method: "init", data: current };
    const inputs = this.snapshot ?? { ...current };
    return {
      method: "resume",
      data: {
        ...(this.token !== null ? { token: this.token } : {}),
        inputs,
        current: { ...current },
      },
    };
  }
}

export { ReconnectState };
export type { FirstMessage, InputValues as ReconnectInputValues };
