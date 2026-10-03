// Time to wait before each reconnection attempt. Past the end, the last
// value repeats. Each has 500 ms added so the notification shows "..." for
// its last half second.
const delays = [1500, 1500, 2500, 2500, 5500, 5500, 10500];

// About a minute of retrying; a server that is simply gone should not keep
// the tab polling forever.
const maxAttempts = 10;

class ReconnectDelay {
  private attempts = 0;

  // The delay for the next attempt; counts the attempt.
  next(): number {
    const i = Math.min(this.attempts, delays.length - 1);

    this.attempts++;
    return delays[i];
  }

  exhausted(): boolean {
    return this.attempts >= maxAttempts;
  }

  // Called when the server answers (`config` or `values`), never on socket open.
  reset(): void {
    this.attempts = 0;
  }
}

export { ReconnectDelay, maxAttempts };
