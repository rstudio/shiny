// Time to wait before each reconnection attempt. Past the end, the last
// value repeats: about a minute of quick attempts, then one every 30 s for
// as long as the tab is open (srcts/PROTOCOL.md, "Client retry rule").
// Each has 500 ms added so the notification shows "..." for its last half
// second.
const delays = [1500, 1500, 2500, 2500, 5500, 5500, 10500, 10500, 10500, 30500];

class ReconnectDelay {
  private attempts = 0;

  // The delay for the next attempt; counts the attempt.
  next(): number {
    const i = Math.min(this.attempts, delays.length - 1);

    this.attempts++;
    return delays[i];
  }

  // Called when the server answers (`config`, `values` or `resumed`), never
  // on socket open.
  reset(): void {
    this.attempts = 0;
  }
}

export { ReconnectDelay };
