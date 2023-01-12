import type { InputPolicy, InputPolicyOpts } from "./inputPolicy";
import type { ShinyApp } from "../shiny/shinyapp";

// Schedules data to be sent to shinyapp at the next setTimeout(0).
// Batches multiple input calls into one websocket message.
class InputBatchSender implements InputPolicy {
  target!: InputPolicy; // We need this field to satisfy the InputPolicy interface
  shinyapp: ShinyApp;
  pendingData: { [key: string]: unknown } = {};
  reentrant = false;
  sendIsEnqueued = false;
  lastChanceCallback: Array<() => void> = [];

  constructor(shinyapp: ShinyApp) {
    this.shinyapp = shinyapp;
  }

  setInput(nameType: string, value: unknown, opts: InputPolicyOpts): void {
    this.pendingData[nameType] = value;

    if (!this.reentrant) {
      if (opts.priority === "event") {
        this._sendNow();
      } else if (!this.sendIsEnqueued) {
        this.shinyapp.actionQueue.enqueue(() => {
          this.sendIsEnqueued = false;
          this._sendNow();
        });
      }
    }
  }

  private _sendNow(): void {
    if (this.reentrant) {
      console.trace("Unexpected reentrancy in InputBatchSender!");
    }

    this.reentrant = true;
    try {
      this.lastChanceCallback.forEach((callback) => callback());
      const currentData = this.pendingData;

      this.pendingData = {};
      this.shinyapp.sendInput(currentData);
    } finally {
      this.reentrant = false;
    }
  }
}

export { InputBatchSender };
