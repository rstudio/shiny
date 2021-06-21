import $ from "jquery";
import type { EventPriority } from "./inputPolicy";
import { InputPolicy } from "./inputPolicy";
import type { ShinyApp } from "../shiny/shinyapp";

// Schedules data to be sent to shinyapp at the next setTimeout(0).
// Batches multiple input calls into one websocket message.
class InputBatchSender extends InputPolicy {
  shinyapp: ShinyApp;
  timerId: NodeJS.Timeout = null;
  pendingData: Record<string, unknown> = {};
  reentrant = false;
  lastChanceCallback: Array<() => void> = [];

  constructor(shinyapp: ShinyApp) {
    super();
    this.shinyapp = shinyapp;
  }

  setInput(
    nameType: string,
    value: unknown,
    opts: { priority: EventPriority }
  ): void {
    this.pendingData[nameType] = value;

    if (!this.reentrant) {
      if (opts.priority === "event") {
        this._sendNow();
      } else if (!this.timerId) {
        this.timerId = setTimeout(this._sendNow.bind(this), 0);
      }
    }
  }

  private _sendNow(): void {
    if (this.reentrant) {
      console.trace("Unexpected reentrancy in InputBatchSender!");
    }

    this.reentrant = true;
    try {
      this.timerId = null;
      $.each(this.lastChanceCallback, (i, callback) => {
        callback();
      });
      const currentData = this.pendingData;

      this.pendingData = {};
      this.shinyapp.sendInput(currentData);
    } finally {
      this.reentrant = false;
    }
  }
}

export { InputBatchSender };
