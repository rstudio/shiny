import $ from "jquery";
import { InputPolicy, priorityType } from ".";
import { ShinyApp } from "../shiny/shinyapp";

// Schedules data to be sent to shinyapp at the next setTimeout(0).
// Batches multiple input calls into one websocket message.
class InputBatchSender extends InputPolicy {
  shinyapp: ShinyApp;
  timerId: NodeJS.Timeout = null;
  pendingData: Record<string, unknown>;
  reentrant = false;
  lastChanceCallback: Array<() => void> = [];

  constructor(shinyapp: ShinyApp) {
    super();
    this.shinyapp = shinyapp;
  }

  setInput(
    nameType: string,
    value: unknown,
    opts: { priority: priorityType }
  ): void {
    this.pendingData[nameType] = value;

    if (!this.reentrant) {
      if (opts.priority === "event") {
        this.$sendNow();
      } else if (!this.timerId) {
        this.timerId = setTimeout(this.$sendNow.bind(this), 0);
      }
    }
  }

  private $sendNow(): void {
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
