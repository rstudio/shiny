import $ from "jquery";

// Schedules data to be sent to shinyapp at the next setTimeout(0).
// Batches multiple input calls into one websocket message.
class InputBatchSender {
  shinyapp;
  timerId = null;
  pendingData = {};
  reentrant = false;
  lastChanceCallback = [];

  constructor(shinyapp) {
    this.shinyapp = shinyapp;
  }

  setInput(nameType, value, opts): void {
    this.pendingData[nameType] = value;

    if (!this.reentrant) {
      if (opts.priority === "event") {
        this.$sendNow();
      } else if (!this.timerId) {
        this.timerId = setTimeout(this.$sendNow.bind(this), 0);
      }
    }
  }

  $sendNow(): void {
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
