type priorityType = "immediate" | "deferred" | "event";

// Schedules data to be sent to shinyapp at the next setTimeout(0).
// Batches multiple input calls into one websocket message.
class InputPolicy {
  target: InputPolicy;

  setInput(
    name: string,
    value: unknown,
    opts: { priority: priorityType }
  ): void {
    throw "not implemented";
    name;
    value;
    opts;
  }
}

export { InputPolicy };
export type { priorityType };
