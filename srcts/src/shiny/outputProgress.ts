/*
 * A state machine for tracking Shiny output progress.
 *
 * Although there are a handful of states, the ultimate goal here is to be able
 * to say whether a given output `isRecalculating()` or not (and thus, whether
 * we should show progress indication or not).
 *
 * When looking through this code, it's highly recommended to see the visual
 * representation of the state machine diagram. It's available under
 * `inst/diagrams/outputProgressStateMachine.svg`
 */

// The possible states of a given output.
enum OutputStates {
  Initial = "initial",
  Running = "running",
  Idle = "idle",
  Value = "value",
  Error = "error",
  Cancel = "cancel",
  Persisting = "persisting",
  Invalidated = "invalidated",
}

// Server->client messages that have an impact on output progress state.
// These derive from the Session's defineOutput() method.
type RecalculatingMessage = {
  recalculating: {
    name: string;
    status: "recalculated" | "recalculating";
  };
};

type FlushMessage = {
  errors: { [key: string]: unknown };
  values: { [key: string]: unknown };
  inputMessages: [];
};

type ProgressMessage = {
  progress: {
    type: "binding";
    message: {
      id: string;
      persistent: boolean;
    };
  };
};

// A generic server->client message type.
type Message = { [key: string]: unknown };

// The state machine that tracks the progress of outputs in a Shiny app.
class OutputProgressState {
  outputStates: Map<string, OutputStates> = new Map();

  isRecalculating(name: string): boolean {
    const state = this.#getState(name);

    // The blue nodes in the state machine diagram
    const recalculatingStates = [
      OutputStates.Initial,
      OutputStates.Running,
      OutputStates.Idle,
      OutputStates.Persisting,
      OutputStates.Invalidated,
    ];

    return recalculatingStates.includes(state);
  }

  processMessage(message: Message): void {
    if (isRecalculatingMessage(message)) {
      // The "1st level" of the state machine diagram
      this.#processRecalculatingMessage(message);
    }

    if (isFlushMessage(message)) {
      // The "2nd level" of the state machine diagram
      this.#processFlushMessage(message);
    }

    if (isProgressMessage(message)) {
      // The "3rd level" of the state machine diagram
      this.#processProgressMessage(message);
    }
  }

  #processRecalculatingMessage(message: RecalculatingMessage): void {
    const { name, status } = message.recalculating;

    const state = this.#getState(name);

    if (status === "recalculating") {
      switch (state) {
        case OutputStates.Initial:
        case OutputStates.Invalidated:
          this.#setState(name, OutputStates.Running);
          break;
        default:
          throw new Error(
            `Shiny server sent a message that the output '${name}' is recalculating,
            but the output is in an unexpected state of: '${state}'.`
          );
      }
    }

    if (status === "recalculated") {
      switch (state) {
        case OutputStates.Running:
          this.#setState(name, OutputStates.Idle);
          break;
        default:
          throw new Error(
            `Shiny server sent a message that the output '${name}' has been recalculated,
            but the output is in an unexpected state of: '${state}'.`
          );
      }
    }
  }

  #processFlushMessage(message: FlushMessage): void {
    for (const name in message.values) {
      this.#processValueOrError(name, OutputStates.Value);
    }

    for (const name in message.errors) {
      this.#processValueOrError(name, OutputStates.Error);
    }

    // Since req(F, cancelOutput = TRUE) doesn't send a message, we need to identify
    // the situation by looking for idle outputs and moving them to cancel.
    for (const [name, state] of this.outputStates) {
      switch (state) {
        case OutputStates.Idle:
          this.#setState(name, OutputStates.Cancel);
          break;
        case OutputStates.Value:
        case OutputStates.Error:
        case OutputStates.Cancel:
        case OutputStates.Persisting:
        case OutputStates.Invalidated: // If the output is suspended
          break;
        default:
          throw new Error(
            `Shiny server sent a flush message, and after processing the values and errors,
            the output '${name}' has an unexpected ending state of: '${state}'.`
          );
      }
    }
  }

  #processProgressMessage(message: ProgressMessage): void {
    const { id, persistent } = message.progress.message;
    const state = this.#getState(id);
    if (persistent) {
      switch (state) {
        case OutputStates.Running:
          this.#setState(id, OutputStates.Persisting);
          break;
        default:
          throw new Error(
            `Shiny server has sent a 'persistent progress' message for ${id},
            but the output is in an unexpected state of: ${state}`
          );
      }
    } else {
      switch (state) {
        case OutputStates.Value:
        case OutputStates.Error:
        case OutputStates.Cancel:
        case OutputStates.Persisting:
          this.#setState(id, OutputStates.Invalidated);
          break;
        default:
          throw new Error(
            `Shiny server has sent a progress message for ${id},
            but the output is in an unexpected state of: ${state}`
          );
      }
    }
  }

  // When receiving values/errors as part of a flush message, outputs should generally
  // be moving from Idle to Value/Error state.
  #processValueOrError(
    name: string,
    type: OutputStates.Error | OutputStates.Value
  ): void {
    const state = this.#getState(name);
    switch (state) {
      case OutputStates.Idle:
        this.#setState(name, type);
        break;
      default:
        throw new Error(
          `Shiny server has sent a ${type} for the output '${name}',
          but the output is in an unexpected state of: '${state}'.`
        );
    }
  }

  #getState(name: string): OutputStates {
    return this.outputStates.get(name) ?? OutputStates.Initial;
  }

  #setState(name: string, state: OutputStates): void {
    this.outputStates.set(name, state);
  }
}

// Type guards
function isRecalculatingMessage(x: Message): x is RecalculatingMessage {
  const m = x as RecalculatingMessage;
  return m.recalculating !== undefined;
}

function isFlushMessage(x: Message): x is FlushMessage {
  const m = x as FlushMessage;
  return m.values !== undefined && m.errors !== undefined;
}

function isProgressMessage(x: Message): x is ProgressMessage {
  const m = x as ProgressMessage;
  return m.progress !== undefined && m.progress.type === "binding";
}

export { OutputProgressState };
