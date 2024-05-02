/*
 * A state machine for tracking Shiny output progress.
 *
 * Although there are a handful of possible states, the ultimate goal here is to
 * be able to say whether a given output `isRecalculating()` or not (and thus,
 * whether we should show progress indication or not).
 *
 * The diagram below depicts the state machine. Each node represents a possible
 * state and each edge represents a server-->client message that moves outputs
 * from one state to another. If a node name is all caps, then the output should
 * be showing a busy state when visible (i.e., `binding.showProgress(true)`).
 *
 * A more polished SVG version of this diagram can be found here:
 * https://github.com/rstudio/shiny/blob/main/inst/diagrams/outputProgressStateMachine.svg
 *
 * +---------+ recalculating  +---------+
 * | INITIAL +--------------->| RUNNING |<--------------+
 * +---------+                +---+-----+               |
 *               +-----------/     |                    |
 *               |              recalculated            |
 *               |                 |                    |
 *               |              +--v---+                |
 *               |           +--+ IDLE +--+-------+     |
 *               |           |  +------+  |       |     |
 *             1 |          2|           3|      4|     |
 *               v           v            v       v     |
 *          +------------+ +------+  +-----+   +-----+  |
 *          | PERSISTENT | |cancel|  |value|   |error|  |
 *          +-------+----+ +---+--+  +-+---+   +-+---+  |
 *                  |         5|      5|         |      |
 *                 5|          v       v        5|      |
 *                  |       +-------------+      |      |
 *                  +------>| INVALIDATED |<-----+      |
 *                          +-----+-------+             |
 *                                |                     |
 *                                |    recalculating    |
 *                                +---------------------+
 *
 *  1. {progress: {type: "binding", message: {persistent: true}}}
 *  2. No message
 *  3. Value
 *  4. Error
 *  5. {progress: {type: "binding"}}
 */

// The possible states of a given output.
enum OutputStates {
  Initial = "initial",
  Running = "running",
  Idle = "idle",
  Value = "value",
  Error = "error",
  Cancel = "cancel",
  Persistent = "persistent",
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
class OutputProgressReporter {
  private outputStates: Map<string, OutputStates> = new Map();

  // Returns whether the output is recalculating or not.
  isRecalculating(name: string): boolean {
    const state = this.#getState(name);

    // The blue nodes in the state machine diagram
    const recalculatingStates = [
      OutputStates.Initial,
      OutputStates.Running,
      OutputStates.Idle,
      OutputStates.Persistent,
      OutputStates.Invalidated,
    ];

    return recalculatingStates.includes(state);
  }

  // Update output state based on the message received from the server.
  // Note that any message can be passed to this method, but only the
  // messages that are relevant to output progress do anything to the state.
  updateStateFromMessage(message: Message): void {
    if (isRecalculatingMessage(message)) {
      // The "1st level" of the state machine diagram
      this.#updateStateFromRecalculating(message);
    }

    if (isFlushMessage(message)) {
      // The "2nd level" of the state machine diagram
      this.#updateStateFromFlush(message);
    }

    if (isProgressMessage(message)) {
      // The "3rd level" of the state machine diagram
      this.#updateStateFromProgress(message);
    }
  }

  #updateStateFromRecalculating(message: RecalculatingMessage): void {
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

  #updateStateFromFlush(message: FlushMessage): void {
    for (const name in message.values) {
      this.#updateStateFromValueOrError(name, OutputStates.Value);
    }

    for (const name in message.errors) {
      this.#updateStateFromValueOrError(name, OutputStates.Error);
    }

    // Since req(F, cancelOutput = TRUE) doesn't send a message, we need to identify
    // the situation by looking for outputs that are still idle and move them to cancel.
    for (const [name, state] of this.outputStates) {
      switch (state) {
        case OutputStates.Idle:
          this.#setState(name, OutputStates.Cancel);
          break;
        case OutputStates.Value:
        case OutputStates.Error:
        case OutputStates.Cancel:
        case OutputStates.Persistent:
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

  #updateStateFromProgress(message: ProgressMessage): void {
    const { id, persistent } = message.progress.message;
    const state = this.#getState(id);
    if (persistent) {
      switch (state) {
        case OutputStates.Running:
          this.#setState(id, OutputStates.Persistent);
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
        case OutputStates.Persistent:
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
  #updateStateFromValueOrError(
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

export { OutputProgressReporter };
