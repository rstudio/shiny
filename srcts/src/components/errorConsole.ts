import { css, html, LitElement } from "lit";
import { Shiny } from "../initialize";
import { ShinyClientError } from "../shiny/error";

const buttonStyles = css`
  button {
    background-color: transparent;
    outline: none;
    border-style: none;
    padding: var(--space-3);
    border-radius: var(--space-1);
    font-size: var(--font-lg);
    background-color: inherit;
    display: block;
  }

  button > svg {
    display: block;
  }
`;
class ShinyErrorConsole extends LitElement {
  static styles = [
    css`
      :host {
        /* We declare hard pixel values here to avoid body font size changes
        messing up the size of the console. This was an issue with bslib setting
        the body font-size at 16px relative to base shiny's 14px. */
        --font-md: 14px;
        --font-lg: 16px;
        --font-xl: 18px;

        /* These are all taken from open-props */
        --space-1: 6px;
        --space-2: calc(var(--space-1) * 2);
        --space-3: calc(var(--space-1) * 3);
        --space-4: calc(var(--space-1) * 4);
        --space-8: calc(var(--space-1) * 8);

        --red-2: #ffc9c9;
        --red-6: #fa5252;
        --red-7: #f03e3e;
        --red-8: #e03131;
        --red-10: #b02525;
        --red-11: #962020;
        --red-12: #7d1a1a;

        --gray-1: #f8f9fa;
        --gray-2: #e9ecef;
        --gray-3: #dee2e6;
        --gray-4: #ced4da;
        --gray-6: #868e96;
        --gray-8: #6c757d;

        --green-8: #51cf66;

        --shadow-color: 220 3% 15%;
        --shadow-strength: 1%;
        --shadow-3:
          0 -1px 3px 0
            hsl(var(--shadow-color) / calc(var(--shadow-strength) + 2%)),
          0 1px 2px -5px
            hsl(var(--shadow-color) / calc(var(--shadow-strength) + 2%)),
          0 2px 5px -5px
            hsl(var(--shadow-color) / calc(var(--shadow-strength) + 4%)),
          0 4px 12px -5px
            hsl(var(--shadow-color) / calc(var(--shadow-strength) + 5%)),
          0 12px 15px -5px
            hsl(var(--shadow-color) / calc(var(--shadow-strength) + 7%));

        --ring-shadow: 0 0 0 1px var(--gray-2);

        /* How fast should the message pop in and out of the screen? */
        --animation-speed: 500ms;

        /* Taken from open-props */
        --ease-3: cubic-bezier(0.25, 0, 0.3, 1);
        --animation-slide-in-left: slide-in-left var(--animation-speed)
          var(--ease-3);

        --animation-slide-out-left: slide-out-left var(--animation-speed)
          var(--ease-3);
        --modal-bg-color: white;

        position: fixed;
        top: var(--space-1);
        right: var(--space-1);
        z-index: 1000;

        display: flex;
        flex-direction: column;

        background-color: var(--modal-bg-color);
        border-radius: var(--space-1);

        animation: var(--animation-slide-in-left);
        box-shadow: var(--shadow-3), var(--ring-shadow);

        /* Dont let the error console burst out of the viewport */
        max-height: calc(100vh - 2 * var(--space-1));
      }

      @keyframes slide-in-left {
        from {
          transform: translateX(100%);
        }
      }
      @keyframes slide-out-left {
        to {
          transform: translateX(100%);
        }
      }

      :host(.leaving) {
        animation: var(--animation-slide-out-left);
      }

      .header {
        display: flex;
        justify-content: flex-end;
        align-items: flex-start;
        gap: var(--space-2);
      }

      .title {
        font-size: var(--font-xl);
        margin-right: auto;
        padding: var(--space-3);
        line-height: 1;
        font-weight: 600;
        color: var(--red-12);
      }

      ${buttonStyles}

      button:hover {
        background-color: var(--gray-2);
      }

      .toggle-button {
        width: fit-content;
        border: none;
        aspect-ratio: 1;
        border-color: var(--gray-4);
      }

      .close-button {
        display: flex;
        align-items: center;
        color: var(--red-11);
      }

      .close-button > svg {
        margin-right: 3px;
      }

      .toggle-button:focus {
        outline: 1px solid black;
      }

      .toggle-icon {
        transition: transform var(--animation-speed) ease-in-out;
      }

      :host(.collapsed) .toggle-icon {
        transform: scaleX(-1) scaleY(-1);
      }

      :host(.collapsed) .close-button {
        display: none;
      }

      .content {
        display: block;
        padding-inline: var(--space-4);
        padding-block-start: 0;
        padding-block-end: var(--space-4);
        max-height: 100%;
        overflow: auto;
      }

      :host(.collapsed) .content {
        display: none;
      }
    `,
  ];

  toggleCollapsed(): void {
    this.classList.toggle("collapsed");
    // Remove focus from the toggle button
    (this.querySelector(".toggle-button") as HTMLButtonElement)?.blur();
  }

  handleDismissAll(): void {
    // Animate out by adding the class "leaving" and then
    // wait for the animation to finish before removing the element
    this.classList.add("leaving");
    this.addEventListener("animationend", () => {
      this.remove();
    });
  }

  static createClientMessageElement({ headline, message }: ShinyClientMessage) {
    const msg = document.createElement("shiny-error-message");
    msg.setAttribute("headline", headline || "");
    msg.setAttribute("message", message);
    return msg;
  }

  appendConsoleMessage({ headline, message }: ShinyClientMessage) {
    const content =
      this.shadowRoot?.querySelector<HTMLSlotElement>("slot.content");

    if (content) {
      const nodeKey = (node: Element) => {
        const headline = node.getAttribute("headline") || "";
        const message = node.getAttribute("message") || "";
        return `${headline}::${message}`;
      };
      const newKey = `${headline}::${message}`;

      for (const node of content.assignedElements()) {
        if (node.tagName.toLowerCase() === "shiny-error-message") {
          if (nodeKey(node) === newKey) {
            // Do nothing, this message is already in the console
            // TODO: Increase count of message here
            return;
          }
        }
      }
    }

    this.appendChild(
      ShinyErrorConsole.createClientMessageElement({ headline, message }),
    );
    return;
  }

  render() {
    return html` <div class="header">
        <span class="title"> Shiny Client Errors </span>
        <button
          class="close-button"
          @click=${this.handleDismissAll}
          title="Dismiss all console messages and close console"
        >
          <svg
            xmlns="http://www.w3.org/2000/svg"
            fill="none"
            viewBox="0 0 24 24"
            stroke-width="1.5"
            height="1em"
            width="1em"
            stroke="currentColor"
            class="close-icon"
          >
            <path
              stroke-linecap="round"
              stroke-linejoin="round"
              d="M6 18L18 6M6 6l12 12"
            />
          </svg>
          Dismiss all
        </button>
        <button class="toggle-button" @click=${this.toggleCollapsed}>
          <svg
            xmlns="http://www.w3.org/2000/svg"
            fill="none"
            viewBox="0 0 24 24"
            stroke-width="1.5"
            height="1em"
            width="1em"
            stroke="currentColor"
            class="toggle-icon"
          >
            <path
              class="collapse"
              stroke-linecap="round"
              stroke-linejoin="round"
              d="M4.5 19.5l15-15m0 0H8.25m11.25 0v11.25"
            />
          </svg>
        </button>
      </div>
      <slot class="content"></slot>`;
  }
}

customElements.define("shiny-error-console", ShinyErrorConsole);

export class ShinyErrorMessage extends LitElement {
  static properties = {
    headline: {},
    message: {},
  };

  headline = "";
  message = "";

  static styles = [
    css`
      :host {
        color: var(--red-11);
        display: block;
        font-size: var(--font-md);

        position: relative;
        --icon-size: var(--font-lg) /* Reset box sizing */
          box-sizing: border-box;
      }

      .container {
        display: flex;
        gap: var(--space-2);
      }

      .contents {
        width: 40ch;
        display: flex;
        flex-direction: column;
        gap: var(--space-1);
        padding-block-start: 0;
        padding-block-end: var(--space-3);
        overflow: auto;
      }

      :host(:last-of-type) .contents {
        padding-block-end: var(--space-1);
      }

      .contents > h3 {
        font-size: 1em;
        font-weight: 500;
        color: var(--red-12);
      }

      .contents > * {
        margin-block: 0;
      }

      .error-message {
        font-family: "Courier New", Courier, monospace;
        white-space: pre-wrap;
      }

      .decoration-container {
        flex-shrink: 0;
        position: relative;

        --line-w: 2px;
        --dot-size: 11px;
      }

      :host(:hover) .decoration-container {
        --scale: 1.25;
      }

      .vertical-line {
        margin-inline: auto;
        width: var(--line-w);
        height: 100%;

        background-color: var(--red-10);
      }

      :host(:first-of-type) .vertical-line {
        height: calc(100% - var(--dot-size));
        margin-top: var(--dot-size);
      }

      .dot {
        position: absolute;
        width: var(--dot-size);
        height: var(--dot-size);
        top: calc(-1px + var(--dot-size) / 2);
        left: calc(50% - var(--dot-size) / 2);
        border-radius: 100%;
        transform: scale(var(--scale, 1));

        color: var(--red-6);
        background-color: var(--red-10);
      }

      .actions {
        transform: scaleX(0);
        transition: transform calc(var(--animation-speed) / 2) ease-in-out;
        display: flex;
        justify-content: center;
        flex-direction: column;
      }

      /* Delay transition on mouseout so the buttons don't jump away if the user
      overshoots them with their mouse */
      :host(:not(:hover)) .actions {
        transition-delay: 0.15s;
      }

      :host(:hover) .actions {
        transform: scaleX(1);
      }

      ${buttonStyles}

      .copy-button {
        padding: 0;
        width: var(--space-8);
        height: var(--space-8);
        position: relative;
        --pad: var(--space-2);
      }

      .copy-button-inner {
        position: relative;
        width: 100%;
        height: 100%;
        border-radius: inherit;
        transition: transform 0.5s;
        transform-style: preserve-3d;
      }

      /* Animate flipping to the other side when the .copy-success class is
      added to the host */
      :host(.copy-success) .copy-button-inner {
        transform: rotateY(180deg);
      }

      /* Position the front and back side */
      .copy-button .front,
      .copy-button .back {
        --side: calc(100% - 2 * var(--pad));
        position: absolute;
        inset: var(--pad);
        height: var(--side);
        width: var(--side);
        -webkit-backface-visibility: hidden; /* Safari */
        backface-visibility: hidden;
      }

      .copy-button:hover .copy-button-inner {
        background-color: var(--gray-2);
      }

      /* Style the back side */
      .copy-button .back {
        --pad: var(--space-1);
        color: var(--green-8);
        transform: rotateY(180deg);
      }
    `,
  ];

  async copyErrorToClipboard(): Promise<void> {
    await navigator.clipboard.writeText(this.message);

    this.classList.add("copy-success");

    // After a second, remove the copy success class
    setTimeout(() => {
      this.classList.remove("copy-success");
    }, 1000);
  }

  // eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
  render() {
    return html`
      <div class="container">
        <div class="decoration-container">
          <div class="vertical-line"></div>
          <div class="dot"></div>
        </div>
        <div class="contents">
          <h3>${this.headline}</h3>
          <pre class="error-message">${this.message}</pre>
        </div>

        <div class="actions">
          <button
            class="copy-button"
            @click=${this.copyErrorToClipboard}
            title="Copy error to clipboard"
          >
            <div class="copy-button-inner">
              <svg
                class="front"
                xmlns="http://www.w3.org/2000/svg"
                fill="none"
                viewBox="0 0 24 24"
                stroke-width="1.5"
                stroke="currentColor"
                height="1em"
                width="1em"
              >
                <path
                  stroke-linecap="round"
                  stroke-linejoin="round"
                  d="M15.666 3.888A2.25 2.25 0 0013.5 2.25h-3c-1.03 0-1.9.693-2.166 1.638m7.332 0c.055.194.084.4.084.612v0a.75.75 0 01-.75.75H9a.75.75 0 01-.75-.75v0c0-.212.03-.418.084-.612m7.332 0c.646.049 1.288.11 1.927.184 1.1.128 1.907 1.077 1.907 2.185V19.5a2.25 2.25 0 01-2.25 2.25H6.75A2.25 2.25 0 014.5 19.5V6.257c0-1.108.806-2.057 1.907-2.185a48.208 48.208 0 011.927-.184"
                />
              </svg>

              <svg
                class="back"
                xmlns="http://www.w3.org/2000/svg"
                fill="none"
                viewBox="0 0 24 24"
                stroke-width="1.5"
                stroke="currentColor"
                height="1em"
                width="1em"
              >
                <path
                  stroke-linecap="round"
                  stroke-linejoin="round"
                  d="M9 12.75L11.25 15 15 9.75M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
                />
              </svg>
            </div>
          </button>
        </div>
      </div>
    `;
  }
}

customElements.define("shiny-error-message", ShinyErrorMessage);

export type ShinyClientMessage = {
  message: string;
  headline?: string;
  status?: "error" | "info" | "warning";
};

function showShinyClientMessage({
  headline = "",
  message,
  status = "warning",
}: ShinyClientMessage): void {
  const consoleMessage = `[shiny] ${headline}${
    headline ? " - " : ""
  }${message}`;

  switch (status) {
    case "error":
      console.error(consoleMessage);
      break;
    case "warning":
      console.warn(consoleMessage);
      break;
    default:
      console.log(consoleMessage);
      break;
  }

  if (!Shiny.inDevMode()) {
    return;
  }

  // Check to see if an Error Console Container element already exists. If it
  // doesn't we need to add it before putting an error on the screen
  let sec = document.querySelector<ShinyErrorConsole>("shiny-error-console");
  if (!sec) {
    sec = document.createElement("shiny-error-console") as ShinyErrorConsole;
    document.body.appendChild(sec);
  }

  sec.appendConsoleMessage({ headline, message });
}

/**
 * Function to show an error message to user in shiny-error-message web
 * component. Only shows the error if we're in development mode.
 * @param e - Error object to show to user. This is whatever is caught in
 * a try-catch statement so it may be a string or it may be a proper Error
 * object.
 */
export function showErrorInClientConsole(e: unknown): void {
  let errorMsg: string | null = null;
  let headline = "Error on client while running Shiny app";

  if (typeof e === "string") {
    errorMsg = e;
  } else if (e instanceof ShinyClientError) {
    errorMsg = e.message;
    headline = e.headline;
  } else if (e instanceof Error) {
    errorMsg = e.message;
  } else {
    errorMsg = "Unknown error";
  }

  showShinyClientMessage({ headline, message: errorMsg, status: "error" });
}

export class ShinyClientMessageEvent extends CustomEvent<ShinyClientMessage> {
  constructor(detail: ShinyClientMessage) {
    super("shiny:client-message", { detail, bubbles: true, cancelable: true });
  }
}

window.addEventListener("shiny:client-message", (ev: Event) => {
  if (!(ev instanceof CustomEvent)) {
    throw new Error("[shiny] shiny:client-message expected a CustomEvent");
  }
  const { headline, message, status } = ev.detail;
  if (!message) {
    throw new Error(
      "[shiny] shiny:client-message expected a `message` property in `event.detail`.",
    );
  }
  showShinyClientMessage({ headline, message, status });
});
