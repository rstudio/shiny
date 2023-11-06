/* eslint-disable @typescript-eslint/naming-convention */

import { LitElement, html, css } from "lit";

const buttonStyles = css`
  button {
    background-color: transparent;
    outline: none;
    border-style: none;
    padding: var(--space-2);
    border-radius: var(--space-1);
    font-size: 1.5rem;
    background-color: inherit;
    display: block;
  }

  button:hover {
    background-color: var(--gray-2);
  }

  button > svg {
    display: block;
  }
`;
class ErrorConsoleContainer extends LitElement {
  static styles = [
    css`
      :host {
        /* These are all taken from open-props */
        --space-1: 0.5rem;
        --space-2: calc(var(--space-1) * 2);
        --space-3: calc(var(--space-1) * 3);
        --space-4: calc(var(--space-1) * 4);

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

        --shadow-color: 220 3% 15%;
        --shadow-strength: 1%;
        --shadow-3: 0 -1px 3px 0 hsl(var(--shadow-color) /
                calc(var(--shadow-strength) + 2%)),
          0 1px 2px -5px hsl(var(--shadow-color) /
                calc(var(--shadow-strength) + 2%)),
          0 2px 5px -5px hsl(var(--shadow-color) /
                calc(var(--shadow-strength) + 4%)),
          0 4px 12px -5px hsl(var(--shadow-color) /
                calc(var(--shadow-strength) + 5%)),
          0 12px 15px -5px hsl(var(--shadow-color) /
                calc(var(--shadow-strength) + 7%));

        --ring-shadow: 0 0 0 1px var(--gray-2);

        /* How fast should the message pop in and out of the screen? */
        --animation-speed: 1s;

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

      ${buttonStyles}

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
        transition: transform var(--anim-speed) ease-in-out;
      }

      :host(.collapsed) .toggle-icon {
        transform: scaleX(-1) scaleY(-1);
      }

      :host(.collapsed) .close-button {
        display: none;
      }

      .content {
        display: block;
        padding: var(--space-4);
        padding-top: var(--space-2);
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

  render() {
    return html` <div class="header">
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

customElements.define("shiny-error-console-container", ErrorConsoleContainer);

export class ErrorConsole extends LitElement {
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
        font-size: 1.4rem;

        position: relative;
        --anim-speed: 0.2s;
        --icon-size: 1.5rem;
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
        padding-bottom: 1.5rem;
        padding-top: 0.5rem;
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
        color: var(--red-11);
        font-family: "Courier New", Courier, monospace;
      }

      .decoration-container {
        flex-shrink: 0;
        padding-inline: var(0.375rem);
        position: relative;

        --line-w: 2px;
        --dot-size: 1rem;
      }

      :host(:hover) .decoration-container {
        --scale: 1.25;
      }

      .vertical-line {
        margin-inline: auto;
        width: var(--line-w);
        height: 100%;

        background-color: var(--red-10);

        transform: scaleX(var(--scale, 1));

        transition: transform var(--anim-speed) ease-in-out;
      }

      .dot {
        position: absolute;
        width: var(--dot-size);
        height: var(--dot-size);
        top: calc(50% - var(--dot-size) / 2);
        left: calc(50% - var(--dot-size) / 2);
        border-radius: 100%;
        transform: scale(var(--scale, 1));

        color: var(--red-6);
        background-color: var(--red-10);
      }

      .actions {
        transform: scaleX(0);
        transition: transform var(--anim-speed) ease-in-out;
      }

      /* Delay transition on mouseout so the buttons don't jump away if the user
      overshoots them with their mouse */
      :host(:not(:hover)) .actions {
        transition-delay: 0.5s;
      }

      :host(:hover) .actions {
        transform: scaleX(1);
      }

      ${buttonStyles}

      .actions > button {
        aspect-ratio: 1;
      }

      .copy-success-msg {
        animation: slide-in-and-out-left 2s ease-in-out;
        position: absolute;
        height: 100%;
        display: grid;
        place-content: center;

        right: 0;
        top: 0;
      }
      .copy-success-msg > div {
        /* Blur content behind div */
        backdrop-filter: blur(3px);
        padding: var(--space-1);
      }

      @keyframes slide-in-and-out-left {
        0% {
          opacity: 0;
          filter: brightness(1) blur(20px);
        }
        5% {
          opacity: 1;
          filter: brightness(2) blur(10px);
        }
        40% {
          opacity: 1;
          filter: brightness(1) blur(0);
        }
        60% {
          opacity: 1;
          filter: brightness(1) blur(0);
        }
        95% {
          opacity: 1;
          filter: brightness(2) blur(10px);
        }
        100% {
          opacity: 0;
          filter: brightness(1) blur(20px);
        }

        /* 0% {
          transform: translateX(100%);
        }
        50% {
          transform: translateX(0);
        }
        100% {
          transform: translateX(100%);
        } */
      }
    `,
  ];

  async copyErrorToClipboard(): Promise<void> {
    await navigator.clipboard.writeText(this.message);

    const copySuccessMsg = document.createElement("div");
    copySuccessMsg.classList.add("copy-success-msg");
    copySuccessMsg.innerHTML = "<div>Copied error!</div>";
    this.shadowRoot?.appendChild(copySuccessMsg);

    // Remove after animation finishes
    copySuccessMsg.addEventListener("animationend", () => {
      copySuccessMsg.remove();
    });
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
          <p class="error-message">${this.message}</p>
        </div>

        <div class="actions">
          <button
            @click=${this.copyErrorToClipboard}
            title="Copy error to clipboard"
          >
            <svg
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
          </button>
        </div>
      </div>
    `;
  }
}

customElements.define("shiny-error-console", ErrorConsole);

/**
 * Function to show an error message to user in shiny-error-console web
 * component
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

  // Check to see if an Error Console Container element already exists. If it
  // doesn't we need to add it before putting an error on the screen
  let errorConsoleContainer = document.querySelector(
    "shiny-error-console-container"
  );
  if (!errorConsoleContainer) {
    errorConsoleContainer = document.createElement(
      "shiny-error-console-container"
    );
    document.body.appendChild(errorConsoleContainer);
  }

  const errorConsole = document.createElement("shiny-error-console");
  errorConsole.setAttribute("headline", headline || "");
  errorConsole.setAttribute("message", errorMsg);

  errorConsoleContainer.appendChild(errorConsole);
}

/**
 * Custom error to throw when a we detect a known error type on the client
 * @param headline - Error headline to show to user. Will be shown in normal
 * font and should be used to give plain language description of problem
 * @param message - Error message to show to user. Will be shown in monospaced
 * font
 */
export class ShinyClientError extends Error {
  headline: string;

  constructor({ headline, message }: { headline: string; message: string }) {
    super(message);
    this.name = "ShinyClientError";
    this.headline = headline;
  }
}
