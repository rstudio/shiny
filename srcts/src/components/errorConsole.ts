/* eslint-disable indent */
import { LitElement, html, css } from "lit";

class ErrorConsoleContainer extends LitElement {
  static styles = [
    css`
      :host {
        --spacing: 0.5rem;

        position: fixed;
        top: var(--spacing);
        right: var(--spacing);
        z-index: 1000;

        display: flex;
        flex-direction: column;
        gap: var(--spacing);
      }
    `,
  ];

  // eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
  render() {
    return html`<slot></slot>`;
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
        /* Height and width of the error icon and close button icon */
        --icon-size: 2rem;

        /* How rounded corners should be. */
        --rounded: 0.5rem;

        /* How much padding should be around all elements in error "card" */
        --container-padding: 1.5rem;

        /* How much space should be between the various parts of the card */
        --content-gaps: 1rem;

        /* How fast should the message pop in and out of the screen? */
        --animation-speed: 1s;

        /* Taken from open-props */
        --ease-3: cubic-bezier(0.25, 0, 0.3, 1);
        --animation-slide-in-left: slide-in-left var(--animation-speed)
          var(--ease-3);
        --animation-slide-out-left: slide-out-left var(--animation-speed)
          var(--ease-3);
        --red-2: #ffc9c9;
        --red-6: #fa5252;
        --red-7: #f03e3e;
        --red-8: #e03131;
        --red-10: #b02525;
        --red-11: #962020;
        --red-12: #7d1a1a;

        color: var(--red-11);
        display: block;
        animation: var(--animation-slide-in-left);
        font-size: 1.4rem;
      }

      :host(.leaving) {
        animation: var(--animation-slide-out-left);
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

      .container {
        background-color: var(--red-2);

        display: flex;
        gap: var(--content-gaps);
        padding: var(--container-padding);
        border-radius: var(--rounded);
      }

      .contents {
        width: 40ch;
        display: flex;
        flex-direction: column;
        gap: var(--content-gaps);
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

      .icon-container,
      .close-button-container {
        --pad: 0.375rem;
        flex-shrink: 0;
      }

      .close-icon,
      .error-icon {
        height: var(--icon-size);
        width: var(--icon-size);
      }

      .error-icon {
        color: var(--red-6);
      }

      .icon-container {
        padding-inline: var(--pad);
      }

      .close-button {
        display: inline-flex;
        padding: var(--pad);

        /* Shift up and out a bit to offset the invisible button padding */
        margin-right: calc(-1 * var(--pad));
        margin-top: calc(-1 * var(--pad));

        color: var(--red-7);
        background-color: inherit;
        outline: none;
        border: none;
        border-radius: var(--rounded);
      }

      .close-button:hover {
        background-color: var(--red-6);
        color: var(--red-10);
      }

      .close-button:focus {
        outline: 1px solid var(--red-10);
      }

      .sr-only {
        position: absolute;
        width: 1px;
        height: 1px;
        padding: 0;
        margin: -1px;
        overflow: hidden;
        clip: rect(0, 0, 0, 0);
        white-space: nowrap;
        border-width: 0;
      }
    `,
  ];

  handleClose(): void {
    // Animate out by adding the class "leaving" and then
    // wait for the animation to finish before removing the element
    this.classList.add("leaving");
    this.addEventListener("animationend", () => {
      this.remove();
    });
  }

  // eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
  render() {
    return html`
      <div class="container">
        <div class="icon-container">
          <svg
            class="error-icon"
            viewBox="0 0 20 20"
            fill="currentColor"
            aria-hidden="true"
          >
            <path
              fill-rule="evenodd"
              d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.28 7.22a.75.75 0 00-1.06 1.06L8.94 10l-1.72 1.72a.75.75 0 101.06 1.06L10 11.06l1.72 1.72a.75.75 0 101.06-1.06L11.06 10l1.72-1.72a.75.75 0 00-1.06-1.06L10 8.94 8.28 7.22z"
              clip-rule="evenodd"
            />
          </svg>
        </div>
        <div class="contents">
          <h3>${this.headline}</h3>
          <p class="error-message">${this.message}</p>
        </div>
        <div class="close-button-container">
          <button
            @click=${this.handleClose}
            type="button"
            class="close-button"
            title="Dismiss error console"
          >
            <span class="sr-only">Dismiss</span>
            <svg
              class="close-icon"
              viewBox="0 0 20 20"
              fill="currentColor"
              aria-hidden="true"
            >
              <path
                d="M6.28 5.22a.75.75 0 00-1.06 1.06L8.94 10l-3.72 3.72a.75.75 0 101.06 1.06L10 11.06l3.72 3.72a.75.75 0 101.06-1.06L11.06 10l3.72-3.72a.75.75 0 00-1.06-1.06L10 8.94 6.28 5.22z"
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
