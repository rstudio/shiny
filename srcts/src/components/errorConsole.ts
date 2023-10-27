import { LitElement, html, css } from "lit";

export class ErrorConsole extends LitElement {
  /**
   * Message to show to user
   */
  static properties = {
    description: {},
    message: {},
  };

  description = "";
  message = "";

  static styles = [
    css`
      :host {
        /* Taken from open-props */
        --ease-3: cubic-bezier(0.25, 0, 0.3, 1);
        --animation-slide-in-left: slide-in-left 0.5s var(--ease-3);
        --animation-slide-out-left: slide-out-left 0.5s var(--ease-3);
        --red-2: #ffc9c9;
        --red-6: #fa5252;
        --red-7: #f03e3e;
        --red-8: #e03131;
        --red-9: #c92a2a;
        --red-10: #b02525;
        --red-11: #962020;
        --red-12: #7d1a1a;

        --icon-size: 2rem;
        --rounded: 0.5rem;

        color: var(--red-11);
        display: block;
        position: fixed;
        top: 0.5rem;
        right: 0.5rem;
        width: 40ch;
        animation: var(--animation-slide-in-left);
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

      .description {
        /* font-weight: bold; */
      }

      .error-message {
        font-family: "Courier New", Courier, monospace;
      }

      .error-icon {
        height: 2rem;
        width: 2rem;
        color: var(--red-7);
      }

      .container {
        background-color: var(--red-2);

        display: flex;
        gap: 1rem;
        padding: 1rem;
        border-radius: var(--rounded);
      }

      .icon-container,
      .close-button-container {
        flex-shrink: 0;
      }

      .close-button {
        --pad: 0.375rem;
        /* inline-flex rounded-md bg-green-50 p-1.5 text-green-500 hover:bg-green-100 focus:outline-none focus:ring-2 focus:ring-green-600 focus:ring-offset-2 focus:ring-offset-green-50 */
        display: inline-flex;
        padding: var(--pad);

        margin-right: calc(-1 * var(--pad));
        margin-top: calc(-1 * var(--pad));

        color: var(--red-7);
        background-color: inherit;
        outline: none;
        border: none;
        border-radius: var(--rounded);
      }

      .close-button:hover {
        /* bg-green-100 text-green-600 */
        background-color: var(--red-6);
        color: var(--red-10);
      }

      .close-icon {
        height: var(--icon-size);
        width: var(--icon-size);
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
        <div class="msg-container">
          <p class="description">${this.description}</p>
          <p class="error-message">${this.message}</p>
        </div>
        <div class="close-button-container">
          <button @click=${this.handleClose} type="button" class="close-button">
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
  let errorMsg: string;

  if (typeof e === "string") {
    errorMsg = e;
  } else if (e instanceof Error) {
    errorMsg = e.message;
  } else {
    errorMsg = "Unknown error";
  }

  const errorConsole = document.createElement("shiny-error-console");
  errorConsole.setAttribute("description", "Error on page");
  errorConsole.setAttribute("message", errorMsg);
  document.body.appendChild(errorConsole);
}
