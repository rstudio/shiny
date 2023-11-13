import { LitElement } from "lit";
export declare class ShinyErrorMessage extends LitElement {
    static properties: {
        headline: {};
        message: {};
    };
    headline: string;
    message: string;
    static styles: import("lit").CSSResult[];
    copyErrorToClipboard(): Promise<void>;
    render(): import("lit-html").TemplateResult<1>;
}
/**
 * Function to show an error message to user in shiny-error-message web
 * component
 * @param e - Error object to show to user. This is whatever is caught in
 * a try-catch statement so it may be a string or it may be a proper Error
 * object.
 */
export declare function showErrorInClientConsole(e: unknown): void;
/**
 * Custom error to throw when a we detect a known error type on the client
 * @param headline - Error headline to show to user. Will be shown in normal
 * font and should be used to give plain language description of problem
 * @param message - Error message to show to user. Will be shown in monospaced
 * font
 */
export declare class ShinyClientError extends Error {
    headline: string;
    constructor({ headline, message }: {
        headline: string;
        message: string;
    });
}
