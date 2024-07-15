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
type ShinyClientMessage = {
    headline?: string;
    message: string;
};
/**
 * Function to show an error message to user in shiny-error-message web
 * component. Only shows the error if we're in development mode.
 * @param e - Error object to show to user. This is whatever is caught in
 * a try-catch statement so it may be a string or it may be a proper Error
 * object.
 */
export declare function showErrorInClientConsole(e: unknown): void;
export declare class ShinyClientMessageEvent extends CustomEvent<ShinyClientMessage> {
    constructor(detail: ShinyClientMessage);
}
export {};
