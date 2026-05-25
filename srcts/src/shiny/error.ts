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
