/**
 * Thrown by a `dom/*` dispatch function when the requested operation
 * requires jQuery and `window.jQuery` is not available.
 *
 * The wrapper layer (see spec 2026-06-03-jquery-wrapper-design.md) treats
 * jQuery as an optional peer dependency. Most operations have a native
 * fallback; some — notably the jQuery-plugin widgets — do not, and throw
 * this error with a descriptive `feature` string and (optionally) the
 * `constructor.name` of the binding that triggered the call.
 */
export class JQueryRequiredError extends Error {
  public readonly feature: string;
  public readonly binding?: string;

  constructor(feature: string, binding?: string) {
    const where = binding ? ` (used by binding "${binding}")` : "";
    super(
      `Shiny feature "${feature}"${where} requires jQuery, ` +
        `which is not loaded on this page. ` +
        `Load jQuery 3.x before Shiny initializes, ` +
        `or replace the binding with a native-compatible alternative.`,
    );
    this.name = "JQueryRequiredError";
    this.feature = feature;
    this.binding = binding;
  }
}
