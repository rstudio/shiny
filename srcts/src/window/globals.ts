/* eslint "@typescript-eslint/ban-ts-comment": 0 */

// Why this file is just an extension of JQuery with no import/exports
// https://stackoverflow.com/a/30961346/591574

// Model:
// https://stackoverflow.com/a/47091565/591574
// interface JQueryStatic {
//   //custom jquery plugin, had no typings
//   bsDatepicker(method: string): string | number | Date | null;
// }

// @ts-ignore
const WindowShiny = (window.Shiny = window.Shiny || {});
// @ts-ignore
const jQuery: JQueryStatic = window.jQuery;

const userAgent = window.navigator.userAgent;

export { jQuery, jQuery as $, WindowShiny, userAgent };
