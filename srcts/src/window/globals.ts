declare global {
  interface Window {
    Shiny: any;
    jQuery: JQueryStatic;
  }
}

/* eslint "@typescript-eslint/ban-ts-comment": 0 */
// @ts-ignore
const WindowShiny = (window.Shiny = window.Shiny || {});
// @ts-ignore
const jQuery: JQueryStatic = window.jQuery;

const userAgent = window.navigator.userAgent;

export { jQuery, jQuery as $, WindowShiny, userAgent };
