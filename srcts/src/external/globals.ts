declare global {
  interface Window {
    Shiny: any;
    jQuery: JQueryStatic;
  }
}

const WindowShiny = (window.Shiny = window.Shiny || {});
const jQuery: JQueryStatic = window.jQuery;

const userAgent = window.navigator.userAgent;

export { jQuery, jQuery as $, WindowShiny, userAgent };
