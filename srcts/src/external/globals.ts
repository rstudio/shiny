declare global {
  interface Window {
    Shiny: any;
    jQuery: JQueryStatic;
  }
}

const WindowShiny = (window.Shiny = window.Shiny || {});
const jQuery: JQueryStatic = window.jQuery;

export { jQuery, jQuery as $, WindowShiny };
