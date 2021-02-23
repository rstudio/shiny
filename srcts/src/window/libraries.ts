// Why this file is just an extension of JQuery with no import/exports
// https://stackoverflow.com/a/30961346/591574

// Model:
// https://stackoverflow.com/a/47091565/591574
// interface JQueryStatic {
//   //custom jquery plugin, had no typings
//   bsDatepicker(method: string): string | number | Date | null;
// }

function windowShiny(): any {
  return (window as any)["Shiny"] || {};
}
function windowJQuery(): JQueryStatic {
  return (window as any)["jQuery"] as JQueryStatic;
}

export { windowShiny, windowJQuery };
