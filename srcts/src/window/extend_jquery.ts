// Why this file is just an extension of JQuery with no import/exports
// https://stackoverflow.com/a/30961346/591574

// Model:
// https://stackoverflow.com/a/47091565/591574
interface JQuery {
  //custom jquery plugin, had no typings
  bsDatepicker(method: string): string | number | Date | null;
}
