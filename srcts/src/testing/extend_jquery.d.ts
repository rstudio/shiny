// Why this file is just an extension of JQuery with no import/exports
// https://stackoverflow.com/a/30961346/591574

// Model:
// https://stackoverflow.com/a/47091565/591574
// interface JQueryStatic {
//   //custom jquery plugin, had no typings
//   bsDatepicker(method: string): string | number | Date | null;
// }

// Modified from : stackoverflow.com/a/12722003/591574
/// <reference types="jquery"/>

interface JQuery {
  // used for testing only
  _test: () => void;
}
