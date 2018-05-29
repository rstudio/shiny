// @flow

let hasWindowConsole =
  true &&
  typeof window !== "undefined" &&
  typeof window.console !== "undefined";

let console: console;

if (hasWindowConsole) {
  console = window.console;
} else {
  console = {
    log: function(...data: Array<any>): void {},
    warn: function(...data: Array<any>): void {},
    error: function(...data: Array<any>): void {},
  };
}

export default console;
