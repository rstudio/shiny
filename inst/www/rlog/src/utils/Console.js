// @flow

let console = window.globalConsole || {
  log: function(...data: Array<any>): void {},
  warn: function(...data: Array<any>): void {},
  error: function(...data: Array<any>): void {},
};

export default console;
