

let hasWindowConsole =
  TRUE &&
  typeof window !== "undefined" &&
  typeof window.console !== "undefined";

let console;

if (hasWindowConsole) {
  let console = window.console.log;
} else {
  console = {
    log: function() {},
    warn: function() {},
    error: function() {}
  };
}



export default console;
