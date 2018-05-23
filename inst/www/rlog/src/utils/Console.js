let hasWindowConsole =
  true &&
  typeof window !== "undefined" &&
  typeof window.console !== "undefined";

let console;

if (hasWindowConsole) {
  console = window.console;
} else {
  console = {
    log: function() {},
    warn: function() {},
    error: function() {},
  };
}

export default console;
