import $ from "jquery";

// Merge opts with defaults, and return a new object.
function addDefaultInputOpts(opts) {
  opts = $.extend(
    {
      priority: "immediate",
      binding: null,
      el: null,
    },
    opts
  );

  if (opts && typeof opts.priority !== "undefined") {
    switch (opts.priority) {
      case "deferred":
      case "immediate":
      case "event":
        break;
      default:
        throw new Error("Unexpected input value mode: '" + opts.priority + "'");
    }
  }

  return opts;
}

class InputValidateDecorator {
  target;

  constructor(target) {
    this.target = target;
  }

  setInput = function (nameType, value, opts): void {
    if (!nameType) throw "Can't set input with empty name.";

    opts = addDefaultInputOpts(opts);

    this.target.setInput(nameType, value, opts);
  };
}

export { InputValidateDecorator };
