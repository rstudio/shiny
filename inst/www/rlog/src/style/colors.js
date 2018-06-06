// @flow

let colors = {
  // regular colors
  regular: {
    white: "#ffffff",
    black: "#000000",

    // http://colorbrewer2.org/#type=sequential&scheme=YlGn&n=4
    // #2-4
    green1: "#f7fcb9", // ready
    green2: "#78c679", // enter
    green3: "#238443", // active enter

    greenLite: "#b2df8a", // green from http://colorbrewer2.org/#type=qualitative&scheme=Paired&n=8

    // http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=9
    red: "#e41a1c", // valueChange
    blue: "#377eb8", // frozen
    green: "#4daf4a", // enter
    purple: "#984ea3", //
    purpleLite: "#f191ff", //
    orange: "#ff7f00", //
    yellow: "#ffff33", //
    brown: "#a65628", //
    pink: "#f781bf", //
    grey: "#999999", // invalidate

    // http://colorbrewer2.org/#type=sequential&scheme=Greys&n=9
    grey1: "#d9d9d9", // invalidate
    grey2: "#969696", // active invalidate
    grey3: "#737373", // active invalidate
  },
  edges: {
    running: "#676767",
    isolate: "#818181",
    active: "#818181",
    inactive: "#ececec",
  },
  ghostEdges: {
    default: "#3c3b39",
  },
  frozen: {
    default: "#2171b5",
  },
  // filtered colors
  lite: {
    white: "#ffffff",
    black: "#b2b2b2", // personal attempt

    // http://colorbrewer2.org/#type=sequential&scheme=YlGn&n=9
    // #1-3
    green1: "#ffffe5",
    green2: "#f7fcb9",
    green3: "#d9f0a3",

    greenLite: "#d6eec0", // personal attempt
    // http://colorbrewer2.org/#type=qualitative&scheme=Pastel1&n=9
    red: "#fbb4ae",
    blue: "#b3cde3",
    green: "#ccebc5",
    purple: "#decbe4",
    orange: "#fed9a6",
    yellow: "#ffffcc",
    brown: "#e5d8bd",
    pink: "#fddaec",
    grey: "#f2f2f2",
  },
};

export { colors };
export default colors;
