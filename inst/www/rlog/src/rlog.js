// @flow

import { GraphAtStep } from "./graph/GraphAtStep";

let ret = {
  log: [],
  cyto: null,
  getGraph: new GraphAtStep([]),
  curTick: 1,
  barret: null,
};

export default ret;
