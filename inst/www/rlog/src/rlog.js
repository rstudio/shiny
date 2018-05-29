// @flow

import { Graph } from "./graph/Graph";
import { GraphAtStep } from "./graph/GraphAtStep";

let ret = {
  log: [],
  cyto: null,
  getGraph: new GraphAtStep([]),
  graph: new Graph([]),
  curTick: 1,
  barret: null,
};

export default ret;
