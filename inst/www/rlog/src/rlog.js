// @flow

import cytoscape from "cytoscape";

import { Graph } from "./graph/Graph";
import { GraphAtStep } from "./graph/GraphAtStep";

import type { CytoscapeType } from "./cyto/cytoFlowType";
import type { LogType } from "./log/logStates";

let ret = {
  log: ([]: LogType),
  cyto: (cytoscape(): CytoscapeType),
  getGraph: new GraphAtStep([]),
  graph: new Graph([]),
  curTick: (1: number),
  barret: (null: any),
};

export default ret;
