// @flow

import cytoscape from "cytoscape";

import { Graph } from "./graph/Graph";
import { GraphAtStep } from "./graph/GraphAtStep";
import * as updateGraph from "./updateGraph";

import type { CytoscapeType } from "./cyto/cytoFlowType";
import type { LogType } from "./log/logStates";

// TODO-barret change to individual exports
let ret = {
  log: ([]: LogType),
  cyto: (cytoscape(): CytoscapeType),
  getGraph: new GraphAtStep([]),
  graph: new Graph([]),
  curTick: (1: number),
  updateGraph: updateGraph,
  barret: (null: any),
};

export { ret as rlog };
