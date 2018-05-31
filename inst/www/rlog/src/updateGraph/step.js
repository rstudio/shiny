// @flow

import { rlog } from "../rlog";
import { updateGraph } from "../updateGraph";

let nextStep = function(): void {
  // Move one step ahead (skipping unneccessary steps)
  updateGraph(rlog.getGraph.nextStep(rlog.curTick));
};

let prevStep = function(): void {
  // Move one step back
  updateGraph(rlog.getGraph.prevStep(rlog.curTick));
};

export { nextStep, prevStep };
