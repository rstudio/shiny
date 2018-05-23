import rlog from "../rlog";
import updateGraph from "./atTick";

let nextStep = function() {
  // Move one step ahead (skipping unneccessary steps)
  updateGraph(rlog.getGraph.nextStep(rlog.curTick));
};

let prevStep = function() {
  // Move one step back
  updateGraph(rlog.getGraph.prevStep(rlog.curTick));
};

export { nextStep, prevStep };
