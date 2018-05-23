import rlog from "../rlog";
import updateGraph from "./atTick";

let nextTick = function() {
  updateGraph(rlog.curTick + 1);
};

let prevTick = function() {
  updateGraph(rlog.curTick - 1);
};

export { nextTick, prevTick };
