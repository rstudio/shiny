// @flow

import { rlog } from "../rlog";

import * as logEntry from "../layout/logEntry";
import * as progressBar from "../layout/progressBar";

let atTick = function(nextTick: number = rlog.curTick) {
  rlog.curTick = nextTick;
  rlog.getGraph.displayAtStep(nextTick, rlog.cyto);
  progressBar.update();
  logEntry.update();
};

export { atTick };
