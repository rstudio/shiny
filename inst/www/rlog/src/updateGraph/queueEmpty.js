// @flow

import { rlog } from "../rlog";
import { updateGraph } from "../updateGraph";

let nextQueueEmpty = function(): boolean {
  let i, val;
  // move to queue empty
  for (i = 0; i < rlog.getGraph.enterExitEmpties.length; i++) {
    val = rlog.getGraph.queueEmpties[i];
    if (rlog.curTick < val) {
      updateGraph(val);
      return true;
    }
  }
  return false;
};
let prevQueueEmpty = function(): boolean {
  let i, val;
  // move to queue empty
  for (i = rlog.getGraph.queueEmpties.length - 1; i >= 0; i--) {
    val = rlog.getGraph.queueEmpties[i];
    if (rlog.curTick > val) {
      updateGraph(val);
      return true;
    }
  }
  return false;
};

let lastQueueEmpty = function(): void {
  let nextTick =
    rlog.getGraph.queueEmpties[rlog.getGraph.queueEmpties.length - 1] || 0;
  updateGraph(nextTick);
};
let firstQueueEmpty = function(): void {
  let nextTick = rlog.getGraph.queueEmpties[0] || 0;
  updateGraph(nextTick);
};

export { nextQueueEmpty, prevQueueEmpty, lastQueueEmpty, firstQueueEmpty };
