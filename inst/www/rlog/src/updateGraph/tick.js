// @flow

import { rlog } from "../rlog";
import { updateGraph } from "../updateGraph";

let nextTick = function(): void {
  updateGraph(rlog.curTick + 1);
};

let prevTick = function(): void {
  updateGraph(rlog.curTick - 1);
};

export { nextTick, prevTick };
