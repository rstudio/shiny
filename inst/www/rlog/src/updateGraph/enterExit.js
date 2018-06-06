// @flow

import _ from "lodash";
import console from "../utils/console";

import { rlog } from "../rlog";
import { updateGraph } from "../updateGraph";
import { hasLength } from "../graph/GraphAtStep";

let nextEnterExitEmpty = function() {
  let nextTick;
  if (_.sortedIndexOf(rlog.getGraph.enterExitEmpties, rlog.curTick) !== -1) {
    // not at a cycle point
    if (hasLength(rlog.getGraph.filterDatas)) {
      // if filtered, will go to previous step, then next step location
      nextTick = rlog.getGraph.nextStep(rlog.getGraph.prevStep(rlog.curTick));
    } else {
      // if not filtered
      nextTick = rlog.curTick;
    }
  } else {
    // at cycle point
    // first move one step forward... then find next enter/exit empty
    nextTick = rlog.getGraph.nextStep(rlog.curTick);
  }
  let val, i;
  // move to queue empty
  for (i = 0; i < rlog.getGraph.enterExitEmpties.length; i++) {
    val = rlog.getGraph.enterExitEmpties[i];
    if (nextTick < val) {
      updateGraph(val);
      return true;
    }
  }
  return false;
};

let prevEnterExitEmpty = function() {
  let prevTick;
  if (_.sortedIndexOf(rlog.getGraph.enterExitEmpties, rlog.curTick) !== -1) {
    // not at a cycle point
    if (hasLength(rlog.getGraph.filterDatas)) {
      // if filtered, will go to next step, then prev step location
      prevTick = rlog.getGraph.prevStep(rlog.getGraph.nextStep(rlog.curTick));
    } else {
      // if not filtered
      prevTick = rlog.curTick;
    }
  } else {
    // at cycle point
    // first move one step forward... then find next enter/exit empty
    prevTick = rlog.getGraph.prevStep(rlog.curTick);
    console.log("at cycle point", rlog.curTick, prevTick);
  }
  let val, i;
  // move to queue empty
  for (i = rlog.getGraph.enterExitEmpties.length - 1; i >= 0; i--) {
    val = rlog.getGraph.enterExitEmpties[i];
    if (prevTick > val) {
      updateGraph(val);
      return true;
    }
  }
  return false;
};

let lastEnterExitEmpty = function() {
  let nextTick =
    rlog.getGraph.enterExitEmpties[rlog.getGraph.enterExitEmpties.length - 1] ||
    0;
  updateGraph(nextTick);
};
let firstEnterExitEmpty = function() {
  let nextTick = rlog.getGraph.enterExitEmpties[0] || 0;
  updateGraph(nextTick);
};

export {
  nextEnterExitEmpty,
  prevEnterExitEmpty,
  firstEnterExitEmpty,
  lastEnterExitEmpty,
};
