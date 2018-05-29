// @flow

import console from "../utils/console";
import * as updateGraph from "./hoverStickyFilterSearch";

// when str length < 3 do not search
// when str length = 0, reset filter
// when str length >= 3, set filter to all elements that match
let withSearchString = function(str: string): void {
  // if less than three chars...
  if (str.length < 3) {
    if (str.length === 0) {
      // TODO-barret show warning of resetting
      console.log("resetting log!");
      updateGraph.searchRegexReset();
    } else {
      // TODO-barret show warning of not enough characters
      console.log("do nothing");
    }
    return;
  }
  // escape the string
  // https://stackoverflow.com/a/17606289
  let escapeRegExp = function(str) {
    return str.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"); // $& means the whole matched string
  };
  let searchRegex = new RegExp(escapeRegExp(str));
  updateGraph.searchRegex(searchRegex);
  return;
};

export { withSearchString };
