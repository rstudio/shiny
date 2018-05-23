import rlog from "../rlog";
import updateGraph from "./atTick";

let hoverData = function(data) {
  rlog.getGraph.updateHoverData(data);
  updateGraph();
};
let hoverDataReset = function() {
  rlog.getGraph.updateHoverDataReset();
  updateGraph();
};
let stickyDatas = function(data) {
  rlog.getGraph.updateStickyDatas(data);
  updateGraph();
};
let stickyDatasReset = function() {
  rlog.getGraph.updateStickyDatasReset();
  updateGraph();
};
let filterDatas = function(data) {
  rlog.getGraph.updateFilterDatas(data);
  updateGraph();
};
let filterDatasReset = function() {
  rlog.getGraph.updateFilterDatasReset();
  updateGraph();
};
let searchRegex = function(searchRegex) {
  rlog.getGraph.updateSearchRegex(searchRegex);
  updateGraph();
};
let searchRegexReset = function(searchRegex) {
  rlog.getGraph.updateSearchRegexReset();
  updateGraph();
};
let resetHoverStickyFilterData = function() {
  rlog.getGraph.updateHoverDataReset();
  rlog.getGraph.updateStickyDatasReset();
  rlog.getGraph.updateFilterDatasReset();
  rlog.getGraph.updateSearchRegexReset();
  updateGraph();
};

export {
  hoverData,
  hoverDataReset,
  stickyDatas,
  stickyDatasReset,
  filterDatas,
  filterDatasReset,
  searchRegex,
  searchRegexReset,
  resetHoverStickyFilterData,
};
