// @flow

import { rlog } from "../rlog";
import { updateGraph } from "../updateGraph";

import type { SomeGraphData } from "../graph/Graph";

let hoverData = function(data: SomeGraphData): void {
  rlog.getGraph.updateHoverData(data);
  updateGraph();
};
let hoverDataReset = function(): void {
  rlog.getGraph.updateHoverDataReset();
  updateGraph();
};
let stickyDatas = function(datas: Array<SomeGraphData>): void {
  rlog.getGraph.updateStickyDatas(datas);
  updateGraph();
};
let stickyDatasReset = function(): void {
  rlog.getGraph.updateStickyDatasReset();
  updateGraph();
};
let filterDatas = function(datas: Array<SomeGraphData>): void {
  rlog.getGraph.updateFilterDatas(datas);
  updateGraph();
};
let filterDatasReset = function(): void {
  rlog.getGraph.updateFilterDatasReset();
  updateGraph();
};
let searchRegex = function(searchRegex: RegExp): void {
  rlog.getGraph.updateSearchRegex(searchRegex);
  updateGraph();
};
let searchRegexReset = function(): void {
  rlog.getGraph.updateSearchRegexReset();
  updateGraph();
};
let resetHoverStickyFilterData = function(): void {
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
