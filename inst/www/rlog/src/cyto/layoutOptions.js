// @flow

import cytoscape from "cytoscape";
import dagre from "cytoscape-dagre";

import type { CytoscapeElement, CytoscapeLibrary } from "./cytoFlowType";

(cytoscape: CytoscapeLibrary).use(dagre);

let layoutOptions = {
  name: "dagre",
  rankDir: "LR", // 'TB' for top to bottom flow, 'LR' for left to right,
  rankSep: 150, // the separation between node columns
  nodeSep: 10, // the separation within a node column
  edgeSep: 50, // the separation between adjacent edges in the same rank
  ranker: "longest-path", // Type of algorithm to assign a rank to each node in the input graph. Possible values: "network-simplex", "tight-tree" or "longest-path"
  nodeDimensionsIncludeLabels: true, // whether labels should be included in determining the space used by a node
  animate: true, // whether to transition the node positions
  animateFilter: function(node: CytoscapeElement, i: number) {
    return true;
  }, // whether to animate specific nodes when animation is on; non-animated nodes immediately go to their final positions
  animationDuration: 1000, // duration of animation in ms if enabled
  animationEasing: "ease-in-out-quad", // easing of animation if enabled
};

export default layoutOptions;
