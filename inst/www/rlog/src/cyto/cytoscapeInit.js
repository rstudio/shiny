// @flow

import cytoscape from "cytoscape";

import * as cytoOn from "./cytoOn";
import graphStyles, { style } from "./cytoStyle";
import layoutOptions from "./layoutOptions";

// // TODO-barret use cytoClasses
import cytoClasses from "./cytoClasses";

let withContainer = function(container: JQuery) {
  let cyto = cytoscape({
    container: container,
    boxSelectionEnabled: false,
    autounselectify: true,
    layout: layoutOptions,
    style: [
      // order of the style definitions are how styles are applied
      style(cytoClasses.node, graphStyles.node.default),
      style(cytoClasses.edge, graphStyles.edge.default),
      style(cytoClasses.edgeGhost, graphStyles.ghostEdge.default),
      style(cytoClasses.edgeIsolate, graphStyles.edge.isolate),
      style(cytoClasses.nodeStart, graphStyles.node.start),
      style(cytoClasses.nodeMiddle, graphStyles.node.middle),
      style(cytoClasses.nodeEnd, graphStyles.node.end),
      style(cytoClasses.nodeStartBig, graphStyles.node.startBig),
      style(cytoClasses.nodeMiddleBig, graphStyles.node.middleBig),
      style(cytoClasses.nodeEndBig, graphStyles.node.endBig),
      style(cytoClasses.nodeEnter, graphStyles.node.enter),
      style(cytoClasses.nodeEnterActive, graphStyles.node.enterActive),
      style(cytoClasses.nodeInvalidate, graphStyles.node.invalidate),
      style(
        cytoClasses.nodeInvalidateActive,
        graphStyles.node.invalidateActive
      ),
      style(cytoClasses.nodeInvalidateDone, graphStyles.node.invalidateDone),
      style(cytoClasses.nodeIsolate, graphStyles.node.isolate),
      style(
        cytoClasses.nodeIsolateInvalidate,
        graphStyles.node.isolateInvalidate
      ),
      style(cytoClasses.nodeValueChanged, graphStyles.node.valueChanged),
      style(cytoClasses.hoverNotFocused, graphStyles.focus.hoverNotFocused),
      style(
        cytoClasses.hoverNotFocusedButSticky,
        graphStyles.focus.hoverNotFocusedButSticky
      ),
      style(
        cytoClasses.edgeGhostHoverNotFocused,
        graphStyles.ghostEdge.hoverNotFocused
      ),
      style(
        cytoClasses.edgeGhostHoverNotFocusedButSticky,
        graphStyles.ghostEdge.hoverNotFocusedButSticky
      ),
      style(cytoClasses.stickyNotFocused, graphStyles.focus.stickyNotFocused),
      style(cytoClasses.nodeSelected, graphStyles.selected.node),
      style(cytoClasses.edgeSelected, graphStyles.selected.edge),
      style(cytoClasses.edgeGhostSelected, graphStyles.selected.ghostEdge),
      style(cytoClasses.nodeFrozen, graphStyles.node.frozen),
    ],
  });

  cytoOn.addOnMethods(cyto);

  return cyto;
};

export { withContainer };
export default withContainer;
