import cytoscape from "cytoscape";

import graphStyles, { style } from "./cytoStyle";
import layoutOptions from "./layoutOptions";

// // TODO-barret use cytoClasses
// import cytoClasses from "./cytoClasses"

let with_container = function(container) {
  return cytoscape({
    container: container,
    boxSelectionEnabled: false,
    autounselectify: true,
    layout: layoutOptions,
    style: [
      // order of the style definitions are how styles are applied
      style("node", graphStyles.node.default),
      style("edge", graphStyles.edge.default),
      style(".edgeGhost", graphStyles.ghostEdge.default),
      style(".edgeIsolate", graphStyles.edge.isolate),
      style(".nodeStart", graphStyles.node.start),
      style(".nodeMiddle", graphStyles.node.middle),
      style(".nodeEnd", graphStyles.node.end),
      style(".nodeStartBig", graphStyles.node.startBig),
      style(".nodeMiddleBig", graphStyles.node.middleBig),
      style(".nodeEndBig", graphStyles.node.endBig),
      style(".nodeEnter", graphStyles.node.enter),
      style(".nodeEnterActive", graphStyles.node.enterActive),
      style(".nodeInvalidate", graphStyles.node.invalidate),
      style(".nodeInvalidateActive", graphStyles.node.invalidateActive),
      style(".nodeInvalidateDone", graphStyles.node.invalidateDone),
      style(".nodeIsolate", graphStyles.node.isolate),
      style(".nodeIsolateInvalidate", graphStyles.node.isolateInvalidate),
      style(".nodeValueChanged", graphStyles.node.valueChanged),
      style(".hoverNotFocused", graphStyles.focus.hoverNotFocused),
      style(
        ".hoverNotFocusedButSticky",
        graphStyles.focus.hoverNotFocusedButSticky
      ),
      style(".edgeGhostHoverNotFocused", graphStyles.ghostEdge.hoverNotFocused),
      style(
        ".edgeGhostHoverNotFocusedButSticky",
        graphStyles.ghostEdge.hoverNotFocusedButSticky
      ),
      style(".stickyNotFocused", graphStyles.focus.stickyNotFocused),
      style(".nodeSelected", graphStyles.selected.node),
      style(".edgeSelected", graphStyles.selected.edge),
      style(".edgeGhostSelected", graphStyles.selected.ghostEdge),
    ],
  });
};

export { with_container };
