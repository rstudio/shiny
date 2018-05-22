
import cytoscape from "cytoscape";

import graphStyles, { styleHelper } from "./cytoStyle";
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
      styleHelper("node", graphStyles.node.default),
      styleHelper("edge", graphStyles.edge.default),
      styleHelper(".edgeGhost", graphStyles.ghostEdge.default),
      styleHelper(".edgeIsolate", graphStyles.edge.isolate),
      styleHelper(".nodeStart", graphStyles.node.start),
      styleHelper(".nodeMiddle", graphStyles.node.middle),
      styleHelper(".nodeEnd", graphStyles.node.end),
      styleHelper(".nodeStartBig", graphStyles.node.startBig),
      styleHelper(".nodeMiddleBig", graphStyles.node.middleBig),
      styleHelper(".nodeEndBig", graphStyles.node.endBig),
      styleHelper(".nodeEnter", graphStyles.node.enter),
      styleHelper(".nodeEnterActive", graphStyles.node.enterActive),
      styleHelper(".nodeInvalidate", graphStyles.node.invalidate),
      styleHelper(".nodeInvalidateActive", graphStyles.node.invalidateActive),
      styleHelper(".nodeInvalidateDone", graphStyles.node.invalidateDone),
      styleHelper(".nodeIsolate", graphStyles.node.isolate),
      styleHelper(".nodeIsolateInvalidate", graphStyles.node.isolateInvalidate),
      styleHelper(".nodeValueChanged", graphStyles.node.valueChanged),
      styleHelper(".hoverNotFocused", graphStyles.focus.hoverNotFocused),
      styleHelper(
        ".hoverNotFocusedButSticky",
        graphStyles.focus.hoverNotFocusedButSticky
      ),
      styleHelper(
        ".edgeGhostHoverNotFocused",
        graphStyles.ghostEdge.hoverNotFocused
      ),
      styleHelper(
        ".edgeGhostHoverNotFocusedButSticky",
        graphStyles.ghostEdge.hoverNotFocusedButSticky
      ),
      styleHelper(".stickyNotFocused", graphStyles.focus.stickyNotFocused),
      styleHelper(".nodeSelected", graphStyles.selected.node),
      styleHelper(".edgeSelected", graphStyles.selected.edge),
      styleHelper(".edgeGhostSelected", graphStyles.selected.ghostEdge),
    ],
  });
};


export {with_container};
