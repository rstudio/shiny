import $ from "jquery";
import _ from "lodash";
import console from "../utils/console";

import rlog from "../rlog";
import * as updateGraph from "../updateGraph/updateGraph";

// cytoFamilySuccPred = function(ele, addExtraLayer = true) {
//   var familyEles = cyto.collection();
//   if (ele.isEdge()) {
//     var edge = ele;
//     if (false) {
//       // TODO-barret attempt at getting all nodes and edges from final graph
//       // abandon, as the edge should be stored and constantly retrieved somewhere else
//       console.log(`#${ele.source().id()} -> #${ele.target().id()}`)
//       ele = getGraph.finalCyto.$(`#${ele.source().id()} -> #${ele.target().id()}`)
//     }
//     familyEles = familyEles
//       .add(edge)
//       .add(ele.target())
//       .add(ele.target().successors())
//       .add(ele.source())
//       .add(ele.source().predecessors());
//   } else {
//     // is node
//     var node = ele;
//     if (false) {
//       ele = getGraph.finalCyto.$id(ele.id())
//     }
//     familyEles = familyEles
//       .add(node)
//       .add(ele.successors())
//       .add(ele.predecessors());
//   }
//   if (addExtraLayer) {
//     var familyNodes = familyEles.nodes();
//     familyEles = familyEles
//       .add(familyNodes.incomers())
//       .add(familyNodes.outgoers());
//   }
//   return familyEles;
// }

let onMouseOver = function(cyto) {
  return function(evt) {
    let target = evt.target;
    if (target === cyto) return;

    // highlight all outgoer's outgoers and all incomer's incomers and self

    let hasCalled = false;
    let debounced = _.debounce(function() {
      hasCalled = true;
      updateGraph.hoverData(target.data());
    }, 200);
    debounced();

    // if a mouseout occurs before the function is executed, cancel it
    // works as mouseout is always called before mouseover
    target.once("mouseout", function(evtOut) {
      debounced.cancel();
      if (hasCalled) {
        // only remove hover if hover added
        updateGraph.hoverDataReset();
      }
    });
  };
};

let onClick = function(cyto) {
  let cytoClickedBefore, cytoClickedTimeout;

  return function(evt) {
    // remove focus on search
    $("#search").blur();

    let target = evt.target;

    // check for double click
    // https://stackoverflow.com/a/44160927
    if (cytoClickedTimeout && cytoClickedBefore) {
      clearTimeout(cytoClickedTimeout);
    }

    if (cytoClickedBefore === target) {
      // is actually a double click... return!
      target.trigger("dblclick", evt);
      cytoClickedBefore = null;
      return;
    } else {
      cytoClickedTimeout = setTimeout(function() {
        cytoClickedBefore = null;
      }, 400);
      cytoClickedBefore = target;
      // continue like regular click
      console.log("click!!", evt);
    }

    if (target === rlog.cyto) {
      // remove sticky focus class
      updateGraph.stickyDatasReset();
      return;
    }

    updateGraph.stickyDatas([target.data()]);
    return;
  };
};

let onDblClick = function(cyto) {
  return function(evt, originalEvt) {
    console.log("dbl click!!");
    // console.log("dbl click!!", evt, originalEvt);
    let target = evt.target;

    if (target === rlog.cyto) {
      // go back to full graph
      updateGraph.resetHoverStickyFilterData();
      return;
    }

    // var holdingShiftKey = originalEvt.originalEvent.shiftKey;
    // if (holdingShiftKey) {
    //   console.log("extra layers!")
    //   var familyEles = cytoFamilySuccPred(target, true);
    //   var familyDatas = elesData(familyEles)
    //
    //   var directFamilyEles = cytoFamilySuccPred(target, false);
    //   getGraph.updateHoverInfo(
    //     elesData(directFamilyEles),
    //     elesData(cyto.$().not(directFamilyEles))
    //   )
    //
    //   updateGraph.withDatas(familyDatas)
    //
    // } else {
    // var familyEles = cytoFamilySuccPred(target, false);
    // var familyDatas = elesData(familyEles)

    updateGraph.filterDatas([target.data()]);
  };
};

let addOnMethods = function(cyto) {
  cyto.on("mouseover", onMouseOver(cyto));

  cyto.on("click", onClick(cyto));

  cyto.on("dblclick", onDblClick(cyto));
};

export { onMouseOver, onClick, onDblClick, addOnMethods };
export default addOnMethods;
