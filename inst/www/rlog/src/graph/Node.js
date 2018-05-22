import HoverStatus from "./HoverStatus";
import ActiveStateStatus from "./ActiveStateStatus";
import StatusArr from "./StatusArr";

// // TODO-barret use log states everywhere
// import logStates from "../log/logStates"

class Node {
  constructor(data) {
    if (typeof data.reactId === "undefined")
      throw "data.reactId not provided in new Node";
    if (typeof data.label === "undefined")
      throw "data.label not provided in new Node";
    if (typeof data.type === "undefined")
      throw "data.type not provided in new Node";
    if (typeof data.session === "undefined")
      throw "data.session not provided in new Node";
    if (typeof data.time === "undefined")
      throw "data.time not provided in new Node";
    this.reactId = data.reactId;
    this.label = data.label;
    this.type = data.type;
    this.session = data.session;
    this.time = data.time;
    this.statusArr = new StatusArr(data.statusArr || []);
    this.value = data.value || null;
    this.hoverStatus = data.hoverStatus || new HoverStatus();

    this.valueChangedStatus =
      data.valueChangedStatus || new ActiveStateStatus();

    // this.inInvalidate = data.inInvalidate || false;
    // this.activeInvalidate = data.activeInvalidate || false;

    this.enterStatus = data.enterStatus || new ActiveStateStatus();

    this.invalidateStatus = data.invalidateStatus || new ActiveStateStatus();
  }
  get id() {
    return this.reactId.replace(/\$/g, "_");
  }
  get key() {
    return this.reactId;
  }
  get hoverKey() {
    return this.key;
  }
  statusAdd(obj) {
    this.statusArr.add(obj);
    return this.statusArr;
  }
  statusRemove() {
    return this.statusArr.remove();
  }
  statusLast() {
    return this.statusArr.last();
  }
  get inEnter() {
    return this.statusArr.containsStatus("enter");
  }
  get inIsolate() {
    return this.statusArr.containsStatus("isolateEnter");
  }
  // get inInvalidate() {return this.statusArr.containsStatus("invalidateStart");}
  get inIsolateInvalidate() {
    return this.statusArr.containsStatus("isolateInvalidateStart");
  }
  get cytoStyle() {
    return {};
  }
  get cytoLabel() {
    return this.label;
  }
  get cytoClasses() {
    var classes = [];
    switch (this.type) {
      case "observer":
        classes.push("nodeEnd");
        break;
      case "observable":
        classes.push("nodeMiddle");
        break;
      default:
        classes.push("nodeStart");
    }

    if (this.inEnter) classes.push("nodeEnter");
    if (this.enterStatus.isActive) classes.push("nodeEnterActive");

    if (this.type === "observer" || this.type === "observable") {
      if (this.invalidateStatus.isActive) classes.push("nodeInvalidateActive");
      else if (this.invalidateStatus.isOn) classes.push("nodeInvalidate");
      else if (this.invalidateStatus.isFinished)
        classes.push("nodeInvalidateDone");
    }
    // if (this.inInvalidate) classes.push("nodeInvalidate");
    if (this.inIsolate) classes.push("nodeIsolate");
    // if (this.inIsolateInvalidate) classes.push("nodeIsolateInvalidate");
    if (this.valueChangedStatus.isOn) classes.push("nodeValueChanged");

    switch (this.hoverStatus.state) {
      case HoverStatus.focused:
        break;
      case HoverStatus.notFocused:
        // console.log("not focused!")
        if (this.hoverStatus.isSticky()) {
          classes.push("hoverNotFocusedButSticky");
        } else {
          classes.push("hoverNotFocused");
        }
        break;
    }
    if (this.hoverStatus.selected) classes.push("nodeSelected");

    return classes.join(" ");
  }
  get cytoData() {
    var retData = this;
    return {
      group: "nodes",
      data: retData,
    };
  }
}

export default Node;
