// @flow

import { LogStates } from "../log/logStates";
import { HoverStatus } from "./HoverStatus";
import { ActiveStateStatus } from "./ActiveStateStatus";
import { StatusArr } from "./StatusArr";
import type { StatusEntry } from "./StatusArr";
import type { CytoData } from "../cyto/cytoFlowType";

// // TODO-barret use log states everywhere
// import logStates from "../log/logStates"

class Node {
  reactId: string;
  label: string;
  type: string;
  session: string;
  time: number;
  isFrozen: boolean;
  statusArr: StatusArr;
  value: ?string;
  hoverStatus: HoverStatus;
  valueChangedStatus: ActiveStateStatus;
  enterStatus: ActiveStateStatus;
  invalidateStatus: ActiveStateStatus;

  constructor(data: NodeInputType) {
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
    this.session = data.session || "Global";
    this.time = data.time;
    this.isFrozen = data.isFrozen || false;
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
  get id(): NodeIdType {
    return this.reactId.replace(/\$/g, "_");
  }
  get key(): NodeKeyType {
    return this.reactId;
  }
  get hoverKey(): NodeHoverKeyType {
    return this.key;
  }
  statusAdd(obj: StatusEntry): StatusArr {
    this.statusArr.add(obj);
    return this.statusArr;
  }
  statusRemove(): StatusEntry {
    return this.statusArr.remove();
  }
  statusLast(): StatusEntry {
    return this.statusArr.last();
  }
  get inEnter(): boolean {
    return this.statusArr.containsStatus(LogStates.enter);
  }
  get inIsolate(): boolean {
    return this.statusArr.containsStatus(LogStates.isolateEnter);
  }
  // get inInvalidate() {return this.statusArr.containsStatus("invalidateStart");}
  get inIsolateInvalidate(): boolean {
    return this.statusArr.containsStatus(LogStates.isolateInvalidateStart);
  }
  get cytoStyle() {
    return {};
  }
  get cytoLabel(): string {
    return this.label;
  }
  get cytoClasses(): string {
    let classes = [];
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
      case HoverStatus.valFocused:
        break;
      case HoverStatus.valNotFocused:
        // console.log("not focused!")
        if (this.hoverStatus.isSticky()) {
          classes.push("hoverNotFocusedButSticky");
        } else {
          classes.push("hoverNotFocused");
        }
        break;
    }
    if (this.hoverStatus.selected) classes.push("nodeSelected");

    if (this.isFrozen) classes.push("nodeFrozen");

    return classes.join(" ");
  }
  get cytoData(): CytoData {
    let retData = this;
    return {
      group: "nodes",
      data: retData,
    };
  }
}

type NodeKeyType = string;
type NodeHoverKeyType = string;
type NodeIdType = string;

type NodeInputType = {
  reactId: string,
  label: string,
  type: string,
  session: ?string,
  time: number,
  isFrozen?: boolean,
  statusArr?: StatusArr,
  value: ?string,
  hoverStatus?: HoverStatus,
  valueChangedStatus?: ActiveStateStatus,
  enterStatus?: ActiveStateStatus,
  invalidateStatus?: ActiveStateStatus,
};

export { Node };
export type { NodeKeyType, NodeHoverKeyType, NodeIdType, NodeInputType };
