import HoverStatus from "./HoverStatus";

class Edge {
  constructor(data) {
    if (typeof data.reactId === "undefined")
      throw "data.reactId not provided to new Edge()";
    if (typeof data.depOnReactId === "undefined")
      throw "data.depOnReactId not provided to new Edge()";
    if (typeof data.ctxId === "undefined")
      throw "data.ctxId not provided to new Edge()";
    if (typeof data.time === "undefined")
      throw "data.time not provided to new Edge()";
    this.reactId = data.reactId;
    this.depOnReactId = data.depOnReactId;
    this.ctxId = data.ctxId;
    this.session = data.session || "Global";
    this.time = data.time;
    this.status = "normal";
    this.isGhost = false;
    this.hoverStatus = data.hoverStatus || new HoverStatus();
  }
  get id() {
    return `${this.reactId}_${this.depOnReactId}_${this.ctxId}`.replace(
      /\$/g,
      "_"
    );
  }
  get source() {
    return this.depOnReactId.replace(/\$/g, "_");
  }
  get target() {
    return this.reactId.replace(/\$/g, "_");
  }
  get key() {
    return `${this.reactId} depends on ${this.depOnReactId} in ${this.ctxId}`;
  }
  get ghostKey() {
    return `${this.reactId} depends on ${this.depOnReactId}`;
  }
  get hoverKey() {
    return this.ghostKey;
  }
  get inIsolate() {
    return this.status === "isolate";
  }
  get cytoClasses() {
    var classes = [];
    if (this.inIsolate) classes.push("edgeIsolate");
    switch (this.hoverStatus.state) {
      case HoverStatus.focused:
        break;
      case HoverStatus.notFocused:
        if (this.hoverStatus.sticky) {
          classes.push("hoverNotFocusedButSticky");
        } else {
          classes.push("hoverNotFocused");
        }
        break;
    }
    if (this.hoverStatus.selected) classes.push("edgeSelected");
    return classes.join(" ");
  }
  get cytoData() {
    var retData = this;
    return {
      group: "edges",
      data: retData,
    };
  }
}

export default Edge;
