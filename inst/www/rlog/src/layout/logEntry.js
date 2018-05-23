import rlog from "../rlog";

let container = null;

let updateLogEntry = function() {
  container.text(JSON.stringify(rlog.log[rlog.curTick], null, "  "));
};

let setContainer = function(container_) {
  container = container_;
};

export { updateLogEntry as update, setContainer };
