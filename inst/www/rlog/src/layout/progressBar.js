import rlog from "../rlog";

let container = null;
let updateProgressBar = function() {
  container.width(rlog.curTick / rlog.log.length * 100 + "%");
};

let setContainer = function(container_) {
  container = container_;
};

export { updateProgressBar as update, setContainer };
