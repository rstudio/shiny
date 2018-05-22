import _ from "underscore";

class StatusArr {
  constructor() {
    this.statusArr = [];
  }
  add(obj) {
    return this.statusArr.push(obj);
  }
  remove() {
    return this.statusArr.pop();
  }
  last() {
    return _.last(this.statusArr);
  }
  containsStatus(status) {
    var arr = this.statusArr,
      n = arr.length;
    for (var i = 0; i < n; i++) {
      if (arr[i].action === status) {
        return true;
      }
    }
    return false;
  }

  static expect_prev_status(curStatus, prevStatus, expectedAction) {
    var on_error = function(msg) {
      console.error("curStatus: ", curStatus);
      console.error("prevStatus: ", prevStatus);
      throw msg;
    };
    if (prevStatus.action !== expectedAction) {
      on_error(`prior node status does not have "${expectedAction}" status`);
    }
    if (prevStatus.ctxId !== curStatus.ctxId) {
      on_error(
        `prior node "ctxId" status does not have the same "ctxId" status`
      );
    }
  }
}

export default StatusArr;
