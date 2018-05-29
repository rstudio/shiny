// @flow

import _ from "lodash";

import console from "../utils/console";

class StatusArr {
  statusArr: Array<StatusEntry>;

  constructor(statusArr_: StatusArr | Array<StatusEntry> = []) {
    if (statusArr_ instanceof StatusArr) {
      this.statusArr = _.cloneDeep(statusArr_.statusArr);
    } else if (Array.isArray(statusArr_)) {
      this.statusArr = statusArr_;
    }
  }
  add(obj: StatusEntry) {
    return this.statusArr.push(obj);
  }
  remove(): StatusEntry {
    return this.statusArr.pop();
  }
  last(): StatusEntry {
    return _.last(this.statusArr);
  }
  containsStatus(status: string) {
    let arr = this.statusArr,
      n = arr.length;
    for (let i = 0; i < n; i++) {
      if (arr[i].action === status) {
        return true;
      }
    }
    return false;
  }

  static expect_prev_status(
    curStatus: CurrentStatusEntry,
    prevStatus: StatusEntry,
    expectedAction: string
  ) {
    function onError(msg: string) {
      console.error("curStatus: ", curStatus);
      console.error("prevStatus: ", prevStatus);
      throw msg;
    }
    if (prevStatus.action !== expectedAction) {
      onError(`prior node status does not have "${expectedAction}" status`);
    }
    if (prevStatus.ctxId !== curStatus.ctxId) {
      onError(
        `prior node "ctxId" status does not have the same "ctxId" status`
      );
    }
  }
}

type StatusEntry = {
  action: string,
  ctxId: string,
};
type CurrentStatusEntry = {
  action: string,
  ctxId: string,
};

export { StatusArr };
export type { CurrentStatusEntry, StatusEntry };
