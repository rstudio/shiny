// @flow

class HoverStatus {
  static keyState = "state";
  static keySticky = "sticky";
  static valFocused = "focused";
  static valNotFocused = "notFocused";
  static valSticky = true;
  static valNotSticky = false;
  static valSelected = true;
  static valNotSelected = false;

  sticky: boolean; // eslint-disable-line no-undef
  state: "focused" | "notFocused"; // eslint-disable-line no-undef
  selected: boolean; // eslint-disable-line no-undef

  constructor(state: "focused" | "notFocused" = HoverStatus.valFocused) {
    this.sticky = HoverStatus.valNotSticky; // true / false
    this.state = state; // "focused", "notFocused"
    this.selected = false;
  }
  isSticky() {
    return this.sticky === HoverStatus.valSticky;
  }
  toNotSticky() {
    this.sticky = HoverStatus.valNotSticky;
  }
  toSticky() {
    this.sticky = HoverStatus.valSticky;
  }

  isFocused() {
    return this.state === HoverStatus.valFocused;
  }
  toFocused() {
    this.state = HoverStatus.valFocused;
  }
  toNotFocused() {
    this.state = HoverStatus.valNotFocused;
  }
}

export { HoverStatus };
