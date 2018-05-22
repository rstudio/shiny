class HoverStatus {
  constructor(state) {
    this.sticky = HoverStatus.notSticky; // true / false
    this.state = state || HoverStatus.focused; // "focused", "notFocused"
    this.selected = false;
  }
  isSticky() {
    return this.sticky === HoverStatus.sticky;
  }
  toNotSticky() {
    this.sticky = HoverStatus.notSticky;
  }
  toSticky() {
    this.sticky = HoverStatus.sticky;
  }

  isFocused() {
    return this.state === HoverStatus.focused;
  }
  toFocused() {
    this.state = HoverStatus.focused;
  }
  toNotFocused() {
    this.state = HoverStatus.notFocused;
  }
}

HoverStatus.focused = "focused";
HoverStatus.notFocused = "notFocused";
HoverStatus.sticky = true;
HoverStatus.notSticky = false;
HoverStatus.isSelected = true;
HoverStatus.isNotSelected = false;

export default HoverStatus;
