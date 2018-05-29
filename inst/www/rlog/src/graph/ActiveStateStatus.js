// @flow

const stateOff = "off";
const stateOn = "on";
const stateFinished = "finished";

type stateEnum = "off" | "on" | "finished";

// pulse on being active at step k; isAtStep(k)
// display engaged; isOn
// display active engaged; isOn and isActive
// display finished; isFinished
// display none; isOff
class ActiveStateStatus {
  state: stateEnum;
  activeStep: number;

  constructor() {
    this.state = stateOff; // "on", "finished", "off"
    this.activeStep = -1;
  }
  setState(state: stateEnum) {
    this.state = state;
  }
  setActiveAtStep(step: number) {
    this.toOn();
    this.activeStep = step;
  }
  reset() {
    this.toOff();
    this.resetActive();
  }
  resetActive() {
    this.activeStep = -1;
  }
  get isOn() {
    return this.state === stateOn;
  }
  get isOff() {
    return this.state === stateOff;
  }
  get isFinished() {
    return this.state === stateFinished;
  }
  get isActive() {
    return this.isOn && this.activeStep > 0;
  }
  isActiveAtStep(k: number) {
    return this.isActive && this.activeStep === k;
  }

  toOn() {
    this.state = stateOn;
  }
  toFinished() {
    this.state = stateFinished;
  }
  toOff() {
    this.state = stateOff;
  }
}

export { ActiveStateStatus };
