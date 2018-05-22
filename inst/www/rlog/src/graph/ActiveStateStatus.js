const stateOff = "off";
const stateOn = "on";
const stateFinished = "finished";

// pulse on being active at step k; isAtStep(k)
// display engaged; isOn
// display active engaged; isOn and isActive
// display finished; isFinished
// display none; isOff
class ActiveStateStatus {
  constructor() {
    this.state = stateOff; // "on", "finished", "off"
    this.activeStep = -1;
  }
  setState(state) {
    this.state = state;
  }
  setActiveAtStep(step) {
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
  isActiveAtStep(k) {
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

export default ActiveStateStatus;
