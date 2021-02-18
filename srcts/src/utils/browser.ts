let isQtVal = false;
let isIEVal = false;
let IEVersionVal = -1;

function setIsQt(isQt: boolean) {
  isQtVal = isQt;
}
function setIsIE(isIE: boolean) {
  isIEVal = isIE;
}
function setIEVersion(IEVersion_: number) {
  IEVersionVal = IEVersion_;
}

function isQt() {
  return isQtVal;
}
function isIE() {
  return isIEVal;
}
function IEVersion() {
  return IEVersionVal;
}

export { isQt, isIE, IEVersion, setIsQt, setIsIE, setIEVersion };
