let isQtVal = false;
let isIEVal = false;
let IEVersionVal = -1;

function setIsQt(isQt: boolean): void {
  isQtVal = isQt;
}
function setIsIE(isIE: boolean): void {
  isIEVal = isIE;
}
function setIEVersion(IEVersion_: number): void {
  IEVersionVal = IEVersion_;
}

function isQt(): boolean {
  return isQtVal;
}
function isIE(): boolean {
  return isIEVal;
}
function IEVersion(): number {
  return IEVersionVal;
}

export { isQt, isIE, IEVersion, setIsQt, setIsIE, setIEVersion };
