let isQtVal = false;
let isIEVal = false;
let versionIE = -1;

function setIsQt(isQt: boolean): void {
  isQtVal = isQt;
}
function setIsIE(isIE: boolean): void {
  isIEVal = isIE;
}
function setIEVersion(versionIE_: number): void {
  versionIE = versionIE_;
}

function isQt(): boolean {
  return isQtVal;
}
function isIE(): boolean {
  return isIEVal;
}

// (Name existed before TS conversion)
// eslint-disable-next-line @typescript-eslint/naming-convention
function IEVersion(): number {
  return versionIE;
}

export { IEVersion, isIE, isQt, setIEVersion, setIsIE, setIsQt };
