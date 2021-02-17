import { userAgent } from "./globals";

let isQt = false;
// For easy handling of Qt quirks using CSS

if (/\bQt\//.test(userAgent)) {
  $(document.documentElement).addClass("qt");
  isQt = true;
}

// Enable special treatment for Qt 5 quirks on Linux
if (/\bQt\/5/.test(userAgent) && /Linux/.test(userAgent)) {
  $(document.documentElement).addClass("qt5");
}

// Detect IE and older (pre-Chromium) Edge
const isIE = /MSIE|Trident|Edge/.test(userAgent);

function getIEVersion() {
  const msie = userAgent.indexOf("MSIE ");

  if (isIE && msie > 0) {
    // IE 10 or older => return version number
    return parseInt(
      userAgent.substring(msie + 5, userAgent.indexOf(".", msie)),
      10
    );
  }
  const trident = userAgent.indexOf("Trident/");

  if (trident > 0) {
    // IE 11 => return version number
    const rv = userAgent.indexOf("rv:");

    return parseInt(
      userAgent.substring(rv + 3, userAgent.indexOf(".", rv)),
      10
    );
  }
  return -1;
}

const IEVersion = getIEVersion();

export { isQt, isIE, IEVersion };
