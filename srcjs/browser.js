var browser = (function() {

  var isQt = false;
  // For easy handling of Qt quirks using CSS
  if (/\bQt\//.test(window.navigator.userAgent)) {
    $(document.documentElement).addClass('qt');
    isQt = true;
  }

  // Enable special treatment for Qt 5 quirks on Linux
  if (/\bQt\/5/.test(window.navigator.userAgent) &&
      /Linux/.test(window.navigator.userAgent)) {
    $(document.documentElement).addClass('qt5');
  }

  // Detect IE information
  // https://stackoverflow.com/a/22551342/1583084
  var ua = window.navigator.userAgent;
  var isIE = /MSIE|Trident/.test(ua);

  function getIEVersion() {
    var msie = ua.indexOf('MSIE ');
    if (isIE && msie > 0) {
      // IE 10 or older => return version number
      return parseInt(ua.substring(msie + 5, ua.indexOf('.', msie)), 10);
    }
    var trident = ua.indexOf('Trident/');
    if (trident > 0) {
      // IE 11 => return version number
      var rv = ua.indexOf('rv:');
      return parseInt(ua.substring(rv + 3, ua.indexOf('.', rv)), 10);
    }
    return -1;
  }

  return {
    isQt: isQt,
    isIE: isIE,
    IEVersion: getIEVersion()
  };

})();
