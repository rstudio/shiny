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
  var isIE = (navigator.appName === 'Microsoft Internet Explorer');

  function getIEVersion() {
    var rv = -1;
    if (isIE) {
      var ua = navigator.userAgent;
      var re  = new RegExp("MSIE ([0-9]{1,}[\\.0-9]{0,})");
      if (re.exec(ua) !== null)
        rv = parseFloat(RegExp.$1);
    }
    return rv;
  }

  return {
    isQt: isQt,
    isIE: isIE,
    IEVersion: getIEVersion()
  };

})();
