/*jshint browser:true, jquery:true, strict:false, curly:false, indent:2*/

(function() {
  // Given a DOM node and a column (count of characters), walk recursively
  // through the node's siblings counting characters until the given number
  // of characters have been found. 
  // 
  // If the given count is bigger than the number of characters contained by
  // the node and its siblings, returns a null node and the number of 
  // characters found.
  function findTextColPoint(node, col) {
    var cols = 0;
    if (node.nodeType === 3) {
      if (node.nodeValue.length >= col) {
        return { element: node, offset: col };
      } else {
        cols += node.nodeValue.length;
      }
    } else if (node.nodeType === 1 && node.firstChild) {
      var ret = findTextColPoint(node.firstChild, col);
      if (ret.element !== null) {
        return ret;
      } else {
        cols += ret.offset;
      }
    }
    if (node.nextSibling)
      return findTextColPoint(node.nextSibling, col - cols);
    else
      return { element: null, offset: cols }
  }

  // Returns an object indicating the element containing the given line and
  // column of text, and the offset into that element where the text was found. 
  //
  // If the given line and column are not found, returns a null element and
  // the number of lines found.
  function findTextPoint(el, line, col) {
    var newlines = 0;
    for (childId in el.childNodes) {
      var child = el.childNodes[childId];
      // If this is a text node, count the number of newlines it contains.
      if (child.nodeType === 3) {  // TEXT_NODE
        var newlinere = /\n/g;
        var match;
        while ((match = newlinere.exec(child.nodeValue)) !== null) {
          newlines++;
          // Found the desired line, now find the column.
          if (newlines === line) {
            return findTextColPoint(child, match.index + col + 1);
          }
        }
      }
      // If this is not a text node, descend recursively to see how many 
      // lines it contains.
      else if (child.nodeType === 1) { // ELEMENT_NODE
        var ret = findTextPoint(child, line - newlines, col);
        if (ret.element !== null)
          return ret; 
        else 
          newlines += ret.offset;
      }
    }
    return { element: null, offset: newlines };
  }

  // Draw a highlight effect for the given source ref. srcref is assumed to be
  // an integer array of length 6, following the standard R format for source
  // refs.
  function highlightSrcref (srcref) {
    // Check to see if we already have a marker for this source ref
    var el = document.getElementById("srcref_" + srcref);
    if (!el) {
      // We don't have a marker, create one 
      el = document.createElement("span");
      el.id = "srcref_" + srcref;
      var ref = srcref;
      var code = document.getElementById("server-r-code"); 
      var start = findTextPoint(code, ref[0], ref[4]); 
      var end = findTextPoint(code, ref[2], ref[5]); 
      var range = document.createRange();
      // If the text points are inside different <SPAN>s, we may not be able to
      // surround them without breaking apart the elements to keep the DOM tree
      // intact. Just move the selection points to encompass the contents of
      // the SPANs. 
      if (start.element.parentNode.nodeName === "SPAN" &&
          start.element !== end.element) {
        range.setStartBefore(start.element.parentNode);
      } else {
        range.setStart(start.element, start.offset);
      }
      if (end.element.parentNode.nodeName === "SPAN" && 
          start.element !== end.element) {
        range.setEndAfter(end.element.parentNode);
      } else {
        range.setEnd(end.element, end.offset);
      }
      range.surroundContents(el);
    }
    // End any previous highlight before starting this one
    jQuery(el).stop(true, true);
    jQuery(el).effect("highlight", null, 1600);
  }

  // Manages the pop-out code window. 
  var codeWindow = window;
  var popOutCode = function() {
    if (codeWindow !== window) {
      // If the code window is already open, just bring it to the front
      codeWindow.focus();
    }
    else {
      // Kill all running animations so we don't clone the state of the DOM
      // mid-animation.
      $("*").stop(true, true);

      // Not already open, open it. 
      codeWindow = window.open("showcase-code-popup.html", 
                               "Shiny Application Code", 
                               "menubar=0,resizeable=1,status=0,titlebar=0," + 
                                 "width=" + (screen.width / 3) + "," +
                                 "height=" + (screen.height / 2) + "," +
                                 "toolbar=0,location=0"); 
    }
  }
  
  var closePopOutCode = function() {
    codeWindow = window;
  }

  // If this is the main Shiny window, wire up our custom message handler.
  if (window.Shiny) {
   Shiny.addCustomMessageHandler('reactlog', function(message) {
     if (message.srcref && codeWindow.highlightSrcref) {
       codeWindow.highlightSrcref(message.srcref)
     }
   });
  }

  window.highlightSrcref = highlightSrcref;
  window.popOutCode = popOutCode;
  window.closePopOutCode = closePopOutCode;
})();

