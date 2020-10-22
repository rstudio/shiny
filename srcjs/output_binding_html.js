var htmlOutputBinding = new OutputBinding();
$.extend(htmlOutputBinding, {
  find: function(scope) {
    return $(scope).find('.shiny-html-output');
  },
  onValueError: function(el, err) {
    exports.unbindAll(el);
    this.renderError(el, err);
  },
  renderValue: function(el, data) {
    exports.renderContent(el, data);
  }
});
outputBindings.register(htmlOutputBinding, 'shiny.htmlOutput');

var renderDependencies = exports.renderDependencies = function(dependencies) {
  if (dependencies) {
    $.each(dependencies, function(i, dep) {
      renderDependency(dep);
    });
  }
};

// Render HTML in a DOM element, add dependencies, and bind Shiny
// inputs/outputs. `content` can be null, a string, or an object with
// properties 'html' and 'deps'.
exports.renderContent = function(el, content, where="replace") {
  if (where === "replace") {
    exports.unbindAll(el);
  }

  var html;
  var dependencies = [];
  if (content === null) {
    html = '';
  } else if (typeof(content) === 'string') {
    html = content;
  } else if (typeof(content) === 'object') {
    html = content.html;
    dependencies = content.deps || [];
  }

  exports.renderHtml(html, el, dependencies, where);

  var scope = el;
  if (where === "replace") {
    exports.initializeInputs(el);
    exports.bindAll(el);
  } else {
    var $parent = $(el).parent();
    if ($parent.length > 0) {
      scope = $parent;
      if (where === "beforeBegin" || where === "afterEnd") {
        var $grandparent = $parent.parent();
        if ($grandparent.length > 0) scope = $grandparent;
      }
    }
    exports.initializeInputs(scope);
    exports.bindAll(scope);
  }
};

// Render HTML in a DOM element, inserting singletons into head as needed
exports.renderHtml = function(html, el, dependencies, where = 'replace') {
  renderDependencies(dependencies);
  return singletons.renderHtml(html, el, where);
};

var htmlDependencies = {};
function registerDependency(name, version) {
  htmlDependencies[name] = version;
}

// Re-render stylesheet(s) if the dependency has specificially requested it
// and it matches an existing dependency (name and version)
function needsRestyle(dep) {
  if (!dep.restyle) {
    return false;
  }
  var names = Object.keys(htmlDependencies);
  var idx = names.indexOf(dep.name);
  if (idx === -1) {
    return false;
  }
  return htmlDependencies[names[idx]] === dep.version;
}

// Client-side dependency resolution and rendering
function renderDependency(dep) {
  var restyle = needsRestyle(dep);
  if (htmlDependencies.hasOwnProperty(dep.name) && !restyle)
    return false;

  registerDependency(dep.name, dep.version);

  var href = dep.src.href;

  var $head = $("head").first();

  if (dep.meta && !restyle) {
    var metas = $.map(asArray(dep.meta), function(obj, idx) {
      // only one named pair is expected in obj as it's already been decomposed
      var name = Object.keys(obj)[0];
      return $("<meta>").attr("name", name).attr("content", obj[name]);
    });
    $head.append(metas);
  }

  if (dep.stylesheet) {
    var links = $.map(asArray(dep.stylesheet), function(stylesheet) {
      return $("<link rel='stylesheet' type='text/css'>").attr("href", href + "/" + encodeURI(stylesheet));
    });

    if (!restyle) {
      $head.append(links);
    } else {
      $.map(links, function(link) {
        // Flag 'stale' stylesheets for disable+removal after new styles are inserted
        for (var i = 0; i < document.styleSheets.length; i++) {
          var sheet = document.styleSheets[i];
          if (typeof sheet.href !== "string") continue;
          // The sheet's href is a full URL
          if (sheet.href.indexOf(link.attr("href")) === -1) continue;
          sheet._shiny_restyle = true;
        }
        // Request the new stylesheet (with a timestamp to prevent caching)
        // and insert a new inline style. Note that this could also done by inserting
        // a the new <link> (w/ timestamp in href), then disabling the 'stale'
        // document.styleSheets, but that approach doesn't work with IE11 (and this one does)
        var xhr = new XMLHttpRequest();
        var url = link.attr("href") + "?restyle=" + new Date().getTime();
        xhr.open('GET', url);
        xhr.onreadystatechange = function() {
          if (this.readyState === 4 && this.status === 200) {
            xhr.onreadystatechange = null;
            var id = "shiny_restyle_" + link.attr("href").replace(/\W/g,'_');
            var style_old = $head.find("style#" + id);
            var style_new = $("<style>").attr("id", id).html(this.responseText);
            $head.append(style_new);
            style_old.remove();
          }
        };
        xhr.send();
      });
      // Disable & remove 'stale' <link> tags
      setTimeout(function() {
        for (var i = 0; i < document.styleSheets.length; i++) {
          var sheet = document.styleSheets[i];
          if (!sheet._shiny_restyle) continue;
          sheet.disabled = true;
          $(sheet.ownerNode).remove();
          // disabled=true doesn't work on IE11, but this does :shrug:
          if (navigator.appName === 'Microsoft Internet Explorer') {
            sheet.cssText = "";
          }
        }
      }, 100);

    }
  }

  if (dep.script && !restyle) {
    var scripts = $.map(asArray(dep.script), function(scriptName) {
      return $("<script>").attr("src", href + "/" + encodeURI(scriptName));
    });
    $head.append(scripts);
  }

  if (dep.attachment && !restyle) {
    // dep.attachment might be a single string, an array, or an object.
    var attachments = dep.attachment;
    if (typeof(attachments) === "string")
      attachments = [attachments];
    if ($.isArray(attachments)) {
      // The contract for attachments is that arrays of attachments are
      // addressed using 1-based indexes. Convert this array to an object.
      var tmp = {};
      $.each(attachments, function(index, attachment) {
        tmp[(index + 1) + ""] = attachment;
      });
      attachments = tmp;
    }

    var attach = $.map(attachments, function(attachment, key) {
      return $("<link rel='attachment'>")
        .attr("id", dep.name + "-" + key + "-attachment")
        .attr("href", href + "/" + encodeURI(attachment));
    });
    $head.append(attach);
  }

  if (dep.head && !restyle) {
    var $newHead = $("<head></head>");
    $newHead.html(dep.head);
    $head.append($newHead.children());
  }
  return true;
}

var singletons = {
  knownSingletons: {},
  renderHtml: function(html, el, where) {
    var processed = this._processHtml(html);
    this._addToHead(processed.head);
    this.register(processed.singletons);
    if (where === "replace") {
      $(el).html(processed.html);
    } else {
      el.insertAdjacentHTML(where, processed.html);
    }
    return processed;
  },
  // Take an object where keys are names of singletons, and merges it into
  // knownSingletons
  register: function(s) {
    $.extend(this.knownSingletons, s);
  },
  // Takes a string or array of strings and adds them to knownSingletons
  registerNames: function(s) {
    if (typeof s === 'string') {
      this.knownSingletons[s] = true;
    } else if (s instanceof Array) {
      for (var i = 0; i < s.length; i++) {
        this.knownSingletons[s[i]] = true;
      }
    }
  },
  // Inserts new content into document head
  _addToHead: function(head) {
    if (head.length > 0) {
      var tempDiv = $("<div>" + head + "</div>")[0];
      var $head = $('head');
      while (tempDiv.hasChildNodes()) {
        $head.append(tempDiv.firstChild);
      }
    }
  },
  // Reads HTML and returns an object with info about singletons
  _processHtml: function(val) {
    var self = this;
    var newSingletons = {};
    var newVal;

    var findNewPayload = function(match, p1, sig, payload) {
      if (self.knownSingletons[sig] || newSingletons[sig])
        return "";
      newSingletons[sig] = true;
      return payload;
    };
    while (true) {
      newVal = val.replace(self._reSingleton, findNewPayload);
      if (val.length === newVal.length)
        break;
      val = newVal;
    }

    var heads = [];
    var headAddPayload = function(match, payload) {
      heads.push(payload);
      return "";
    };
    while (true) {
      newVal = val.replace(self._reHead, headAddPayload);
      if (val.length === newVal.length)
        break;
      val = newVal;
    }

    return {
      html: val,
      head: heads.join("\n"),
      singletons: newSingletons
    };
  },
  _reSingleton: /<!--(SHINY.SINGLETON\[([\w]+)\])-->([\s\S]*?)<!--\/\1-->/,
  _reHead: /<head(?:\s[^>]*)?>([\s\S]*?)<\/head>/
};
