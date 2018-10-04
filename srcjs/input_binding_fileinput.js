var IE8FileUploader = function(shinyapp, id, fileEl) {
  this.shinyapp = shinyapp;
  this.id = id;
  this.fileEl = fileEl;
  this.beginUpload();
};
(function() {
  this.beginUpload = function() {
    var self = this;
    // Create invisible frame
    var iframeId = 'shinyupload_iframe_' + this.id;
    this.iframe = document.createElement('iframe');
    this.iframe.id = iframeId;
    this.iframe.name = iframeId;
    this.iframe.setAttribute('style', 'position: fixed; top: 0; left: 0; width: 0; height: 0; border: none');
    $('body').append(this.iframe);
    var iframeDestroy = function() {
      // Forces Shiny to flushReact, flush outputs, etc. Without this we get
      // invalidated reactives, but observers don't actually execute.
      self.shinyapp.makeRequest('uploadieFinish', [], function(){}, function(){});
      $(self.iframe).remove();
      // Reset the file input's value to "". This allows the same file to be
      // uploaded again. https://stackoverflow.com/a/22521275
      $(self.fileEl).val("");
    };
    if (this.iframe.attachEvent) {
      this.iframe.attachEvent('onload', iframeDestroy);
    } else {
      this.iframe.onload = iframeDestroy;
    }

    this.form = document.createElement('form');
    this.form.method = 'POST';
    this.form.setAttribute('enctype', 'multipart/form-data');
    this.form.action = "session/" + encodeURI(this.shinyapp.config.sessionId) +
                       "/uploadie/" + encodeURI(this.id);
    this.form.id = 'shinyupload_form_' + this.id;
    this.form.target = iframeId;
    $(this.form).insertAfter(this.fileEl).append(this.fileEl);
    this.form.submit();
  };
}).call(IE8FileUploader.prototype);

var FileUploader = function(shinyapp, id, files, el) {
  this.shinyapp = shinyapp;
  this.id = id;
  this.el = el;
  FileProcessor.call(this, files);
};
$.extend(FileUploader.prototype, FileProcessor.prototype);
(function() {
  this.makeRequest = function(method, args, onSuccess, onFailure, blobs) {
    this.shinyapp.makeRequest(method, args, onSuccess, onFailure, blobs);
  };
  this.onBegin = function(files, cont) {
    var self = this;

    // Reset progress bar
    this.$setError(null);
    this.$setActive(true);
    this.$setVisible(true);
    this.onProgress(null, 0);

    this.totalBytes = 0;
    this.progressBytes = 0;
    $.each(files, function(i, file) {
      self.totalBytes += file.size;
    });

    var fileInfo = $.map(files, function(file, i) {
      return {
        name: file.name,
        size: file.size,
        type: file.type
      };
    });

    this.makeRequest(
      'uploadInit', [fileInfo],
      function(response) {
        self.jobId = response.jobId;
        self.uploadUrl = response.uploadUrl;
        cont();
      },
      function(error) {
        self.onError(error);
      });
  };
  this.onFile = function(file, cont) {
    var self = this;
    this.onProgress(file, 0);

    $.ajax(this.uploadUrl, {
      type: 'POST',
      cache: false,
      xhr: function() {
        var xhrVal = $.ajaxSettings.xhr();
        if (xhrVal.upload) {
          xhrVal.upload.onprogress = function(e) {
            if (e.lengthComputable) {
              self.onProgress(
                file,
                (self.progressBytes + e.loaded) / self.totalBytes);
            }
          };
        }
        return xhrVal;
      },
      data: file,
      contentType: 'application/octet-stream',
      processData: false,
      success: function() {
        self.progressBytes += file.size;
        cont();
      },
      error: function(jqXHR, textStatus, errorThrown) {
        self.onError(jqXHR.responseText || textStatus);
      }
    });
  };
  this.onComplete = function() {
    var self = this;

    var fileInfo = $.map(this.files, function(file, i) {
      return {
        name: file.name,
        size: file.size,
        type: file.type
      };
    });

    // Trigger shiny:inputchanged. Unlike a normal shiny:inputchanged event,
    // it's not possible to modify the information before the values get
    // sent to the server.
    var evt = jQuery.Event("shiny:inputchanged");
    evt.name = this.id;
    evt.value = fileInfo;
    evt.binding = fileInputBinding;
    evt.el = this.el;
    evt.inputType = 'shiny.fileupload';
    $(document).trigger(evt);

    this.makeRequest(
      'uploadEnd', [this.jobId, this.id],
      function(response) {
        self.$setActive(false);
        self.onProgress(null, 1);
        self.$bar().text('Upload complete');
        // Reset the file input's value to "". This allows the same file to be
        // uploaded again. https://stackoverflow.com/a/22521275
        $(evt.el).val("");
      },
      function(error) {
        self.onError(error);
      });
    this.$bar().text('Finishing upload');
  };
  this.onError = function(message) {
    this.$setError(message || '');
    this.$setActive(false);
  };
  this.onAbort = function() {
    this.$setVisible(false);
  };
  this.onProgress = function(file, completed) {
    this.$bar().width(Math.round(completed*100) + '%');
    this.$bar().text(file ? file.name : '');
  };
  this.$container = function() {
    return $('#' + $escape(this.id) + '_progress.shiny-file-input-progress');
  };
  this.$bar = function() {
    return $('#' + $escape(this.id) + '_progress.shiny-file-input-progress .progress-bar');
  };
  this.$setVisible = function(visible) {
    this.$container().css('visibility', visible ? 'visible' : 'hidden');
  };
  this.$setError = function(error) {
    this.$bar().toggleClass('progress-bar-danger', (error !== null));
    if (error !== null) {
      this.onProgress(null, 1);
      this.$bar().text(error);
    }
  };
  this.$setActive = function(active) {
    this.$container().toggleClass('active', !!active);
  };
}).call(FileUploader.prototype);


// NOTE On Safari, at least version 10.1.2, *if the developer console is open*,
// setting the input's value will behave strangely because of a Safari bug. The
// uploaded file's name will appear over the placeholder value, instead of
// replacing it. The workaround is to restart Safari. When I (Alan Dipert) ran
// into this bug Winston Chang helped me diagnose the exact problem, and Winston
// then submitted a bug report to Apple.
function setFileText($el, files) {
  var $fileText = $el.closest('div.input-group').find('input[type=text]');
  if (files.length === 1) {
    $fileText.val(files[0].name);
  } else {
    $fileText.val(files.length + " files");
  }
}

// If previously selected files are uploading, abort that.
function abortCurrentUpload($el) {
  var uploader = $el.data('currentUploader');
  if (uploader) uploader.abort();
  // Clear data-restore attribute if present.
  $el.removeAttr('data-restore');
}

function uploadDroppedFilesIE10Plus(el, files) {
  var $el = $(el);
  abortCurrentUpload($el);

  // Set the label in the text box
  setFileText($el, files);

  // Start the new upload and put the uploader in 'currentUploader'.
  $el.data('currentUploader',
           new FileUploader(exports.shinyapp,
                            fileInputBinding.getId(el),
                            files,
                            el));
}

function uploadFiles(evt) {
  var $el = $(evt.target);
  abortCurrentUpload($el);

  var files = evt.target.files;
  // IE8 here does not necessarily mean literally IE8; it indicates if the web
  // browser supports the FileList object (IE8/9 do not support it)
  var IE8 = typeof(files) === 'undefined';
  var id = fileInputBinding.getId(evt.target);

  if (!IE8 && files.length === 0)
    return;

  // Set the label in the text box
  var $fileText = $el.closest('div.input-group').find('input[type=text]');
  if (IE8) {
    // If we're using IE8/9, just use this placeholder
    $fileText.val("[Uploaded file]");
  } else {
    setFileText($el, files);
  }

  // Start the new upload and put the uploader in 'currentUploader'.
  if (IE8) {
    /*jshint nonew:false */
    new IE8FileUploader(exports.shinyapp, id, evt.target);
  } else {
    $el.data('currentUploader',
      new FileUploader(exports.shinyapp, id, files, evt.target));
  }
}

// Here we maintain a list of all the current file inputs. This is necessary
// because we need to trigger events on them in order to respond to file drag
// events. For example, they should all light up when a file is dragged on to
// the page.
var $fileInputs = $();

var fileInputBinding = new InputBinding();
$.extend(fileInputBinding, {
  find: function(scope) {
    return $(scope).find('input[type="file"]');
  },
  getId: function(el) {
    return InputBinding.prototype.getId.call(this, el) || el.name;
  },
  getValue: function(el) {
    // This returns a non-undefined value only when there's a 'data-restore'
    // attribute, which is set only when restoring Shiny state. If a file is
    // uploaded through the browser, 'data-restore' gets cleared.
    var data = $(el).attr('data-restore');
    if (data) {
      data = JSON.parse(data);

      // Set the label in the text box
      var $fileText = $(el).closest('div.input-group').find('input[type=text]');
      if (data.name.length === 1) {
        $fileText.val(data.name[0]);
      } else {
        $fileText.val(data.name.length + " files");
      }

      // Manually set up progress bar. A bit inelegant because it duplicates
      // code from FileUploader, but duplication is less bad than alternatives.
      var $progress = $(el).closest('div.form-group').find('.progress');
      var $bar = $progress.find('.progress-bar');
      $progress.removeClass('active');
      $bar.width('100%');
      $bar.css('visibility', 'visible');

      return data;

    } else {
      return null;
    }
  },
  setValue: function(el, value) {
    // Not implemented
  },
  getType: function(el) {
    // This will be used only when restoring a file from a saved state.
    return 'shiny.file';
  },
  _getZone: function(el) {
    return $(el).closest("div.input-group");
  },
  // This implements draghoverstart/draghoverend events that occur once per
  // selector, instead of once for every child the way native
  // dragenter/dragleave do. Inspired by https://gist.github.com/meleyal/3794126
  _enableDraghover: function($el, startEvent = "draghoverstart", endEvent = "draghoverend") {
    // Create an empty jQuery collection. This is a set-like data structure that
    // jQuery normally uses to contain the results of a selection.
    let collection = $();

    // Attach a dragenter handler to $el and all of its children. When the first
    // child is entered, trigger a draghoverstart event.
    $el.on("dragenter.dragHover", e => {
      if (collection.length === 0) {
        $el.trigger(startEvent, e.originalEvent);
      }
      // Every child that has fired dragenter is added to the collection.
      // Addition is idempotent, which accounts for elements producing dragenter
      // multiple times.
      collection = collection.add(e.originalEvent.target);
    });

    // If a drop happens, clear the collection and trigger a draghoverend.
    $el.on("drop.dragHover", e => {
      collection = $();
      $el.trigger(endEvent, e.originalEvent);
    });

    // Attach dragleave to $el and its children. Whenever a
    // child fires either of these events, remove it from the collection.
    $el.on("dragleave.dragHover", e => {
      collection = collection.not(e.originalEvent.target);
      // When the collection has no elements, all of the children have been
      // removed, and produce draghoverend event.
      if (collection.length === 0) {
        $el.trigger(endEvent, e.originalEvent);
      }
    });
  },
  _disableDraghover: function($el) {
    $el.off(".dragHover");
  },
  _enableDocumentEvents: function() {
    let $doc = $("html");

    this._enableDraghover($doc);
    $doc.on({
      "draghoverstart.fileDrag": e => {
        $fileInputs.trigger("showZone");
      },
      "draghoverend.fileDrag": e => {
        $fileInputs.trigger("hideZone");
      },
      "dragover.fileDrag drop.fileDrag": e => {
        e.preventDefault();
      }
    });
  },
  _disableDocumentEvents: function() {
    let $doc = $("html");

    $doc.off(".fileDrag");
    this._disableDraghover($doc);
  },
  _zoneEvents: [
    "showZone",
    "hideZone",
    "draghoverstart:zone",
    "draghoverend:zone",
    "drop"
  ].join(" "),
  _canSetFiles: function(fileList) {
    var testEl = document.createElement("input");
    testEl.type = "file";
    try {
      testEl.files = fileList;
    } catch (e) {
      return false;
    }
    return true;
  },
  _handleDrop: function(e, el) {
    const files = e.originalEvent.dataTransfer.files,
          $el   = $(el);
    if (files === undefined || files === null) {
      // 1. The FileList object isn't supported by this browser, and
      // there's nothing else we can try. (< IE 10)
      console.log("Dropping files is not supported on this browser. (no FileList)");
    } else if (!this._canSetFiles(files)) {
      // 2. The browser doesn't support assigning a type=file input's .files
      // property, but we do have a FileList to work with. (IE10+/Edge)
      $el.val("");
      uploadDroppedFilesIE10Plus(el, files);
    } else {
      // 3. The browser supports FileList and input.files assignment.
      // (Chrome, Safari)
      $el.val("");
      el.files = e.originalEvent.dataTransfer.files;
      // Recent versions of Firefox (57+, or "Quantum" and beyond) don't seem to
      // automatically trigger a change event, so we trigger one manually here.
      // On browsers that do trigger change, this operation appears to be
      // idempotent, as el.files doesn't change between events.
      $el.trigger("change");
    }
  },
  _activeClass: "shiny-file-input-active",
  _overClass: "shiny-file-input-over",
  _isIE9: function() {
    try {
      return (window.navigator.userAgent.match(/MSIE 9\./) && true) || false;
    } catch (e) {
      return false;
    }
  },
  subscribe: function(el, callback) {
    let $el = $(el);
    // Here we try to set up the necessary events for Drag and Drop ("DnD") on
    // every browser except IE9. We specifically exclude IE9 because it's one
    // browser that supports just enough of the functionality we need to be
    // confusing. In particular, it supports drag events, so drop zones will
    // highlight when a file is dragged into the browser window. It doesn't
    // support the FileList object though, so the user's expectation that DnD is
    // supported based on this highlighting would be incorrect.
    if (!this._isIE9()) {
      let $zone       = this._getZone(el),
          getState    = () => $el.data("state"),
          setState    = (newState) => $el.data("state", newState),
          transition  = multimethod()
          .dispatch(e => [getState(), e.type])
          .when(["plain", "showZone"], e => {
            $zone.removeClass(this._overClass);
            $zone.addClass(this._activeClass);
            setState("activated");
          })
          .whenAny([
            // If we're in the dropped-on state and we receive a draghoverend:zone
            // event, it means that we've just handled a file drop and need to be in
            // the plain state.
            ["dropped-on", "draghoverend:zone"],
            // If we're plain and receive hideZone, it means we've made ourselves
            // plain already and are now receiving the hideZone message sent to all
            // file inputs after a drag has stopped.
            ["plain", "hideZone"],
            // If we're activated and receive a hideZone, it means the drag has
            // left the browser window and we need to be plain. This can happen
            // when the browser is occluded by an OS window and the user drags
            // the file from the browser back to that window.
            ["activated", "hideZone"]], e => {
            $zone.removeClass(this._overClass);
            $zone.removeClass(this._activeClass);
            setState("plain");
          })
          .when(["activated", "draghoverstart:zone"], e => {
            $zone.addClass(this._overClass);
            $zone.removeClass(this._activeClass);
            setState("over");
          })
          // Here we handle the drop event, and enter the "dropped-on" state.
          // This is necessary because draghoverend:zone can denote that either
          // the file was dragged away *or* that it has been dropped on us, and
          // we will need to know what happened in order to properly interpret
          // the draghoverend:zone event that was also triggered by this drop.
          .when(["over", "drop"], e => {
            this._handleDrop(e, el);
            setState("dropped-on");
          })
          // If we're in the "over" state and we receive draghoverend:zone, it means
          // the file has been dragged away and *not* dropped on us, since our state
          // is not "dropped-on"
          .when(["over", "draghoverend:zone"], e => {
            $zone.removeClass(this._overClass);
            $zone.addClass(this._activeClass);
            setState("activated");
          })
          // This next case happens when the window (like Finder) that a file is
          // being dragged from occludes the browser window, and the dragged
          // item first enters the page over a drop zone instead of entering
          // through a none-zone element.
          //
          // The dragenter event that caused this draghoverstart to occur will
          // bubble to the document, where it will cause a showZone event to be
          // fired, and drop zones will activate and their states will
          // transition to "activated".
          //
          // We schedule a function to be run *after* that happens, using
          // setTimeout. The function we schedule will set the current element's
          // state to "over", preparing us to deal with a subsequent
          // "draghoverend".
          .when(["plain", "draghoverstart:zone"], e => {
            window.setTimeout(() => {
              $zone.addClass(this._overClass);
              $zone.removeClass(this._activeClass);
              setState("over");
            }, 0);
          })
          .else(e => {
            console.log("fileInput DnD unhandled transition", getState(), e.type, e);
          });

      if ($fileInputs.length === 0) this._enableDocumentEvents();
      setState("plain");
      $zone.on(this._zoneEvents, transition);
      $fileInputs = $fileInputs.add(el);
      this._enableDraghover($zone, "draghoverstart:zone", "draghoverend:zone");
    }

    $el.on("change.fileInputBinding", uploadFiles);
  },

  unsubscribe: function(el) {
    let $el   = $(el),
        $zone = this._getZone(el);

    $el.removeData("state");

    $zone.removeClass(this._overClass);
    $zone.removeClass(this._activeClass);

    this._disableDraghover($zone);

    // Clean up local event handlers.
    $el.off(".fileInputBinding");
    $zone.off(this._zoneEvents);

    // Remove el from list of inputs and (maybe) clean up global event handlers.
    $fileInputs = $fileInputs.not(el);
    if ($fileInputs.length === 0) this._disableDocumentEvents();
  }
});
inputBindings.register(fileInputBinding, 'shiny.fileInputBinding');
