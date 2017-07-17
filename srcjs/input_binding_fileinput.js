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
  getZone: function(el) {
    return $(el).closest("div.input-group");
  },
  enableDraghover: function($el) {
    // Create an empty jQuery collection. This is a set-like data structure that
    // jQuery normally uses to contain the results of a selection.
    let collection = $();

    // Attach a dragenter handler to $el and all of its children. When the first
    // child is entered, trigger a draghoverstart event.
    $el.on('dragenter.dragHover', function(e) {
      if (collection.size() === 0) {
        $el.trigger('draghoverstart', e.originalEvent);
      }
      // Every child that has fired dragenter is added to the collection.
      // Addition is idempotent, which accounts for elements producing dragenter
      // multiple times.
      collection = collection.add(e.originalEvent.target);
    });

    // Attach dragleave and drop handlers to $el and its children. Whenever a
    // child fires either of these events, remove it from the collection.
    $el.on('dragleave.dragHover drop.dragHover', function(e) {
      // Timeout might be required by some browsers (Firefox) that fire
      // dragleave before firing dragenter.
      setTimeout(() => {
        collection = collection.not(e.originalEvent.target);
        // When the collection has no elements, all of the children have been
        // removed, and produce draghoverend event.
        if (collection.size() === 0) {
          $el.trigger('draghoverend', e.originalEvent);
        }
      }, 0);
    });
  },
  disableDraghover: function($el) {
    $el.off(".dragHover");
  },
  enableDocumentEvents: function() {
    let $doc = $("html");

    this.enableDraghover($doc);
    $doc.on("draghoverstart.fileDrag", e => {
      $fileInputs.trigger("showZone.fileDrag");
    });
    $doc.on("draghoverend.fileDrag", e => {
      $fileInputs.trigger("hideZone.fileDrag");
    });
    // Enable the drop event and prevent download if the file is dropped outside
    // of a drop zone.
    $doc.on("dragover.fileDrag drop.fileDrag", e => {
      e.preventDefault();
    });

  },
  disableDocumentEvents: function() {
    let $doc = $("html");

    $doc.off(".fileDrag");
    this.disableDraghover($doc);
  },
  zoneEvents: [
    "showZone.fileDrag",
    "hideZone.fileDrag",
    "dragenter",
    "dragleave",
    "drop"
  ].join(" "),
  canSetFiles: function(fileList) {
    var testEl = document.createElement("input");
    testEl.type = "file";
    try {
      testEl.files = fileList;
    } catch (e) {
      return false;
    }
    return true;
  },
  handleDrop: function(e, el) {
    const files = e.originalEvent.dataTransfer.files,
          $el   = $(el);
    if (files === undefined || files === null) {
      // 1. The FileList object isn't supported by this browser, and
      // there's nothing else we can try. (< IE 10)
      console.log("Dropping files is not supported on this browser. (no FileList)");
    } else if (!this.canSetFiles(files)) {
      // 2. The browser doesn't support assigning a type=file input's .files
      // property, but we do have a FileList to work with. (IE10+)
      $el.val("");
      uploadDroppedFilesIE10Plus(el, files);
    } else {
      // 3. The browser supports FileList and input.files assignment.
      // (Chrome, Safari)
      $el.val("");
      el.files = e.originalEvent.dataTransfer.files;
    }
    // We manually trigger the hideZone event because for some reason the
    // document-level handler is disabled by the drop event.
    $fileInputs.trigger("hideZone.fileDrag");
    // Because the document handlers are removed inexplicably, we re-add them
    // here.
    this.disableDocumentEvents();
    this.enableDocumentEvents();
  },
  subscribe: function(el, callback) {
    let $el         = $(el),
        $zone       = this.getZone(el),
        activeClass = "shiny-file-input-active",
        overClass   = "shiny-file-input-over",
        getState    = () => $el.data("state"),
        setState    = (newState) => $el.data("state", newState),
        transition  = multimethod()
          .dispatch(e => [getState(), e.type])
          .whenAny([
            ["plain", "showZone"],
            ["over", "dragleave"],
          ], e => {
            $zone.removeClass(overClass);
            $zone.addClass(activeClass);
            setState("activated");
          })
          .whenAny([
            ["activated", "hideZone"],
            ["over", "hideZone"]
          ], e => {
            $zone.removeClass(overClass);
            $zone.removeClass(activeClass);
            setState("plain");
          })
          .whenAny([
            ["activated", "dragenter"],
            ["over", "dragenter"]
          ], e => {
            $zone.addClass(overClass);
            setState("over");
          })
          .when(["over", "drop"], (e) => {
            // In order to have reached this code, the browser must support Drag
            // and Drop ("DnD"), because a "drop" event has been triggered. That
            // means it's probably Chrome, Safari, or IE9+
            this.handleDrop(e, el);
            e.preventDefault();
            e.stopPropagation();
          });

    if ($fileInputs.length === 0) this.enableDocumentEvents();

    setState("plain");

    $el.on("change.fileInputBinding", uploadFiles);
    $zone.on(this.zoneEvents, transition);

    $fileInputs = $fileInputs.add(el);
  },

  unsubscribe: function(el) {
    let $el   = $(el),
        $zone = this.getZone(el);

    $el.removeData("state");

    // Clean up local event handlers.
    $el.off(".fileInputBinding");
    $zone.off(this.zoneEvents);

    // Remove el from list of inputs and (maybe) clean up global event handlers.
    $fileInputs = $fileInputs.not(el);
    if ($fileInputs.length === 0) this.disableDocumentEvents();
  }
});
inputBindings.register(fileInputBinding, 'shiny.fileInputBinding');
