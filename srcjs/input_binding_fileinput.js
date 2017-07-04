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


function uploadFiles(evt) {
  // If previously selected files are uploading, abort that.
  var $el = $(evt.target);
  var uploader = $el.data('currentUploader');
  if (uploader)
    uploader.abort();

  var files = evt.target.files;
  // IE8 here does not necessarily mean literally IE8; it indicates if the web
  // browser supports the FileList object (IE8/9 do not support it)
  var IE8 = typeof(files) === 'undefined';
  var id = fileInputBinding.getId(evt.target);

  if (!IE8 && files.length === 0)
    return;

  // Clear data-restore attribute if present.
  $el.removeAttr('data-restore');

  // Set the label in the text box
  var $fileText = $el.closest('div.input-group').find('input[type=text]');
  if (IE8) {
    // If we're using IE8/9, just use this placeholder
    $fileText.val("[Uploaded file]");
  } else if (files.length === 1) {
    $fileText.val(files[0].name);
  } else {
    $fileText.val(files.length + " files");
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

function enableDraghover($el) {
  let collection = $();

  $el.on('dragenter.dragHover', function(e) {
    if (collection.size() === 0) {
      $el.trigger('draghoverstart', e.originalEvent);
    }
    collection = collection.add(e.target);
  });

  $el.on('dragleave.dragHover drop.dragHover', function(e) {
    setTimeout(() => {
      collection = collection.not(e.target);
      if (collection.size() === 0)
        $el.trigger('draghoverend', e.originalEvent);
    }, 0);
  });
}

function disableDraghover($el) {
  $el.off(".dragHover");
}

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
  enableDocumentEvents: function() {
    let $doc = $(document);

    enableDraghover($doc);
    $doc.on("draghoverstart.fileDrag", e => {
      $fileInputs.trigger("showZone.fileDrag");
    });
    $doc.on("draghoverend.fileDrag", e => {
      $fileInputs.trigger("hideZone.fileDrag");
    });
    // Enable the drop event and prevent download if the file is dropped outside
    // of a drop zone.
    $doc.on("dragover.fileDrag drop.fileDrag", e => e.preventDefault());
  },
  disableDocumentEvents: function() {
    let $doc = $(document);

    $doc.off(".fileDrag");
    disableDraghover($doc);
  },
  zoneStyles: {
    "plain":     {"box-shadow": "none"},
    "activated": {"box-shadow": "0 0 6px 3px #5cb85c"},
    "over":      {"box-shadow": "0 0 6px 3px #00f"}
  },
  zoneEvents: [
    "showZone.fileDrag",
    "hideZone.fileDrag",
    "dragenter",
    "dragleave",
    "drop"
  ].join(" "),
  subscribe: function(el, callback) {
    let $el        = $(el),
        $zone      = this.getZone(el),
        getState   = () => $el.data("state"),
        setState   = (newState) => $el.data("state", newState),
        transition = multimethod()
          .dispatch(e => [getState(), e.type])
          .test(([s1, e1], [s2, e2]) => s1 === s2 && e1 === e2)
          .when(["plain", "showZone"], e => {
            $zone.css(this.zoneStyles["activated"]);
            setState("activated");
          })
          .when(["activated", "hideZone"], e => {
            $zone.css(this.zoneStyles["plain"]);
            setState("plain");
          })
          .when(["over", "hideZone"], e => {
            $zone.css(this.zoneStyles["plain"]);
            setState("plain");
          })
          .when(["activated", "dragenter"], e => {
            $zone.css(this.zoneStyles["over"]);
            setState("over");
          })
          .when(["over", "drop"], e => {
            // Set the input value to the empty string so that files with
            // identical names may be uploaded in succession.
            $el.val("");
            el.files = e.originalEvent.dataTransfer.files;
            e.preventDefault();
            // Allowing propagation here lets the event bubble to the top-level
            // document drop handler, which triggers a "hideZone" event on all
            // inputs.
          })
          .when(["over", "dragleave"], e => {
            $zone.css(this.zoneStyles["activated"]);
            setState("activated");
          });

    if ($fileInputs.length === 0) this.enableDocumentEvents();

    // Initialize this input's state and style.
    setState("plain");
    $zone.css(this.zoneStyles["plain"]);

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
