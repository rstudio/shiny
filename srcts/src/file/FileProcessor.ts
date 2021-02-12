// Generic driver class for doing chunk-wise asynchronous processing of a
// FileList object. Subclass/clone it and override the `on*` functions to
// make it do something useful.
const FileProcessor = function (files) {
  this.files = files;
  this.fileIndex = -1;
  // Currently need to use small chunk size because R-Websockets can't
  // handle continuation frames
  this.aborted = false;
  this.completed = false;

  // TODO: Register error/abort callbacks

  this.$run();
};

(function () {
  // Begin callbacks. Subclassers/cloners may override any or all of these.
  this.onBegin = function (files, cont) {
    setTimeout(cont, 0);
  };
  this.onFile = function (file, cont) {
    setTimeout(cont, 0);
  };
  this.onComplete = function () {
    return;
  };
  this.onAbort = function () {
    return;
  };
  // End callbacks

  // Aborts processing, unless it's already completed
  this.abort = function () {
    if (this.completed || this.aborted) return;

    this.aborted = true;
    this.onAbort();
  };

  // Returns a bound function that will call this.$run one time.
  this.$getRun = function () {
    let called = false;

    return () => {
      if (called) return;
      called = true;
      this.$run();
    };
  };

  // This function will be called multiple times to advance the process.
  // It relies on the state of the object's fields to know what to do next.
  this.$run = function () {
    if (this.aborted || this.completed) return;

    if (this.fileIndex < 0) {
      // Haven't started yet--begin
      this.fileIndex = 0;
      this.onBegin(this.files, this.$getRun());
      return;
    }

    if (this.fileIndex === this.files.length) {
      // Just ended
      this.completed = true;
      this.onComplete();
      return;
    }

    // If we got here, then we have a file to process, or we are
    // in the middle of processing a file, or have just finished
    // processing a file.

    const file = this.files[this.fileIndex++];

    this.onFile(file, this.$getRun());
  };
}.call(FileProcessor.prototype));

export { FileProcessor };
