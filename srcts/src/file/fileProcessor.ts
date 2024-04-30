import $ from "jquery";
import { triggerFileInputChanged } from "../events/inputChanged";
import { $escape } from "../utils";
import type { ShinyApp } from "../shiny/shinyapp";
import { getFileInputBinding } from "../shiny/initedMethods";

type JobId = string;
type UploadUrl = string;
type UploadInitValue = { jobId: JobId; uploadUrl: UploadUrl };
type UploadEndValue = never;

// Generic driver class for doing chunk-wise asynchronous processing of a
// FileList object. Subclass/clone it and override the `on*` functions to
// make it do something useful.
class FileProcessor {
  files: File[];
  fileIndex = -1;
  // Currently need to use small chunk size because R-Websockets can't
  // handle continuation frames
  aborted = false;
  completed = false;

  constructor(files: FileList, exec$run = true) {
    this.files = Array.from(files);

    // TODO: Register error/abort callbacks
    if (exec$run) {
      this.$run();
    }
  }

  // Begin callbacks. Subclassers/cloners may override any or all of these.
  onBegin(files: File[], cont: () => void): void {
    files;
    setTimeout(cont, 0);
  }
  onFile(file: File, cont: () => void): void {
    file;
    setTimeout(cont, 0);
  }
  onComplete(): void {
    return;
  }
  onAbort(): void {
    return;
  }
  // End callbacks

  // Aborts processing, unless it's already completed
  abort(): void {
    if (this.completed || this.aborted) return;

    this.aborted = true;
    this.onAbort();
  }

  // Returns a bound function that will call this.$run one time.
  $getRun(): () => void {
    let called = false;

    return () => {
      if (called) return;
      called = true;
      this.$run();
    };
  }

  // This function will be called multiple times to advance the process.
  // It relies on the state of the object's fields to know what to do next.
  $run(): void {
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
  }
}

class FileUploader extends FileProcessor {
  shinyapp: ShinyApp;
  id: string;
  el: HTMLElement;

  jobId!: JobId;
  uploadUrl!: UploadUrl;
  progressBytes!: number;
  totalBytes!: number;

  constructor(
    shinyapp: ShinyApp,
    id: string,
    files: FileList,
    el: HTMLElement
  ) {
    // Init super with files, do not execute `this.$run()` before setting variables
    super(files, false);
    this.shinyapp = shinyapp;
    this.id = id;
    this.el = el;
    this.$run();
  }

  makeRequest(
    method: "uploadInit",
    args: Array<Array<{ name: string; size: number; type: string }>>,
    onSuccess: (value: UploadInitValue) => void,
    onFailure: Parameters<ShinyApp["makeRequest"]>[3],
    blobs: Parameters<ShinyApp["makeRequest"]>[4]
  ): void;
  makeRequest(
    method: "uploadEnd",
    args: [string, string],
    // UploadEndValue can not be used as the type will not conform
    onSuccess: (value: unknown) => void,
    onFailure: Parameters<ShinyApp["makeRequest"]>[3],
    blobs: Parameters<ShinyApp["makeRequest"]>[4]
  ): void;
  makeRequest(
    method: string,
    args: unknown[],
    onSuccess: Parameters<ShinyApp["makeRequest"]>[2],
    onFailure: Parameters<ShinyApp["makeRequest"]>[3],
    blobs: Parameters<ShinyApp["makeRequest"]>[4]
  ): void {
    this.shinyapp.makeRequest(method, args, onSuccess, onFailure, blobs);
  }
  onBegin(files: File[], cont: () => void): void {
    // Reset progress bar
    this.$setError(null);
    this.$setActive(true);
    this.$setVisible(true);
    this.onProgress(null, 0);

    this.totalBytes = 0;
    this.progressBytes = 0;
    $.each(files, (i, file) => {
      this.totalBytes += file.size;
    });

    const fileInfo = $.map(files, function (file: File) {
      return {
        name: file.name,
        size: file.size,
        type: file.type,
      };
    });

    this.makeRequest(
      "uploadInit",
      [fileInfo],
      (response) => {
        this.jobId = response.jobId;
        this.uploadUrl = response.uploadUrl;
        cont();
      },
      (error) => {
        this.onError(error);
      },
      undefined
    );
  }
  onFile(file: File, cont: () => void): void {
    this.onProgress(file, 0);

    /* eslint-disable-next-line @typescript-eslint/no-floating-promises */
    $.ajax(this.uploadUrl, {
      type: "POST",
      cache: false,
      xhr: () => {
        if (typeof $.ajaxSettings.xhr !== "function")
          throw "jQuery's XHR is not a function";

        const xhrVal = $.ajaxSettings.xhr();

        if (xhrVal.upload) {
          xhrVal.upload.onprogress = (e) => {
            if (e.lengthComputable) {
              this.onProgress(
                file,
                (this.progressBytes + e.loaded) / this.totalBytes
              );
            }
          };
        }
        return xhrVal;
      },
      data: file,
      contentType: "application/octet-stream",
      processData: false,
      success: () => {
        this.progressBytes += file.size;
        cont();
      },
      error: (jqXHR, textStatus, errorThrown) => {
        errorThrown;
        this.onError(jqXHR.responseText || textStatus);
      },
    });
  }
  onComplete(): void {
    const fileInfo = $.map(this.files, function (file: File, i) {
      return {
        name: file.name,
        size: file.size,
        type: file.type,
      };
      i;
    });

    // Trigger shiny:inputchanged. Unlike a normal shiny:inputchanged event,
    // it's not possible to modify the information before the values get
    // sent to the server.
    const evt = triggerFileInputChanged(
      this.id,
      fileInfo,
      getFileInputBinding(),
      this.el,
      "shiny.fileupload",
      document
    );

    this.makeRequest(
      "uploadEnd",
      [this.jobId, this.id],
      () => {
        this.$setActive(false);
        this.onProgress(null, 1);
        this.$bar().text("Upload complete");
        // Reset the file input's value to "". This allows the same file to be
        // uploaded again. https://stackoverflow.com/a/22521275
        $(evt.el as HTMLElement).val("");
      },
      (error) => {
        this.onError(error);
      },
      undefined
    );
    this.$bar().text("Finishing upload");
  }
  onError(message: string): void {
    this.$setError(message || "");
    this.$setActive(false);
  }
  onAbort(): void {
    this.$setVisible(false);
  }
  onProgress(file: File | null, completed: number): void {
    this.$bar().width(Math.round(completed * 100) + "%");
    this.$bar().text(file ? file.name : "");
  }
  $container(): JQuery<HTMLElement> {
    return $("#" + $escape(this.id) + "_progress.shiny-file-input-progress");
  }
  $bar(): JQuery<HTMLElement> {
    return $(
      "#" +
        $escape(this.id) +
        "_progress.shiny-file-input-progress .progress-bar"
    );
  }
  $setVisible(visible: boolean): void {
    this.$container().css("visibility", visible ? "visible" : "hidden");
  }
  $setError(error: string | null): void {
    this.$bar().toggleClass("progress-bar-danger", error !== null);
    if (error !== null) {
      this.onProgress(null, 1);
      this.$bar().text(error);
    }
  }
  $setActive(active: boolean): void {
    this.$container().toggleClass("active", !!active);
  }
}

export { FileUploader };
export type { UploadInitValue, UploadEndValue };
