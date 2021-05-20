import $ from "jquery";
import { InputBinding } from ".";
import { FileUploader } from "../../file/FileProcessor";
import { Shiny } from "../../shiny";

// NOTE On Safari, at least version 10.1.2, *if the developer console is open*,
// setting the input's value will behave strangely because of a Safari bug. The
// uploaded file's name will appear over the placeholder value, instead of
// replacing it. The workaround is to restart Safari. When I (Alan Dipert) ran
// into this bug Winston Chang helped me diagnose the exact problem, and Winston
// then submitted a bug report to Apple.
function setFileText($el: JQuery<EventTarget>, files) {
  const $fileText = $el.closest("div.input-group").find("input[type=text]");

  if (files.length === 1) {
    $fileText.val(files[0].name);
  } else {
    $fileText.val(files.length + " files");
  }
}

// If previously selected files are uploading, abort that.
function abortCurrentUpload($el: JQuery<EventTarget>) {
  const uploader = $el.data("currentUploader");

  if (uploader) uploader.abort();
  // Clear data-restore attribute if present.
  $el.removeAttr("data-restore");
}

function uploadDroppedFilesIE10Plus(el: FileInputHTMLElement, files): void {
  const $el = $(el);

  abortCurrentUpload($el);

  // Set the label in the text box
  setFileText($el, files);

  // Start the new upload and put the uploader in 'currentUploader'.
  $el.data(
    "currentUploader",
    new FileUploader(Shiny.shinyapp, fileInputBindingGetId(el), files, el)
  );
}

function uploadFiles(evt: Event): void {
  const $el = $(evt.target);

  abortCurrentUpload($el);

  const files = evt.target.files;
  const id = fileInputBindingGetId(evt.target);

  if (files.length === 0) return;

  // Set the label in the text box
  setFileText($el, files);

  // Start the new upload and put the uploader in 'currentUploader'.
  $el.data(
    "currentUploader",
    new FileUploader(Shiny.shinyapp, id, files, evt.target)
  );
}

// Here we maintain a list of all the current file inputs. This is necessary
// because we need to trigger events on them in order to respond to file drag
// events. For example, they should all light up when a file is dragged on to
// the page.
// TODO-barret ; Should this be an internal class property?
let $fileInputs = $();

interface FileInputHTMLElement extends HTMLElement {
  name: string;
  files: FileList;
}

function fileInputBindingGetId(el: FileInputHTMLElement): string {
  return InputBinding.prototype.getId.call(this, el) || el.name;
}

class FileInputBinding extends InputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find('input[type="file"]');
  }
  getId(el: FileInputHTMLElement): string {
    return fileInputBindingGetId(el);
  }
  getValue(el: HTMLElement): { name?: string } | null {
    // This returns a non-undefined value only when there's a 'data-restore'
    // attribute, which is set only when restoring Shiny state. If a file is
    // uploaded through the browser, 'data-restore' gets cleared.
    const data = $(el).attr("data-restore");

    if (data) {
      const dataParsed = JSON.parse(data);

      // Set the label in the text box
      const $fileText = $(el)
        .closest("div.input-group")
        .find("input[type=text]");

      if (dataParsed.name.length === 1) {
        $fileText.val(dataParsed.name[0]);
      } else {
        $fileText.val(dataParsed.name.length + " files");
      }

      // Manually set up progress bar. A bit inelegant because it duplicates
      // code from FileUploader, but duplication is less bad than alternatives.
      const $progress = $(el).closest("div.form-group").find(".progress");
      const $bar = $progress.find(".progress-bar");

      $progress.removeClass("active");
      $bar.width("100%");
      $bar.css("visibility", "visible");

      return dataParsed;
    } else {
      return null;
    }
  }
  setValue(el: HTMLElement, value: void): void {
    // Not implemented
    el;
    value;
  }
  getType(el: HTMLElement): string {
    // This will be used only when restoring a file from a saved state.
    return "shiny.file";
    el;
  }
  _zoneOf(el: HTMLElement | JQuery<HTMLElement>): JQuery<HTMLElement> {
    return $(el).closest("div.input-group");
  }
  // This function makes it possible to attach listeners to the dragenter,
  // dragleave, and drop events of a single element with children. It's not
  // intuitive to do directly because outer elements fire "dragleave" events
  // both when the drag leaves the element and when the drag enters a child. To
  // make it easier, we maintain a count of the elements being dragged across
  // and trigger 3 new types of event:
  //
  // 1. draghover:enter - When a drag enters el and any of its children.
  // 2. draghover:leave - When the drag leaves el and all of its children.
  // 3. draghover:drop - When an item is dropped on el or any of its children.
  _enableDraghover(el: JQuery<HTMLElement>): JQuery<HTMLElement> {
    const $el = $(el);
    let childCounter = 0;

    $el.on({
      "dragenter.draghover": (e) => {
        if (childCounter++ === 0) {
          $el.trigger("draghover:enter", e);
        }
      },
      "dragleave.draghover": (e) => {
        if (--childCounter === 0) {
          $el.trigger("draghover:leave", e);
        }
        if (childCounter < 0) {
          console.error("draghover childCounter is negative somehow");
        }
      },
      "dragover.draghover": (e) => {
        e.preventDefault();
      },
      "drop.draghover": (e) => {
        childCounter = 0;
        $el.trigger("draghover:drop", e);
        e.preventDefault();
      },
    });
    return $el;
  }
  _disableDraghover(el: JQuery<HTMLElement>): JQuery<HTMLElement> {
    return $(el).off(".draghover");
  }
  _ZoneClass: {
    ACTIVE: "shiny-file-input-active";
    OVER: "shiny-file-input-over";
  };
  _enableDocumentEvents(): void {
    const $doc = $("html"),
      { ACTIVE, OVER } = this._ZoneClass;

    this._enableDraghover($doc).on({
      "draghover:enter.draghover":
        // e: Event
        () => {
          this._zoneOf($fileInputs).addClass(ACTIVE);
        },
      "draghover:leave.draghover":
        // e: Event
        () => {
          this._zoneOf($fileInputs).removeClass(ACTIVE);
        },
      "draghover:drop.draghover":
        // e: Event
        () => {
          this._zoneOf($fileInputs).removeClass(OVER).removeClass(ACTIVE);
        },
    });
  }
  _disableDocumentEvents(): void {
    const $doc = $("html");

    $doc.off(".draghover");
    this._disableDraghover($doc);
  }
  _canSetFiles(fileList: FileList): boolean {
    const testEl = document.createElement("input");

    testEl.type = "file";
    try {
      testEl.files = fileList;
    } catch (e) {
      return false;
    }
    return true;
  }
  _handleDrop(e: JQuery.TriggeredEvent, el: FileInputHTMLElement): void {
    const files = e.originalEvent.dataTransfer.files,
      $el = $(el);

    if (files === undefined || files === null) {
      // 1. The FileList object isn't supported by this browser, and
      // there's nothing else we can try. (< IE 10)
      console.log(
        "Dropping files is not supported on this browser. (no FileList)"
      );
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
  }
  subscribe(el: FileInputHTMLElement, callback: (x: boolean) => void): void {
    callback;

    $(el).on("change.fileInputBinding", uploadFiles);
    // Here we try to set up the necessary events for Drag and Drop ("DnD").
    if ($fileInputs.length === 0) this._enableDocumentEvents();
    $fileInputs = $fileInputs.add(el);
    const $zone = this._zoneOf(el),
      { OVER } = this._ZoneClass;

    this._enableDraghover($zone).on({
      "draghover:enter.draghover": (e) => {
        e;
        $zone.addClass(OVER);
      },
      "draghover:leave.draghover": (e) => {
        $zone.removeClass(OVER);
        // Prevent this event from bubbling to the document handler,
        // which would deactivate all zones.
        e.stopPropagation();
      },
      "draghover:drop.draghover": (e, dropEvent) => {
        e;
        this._handleDrop(dropEvent, el);
      },
    });
  }

  unsubscribe(el: HTMLElement): void {
    const $el = $(el),
      $zone = this._zoneOf(el);

    $zone.removeClass(this._ZoneClass.OVER).removeClass(this._ZoneClass.ACTIVE);

    this._disableDraghover($zone);
    $el.off(".fileInputBinding");
    $zone.off(".draghover");

    // Remove el from list of inputs and (maybe) clean up global event handlers.
    $fileInputs = $fileInputs.not(el);
    if ($fileInputs.length === 0) this._disableDocumentEvents();
  }
}

export { FileInputBinding };
