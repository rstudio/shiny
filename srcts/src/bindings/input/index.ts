import { BindingRegistry } from "../registry";

interface NameValueHTMLElement extends HTMLElement {
  name: string;
  value: any;
}

class InputBinding {
  name: string;

  // Returns a jQuery object or element array that contains the
  // descendants of scope that match this binding
  find(scope: HTMLElement): JQuery<HTMLElement> | Array<HTMLElement> {
    throw "Not implemented";
    // add so that typescript isn't mad about an unused var
    scope;
  }

  getId(el: HTMLElement): string {
    return el["data-input-id"] || el.id;
  }

  // Gives the input a type in case the server needs to know it
  // to deserialize the JSON correctly
  getTypefunction(): false {
    return false;
  }
  getValue(el: HTMLElement): any {
    throw "Not implemented";
    el; // unused var
  }

  // The callback method takes one argument, whose value is boolean. If true,
  // allow deferred (debounce or throttle) sending depending on the value of
  // getRatePolicy. If false, send value immediately. Default behavior is `false`
  subscribe(el: HTMLElement, callback: (value: boolean) => void): void {
    // empty
    el;
    callback;
  }
  unsubscribe(el: HTMLElement): void {
    // empty
    el;
  }

  // This is used for receiving messages that tell the input object to do
  // things, such as setting values (including min, max, and others).
  // 'data' should be an object with elements corresponding to value, min,
  // max, etc., as appropriate for the type of input object. It also should
  // trigger a change event.
  receiveMessage(el: HTMLElement, data: any): void {
    throw "Not implemented";
    el;
    data;
  }
  getState(el: HTMLElement, data: any): any {
    throw "Not implemented";
    el;
    data;
  }

  getRatePolicy(): { policy: string; delay: number } | null {
    return null;
  }

  // Some input objects need initialization before being bound. This is
  // called when the document is ready (for statically-added input objects),
  // and when new input objects are added to the document with
  // htmlOutputBinding.renderValue() (for dynamically-added input objects).
  // This is called before the input is bound.
  initialize(el: HTMLElement): void {
    //empty
    el;
  }

  // This is called after unbinding the output.
  dispose(el: HTMLElement): void {
    //empty
    el;
  }
}

const inputBindings = new BindingRegistry();
