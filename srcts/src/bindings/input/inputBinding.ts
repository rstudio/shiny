import type { EventPriority } from "../../inputPolicies/inputPolicy";
import type { RatePolicyModes } from "../../inputPolicies/inputRateDecorator";
import type { BindScope } from "../../shiny/bind";

type SubscribeEventPriority =
  | EventPriority
  | boolean
  | { priority: EventPriority };
// Historically, the .subscribe()'s callback value only took a boolean. In this
// case:
//   * false: send value immediately (i.e., priority = "immediate")
//   * true: send value later (i.e., priority = "deferred")
//     * The input rate policy is also consulted on whether to debounce or
//       throttle
// In recent versions, the value can also be "event", meaning that the
// value should be sent immediately regardless of whether it has changed.

class InputBinding {
  name!: string;

  // Returns a jQuery object or element array that contains the
  // descendants of scope that match this binding
  find(scope: BindScope): JQuery<HTMLElement> {
    throw "Not implemented";
    scope; // unused var
  }

  getId(el: HTMLElement): string {
    return el.getAttribute("data-input-id") || el.id;
  }

  // Gives the input a type in case the server needs to know it
  // to deserialize the JSON correctly
  getType(el: HTMLElement): string | null {
    return null;
    el; // unused var
  }
  getValue(el: HTMLElement): any {
    throw "Not implemented";
    el; // unused var
  }

  subscribe(
    el: HTMLElement,
    callback: (value: SubscribeEventPriority) => void
  ): void {
    // empty
    el; // unused var
    callback; // unused var
  }
  unsubscribe(el: HTMLElement): void {
    // empty
    el; // unused var
  }

  // This is used for receiving messages that tell the input object to do
  // things, such as setting values (including min, max, and others).
  // 'data' should be an object with elements corresponding to value, min,
  // max, etc., as appropriate for the type of input object. It also should
  // trigger a change event.
  receiveMessage(el: HTMLElement, data: unknown): Promise<void> | void {
    throw "Not implemented";
    el; // unused var
    data; // unused var
  }
  getState(el: HTMLElement): unknown {
    throw "Not implemented";
    el; // unused var
  }

  getRatePolicy(
    el: HTMLElement
  ): { policy: RatePolicyModes; delay: number } | null {
    return null;
    el; // unused var
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

//// NOTES FOR FUTURE DEV
// Turn register systemin into something that is intialized for every instance.
// "Have a new instance for every item, not an instance that does work on every item"
//
// * Keep register as is for historical purposes
// make a new register function that would take a class
// these class could be constructed at build time
// store the constructed obj on the ele and retrieve

// Then the classes could store their information within their local class, rather than on the element
// VERY CLEAN!!!

// to invoke methods, it would be something like `el.shinyClass.METHOD(x,y,z)`
// * See https://github.com/rstudio/shinyvalidate/blob/c8becd99c01fac1bac03b50e2140f49fca39e7f4/srcjs/shinyvalidate.js#L157-L167
// these methods would be added using a new method like `inputBindings.registerClass(ClassObj, name)`

// things to watch out for:
// * unbind, then rebind. Maybe we stash the local content.

// Updates:
// * Feel free to alter method names on classes. (And make them private)
//// END NOTES FOR FUTURE DEV

export { InputBinding };
export type { SubscribeEventPriority };
