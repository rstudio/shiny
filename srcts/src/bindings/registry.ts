import { debounce } from "../time";
import { mergeSort } from "../utils";

interface BindingBase {
  name: string;
}

interface BindingObj<Binding> {
  binding: Binding;
  priority: number;
  name?: string;
}

const debouncedBind = debounce(10, () => {
  Shiny.bindAll(document.documentElement);
});

class BindingRegistry<Binding extends BindingBase> {
  name: string;
  bindings: Array<BindingObj<Binding>> = [];
  bindingNames: { [key: string]: BindingObj<Binding> } = {};

  register(binding: Binding, bindingName: string, priority = 0): void {
    const bindingObj = { binding, priority };

    this.bindings.unshift(bindingObj);
    if (bindingName) {
      this.bindingNames[bindingName] = bindingObj;
      binding.name = bindingName;
    }

    // If this registration happens after Shiny has initialized and outside of a
    // renderHtml() context, then this is likely happening after Shiny has
    // attempted to bindAll() before all bindings are registered. At least one
    // scenario of where this can happen is when some JS code calls register()
    // in a require() callback (or, more generally, a setTimeout()). (#3635)
    if (
      Shiny &&
      Shiny.bindAll &&
      !(window as unknown)["Shiny"]._renderingHtml
    ) {
      debouncedBind();
    }
  }

  setPriority(bindingName: string, priority: number): void {
    const bindingObj = this.bindingNames[bindingName];

    if (!bindingObj)
      throw "Tried to set priority on unknown binding " + bindingName;
    bindingObj.priority = priority || 0;
  }

  getPriority(bindingName: string): number | false {
    const bindingObj = this.bindingNames[bindingName];

    if (!bindingObj) return false;
    return bindingObj.priority;
  }

  getBindings(): Array<BindingObj<Binding>> {
    // Sort the bindings. The ones with higher priority are consulted
    // first; ties are broken by most-recently-registered.
    return mergeSort(this.bindings, function (a, b) {
      return b.priority - a.priority;
    });
  }
}

export { BindingRegistry };
