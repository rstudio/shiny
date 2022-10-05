import { mergeSort } from "../utils";
import { Callbacks } from "../utils/callbacks";

interface BindingBase {
  name: string;
}

interface BindingObj<Binding> {
  binding: Binding;
  priority: number;
  name?: string;
}

class BindingRegistry<Binding extends BindingBase> {
  name!: string;
  bindings: Array<BindingObj<Binding>> = [];
  bindingNames: { [key: string]: BindingObj<Binding> } = {};
  registerCallbacks: Callbacks = new Callbacks();

  register(binding: Binding, bindingName: string, priority = 0): void {
    const bindingObj = { binding, priority };

    this.bindings.unshift(bindingObj);
    if (bindingName) {
      this.bindingNames[bindingName] = bindingObj;
      binding.name = bindingName;
    }

    this.registerCallbacks.invoke();
  }

  onRegister(fn: () => void, once = true): void {
    this.registerCallbacks.register(fn, once);
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
