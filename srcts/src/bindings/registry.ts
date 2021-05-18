import { mergeSort } from "../utils";

interface BindingInterface {
  name: string;
}

interface BindingObjType<BindingType> {
  binding: BindingType;
  priority: number;
  name?: string;
}

class BindingRegistry<BindingType extends BindingInterface> {
  bindings: Array<BindingObjType<BindingType>> = [];
  bindingNames: Record<string, BindingObjType<BindingType>> = {};

  register(binding: BindingType, bindingName: string, priority = 0): void {
    const bindingObj = { binding, priority };

    this.bindings.unshift(bindingObj);
    if (bindingName) {
      this.bindingNames[bindingName] = bindingObj;
      binding.name = bindingName;
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

  getBindings(): Array<BindingObjType<BindingType>> {
    // Sort the bindings. The ones with higher priority are consulted
    // first; ties are broken by most-recently-registered.
    return mergeSort(this.bindings, function (a, b) {
      return b.priority - a.priority;
    });
  }
}

export { BindingRegistry };
