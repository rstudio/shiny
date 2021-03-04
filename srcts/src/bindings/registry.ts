import { InputBinding, OutputBinding } from ".";
import { mergeSort } from "../utils";

// type BindingType = OutputBinding | InputBinding;

interface BindingType {
  binding: InputBinding | OutputBinding;
  priority: number;
  name?: string;
}

type BindingsType = Array<BindingType>;

class BindingRegistry {
  bindings: BindingsType = [];
  bindingNames: Record<string, BindingType> = {};

  register(
    binding: InputBinding | OutputBinding,
    bindingName: string,
    priority = 0
  ): void {
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

  getBindings(): BindingsType {
    // Sort the bindings. The ones with higher priority are consulted
    // first; ties are broken by most-recently-registered.
    return mergeSort(this.bindings, function (a, b) {
      return b.priority - a.priority;
    });
  }
}

export { BindingRegistry };
