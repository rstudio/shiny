var BindingRegistry = function() {
  this.bindings = [];
  this.bindingNames = {};
};
(function() {
  this.register = function(binding, bindingName, priority) {
    var bindingObj = {binding: binding, priority: priority || 0};
    this.bindings.unshift(bindingObj);
    if (bindingName) {
      this.bindingNames[bindingName] = bindingObj;
      binding.name = bindingName;
    }
  };
  this.setPriority = function(bindingName, priority) {
    var bindingObj = this.bindingNames[bindingName];
    if (!bindingObj)
      throw "Tried to set priority on unknown binding " + bindingName;
    bindingObj.priority = priority || 0;
  };
  this.getPriority = function(bindingName) {
    var bindingObj = this.bindingNames[bindingName];
    if (!bindingObj)
      return false;
    return bindingObj.priority;
  };
  this.getBindings = function() {
    // Sort the bindings. The ones with higher priority are consulted
    // first; ties are broken by most-recently-registered.
    return mergeSort(this.bindings, function(a, b) {
      return b.priority - a.priority;
    });
  };
}).call(BindingRegistry.prototype);


var inputBindings = exports.inputBindings = new BindingRegistry();
var outputBindings = exports.outputBindings = new BindingRegistry();
