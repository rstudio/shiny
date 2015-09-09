var OutputBindingAdapter = function(el, binding) {
  this.el = el;
  this.binding = binding;

  // If the binding actually has a resize method, override the prototype of
  // onResize with a version that does a makeResizeFilter on the element.
  if (binding.resize) {
    this.onResize = makeResizeFilter(el, function(width, height) {
      binding.resize(el, width, height);
    });
  }
};
(function() {
  this.getId = function() {
    return this.binding.getId(this.el);
  };
  this.onValueChange = function(data) {
    this.binding.onValueChange(this.el, data);
  };
  this.onValueError = function(err) {
    this.binding.onValueError(this.el, err);
  };
  this.showProgress = function(show) {
    this.binding.showProgress(this.el, show);
  };
  this.onResize = function() {
    // Intentionally left blank; see constructor
  };
}).call(OutputBindingAdapter.prototype);
