var InputBinding = exports.InputBinding = function() {};

(function() {

// Returns a jQuery object or element array that contains the
// descendants of scope that match this binding
this.find = function(scope) { throw "Not implemented"; };

this.getId = function(el) {
  return el['data-input-id'] || el.id;
};

// Gives the input a type in case the server needs to know it
// to deserialize the JSON correctly
this.getType = function() { return false; };
this.getValue = function(el) { throw "Not implemented"; };

// The callback method takes one argument, whose value is boolean. If true,
// allow deferred (debounce or throttle) sending depending on the value of
// getRatePolicy. If false, send value immediately.
this.subscribe = function(el, callback) { };
this.unsubscribe = function(el) { };

// This is used for receiving messages that tell the input object to do
// things, such as setting values (including min, max, and others).
// 'data' should be an object with elements corresponding to value, min,
// max, etc., as appropriate for the type of input object. It also should
// trigger a change event.
this.receiveMessage = function(el, data) { throw "Not implemented"; };
this.getState = function(el, data) { throw "Not implemented"; };

this.getRatePolicy = function() { return null; };

// Some input objects need initialization before being bound. This is
// called when the document is ready (for statically-added input objects),
// and when new input objects are added to the document with
// htmlOutputBinding.renderValue() (for dynamically-added input objects).
// This is called before the input is bound.
this.initialize = function(el) { };

// This is called after unbinding the output.
this.dispose = function(el) { };

}).call(InputBinding.prototype);
