/*jshint forin:true, noarg:true, noempty:true, eqeqeq:true, bitwise:true, undef:true, unused:true, browser:true, jquery:true, maxerr:50 */
/*global Shiny*/
/*global describe, it, expect, beforeEach, afterEach*/

describe("Input Bindings", function() {

  // How to talk to an input binding (the long way)
  // $obj = $('.shiny-bound-input#in_text');
  // $obj.data('shiny-input-binding').receiveMessage($obj[0], {value: "foo"})

  var select_input_object = function(id) {
    return $('.shiny-bound-input#' + id);
  };

  // Given the name of the input binding, return the corresponding inputBinding
  // object.
  var get_input_binding_name = function(name) {
    return Shiny.inputBindings.bindingNames['shiny.' + name].binding;
  };

  // Given the id of an input object, return the corresponding inputBinding
  // object.
  var get_input_binding_id = function(id) {
    return select_input_object(id).data('shiny-input-binding');
  };

  var get_value = function(id) {
    var $obj = select_input_object(id);
    return get_input_binding_id(id).getValue($obj[0]);
  };

  var receive_message = function(id, data) {
    var $obj = select_input_object(id);
    get_input_binding_id(id).receiveMessage($obj[0], data);
  };

  var get_state = function(id) {
    var $obj = select_input_object(id);
    return get_input_binding_id(id).getState($obj[0]);
  };



  // These functions are here to reduce repetition. They are exactly the same
  // across different input bindings.

  var common_tests = function(id, binding_name) {

    it("select the object by id", function() {
      var $obj = select_input_object(id);
      expect($obj.length).toBe(1);
    });

    it("find the input binding", function() {
      // Should be able to find it by binding name
      expect(get_input_binding_name(binding_name)).not.toBeNull();

      // Should be same as when we retrieve it using input object id
      expect(get_input_binding_name(binding_name)).toBe(get_input_binding_id(id));
    });

    it("find() works", function() {
      var $obj = select_input_object(id);
      var input_binding = get_input_binding_name(binding_name);
      var find_result = input_binding.find(document).filter('#' + id);

      expect(find_result.length).toBe(1);
      expect(find_result instanceof jQuery).toBe(true);

      // Need to extract first element for testing equality of jQuery objects
      expect(find_result[0]).toBe($obj[0]);
    });

    it("getId() works", function() {
      var $obj = select_input_object(id);
      var input_binding = get_input_binding_name(binding_name);

      expect(input_binding.getId($obj[0])).toEqual(id);
    });

    it("getRatePolicy() works", function() {
      var input_binding = get_input_binding_name(binding_name);
      var rate_policy = input_binding.getRatePolicy();
      var valid_policies = ['direct', 'debounce', 'throttle'];
      var timed_policies = ['debounce', 'throttle'];

      // The policy can be null. If so, don't continue onto other expectations
      if (rate_policy === null)
        return;

      // If the policy is in valid_policies, then $.inArray should not return -1
      expect($.inArray(rate_policy.policy, valid_policies)).not.toBe(-1);

      // If it's a policy that requires a specified delay, check for delay
      if ($.inArray(rate_policy.policy, timed_policies) !== -1) {
        expect(typeof rate_policy.delay).toBe('number');
      }
    });

    // it("subscribe() works", function() {
    //   // TODO
    // });

    // it("unsubscribe() works", function() {
    //   // TODO
    // });

  };


  // ===========================================================================
  describe("textInputBinding", function() {
    var id           = 'in_text';   // id of the DOM object
    var binding_name = 'textInput'; // Name of the input binding in the registry

    // Some convenience objects
    var $obj;
    var input_binding;

    beforeEach(function(){
      var el = $('<input id="' + id +'" type="text" value="starting value"/>').prependTo('body');
      // Wrap the input object in a div so we can select and remove it later
      el.wrap('<div id="input_binding_test">');
      Shiny.bindAll();

      // Assign values to convenience objects
      $obj = select_input_object(id);
      input_binding = get_input_binding_name(binding_name);
    });

    afterEach(function(){
      Shiny.unbindAll();
      $('#input_binding_test').remove();
    });


    // Run tests that are exactly the same for all InputBindings
    common_tests(id, binding_name);


    it("getValue() works", function() {
      expect(input_binding.getValue($obj[0])).toEqual('starting value');
    });

    it("setValue() works", function() {
      input_binding.setValue($obj[0], 'value 2');
      expect(input_binding.getValue($obj[0])).toEqual('value 2');
    });

    it("getState() works", function() {
      expect(get_state(id)).toEqual({ value:"starting value" });

      receive_message(id, { value:"foo" });
      expect(get_state(id)).toEqual({ value:"foo" });
    });

    it("receiveMessage() works", function() {
      receive_message(id, { value:"foo" });
      expect(get_value(id)).toBe("foo");

      // Set and check the value again
      receive_message(id, { value:"bar" });
      expect(get_value(id)).toBe("bar");
    });

  });

  // ===========================================================================
  describe("numberInputBinding", function() {
    var id = 'in_number';
    var binding_name = 'numberInput'; // Name of the input binding in the registry

    // Some convenience objects
    var $obj;
    var input_binding;

    beforeEach(function(){
      var el = $('<input id="' + id +'" type="number" value="8" min="4" max="10" step="0.5"/>').prependTo('body');
      // Wrap the input object in a div so we can select and remove it later
      el.wrap('<div id="input_binding_test">');
      Shiny.bindAll();

      $obj          = select_input_object(id);
      input_binding = get_input_binding_name(binding_name);
    });

    afterEach(function(){
      Shiny.unbindAll();
      $('#input_binding_test').remove();
    });

    // Run tests that are exactly the same for all InputBindings
    common_tests(id, binding_name);


    it("getValue() works", function() {
      expect(input_binding.getValue($obj[0])).toBe(8);
    });

    it("setValue() works", function() {
      input_binding.setValue($obj[0], 5);
      expect(input_binding.getValue($obj[0])).toBe(5);

      // getValue should coerce to number
      input_binding.setValue($obj[0], '6');
      expect(input_binding.getValue($obj[0])).toBe(6);

      // getValue should return the numeric value when input is scientific notation
      input_binding.setValue($obj[0], '1e6');
      expect(input_binding.getValue($obj[0])).toBe(1000000);
    });

    it("getState() works", function() {
      expect(get_state(id)).toEqual({ value:8, min:4, max:10, step:0.5 });

      receive_message(id, { min:5, value:5, max:5, step:0.2 });
      expect(get_state(id)).toEqual({ min:5, value:5, max:5, step:0.2 });
    });

    it("receiveMessage() works", function() {
      // Set value
      // getValue() and getState().value should be the same
      receive_message(id, { value:7.5 });
      expect(get_value(id)).toBe(7.5);
      expect(get_state(id).value).toBe(7.5);

      // Setting min, max, step shouldn't affect value
      receive_message(id, { min:-5 , max:20, step:0.25 });
      expect(get_value(id)).toBe(7.5);

      receive_message(id, { min:5, value:5, max:5, step:0.2 });
      expect(get_value(id)).toBe(5);
    });

  });


  // ===========================================================================
  describe("checkboxInputBinding", function() {
    var id = 'in_checkbox';
    var binding_name = 'checkboxInput'; // Name of the input binding in the registry

    // Some convenience objects
    var $obj;
    var input_binding;

    beforeEach(function(){
      var el = $('<input id="' + id +'" type="checkbox"/>').prependTo('body');
      // Wrap the input object in a div so we can select and remove it later
      el.wrap('<div id="input_binding_test">');
      Shiny.bindAll();

      $obj          = select_input_object(id);
      input_binding = get_input_binding_name(binding_name);
    });

    afterEach(function(){
      Shiny.unbindAll();
      $('#input_binding_test').remove();
    });


    // Run tests that are exactly the same for all InputBindings
    common_tests(id, binding_name);


    it("getValue() works", function() {
      expect(input_binding.getValue($obj[0])).toBe(false);
    });

    it("setValue() works", function() {
      input_binding.setValue($obj[0], false);
      expect(input_binding.getValue($obj[0])).toBe(false);

      input_binding.setValue($obj[0], true);
      expect(input_binding.getValue($obj[0])).toBe(true);
    });

    it("getState() works", function() {
      expect(get_state(id)).toEqual({ value:false });

      receive_message(id, { value:true });
      expect(get_state(id)).toEqual({ value:true });
    });

    it("receiveMessage() works", function() {
      // Should use 'value', and ignore 'checked'
      receive_message(id, { value:true, checked:false });
      expect(get_state(id)).toEqual({ value:true });

      receive_message(id, { checked:false });
      expect(get_state(id)).toEqual({ value:true });

    });

  });



  // ===========================================================================
  describe("sliderInputBinding", function() {
    var id = 'in_slider';
    var binding_name = 'sliderInput'; // Name of the input binding in the registry

    // Some convenience objects
    var $obj;
    var input_binding;

    beforeEach(function(){
      var el = $('<input id="' + id + '" type="slider" name="in_slider" value="20" class="jslider" data-from="5" data-to="40" data-step="1" data-skin="plastic" data-round="false" data-locale="us" data-format="#,##0.#####" data-smooth="false"/>').prependTo('body');
      // Wrap the input object in a div so we can select and remove it later
      el.wrap('<div id="input_binding_test">');
      Shiny.bindAll();

      $obj          = select_input_object(id);
      input_binding = get_input_binding_name(binding_name);
    });

    afterEach(function(){
      Shiny.unbindAll();
      $('#input_binding_test').remove();
    });

    // Run tests that are exactly the same for all InputBindings
    common_tests(id, binding_name);

    it("getValue() works", function() {
      expect(input_binding.getValue($obj[0])).toBe(20);
    });

    it("setValue() works", function() {
      input_binding.setValue($obj[0], 5);
      expect(input_binding.getValue($obj[0])).toBe(5);

      // getValue should coerce to number
      input_binding.setValue($obj[0], '6');
      expect(input_binding.getValue($obj[0])).toBe(6);

      // Sould round value to nearest step
      input_binding.setValue($obj[0], '7.8');
      expect(input_binding.getValue($obj[0])).toBe(8);

      // Below min: should report min
      input_binding.setValue($obj[0], 2);
      expect(input_binding.getValue($obj[0])).toBe(5);

      // Above max: should report max
      input_binding.setValue($obj[0], 100);
      expect(input_binding.getValue($obj[0])).toBe(40);
    });

    it("getState() works", function() {
      expect(get_state(id)).toEqual({
        value:20, min:5, max:40, step:1, round:false, 
        format:"#,##0.#####", locale:"us"
      });

      // Are format and locale needed?
      // TODO: deal with animate?

      // receive_message(id, { min:5, value:5, max:5, step:0.2 });
      // expect(get_state(id)).toEqual({ min:5, value:5, max:5, step:0.2 });
    });

    it("receiveMessage() works", function() {
      // Set value
      // getValue() and getState().value should be the same
      receive_message(id, { value:6 });
      expect(get_value(id)).toBe(6);
      expect(get_state(id).value).toBe(6);

      // Sould round value to nearest step
      receive_message(id, { value:7.8 });
      expect(get_value(id)).toBe(8);

      // Setting other values isn't implemented yet
    });

  });


  // ===========================================================================
  describe("selectInputBinding", function() {
    var id = 'in_select';
    var binding_name = 'selectInput'; // Name of the input binding in the registry

    // Some convenience objects
    var $obj;
    var input_binding;

    beforeEach(function(){
      var htmlstring =
        '<select id="' + id + '">\
          <option value="option1" selected="selected">option1 label</option>\
          <option value="option2">option2 label</option>\
        </select>';

      // Wrapper div for the htmlstring
      var el = $('<div id="input_binding_test">').prependTo('body');
      el.html(htmlstring);

      Shiny.bindAll();

      $obj          = select_input_object(id);
      input_binding = get_input_binding_name(binding_name);
    });

    afterEach(function(){
      Shiny.unbindAll();
      $('#input_binding_test').remove();
    });

    // Run tests that are exactly the same for all InputBindings
    common_tests(id, binding_name);

    it("getValue() works", function() {
      expect(input_binding.getValue($obj[0])).toBe('option1');
    });

    it("setValue() works", function() {
      input_binding.setValue($obj[0], 'option2');
      expect(input_binding.getValue($obj[0])).toBe('option2');

      // Setting to nonexistent option should have no effect
      // NOTE: this actually resets it to the first option
      input_binding.setValue($obj[0], 'option999');
      expect(input_binding.getValue($obj[0])).toBe('option1');
    });

    it("getState() works", function() {
      expect(get_state(id)).toEqual({
        value: 'option1',
        options: [
          { value: 'option1', label: 'option1 label', selected: true },
          { value: 'option2', label: 'option2 label', selected: false }
        ]
      });
    });

    it("receiveMessage() works", function() {
      var state_complete = {
        value: 'option4',
        options: [
          { value: 'option3', label: 'option3 label', selected: false },
          { value: 'option4', label: 'option4 label', selected: true }
        ]
      };
      receive_message(id, state_complete);
      expect(get_value(id)).toBe('option4');
      expect(get_state(id)).toEqual(state_complete);


      // Don't provide value, but set selected:true on an option
      var state_novalue = {
        options: [
          { value: 'option5', label: 'option5 label', selected: false },
          { value: 'option6', label: 'option6 label', selected: true }
        ]
      };
      var state_novalue_expected = {
        value: 'option6',
        options: state_novalue.options
      };
      receive_message(id, state_novalue);
      expect(get_value(id)).toBe('option6');
      expect(get_state(id)).toEqual(state_novalue_expected);


      // Provide value, but no selected:true
      var state_noselected = {
        value: 'option7',
        options: [
          { value: 'option7', label: 'option7 label'},
          { value: 'option8', label: 'option8 label'}
        ]
      };
      var state_noselected_expected = {
        value: 'option7',
        options: [
          { value: 'option7', label: 'option7 label', selected: true },
          { value: 'option8', label: 'option8 label', selected: false }
        ]
      };
      receive_message(id, state_noselected);
      expect(get_value(id)).toBe('option7');
      expect(get_state(id)).toEqual(state_noselected_expected);

    });

  });


  // ===========================================================================
  describe("radioInputBinding", function() {
    var id = 'in_radio';
    var binding_name = 'radioInput'; // Name of the input binding in the registry

    // Some convenience objects
    var $obj;
    var input_binding;

    beforeEach(function(){
      var htmlstring =
        '<div id="' + id + '" class="control-group shiny-input-radiogroup">\
          <label class="control-label">Radio buttons:</label>\
          <label class="radio">\
            <input type="radio" name="' + id + '" id="' + id + '1" value="option1" checked="checked"/>\
            <span>option1 label</span>\
          </label>\
          <label class="radio">\
            <input type="radio" name="' + id + '" id="' + id + '2" value="option2"/>\
            <span>option2 label</span>\
          </label>\
        </div>';

      // Wrapper div for the htmlstring
      var el = $('<div id="input_binding_test">').prependTo('body');
      el.html(htmlstring);

      Shiny.bindAll();

      $obj          = select_input_object(id);
      input_binding = get_input_binding_name(binding_name);
    });

    afterEach(function(){
      Shiny.unbindAll();
      $('#input_binding_test').remove();
    });

    // Run tests that are exactly the same for all InputBindings
    common_tests(id, binding_name);

    it("getValue() works", function() {
      expect(input_binding.getValue($obj[0])).toBe('option1');
    });

    it("setValue() works", function() {
      input_binding.setValue($obj[0], 'option2');
      expect(input_binding.getValue($obj[0])).toBe('option2');

      // Setting to nonexistent option should have no effect
      input_binding.setValue($obj[0], 'option100');
      expect(input_binding.getValue($obj[0])).toBe('option2');
    });

    it("getState() works", function() {
      expect(get_state(id)).toEqual({
        value: 'option1',
        options: [
          { value: 'option1', label: 'option1 label', checked: true },
          { value: 'option2', label: 'option2 label', checked: false }
        ]
      });
    });

    it("receiveMessage() works", function() {
      var state_complete = {
        value: 'option4',
        options: [
          { value: 'option3', label: 'option3 label', checked: false },
          { value: 'option4', label: 'option4 label', checked: true }
        ]
      };
      receive_message(id, state_complete);
      expect(get_value(id)).toBe('option4');
      expect(get_state(id)).toEqual(state_complete);


      // Don't provide value, but set checked:true on an option
      var state_novalue = {
        options: [
          { value: 'option5', label: 'option5 label', checked: false },
          { value: 'option6', label: 'option6 label', checked: true }
        ]
      };
      var state_novalue_expected = {
        value: 'option6',
        options: state_novalue.options
      };
      receive_message(id, state_novalue);
      expect(get_value(id)).toBe('option6');
      expect(get_state(id)).toEqual(state_novalue_expected);


      // Provide value, but no checked:true
      var state_nochecked = {
        value: 'option7',
        options: [
          { value: 'option7', label: 'option7 label'},
          { value: 'option8', label: 'option8 label'}
        ]
      };
      var state_nochecked_expected = {
        value: 'option7',
        options: [
          { value: 'option7', label: 'option7 label', checked: true },
          { value: 'option8', label: 'option8 label', checked: false }
        ]
      };
      receive_message(id, state_nochecked);
      expect(get_value(id)).toBe('option7');
      expect(get_state(id)).toEqual(state_nochecked_expected);

    });

  });

});
