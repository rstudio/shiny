/*jshint forin:true, noarg:true, noempty:true, eqeqeq:true, bitwise:true,
    strict:false, undef:true, unused:true, browser:true, jquery:true, maxerr:50,
    curly:false, multistr:true */
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

  var set_value = function(id, value) {
    var $obj = select_input_object(id);
    return get_input_binding_id(id).setValue($obj[0], value);
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

    beforeEach(function(){
      var el = $('<input id="' + id +'" type="text" value="starting value"/>').prependTo('body');
      // Wrap the input object in a div so we can select and remove it later
      el.wrap('<div id="input_binding_test">');
      Shiny.bindAll();
    });

    afterEach(function(){
      Shiny.unbindAll();
      $('#input_binding_test').remove();
    });


    // Run tests that are exactly the same for all InputBindings
    common_tests(id, binding_name);


    it("getValue() works", function() {
      expect(get_value(id)).toEqual('starting value');
    });

    it("setValue() works", function() {
      set_value(id, 'value 2');
      expect(get_value(id)).toEqual('value 2');
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

    beforeEach(function(){
      var el = $('<input id="' + id +'" type="number" value="8" min="4" max="10" step="0.5"/>').prependTo('body');
      // Wrap the input object in a div so we can select and remove it later
      el.wrap('<div id="input_binding_test">');
      Shiny.bindAll();
    });

    afterEach(function(){
      Shiny.unbindAll();
      $('#input_binding_test').remove();
    });

    // Run tests that are exactly the same for all InputBindings
    common_tests(id, binding_name);


    it("getValue() works", function() {
      expect(get_value(id)).toBe(8);
    });

    it("setValue() works", function() {
      set_value(id, 5);
      expect(get_value(id)).toBe(5);

      // getValue should coerce to number
      set_value(id, '6');
      expect(get_value(id)).toBe(6);

      // getValue should return the numeric value when input is scientific notation
      set_value(id, '1e6');
      expect(get_value(id)).toBe(1000000);
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

    beforeEach(function(){
      var el = $('<input id="' + id +'" type="checkbox"/>').prependTo('body');
      // Wrap the input object in a div so we can select and remove it later
      el.wrap('<div id="input_binding_test">');
      Shiny.bindAll();
    });

    afterEach(function(){
      Shiny.unbindAll();
      $('#input_binding_test').remove();
    });


    // Run tests that are exactly the same for all InputBindings
    common_tests(id, binding_name);


    it("getValue() works", function() {
      expect(get_value(id)).toBe(false);
    });

    it("setValue() works", function() {
      set_value(id, false);
      expect(get_value(id)).toBe(false);

      set_value(id, true);
      expect(get_value(id)).toBe(true);
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

      // Empty message has no effect
      receive_message(id, { });
      expect(get_state(id)).toEqual({ value:true });
    });
  });



  // ===========================================================================
  describe("sliderInputBinding", function() {
    var id = 'in_slider';
    var binding_name = 'sliderInput'; // Name of the input binding in the registry

    beforeEach(function(){
      var htmlstring =
        '<input id="' + id + '" type="slider" name="' + id + '" value="20"\
            class="jslider" data-from="5" data-to="40" data-step="1"\
            data-skin="plastic" data-round="false" data-locale="us"\
            data-format="#,##0.#####" data-smooth="false"/>';

      // Wrapper div for the htmlstring
      var el = $('<div id="input_binding_test">').prependTo('body');
      el.html(htmlstring);
      Shiny.bindAll();
    });

    afterEach(function(){
      Shiny.unbindAll();
      $('#input_binding_test').remove();
    });

    // Run tests that are exactly the same for all InputBindings
    common_tests(id, binding_name);

    it("getValue() works", function() {
      expect(get_value(id)).toBe(20);
    });

    it("setValue() works", function() {
      set_value(id, 5);
      expect(get_value(id)).toBe(5);

      // setValue should coerce to number
      set_value(id, '6');
      expect(get_value(id)).toBe(6);

      // Sould round value to nearest step
      set_value(id, '7.8');
      expect(get_value(id)).toBe(8);

      // Below min: should report min
      set_value(id, 2);
      expect(get_value(id)).toBe(5);

      // Above max: should report max
      set_value(id, 100);
      expect(get_value(id)).toBe(40);
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

      // Empty message has no effect
      receive_message(id, { });
      expect(get_value(id)).toBe(8);

      // Setting other values isn't implemented yet
    });
  });


  // ===========================================================================
  describe("sliderInputBinding with range (two values)", function() {
    var id  = 'in_slider';

    beforeEach(function(){
      var htmlstring =
         '<input id="' + id + '" type="slider" name="' + id + '" value="10;30"\
            class="jslider" data-from="5" data-to="40" data-step="1"\
            data-skin="plastic" data-round="false" data-locale="us"\
            data-format="#,##0.#####" data-smooth="false"/>';

      // Wrapper div for the htmlstring
      var el = $('<div id="input_binding_test">').prependTo('body');
      el.html(htmlstring);
      Shiny.bindAll();
    });

    afterEach(function(){
      Shiny.unbindAll();
      $('#input_binding_test').remove();
    });

    it("getValue() works", function() {
      expect(get_value(id)).toEqual([10, 30]);
    });

    it("setValue() works", function() {
      set_value(id, [15, 25]);
      expect(get_value(id)).toEqual([15, 25]);

      // setValue should coerce to number
      set_value(id, ['16', '26']);
      expect(get_value(id)).toEqual([16, 26]);

      // Min and max in wrong order: Behavior not defined, so don't run test
      // set_value(id, [25, 15]);
      // expect(get_value(id)).toEqual([25, 15]);

      // Below min and above max: should go to min and max
      set_value(id, [0, 100]);
      expect(get_value(id)).toEqual([5, 40]);

      // Set single value: only changes lower
      set_value(id, 15);
      expect(get_value(id)).toEqual([15, 40]);

      // Pass null: no effect on value when null
      set_value(id, [null, 25]);
      expect(get_value(id)).toEqual([15, 25]);
      set_value(id, [10, null]);
      expect(get_value(id)).toEqual([10, 25]);
    });

    it("getState() works", function() {
      expect(get_state(id)).toEqual({
        value:[10, 30], min:5, max:40, step:1, round:false,
        format:"#,##0.#####", locale:"us"
      });
    });

    it("receiveMessage() works", function() {
      // Set value
      // getValue() and getState().value should be the same
      receive_message(id, { value:[6, 20] });
      expect(get_value(id)).toEqual([6, 20]);
      expect(get_state(id).value).toEqual([6, 20]);

      // Empty message has no effect
      receive_message(id, { });
      expect(get_state(id).value).toEqual([6, 20]);

      // Pass null: no effect on value when null
      receive_message(id, { value:[null, 25] });
      expect(get_state(id).value).toEqual([6, 25]);
      receive_message(id, { value:[10, null] });
      expect(get_state(id).value).toEqual([10, 25]);

      // Setting other values isn't implemented yet
    });
  });


  // ===========================================================================
  describe("selectInputBinding", function() {
    var id = 'in_select';
    var binding_name = 'selectInput'; // Name of the input binding in the registry

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
    });

    afterEach(function(){
      Shiny.unbindAll();
      $('#input_binding_test').remove();
    });

    // Run tests that are exactly the same for all InputBindings
    common_tests(id, binding_name);

    it("getValue() works", function() {
      expect(get_value(id)).toBe('option1');
    });

    it("setValue() works", function() {
      set_value(id, 'option2');
      expect(get_value(id)).toBe('option2');

      // Setting to nonexistent option should have no effect
      // NOTE: this actually resets it to the first option
      set_value(id, 'option999');
      expect(get_value(id)).toBe('option1');
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

      // Empty message has no effect
      receive_message(id, { });
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
    });

    afterEach(function(){
      Shiny.unbindAll();
      $('#input_binding_test').remove();
    });

    // Run tests that are exactly the same for all InputBindings
    common_tests(id, binding_name);

    it("getValue() works", function() {
      expect(get_value(id)).toBe('option1');
    });

    it("setValue() works", function() {
      set_value(id, 'option2');
      expect(get_value(id)).toBe('option2');

      // Setting to nonexistent option should have no effect
      set_value(id, 'option100');
      expect(get_value(id)).toBe('option2');
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

      // Empty message has no effect
      receive_message(id, { });
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



  // ===========================================================================
  describe("checkboxGroupInputBinding", function() {
    var id = 'in_checkboxgroup';
    var binding_name = 'checkboxGroupInput'; // Name of the input binding in the registry

    beforeEach(function(){
      var htmlstring =
        '<div id="' + id + '" class="control-group shiny-input-checkboxgroup">\
          <label class="control-label">Checkbox group:</label>\
          <label class="checkbox">\
            <input type="checkbox" name="' + id + '" id="' + id + '1" value="option1" checked="checked"/>\
            <span>option1 label</span>\
          </label>\
          <label class="checkbox">\
            <input type="checkbox" name="' + id + '" id="' + id + '2" value="option2"/>\
            <span>option2 label</span>\
          </label>\
        </div>';

      // Wrapper div for the htmlstring
      var el = $('<div id="input_binding_test">').prependTo('body');
      el.html(htmlstring);

      Shiny.bindAll();
    });

    afterEach(function(){
      Shiny.unbindAll();
      $('#input_binding_test').remove();
    });

    // Run tests that are exactly the same for all InputBindings
    common_tests(id, binding_name);

    it("getValue() works", function() {
      // Should return an array of values
      expect(get_value(id)).toEqual(['option1']);
    });

    it("setValue() works", function() {
      // Accept single value
      set_value(id, 'option2');
      expect(get_value(id)).toEqual(['option2']);

      // Accept array of values
      set_value(id, ['option1', 'option2']);
      expect(get_value(id)).toEqual(['option1', 'option2']);

      set_value(id, ['option2']);
      expect(get_value(id)).toEqual(['option2']);

      // Accept empty array of values
      set_value(id, [ ]);
      expect(get_value(id)).toEqual([ ]);

      // Setting to nonexistent option should have no effect
      set_value(id, 'option100');
      expect(get_value(id)).toEqual([ ]);

      set_value(id, ['option100', 'option2']);
      expect(get_value(id)).toEqual(['option2']);
    });

    it("getState() works", function() {
      expect(get_state(id)).toEqual({
        value: ['option1'],
        options: [
          { value: 'option1', label: 'option1 label', checked: true },
          { value: 'option2', label: 'option2 label', checked: false }
        ]
      });
    });

    it("receiveMessage() works", function() {
      var state_complete = {
        value: ['option4'],
        options: [
          { value: 'option3', label: 'option3 label', checked: false },
          { value: 'option4', label: 'option4 label', checked: true }
        ]
      };
      receive_message(id, state_complete);
      expect(get_value(id)).toEqual(['option4']);
      expect(get_state(id)).toEqual(state_complete);

      // Empty message has no effect
      receive_message(id, { });
      expect(get_state(id)).toEqual(state_complete);

      // Don't provide value, but set checked:true on an option
      var state_novalue = {
        options: [
          { value: 'option5', label: 'option5 label', checked: true },
          { value: 'option6', label: 'option6 label', checked: true }
        ]
      };
      var state_novalue_expected = {
        value: ['option5', 'option6'],
        options: state_novalue.options
      };
      receive_message(id, state_novalue);
      expect(get_value(id)).toEqual(['option5', 'option6']);
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
        value: ['option7'],
        options: [
          { value: 'option7', label: 'option7 label', checked: true },
          { value: 'option8', label: 'option8 label', checked: false }
        ]
      };
      receive_message(id, state_nochecked);
      expect(get_value(id)).toEqual(['option7']);
      expect(get_state(id)).toEqual(state_nochecked_expected);
    });
  });

});
