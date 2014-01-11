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

  function padZeros(n, digits) {
    var str = n.toString();
    while (str.length < digits)
      str = "0" + str;
    return str;
  }

  // Get a date string with format yyyy-mm-dd (date is in local time)
  function local_date_string() {
    var date = new Date();

    return date.getFullYear() + '-' +
           padZeros(date.getMonth()+1, 2) + '-' +
           padZeros(date.getDate(), 2);
  }


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
      var htmlstring =
        '<label for="' + id + '">Text input:</label>\
        <input id="' + id + '" type="text" value="starting value"/>';

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
      expect(get_value(id)).toEqual('starting value');
    });

    it("setValue() works", function() {
      set_value(id, 'value 2');
      expect(get_value(id)).toEqual('value 2');
    });

    it("getState() works", function() {
      expect(get_state(id)).toEqual({ label : 'Text input:', value:"starting value" });

      receive_message(id, { value:"foo" });
      expect(get_state(id)).toEqual({ label : 'Text input:', value:"foo" });
    });

    it("receiveMessage() works", function() {
      receive_message(id, { value:"foo" });
      expect(get_value(id)).toBe("foo");

      // Set and check the value again
      receive_message(id, { value:"bar" });
      expect(get_value(id)).toBe("bar");

      // Set label
      receive_message(id, { label: "new label", value:"foo" });
      expect(get_state(id)).toEqual({ label: "new label", value:"foo" });
    });

  });

  // ===========================================================================
  describe("numberInputBinding", function() {
    var id = 'in_number';
    var binding_name = 'numberInput'; // Name of the input binding in the registry

    beforeEach(function(){
      var htmlstring =
        '<label for="' + id + '">Numeric input:</label>\
        <input id="' + id + '" type="number" value="8" min="4" max="10" step="0.5"/>';

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
      expect(get_state(id)).toEqual(
        { label:'Numeric input:', value:8, min:4, max:10, step:0.5 });

      receive_message(id, { min:5, value:5, max:5, step:0.2 });
      expect(get_state(id)).toEqual(
        { label:'Numeric input:', min:5, value:5, max:5, step:0.2 });
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

      // Set label
      receive_message(id, { label:'new label', value:5 });
      expect(get_state(id)).toEqual(
        { label:'new label', min:5, value:5, max:5, step:0.2 });

    });

  });


  // ===========================================================================
  describe("checkboxInputBinding", function() {
    var id = 'in_checkbox';
    var binding_name = 'checkboxInput'; // Name of the input binding in the registry

    beforeEach(function(){
      var htmlstring =
        '<label class="checkbox" for="' + id + '">\
          <input id="' + id + '" type="checkbox"/>\
          <span>Checkbox input:</span>\
        </label>';

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
      expect(get_value(id)).toBe(false);
    });

    it("setValue() works", function() {
      set_value(id, false);
      expect(get_value(id)).toBe(false);

      set_value(id, true);
      expect(get_value(id)).toBe(true);
    });

    it("getState() works", function() {
      expect(get_state(id)).toEqual({ label:'Checkbox input:', value:false });

      receive_message(id, { value:true });
      expect(get_state(id)).toEqual({ label:'Checkbox input:', value:true });
    });

    it("receiveMessage() works", function() {
      // Should use 'value', and ignore 'checked'
      receive_message(id, { value:true, checked:false });
      expect(get_state(id)).toEqual({ label:'Checkbox input:', value:true });

      receive_message(id, { checked:false });
      expect(get_state(id)).toEqual({ label:'Checkbox input:', value:true });

      // Empty message has no effect
      receive_message(id, { });
      expect(get_state(id)).toEqual({ label:'Checkbox input:', value:true });

      // Set label
      receive_message(id, { label:'new label' });
      expect(get_state(id)).toEqual({ label:'new label', value:true });
    });
  });



  // ===========================================================================
  describe("sliderInputBinding", function() {
    var id = 'in_slider';
    var binding_name = 'sliderInput'; // Name of the input binding in the registry

    beforeEach(function(){
      var htmlstring =
        '<div>\
          <label class="control-label" for="' + id + '">Slider input:</label>\
          <input id="' + id + '" type="slider" name="' + id + '" value="20"\
            class="jslider" data-from="5" data-to="40" data-step="1"\
            data-skin="plastic" data-round="false" data-locale="us"\
            data-format="#,##0.#####" data-smooth="false"/>\
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
        label: 'Slider input:',
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
      expect(get_state(id).value).toEqual(6);
      expect(get_state(id).label).toEqual('Slider input:');

      // Sould round value to nearest step
      receive_message(id, { value:7.8 });
      expect(get_state(id).value).toEqual(8);

      // Empty message has no effect
      receive_message(id, { });
      expect(get_state(id).value).toEqual(8);

      // Set label
      receive_message(id, { label:'new label' });
      expect(get_state(id).value).toEqual(8);
      expect(get_state(id).label).toEqual('new label');

      // Setting other values isn't implemented yet
    });
  });


  // ===========================================================================
  describe("sliderInputBinding with range (two values)", function() {
    var id  = 'in_slider';

    beforeEach(function(){
      var htmlstring =
        '<div>\
          <label class="control-label" for="' + id + '">Slider input:</label>\
          <input id="' + id + '" type="slider" name="' + id + '" value="10;30"\
            class="jslider" data-from="5" data-to="40" data-step="1"\
            data-skin="plastic" data-round="false" data-locale="us"\
            data-format="#,##0.#####" data-smooth="false"/>\
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
        label: 'Slider input:',
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
      expect(get_state(id).label).toEqual('Slider input:');

      // Empty message has no effect
      receive_message(id, { });
      expect(get_state(id).value).toEqual([6, 20]);
      expect(get_state(id).label).toEqual('Slider input:');

      // Pass null: no effect on value when null
      receive_message(id, { value:[null, 25] });
      expect(get_state(id).value).toEqual([6, 25]);
      receive_message(id, { value:[10, null] });
      expect(get_state(id).value).toEqual([10, 25]);

      // Set label
      receive_message(id, { label:'new label' });
      expect(get_state(id).value).toEqual([10, 25]);
      expect(get_state(id).label).toEqual('new label');

      // Setting other values isn't implemented yet
    });
  });


  // ===========================================================================
  describe("dateInputBinding", function() {
    var id  = 'in_date';
    var binding_name = 'dateInput';

    beforeEach(function(){
      // Generated by:
      // cat(format(dateInput('id', 'Date input:', value = '2013-04-10')))
      var htmlstring =
        '<div id="' + id + '"\
          class="shiny-date-input">\
          <label class="control-label" for="' + id + '">Date input:</label>\
          <input type="text"\
            name="date"\
            class="input-medium datepicker"\
            data-date-language="en"\
            data-date-weekstart="0"\
            data-date-format="yyyy-mm-dd"\
            data-date-start-view="month"\
            data-initial-date="2013-04-10"/>\
        </div>';

      // Wrapper div for the htmlstring
      var el = $('<div id="input_binding_test">').prependTo('body');
      el.html(htmlstring);

      Shiny.initializeInputs(el);
      Shiny.bindAll();
    });

    afterEach(function(){
      Shiny.unbindAll();
      $('#input_binding_test').remove();
    });

    // Run tests that are exactly the same for all InputBindings
    common_tests(id, binding_name);

    it("getValue() works", function() {
      expect(get_value(id)).toEqual('2013-04-10');
    });

    it("setValue() works", function() {
      set_value(id, '2012-02-29');
      expect(get_value(id)).toEqual('2012-02-29');

      // Invalid date - no effect
      set_value(id, '2012-02-40');
      expect(get_value(id)).toEqual('2012-02-29');

      // Date object
      set_value(id, new Date('2011-10-09'));
      expect(get_value(id)).toEqual('2011-10-09');

      // Invalid Date object -  no effect
      set_value(id, new Date('2000-01-100'));
      expect(get_value(id)).toEqual('2011-10-09');
    });

    it("getState() works", function() {
      expect(get_state(id)).toEqual({
        label: 'Date input:',
        value: '2013-04-10',
        valueString : '2013-04-10',
        min: null,
        max: null,
        language: 'en',
        weekstart: 0,
        format: 'yyyy-mm-dd',
        startview: 'month'
      });
    });

    it("receiveMessage() works", function() {
      // Set value
      // getValue() and getState().value should be the same
      receive_message(id, { value: '2012-02-29' });
      var fullstate =  {
        label: 'Date input:',
        value: '2012-02-29',
        valueString: '2012-02-29',
        min: null,
        max: null,
        language: 'en',
        weekstart: 0,
        format: 'yyyy-mm-dd',
        startview: 'month'
      };
      expect(get_value(id)).toEqual('2012-02-29');
      expect(get_state(id)).toEqual(fullstate);

      // Empty message has no effect
      receive_message(id, { });
      expect(get_state(id)).toEqual(fullstate);

      // Set label
      receive_message(id, { label:'new label' });
      expect(get_state(id)).toEqual(
        $.extend(fullstate, { label:'new label' })
      );

      // Set min and max
      receive_message(id, { min:'2011-03-02', max:'2012-03-02' });
      expect(get_state(id)).toEqual(
        $.extend(fullstate, { min:'2011-03-02', max:'2012-03-02' })
      );

      // Unset min and max
      receive_message(id, { min:null, max:null });
      expect(get_state(id)).toEqual(
        $.extend(fullstate, { min:null, max:null })
      );

      // Setting format isn't implemented
    });
  });


  // ===========================================================================
  describe("dateInputBinding with mm/dd/yy format, min, max, and startview", function() {
    var id  = 'in_date';

    beforeEach(function(){
      // Generated by:
      // cat(format(dateInput('id', 'Date input:',
      //   value = '2013-04-10', min = '2012-02-01', max = '2012-05-04',
      //   format = 'mm/dd/yy', startview = 'decade', language = 'de', weekstart = 1)))
      var htmlstring =
        '<div id="' + id + '"\
          class="shiny-date-input">\
          <label class="control-label" for="' + id + '">Date input:</label>\
          <input type="text"\
            class="input-medium datepicker"\
            data-date-language="de"\
            data-date-weekstart="1"\
            data-date-format="mm/dd/yy"\
            data-date-start-view="decade"\
            data-min-date="2012-02-01"\
            data-max-date="2013-05-04"\
            data-initial-date="2013-04-10"/>\
        </div>';

      // Wrapper div for the htmlstring
      var el = $('<div id="input_binding_test">').prependTo('body');
      el.html(htmlstring);

      Shiny.initializeInputs(el);
      Shiny.bindAll();
    });

    afterEach(function(){
      Shiny.unbindAll();
      $('#input_binding_test').remove();
    });

    it("getValue() works", function() {
      expect(get_value(id)).toEqual('2013-04-10');
    });

    it("setValue() works", function() {
      set_value(id, '2012-02-29');
      expect(get_value(id)).toEqual('2012-02-29');

      // Invalid date - no effect
      set_value(id, '2012-02-40');
      expect(get_value(id)).toEqual('2012-02-29');

      // Date object
      set_value(id, new Date('2011-10-09'));
      expect(get_value(id)).toEqual('2011-10-09');

      // Invalid Date object -  no effect
      set_value(id, new Date('2000-01-100'));
      expect(get_value(id)).toEqual('2011-10-09');
    });

    it("getState() works", function() {
      expect(get_state(id)).toEqual({
        label: 'Date input:',
        value: '2013-04-10',
        valueString : '04/10/13',
        min: "2012-02-01",
        max: "2013-05-04",
        language: 'de',
        weekstart: 1,
        format: 'mm/dd/yy',
        startview: 'decade'
      });
    });

    it("receiveMessage() works", function() {
      // Set value
      // getValue() and getState().value should be the same
      receive_message(id, { value: '2012-02-29' });
      var fullstate =  {
        label: 'Date input:',
        value: '2012-02-29',
        valueString: '02/29/12',
        min: "2012-02-01",
        max: "2013-05-04",
        language: 'de',
        weekstart: 1,
        format: 'mm/dd/yy',
        startview: 'decade'
      };
      expect(get_value(id)).toEqual('2012-02-29');
      expect(get_state(id)).toEqual(fullstate);

      // Empty message has no effect
      receive_message(id, { });
      expect(get_state(id)).toEqual(fullstate);

      // Set label
      receive_message(id, { label:'new label' });
      expect(get_state(id)).toEqual(
        $.extend(fullstate, { label:'new label' })
      );

      // Set min and max
      // receive_message(id, { min:'2011-02-01', max:'2015-03-02' });
      // debugger;
      // expect(get_state(id)).toEqual(
      //   $.extend(fullstate, { min:'2011-02-01', max:'2015-03-02' })
      // );

      // Setting format isn't implemented
    });
  });


  // ===========================================================================
  describe("dateInputBinding with empty starting value", function() {
    var id  = 'in_date';

    beforeEach(function(){
      // Generated by (with some settings removed): 
      // cat(format(dateInput('id', 'Date input:')))
      var htmlstring =
        '<div id="' + id + '"\
          class="shiny-date-input">\
          <label class="control-label" for="' + id + '">Date input:</label>\
          <input type="text"\
            class="input-medium datepicker"/>\
        </div>';

      // Wrapper div for the htmlstring
      var el = $('<div id="input_binding_test">').prependTo('body');
      el.html(htmlstring);

      Shiny.initializeInputs(el);
      Shiny.bindAll();
    });

    afterEach(function(){
      Shiny.unbindAll();
      $('#input_binding_test').remove();
    });

    it("default value is local date", function() {
      expect(get_value(id)).toEqual(local_date_string());
    });
  });


  // ===========================================================================
  describe("dateRangeInputBinding", function() {
    var id  = 'in_daterange';
    var binding_name = 'dateRangeInput';

    beforeEach(function(){
      // Generated by:
      // cat(format(dateRangeInput('id', 'Date range input:',
      //   start = '2012-02-09', end = '2013-03-01')))
      var htmlstring =
        '<div id="' + id + '" class="shiny-date-range-input input-daterange">\
          <label class="control-label" for="' + id + '">Date range input:</label>\
          <input\
            class="input-small"\
            type="text"\
            data-date-language="en"\
            data-date-weekstart="0"\
            data-date-format="yyyy-mm-dd"\
            data-date-start-view="month"\
            data-initial-date="2012-02-29"/>\
           to \
          <input\
            class="input-small"\
            type="text"\
            data-date-language="en"\
            data-date-weekstart="0"\
            data-date-format="yyyy-mm-dd"\
            data-date-start-view="month"\
            data-initial-date="2013-03-01"/>\
        </div>';

      // Wrapper div for the htmlstring
      var el = $('<div id="input_binding_test">').prependTo('body');
      el.html(htmlstring);

      Shiny.initializeInputs(el);
      Shiny.bindAll();
    });

    afterEach(function(){
      Shiny.unbindAll();
      $('#input_binding_test').remove();
    });

    // Run tests that are exactly the same for all InputBindings
    common_tests(id, binding_name);

    it("getValue() works", function() {
      expect(get_value(id)).toEqual(['2012-02-29', '2013-03-01']);
    });

    it("setValue() works", function() {
      set_value(id, ['2001-01-01', '2014-02-28']);
      expect(get_value(id)).toEqual(['2001-01-01', '2014-02-28']);

      // Single value only changes the min
      set_value(id, ['2002-02-02']);
      expect(get_value(id)).toEqual(['2002-02-02', '2014-02-28']);

      // Empty first value: only changes the max
      set_value(id, [undefined, '2010-03-03']);
      expect(get_value(id)).toEqual(['2002-02-02', '2010-03-03']);

      // Non-array value has no effect
      set_value(id, '2003-03-03');
      expect(get_value(id)).toEqual(['2002-02-02', '2010-03-03']);

      // Null values have no effect
      set_value(id, [null, null]);
      expect(get_value(id)).toEqual(['2002-02-02', '2010-03-03']);

      // Invalid date has no effect (the other, valid date works)
      set_value(id, ['2012-02-40', '2012-04-20']);
      expect(get_value(id)).toEqual(['2002-02-02', '2012-04-20']);

      // Reversed order - works?
      set_value(id, ['2002-02-02', '2001-01-01']);
      expect(get_value(id)).toEqual(['2002-02-02', '2001-01-01']);
    });

    it("getState() works", function() {
      expect(get_state(id)).toEqual({
        label: 'Date range input:',
        min: null,
        max: null,
        value: ['2012-02-29', '2013-03-01'],
        valueString: ['2012-02-29', '2013-03-01'],
        format: 'yyyy-mm-dd',
        weekstart: 0,
        language: 'en',
        startview: 'month'
      });
    });

    it("receiveMessage() works", function() {
      var expected_state = {
        label: 'Date range input:',
        min: null,
        max: null,
        value: ['2001-01-01', '2002-02-02'],
        valueString: ['2001-01-01', '2002-02-02'],
        format: 'yyyy-mm-dd',
        weekstart: 0,
        language: 'en',
        startview: 'month'
      };

      // Set value
      // getValue() and getState().value should be the same
      receive_message(id, { value: ['2001-01-01', '2002-02-02'] });
      expect(get_value(id)).toEqual(['2001-01-01', '2002-02-02']);
      expect(get_state(id)).toEqual(expected_state);

      // Empty message has no effect
      receive_message(id, { });
      expect(get_state(id)).toEqual(expected_state);

      // Set label
      receive_message(id, { label:'new label' });
      expect(get_state(id)).toEqual(
        $.extend(expected_state, { label:'new label' })
      );

      // Set format, separator, minDate, maxDate
      receive_message(id, {
        min: '2000-01-01',
        max: '2020-12-31'
      });
      expect(get_state(id)).toEqual(
        $.extend(expected_state, {
          min: '2000-01-01',
          max: '2020-12-31'
        })
      );

    });
  });


  // ===========================================================================
  describe("dateRangeInputBinding with mm/dd/yy format, min, max, and startview", function() {
    var id  = 'in_daterange';

    beforeEach(function(){
      // Generated by:
      // cat(format(dateRangeInput('id', 'Date range input:',
      //   start = '2012-02-09', end = '2013-03-01',
      //   min   = '2012-02-01', max = '2013-05-04',
      //   format = 'mm/dd/yy', weekstart = 2, startview = 'decade')))
      var htmlstring =
        '<div id="' + id + '" class="shiny-date-range-input input-daterange">\
          <label class="control-label" for="' + id + '">Date range input:</label>\
          <input\
            class="input-small"\
            type="text"\
            data-date-language="en"\
            data-date-weekstart="2"\
            data-date-format="mm/dd/yy"\
            data-date-start-view="decade"\
            data-min-date="2012-02-01"\
            data-max-date="2013-05-04"\
            data-initial-date="2012-02-29"/>\
           to \
          <input\
            class="input-small"\
            type="text"\
            data-date-language="en"\
            data-date-weekstart="2"\
            data-date-format="mm/dd/yy"\
            data-date-start-view="decade"\
            data-min-date="2012-02-01"\
            data-max-date="2013-05-04"\
            data-initial-date="2013-03-01"/>\
        </div>';

      // Wrapper div for the htmlstring
      var el = $('<div id="input_binding_test">').prependTo('body');
      el.html(htmlstring);

      Shiny.initializeInputs(el);
      Shiny.bindAll();
    });

    afterEach(function(){
      Shiny.unbindAll();
      $('#input_binding_test').remove();
    });

    it("getValue() works", function() {
      expect(get_value(id)).toEqual(['2012-02-29', '2013-03-01']);
    });

    it("setValue() works", function() {
      set_value(id, ['2001-01-01', '2014-02-28']);
      expect(get_value(id)).toEqual(['2001-01-01', '2014-02-28']);

      // Single value only changes the min
      set_value(id, ['2002-02-02']);
      expect(get_value(id)).toEqual(['2002-02-02', '2014-02-28']);

      // Empty first value: only changes the max
      set_value(id, [undefined, '2010-03-03']);
      expect(get_value(id)).toEqual(['2002-02-02', '2010-03-03']);

      // Non-array value has no effect
      set_value(id, '2003-03-03');
      expect(get_value(id)).toEqual(['2002-02-02', '2010-03-03']);

      // Null values have no effect
      set_value(id, [null, null]);
      expect(get_value(id)).toEqual(['2002-02-02', '2010-03-03']);

      // Invalid date has no effect (the other, valid date works)
      set_value(id, ['2012-02-40', '2012-04-20']);
      expect(get_value(id)).toEqual(['2002-02-02', '2012-04-20']);

      // Reversed order - works?
      set_value(id, ['2002-02-02', '2001-01-01']);
      expect(get_value(id)).toEqual(['2002-02-02', '2001-01-01']);
    });

    it("getState() works", function() {
      expect(get_state(id)).toEqual({
        label: 'Date range input:',
        min: "2012-02-01",
        max: "2013-05-04",
        value: ['2012-02-29', '2013-03-01'],
        valueString: ['02/29/12', '03/01/13'],
        format: 'mm/dd/yy',
        weekstart: 2,
        language: 'en',
        startview: 'decade'
      });
    });

    it("receiveMessage() works", function() {
      var expected_state = {
        label: 'Date range input:',
        min: "2012-02-01",
        max: "2013-05-04",
        value: ['2012-04-02', '2012-04-03'],
        valueString: ['04/02/12', '04/03/12'],
        format: 'mm/dd/yy',
        weekstart: 2,
        language: 'en',
        startview: 'decade'
      };

      // Set value
      // getValue() and getState().value should be the same
      receive_message(id, { value: ['2012-04-02', '2012-04-03'] });
      expect(get_value(id)).toEqual(['2012-04-02', '2012-04-03']);
      expect(get_state(id)).toEqual(expected_state);

      // Empty message has no effect
      receive_message(id, { });
      expect(get_state(id)).toEqual(expected_state);

      // Set label
      receive_message(id, { label:'new label' });
      expect(get_state(id)).toEqual(
        $.extend(expected_state, { label:'new label' })
      );

      // Set format, separator, minDate, maxDate
      receive_message(id, {
        min: '2000-01-01',
        max: '2020-12-31'
      });
      expect(get_state(id)).toEqual(
        $.extend(expected_state, {
          min: '2000-01-01',
          max: '2020-12-31'
        })
      );

    });
  });


  // ===========================================================================
  describe("dateRangeInputBinding with empty start/end values", function() {
    var id  = 'in_daterange';

    beforeEach(function(){
      // Generated by (with some bits removed):
      // cat(format(dateRangeInput('id', 'Date range input:')))
      var htmlstring =
        '<div id="' + id + '" class="shiny-date-range-input input-daterange">\
          <label class="control-label" for="' + id + '">Date range input:</label>\
          <input\
            class="input-small"\
            type="text"/>\
           to \
          <input\
            class="input-small"\
            type="text"/>\
        </div>';

      // Wrapper div for the htmlstring
      var el = $('<div id="input_binding_test">').prependTo('body');
      el.html(htmlstring);

      Shiny.initializeInputs(el);
      Shiny.bindAll();
    });

    afterEach(function(){
      Shiny.unbindAll();
      $('#input_binding_test').remove();
    });

    it("getValue() works", function() {
      var date = local_date_string()
      expect(get_value(id)).toEqual([date, date]);
    });
  });


  // ===========================================================================
  describe("selectInputBinding", function() {
    var id = 'in_select';
    var binding_name = 'selectInput'; // Name of the input binding in the registry

    beforeEach(function(){
      var htmlstring =
        '<label class="control-label" for="' + id + '">Select input:</label>\
        <select id="' + id + '">\
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

      // Setting to nonexistent option turns the value to null
      set_value(id, 'option999');
      expect(get_value(id)).toBe(null);
    });

    it("getState() works", function() {
      expect(get_state(id)).toEqual({
        label: 'Select input:',
        value: 'option1',
        options: [
          { value: 'option1', label: 'option1 label' },
          { value: 'option2', label: 'option2 label' }
        ]
      });
    });

    it("receiveMessage() works", function() {
      var state_complete = {
        label: 'Select input:',
        value: 'option4',
        options: [
          { value: 'option3', label: 'option3 label' },
          { value: 'option4', label: 'option4 label' }
        ]
      };
      receive_message(id, state_complete);
      expect(get_value(id)).toBe('option4');
      expect(get_state(id)).toEqual(state_complete);

      // Empty message has no effect
      receive_message(id, { });
      expect(get_state(id)).toEqual(state_complete);

      // Don't provide value, and the default should be the first option
      var state_novalue = {
        options: [
          { value: 'option5', label: 'option5 label' },
          { value: 'option6', label: 'option6 label' }
        ]
      };
      var state_novalue_expected = {
        label: 'Select input:',
        value: 'option5',
        options: state_novalue.options
      };
      receive_message(id, state_novalue);
      expect(get_value(id)).toBe('option5');
      expect(get_state(id)).toEqual(state_novalue_expected);

      // Only update value
      var state_value = {
        value: 'option6'
      };
      var state_value_expected = {
        label: 'Select input:',
        value: 'option6',
        options: [
          { value: 'option5', label: 'option5 label' },
          { value: 'option6', label: 'option6 label' }
        ]
      };
      receive_message(id, state_value);
      expect(get_value(id)).toEqual('option6');
      expect(get_state(id)).toEqual(state_value_expected);

      // Set label
      var state_newlabel_complete = {
        label: 'new label',
        value: 'option9',
        options: [
          { value: 'option9',  label: 'option9 label' },
          { value: 'option10', label: 'option10 label' }
        ]
      };
      receive_message(id, state_newlabel_complete);
      expect(get_state(id)).toEqual(state_newlabel_complete);
    });
  });


  // ===========================================================================
  describe("radioInputBinding", function() {
    var id = 'in_radio';
    var binding_name = 'radioInput'; // Name of the input binding in the registry

    beforeEach(function(){
      var htmlstring =
        '<div id="' + id + '" class="control-group shiny-input-radiogroup">\
          <label class="control-label" for="' + id + '">Radio buttons:</label>\
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
        label: 'Radio buttons:',
        value: 'option1',
        options: [
          { value: 'option1', label: 'option1 label' },
          { value: 'option2', label: 'option2 label' }
        ]
      });
    });

    it("receiveMessage() works", function() {
      var state_complete = {
        label: 'Radio buttons:',
        value: 'option4',
        options: [
          { value: 'option3', label: 'option3 label' },
          { value: 'option4', label: 'option4 label' }
        ]
      };
      receive_message(id, state_complete);
      expect(get_value(id)).toBe('option4');
      expect(get_state(id)).toEqual(state_complete);

      // Empty message has no effect
      receive_message(id, { });
      expect(get_state(id)).toEqual(state_complete);

      // Don't provide value, and the value will be undefined
      // since no option is checked
      var state_novalue = {
        options: [
          { value: 'option5', label: 'option5 label' },
          { value: 'option6', label: 'option6 label' }
        ]
      };
      var state_novalue_expected = {
        label: 'Radio buttons:',
        value: undefined,
        options: state_novalue.options
      };
      receive_message(id, state_novalue);
      expect(get_value(id)).toBe(undefined);
      expect(get_state(id)).toEqual(state_novalue_expected);

      // Only update value
      var state_value = {
        value: 'option6'
      };
      var state_value_expected = {
        label: 'Radio buttons:',
        value: 'option6',
        options: [
          { value: 'option5', label: 'option5 label' },
          { value: 'option6', label: 'option6 label' }
        ]
      };
      receive_message(id, state_value);
      expect(get_value(id)).toEqual('option6');
      expect(get_state(id)).toEqual(state_value_expected);

      // Set label
      var state_newlabel_complete = {
        label: 'new label',
        value: 'option9',
        options: [
          { value: 'option9',  label: 'option9 label' },
          { value: 'option10', label: 'option10 label' }
        ]
      };
      receive_message(id, state_newlabel_complete);
      expect(get_state(id)).toEqual(state_newlabel_complete);
    });
  });



  // ===========================================================================
  describe("checkboxGroupInputBinding", function() {
    var id = 'in_checkboxgroup';
    var binding_name = 'checkboxGroupInput'; // Name of the input binding in the registry

    beforeEach(function(){
      // Generated by:
      // cat(format(checkboxGroupInput('in_checkboxgroup', 'Checkbox group:',
      //   choices = c('option1 label' = 'option1', 'option2 label' = 'option2'))))
      var htmlstring =
        '<div id="' + id + '" class="control-group shiny-input-checkboxgroup">\
          <label class="control-label" for="in_checkboxgroup">Checkbox group:</label>\
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
        label: 'Checkbox group:',
        value: ['option1'],
        options: [
          { value: 'option1', label: 'option1 label' },
          { value: 'option2', label: 'option2 label' }
        ]
      });
    });

    it("receiveMessage() works", function() {
      var state_complete = {
        label: 'Checkbox group:',
        value: ['option4'],
        options: [
          { value: 'option3', label: 'option3 label' },
          { value: 'option4', label: 'option4 label' }
        ]
      };
      receive_message(id, state_complete);
      expect(get_value(id)).toEqual(['option4']);
      expect(get_state(id)).toEqual(state_complete);

      // Empty message has no effect
      receive_message(id, { });
      expect(get_state(id)).toEqual(state_complete);

      // Don't provide value
      var state_novalue = {
        options: [
          { value: 'option5', label: 'option5 label' },
          { value: 'option6', label: 'option6 label' }
        ]
      };
      var state_novalue_expected = {
        label: 'Checkbox group:',
        value: [ ],
        options: state_novalue.options
      };
      receive_message(id, state_novalue);
      expect(get_value(id)).toEqual([ ]);
      expect(get_state(id)).toEqual(state_novalue_expected);


      // Only update value
      var state_value = {
        value: 'option6'
      };
      var state_value_expected = {
        label: 'Checkbox group:',
        value: ['option6'],
        options: [
          { value: 'option5', label: 'option5 label' },
          { value: 'option6', label: 'option6 label' }
        ]
      };
      receive_message(id, state_value);
      expect(get_value(id)).toEqual(['option6']);
      expect(get_state(id)).toEqual(state_value_expected);

      // Set label
      var state_newlabel_complete = {
        label: 'Checkbox group new label:',
        value: ['option4'],
        options: [
          { value: 'option3', label: 'option3 label' },
          { value: 'option4', label: 'option4 label' }
        ]
      };
      receive_message(id, state_newlabel_complete);
      expect(get_state(id)).toEqual(state_newlabel_complete);

    });
  });


  // ===========================================================================
  describe("actionButtonInputBinding", function() {
    var id = 'in_actionbutton';
    var binding_name = 'actionButtonInput'; // Name of the input binding in the registry

    beforeEach(function(){
      var htmlstring =
        '<button id="' + id + '" type="button" class="btn action-button">button label</button>';

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
      expect(get_value(id)).toEqual(0);
    });

    it("click event increments counter", function() {
      expect(get_value(id)).toEqual(0);

      select_input_object(id).trigger('click');
      expect(get_value(id)).toEqual(1);

      select_input_object(id).trigger('click');
      expect(get_value(id)).toEqual(2);
    });

    it("setValue() works", function() {
      set_value(id, 2000);
      expect(get_value(id)).toEqual(2000);
    });

    it("getState() works", function() {
      expect(get_state(id)).toEqual({ value: 0 });
    });

    it("receiveMessage() has no effect", function() {
      receive_message(id, { value: 2000 });
      expect(get_state(id)).toEqual({ value: 0 });

      receive_message(id, { });
      expect(get_state(id)).toEqual({ value: 0 });
    });
  });


  // ===========================================================================
  describe("bootstrapTabInputBinding", function() {
    var id = 'in_tabset';
    var binding_name = 'bootstrapTabInput'; // Name of the input binding in the registry

    beforeEach(function(){
      var htmlstring =
        '<div class="tabbable">\
          <ul class="nav shiny-tab-input" id="' + id + '">\
            <li class="active">\
              <a href="#tab-455-1" data-toggle="tab">panel1</a>\
            </li>\
            <li>\
              <a href="#tab-455-2" data-toggle="tab">panel2</a>\
            </li>\
          </ul>\
          <div class="tab-content">\
            <div class="tab-pane active" title="panel1" id="tab-455-1">\
              <p>This is the first panel.</p>\
            </div>\
            <div class="tab-pane" title="panel2" id="tab-455-2">\
              <p>This is the second panel.</p>\
            </div>\
          </div>\
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
      expect(get_value(id)).toEqual('panel1');
    });

    it("setValue() works", function() {
      set_value(id, 'panel2');
      expect(get_value(id)).toEqual('panel2');

      set_value(id, 'panel1');
      expect(get_value(id)).toEqual('panel1');
    });

    it("getState() works", function() {
      expect(get_state(id)).toEqual({ value: 'panel1' });
    });

    it("receiveMessage() works", function() {
      receive_message(id, { value: 'panel2' });
      expect(get_value(id)).toEqual('panel2');
      expect(get_state(id)).toEqual({ value: 'panel2' });

      // Empty message has no effect
      receive_message(id, { });
      expect(get_state(id)).toEqual({ value: 'panel2' });
    });
  });

});
