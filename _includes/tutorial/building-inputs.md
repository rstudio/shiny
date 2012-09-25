## Building Inputs

Shiny comes equipped with a variety of useful input components, but as you build more ambitious applications, you may find yourself needing input widgets that we don't include. Fortunately, Shiny is designed to let you create your own custom input components. If you can implement it using HTML, CSS, and JavaScript, you can use it as a Shiny input!

(If you're only familiar with R and not with HTML/CSS/JavaScript, then you will likely find it tough to create all but the simplest custom input components on your own. However, other people can&nbsp;&ndash; and hopefully will&nbsp;&ndash; bundle up their custom Shiny input components as R packages and make them available to the rest of the community.)

### Design the Component

The first steps in creating a custom input component is no different than in any other form of web development. You write HTML markup that lays out the component, CSS rules to style it, and use JavaScript (mostly event handlers) to give it behavior, if necessary.

Shiny input components should try to adhere to the following principles, if possible:

* **Designed to be used from HTML and R:** Shiny user interfaces can either be written using R code (that generates HTML), or by writing the HTML directly. A well-designed Shiny input component will take both styles into account: offer an R function for creating the component, but also have thoughtfully designed and documented HTML markup.
* **Configurable using HTML attributes:** Avoid requiring the user to make JavaScript calls to configure the component. Instead, it's better to use HTML attributes. In your component's JavaScript logic, you can [easily access these values using jQuery](http://api.jquery.com/data/#data-html5) (or simply by reading the DOM attribute directly).

When used in a Shiny application, your component's HTML markup will be repeated once for each instance of the component on the page, but the CSS and JavaScript will generally only need to appear once, most likely in the `<head>`. For R-based interface code, you can use the functions `singleton` and `tags$head` together to ensure these tags appear once and only once, in the head. (See the full example below.)

### Write an Input Binding

Each custom input component also needs an *input binding*, an object you create that tells Shiny how to identify instances of your component and how to interact with them. (Note that each *instance* of the input component doesn't need its own input binding object; rather, all instances of a particular type of input component share a single input binding object.)

An input binding object needs to have the following methods:

<dl>
  <dt>
    <code>find(scope)</code>
  </dt>
  <dd>
    <p>
      Given an HTML document or element (<code>scope</code>), find any descendant elements that are an instance of your component and return them as an array (or array-like object). The other input binding methods all take an <code>el</code> argument; that value will always be an element that was returned from <code>find</code>.
    </p>

    <p>
      A very common implementation is to use jQuery's <code>find</code> method to identify elements with a specific class, for example:
    </p>
<pre>exampleInputBinding.find = function(scope) {
  return $(scope).find(".exampleComponentClass");
};</pre>
  </dd>
  <dt>
    <code>getId(el)</code>
  </dt>
  <dd>
    Return the Shiny input ID for the element <code>el</code>, or <code>null</code> if the element doesn't have an ID and should therefore be ignored. The default implementation in <code>Shiny.InputBinding</code> reads the <code>data-input-id</code> attribute and falls back to the element's <code>id</code> if not present.
  </dd>
  <dt>
    <code>getValue(el)</code>
  </dt>
  <dd>
    Return the Shiny value for the element <code>el</code>. This can be any JSON-compatible value.
  </dd>
  <dt>
    <code>setValue(el, value)</code>
  </dt>
  <dd>
    Set the element to the specified value. (This is not currently used, but in the future we anticipate adding features that will require the server to push input values to the client.)
  </dd>
  <dt>
    <code>subscribe(el, callback)</code>
  </dt>
  <dd>
    <p>
      Subscribe to DOM events on the element <code>el</code> that indicate the value has changed. When the DOM events fire, call <code>callback</code> (a function) which will tell Shiny to retrieve the value.
    </p>
    <p>
      We recommend using jQuery's event namespacing feature when subscribing, as unsubscribing becomes very easy (see <code>unsubscribe</code>, below). In this example, <code>exampleComponentName</code> is used as a namespace:
    </p>
<pre>exampleInputBinding.subscribe = function(el, callback) {
  $(el).on("keyup.exampleComponentName", function(event) {
    callback(true);
  });
  $(el).on("change.exampleComponentName", function(event) {
    callback();
  });
};</pre>
    <p>
      Later on, we can unsubscribe <code>".exampleComponentName"</code> which will remove all of our handlers without touching anyone else's.
    </p>
    <p>
      The <code>callback</code> function optionally takes an argument: a boolean value that indicates whether the component's rate policy should apply (<code>true</code> means the rate policy should apply). See <code>getRatePolicy</code> below for more details.
    </p>
  </dd>
  <dt>
    <code>unsubscribe(el)</code>
  </dt>
  <dd>
    <p>Unsubscribe DOM event listeners that were bound in <code>subscribe</code>.</p>
    <p>Example:</p>
<pre><code class="javascript">exampleInputBinding.unsubscribe = function(el) {
  $(el).off(".exampleComponentName");
};</code></pre>
  </dd>
  <dt>
    <code>getRatePolicy()</code>
  </dt>
  <dd>
    <p>Return an object that describes the rate policy of this component (or <code>null</code> for default).</p>
    <p>
      Rate policies are helpful for slowing down the rate at which input events get sent to the server. For example, as the user drags a slider from value A to value B, dozens of change events may occur. It would be wasteful to send all of those events to the server, where each event would potentially cause expensive computations to occur.
    </p>
    <p>
      A rate policy slows down the rate of events using one of two algorithms (so far). <strong>Throttling</strong> means no more than one event will be sent per X milliseconds. <strong>Debouncing</strong> means all of the events will be ignored until no events have been received for X milliseconds, at which time the most recent event will be sent. <a href="http://benalman.com/projects/jquery-throttle-debounce-plugin/">This blog post</a> goes into more detail about the difference between throttle and debounce.
    </p>
    <p>
      A rate policy object has two members:
    </p>
    <ul>
      <li>
        <code>policy</code> - Valid values are the strings <code>"direct"</code>, <code>"debounce"</code>, and <code>"throttle"</code>. <code>"direct"</code> means that all events are sent immediately.
      </li>
      <li>
        <code>delay</code> - Number indicating the number of milliseconds that should be used when debouncing or throttling. Has no effect if the policy is <code>direct</code>.
      </li>
    </ul>
    <p>
      Rate policies are only applied when the <code>callback</code> function in <code>subscribe</code> is called with <code>true</code> as the first parameter. It's important that input components be able to control which events are rate-limited and which are not, as different events may have different expectations to the user. For example, for a textbox, it would make sense to rate-limit events while the user is typing, but if the user hits Enter or focus leaves the textbox, then the input should always be sent immediately.
    </p>
  </dd>
</dl>

### Register Input Binding

Once you've created an input binding object, you need to tell Shiny to use it:
<pre><code class="javascript">Shiny.inputBindings.register(exampleInputBinding, "yourname.exampleInputBinding");</code></pre>

The second argument is a name the user can use to change the priority of the binding. On the off chance that the user has multiple bindings that all want to claim the same HTML element as their own, this call can be used to control the priority of the bindings:

<pre><code class="javascript">Shiny.inputBindings.setPriority("yourname.exampleInputBinding", 10);</code></pre>

Higher numbers indicate a higher priority; the default priority is 0. All of Shiny's built-in input component bindings default to a priority of 0.

If two bindings have the same priority value, then the more recently registered binding has the higher priority.

### Example

For this example, we'll create a button that displays a number, whose value increases by one each time the button is clicked. Here's what the end result will look like:

<p>
  <button class="increment btn" type="button">0</button>​​​​​​​​​​​​​​​​​​​​​​​
</p>
<script>
$(document).on("click", "button.increment", function(evt) {
  // evt.target is the button that was clicked
  var el = $(evt.target);
  // Set the button's text to its current value plus 1
  el.text(parseInt(el.text()) + 1);
  // Raise an event to signal that the value changed
  el.trigger("change");
});
</script>

To start, let's design the HTML markup for this component:

<pre><code class="html">&lt;button id="inputId" class="increment btn" type="button">0&lt;/button>​​​​​​​​​​​​​​​​​​​​​​​</code></pre>

The CSS class `increment` is what will differentiate our buttons from any other kind of buttons. (The `btn` class is just to make the button look decent in [Twitter Bootstrap](http://twitter.github.com/bootstrap).)

Now we'll write the JavaScript that drives the button's basic behavior:

<pre><code class="javascript">$(document).on("click", "button.increment", function(evt) {

  // evt.target is the button that was clicked
  var el = $(evt.target);

  // Set the button's text to its current value plus 1
  el.text(parseInt(el.text()) + 1);

  // Raise an event to signal that the value changed
  el.trigger("change");
});</code></pre>

This code uses [jQuery's delegated events feature](http://api.jquery.com/on/) to bind all increment buttons at once.

Now we'll create the Shiny binding object for our component, and register it:

<pre><code class="javascript">var incrementBinding = new Shiny.InputBinding();
$.extend(incrementBinding, {
  find: function(scope) {
    return $(scope).find(".increment");
  },
  getValue: function(el) {
    return parseInt($(el).text());
  },
  setValue: function(el, value) {
    $(el).text(value);
  },
  subscribe: function(el, callback) {
    $(el).on("change.incrementBinding", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".incrementBinding");
  }
});

Shiny.inputBindings.register(incrementBinding);</code></pre>

Both the behavioral JavaScript code and the Shiny binding code should generally be run when the page loads. (It's important that they run before Shiny initialization, which occurs after all the document ready event handlers are executed.)

The cleanest way to do this is to put both chunks of JavaScript into a file. In this case, we'll use the path `./www/js/increment.js`, which we can then access as `http://localhost:8100/js/increment.js`.

If you're using an `index.html` style user interface, you'll just need to add this line to your `<head>` (make sure it comes after the script tag that loads `shiny.js`):

<pre><code class="html">&lt;script src="js/increment.js"&gt;&lt;/script&gt;</code></pre>

On the other hand, if you're using `ui.R`, then you can define this function before the call to `shinyUI`:

<pre><code class="r">incrementButton &lt;- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "js/increment.js"))),
    tags$button(id = inputId,
                class = "increment btn",
                type = "button",
                as.character(value))
  )
}</code></pre>

Then in your `shinyUI` page definition you can call `incrementButton` wherever you want an increment button rendered. Notice the line that begins with `singleton` will ensure that the `increment.js` file will be included just one time, in the `<head>`, no matter how many buttons you insert into the page or where you place them.
