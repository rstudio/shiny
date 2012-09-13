## Building Outputs

Similar to <a href="#building-inputs">custom inputs</a>, if you have some knowledge of HTML/CSS/JavaScript you can also build reusable, custom *output* components.

Output values are received from the server and are directed to an *output binding*, which then 










### Write an Output Binding

Each custom output component needs an *output binding*, an object you create that tells Shiny how to identify instances of your component and how to interact with them. (Note that each *instance* of the output component doesn't need its own output binding object; rather, all instances of a particular type of output component share a single output binding object.)

An output binding object needs to have the following methods:

<dl>
  <dt>
    <code>find(scope)</code>
  </dt>
  <dd>
    <p>
      Given an HTML document or element (<code>scope</code>), find any descendant elements that are an instance of your component and return them as an array (or array-like object). The other output binding methods all take an <code>el</code> argument; that value will always be an element that was returned from <code>find</code>.
    </p>

    <p>
      A very common implementation is to use jQuery's <code>find</code> method to identify elements with a specific class, for example:
    </p>
<pre>exampleOutputBinding.find = function(scope) {
  return $(scope).find(".exampleComponentClass");
};</pre>
  </dd>
  <dt>
    <code>getId(el)</code>
  </dt>
  <dd>
    Return the Shiny output ID for the element <code>el</code>, or <code>null</code> if the element doesn't have an ID and should therefore be ignored. The default implementation in <code>Shiny.InputBinding</code> reads the <code>data-output-id</code> attribute and falls back to the element's <code>id</code> if not present.
  </dd>
  <dt>
    <code>renderValue(el, data)</code>
  </dt>
  <dd>
    Called when a new value that matches this element's ID is received from the server. The function should render the data on the element. The type/shape of the `data` argument depends on the server logic that generated it; whatever value is returned from the R code is converted to JSON using the RJSONIO package.
  </dd>
  <dt>
    <code>renderError(el, err)</code>
  </dt>
  <dd>
    Called when the server attempts to update the output value for this element, and an error occurs. The function should render the error on the element. <code>err</code> is an object with a <code>message</code> String property.
  </dd>
  <dt>
    <code>clearError(el)</code>
  </dt>
  <dd>
    If the element <code>el</code> is currently displaying an error, clear it.
  </dd>
  <dt>
    <code>showProgress(el, show)</code>
  </dt>
  <dd>
  </dd>
</dl>

### Register Output Binding

Once you've created an output binding object, you need to tell Shiny to use it:
<pre><code class="javascript">Shiny.outputBindings.register(exampleOutputBinding, "yourname.exampleOutputBinding");</code></pre>

The second argument is a name the user can use to change the priority of the binding. On the off chance that the user has multiple bindings that all want to claim the same HTML element as their own, this call can be used to control the priority of the bindings:

<pre><code class="javascript">Shiny.outputBindings.setPriority("yourname.exampleOutputBinding", 10);</code></pre>

Higher numbers indicate a higher priority; the default priority is 0. All of Shiny's built-in input component bindings default to a priority of 0.

If two bindings have the same priority value, then the more recently registered binding has the higher priority.

### Example


