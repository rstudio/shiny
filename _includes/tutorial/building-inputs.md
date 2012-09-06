## Building Inputs

If you have some knowledge of HTML/CSS/JavaScript, you can create your own custom input components that can be reused by others.

Shiny input components should try to adhere to the following principles, if possible:

* **Designed to be used from HTML and R:** Shiny user interfaces can either be written using R code (that generates HTML), or by writing the HTML directly. A well-designed Shiny input component will take both styles into account: offer an R function for creating the component, but also have thoughtfully designed and documented HTML markup.
* **Configurable using HTML attributes:** Avoid requiring the user to make JavaScript calls to configure the component. Instead, it's better to use HTML attributes. In your component's JavaScript logic, you can [easily access these values using jQuery](http://api.jquery.com/data/#data-html5) (or simply by reading the DOM attribute directly).

Each custom input component you design needs an *input binding*, an object you create that tells Shiny how to identify instances of your component and how to interact with it. An input binding object needs to have the following methods:

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
      We recommend using jQuery's event namespacing feature when subscribing, as it makes unsubscribing these events later very easy. An example:
    </p>
<pre>exampleInputBinding.subscribe = function(el, callback) {
  $(el).on("change.exampleComponentName", function(event) {
    callback();
  });
};</pre>
    <p>
      Later on, we can unsubscribe <code>".exampleComponentName"</code> which will remove all of our handlers without touching anyone else's.
    </p>
  </dd>
  <dt>
    <code>unsubscribe(el)</code>
  </dt>
  <dd>
    <p>Unsubscribe DOM event listeners that were bound in <code>subscribe</code>.</p>
    <p>Example:</p>
<pre>exampleInputBinding.unsubscribe = function(el) {
  $(el).off(".exampleComponentName");
};</pre>
  </dd>
</dl>