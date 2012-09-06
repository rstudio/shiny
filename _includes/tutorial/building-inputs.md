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
<pre>exampleInputBinding.unsubscribe = function(el) {
  $(el).off(".exampleComponentName");
};</pre>
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