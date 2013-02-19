## Understanding Reactivity

It's easy to build interactive applications with Shiny, but to get the most out of it, you'll need to understand the reactive programming model used by Shiny.

In Shiny, there are three kinds of objects in reactive programming: reactive sources, reactive conductors, and reactive endpoints, which are represented with these symbols:

![Reactive roles](reactivity_diagrams/roles.png)

The simplest structure of a reactive program involves just a source and an endpoint:

![Simplest structure](reactivity_diagrams/simplest.png)

In a Shiny application, the source typically is user input through a browser interface. For example, when the selects an item, types input, or clicks on a button, these actions will set values that are reactive sources. A reactive endpoint is usually something that appears in the user's browser window, such as a plot or a table of values.

In a simple Shiny application, reactive sources are accessible through the `input` object, and reactive endpoints are accessible through the `output` object. (Actually, there are other possible kinds of sources and endpoints, which we'll talk about later, but for now we'll just talk about `input` and `output`.)

This simple structure, with one source and one endpoint, is used by the `01_hello` example. The `server.R` code for that example looks something like this:

{% highlight r %}
shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs))
  })
})
{% endhighlight %}

![Structure of 01_hello](reactivity_diagrams/01_hello.png)

You can see it in action at [http://glimmer.rstudio.com/shiny/01_hello/](http://glimmer.rstudio.com/shiny/01_hello/).

The `output$distPlot` object is a reactive endpoint, and it uses the reactive source `input$obs`. Whenever `input$obs` changes, `output$distPlot` is notified that it needs to re-execute. In traditional program with an interactive user interface, this might involve setting up event handlers and writing code to read values and transfer data. Shiny does all these things for you behind the scenes, so that you can simply write code that looks like regular R code.

A reactive source can be connected to multiple endpoints, and vice versa. Here is a slightly more complex Shiny application:


{% highlight r %}
shinyServer(function(input, output) {
  output$plotOut <- renderPlot({
    hist(faithful$eruptions, breaks = as.numeric(input$nBreaks))
    if (input$individualObs)
      rug(faithful$eruptions)
  })

  output$tableOut <- renderTable({
    if (input$individualObs)
      faithful
    else
      NULL
  })
})
{% endhighlight %}

![Structure of Old Faithful example](reactivity_diagrams/faithful.png)

In a Shiny application, there's no need to explictly describe each of these relationships and tell R what to do when each input component changes; Shiny automatically handles these details for you.

In an app with the structure above, whenever the value of the `input$nBreaks` changes, the expression that generates the plot will automatically re-execute. Whenever the value of the `input$individualObs` changes, the plot and table functions will automatically re-execute. (In a Shiny application, most endpoint functions have their results automatically wrapped up and sent to the web browser.)


### Invalidation and execution scheduling

At the core of Shiny is its reactive engine: this is how Shiny knows when to re-execute each component of an application. To understand how it works, we need look at the pieces in more depth.

We've discussed reactive sources and reactive endpoints. These are general terms for parts that play a particular role in a reactive program. Presently, Shiny has one class of objects that act as reactive sources, and one class of objects that act as reactive endpoints, but in principle there could be other classes that implement these roles.

* **Reactive values** are an implementation of Reactive sources; that is, they are an implementation of that role. The `input` object is a `ReactiveValues` object which looks something like a list, and it contains many individual reactive values.
* **Reactive expressions** are an implementation of Reactive conductors. We haven't seen these yet, but we'll explore in the next section.
* **Observers** are an implementation of Reactive endpoints. The `output` object looks something like a list, and it can contain many individual observers.

![Implementations of reactive roles](reactivity_diagrams/roles_implement.png)


At an abstract level, we can describe the `01_hello` example as containing one source and one endpoint. When we talk about it more concretely, we can describe it as having one reactive value, `input$obs`, and one reactive observer, `output$distPlot`.

As shown in the diagram below, a reactive value has a value (no surprise there). A reactive observer, on the other hand, doesn't have a value. Instead, it contains an R expression which, when executed, has some side effect (in most cases, this involves sending data to the web browser). But the observer doesn't return a value. Reactive observers have another property: they have a flag that indicates whether they have been _invalidated_. We'll see what that means shortly.

![](reactivity_diagrams/01_hello_process_1.png)

When you load this application in a web page, it be in the state shown above, with `input$obs` having the value 500 (this is set in the `ui.r` file, which isn't shown here). If you change the value to 1000, it triggers a series of events that result in a new image being sent to your browser.

When the value of `input$obs` changes, two things happen:
* All of its descendants in the graph are invalidated. Sometimes for brevity we'll say that an observer is _dirty_, meaning that it is invalidated, or _clean_, meaning that it is _not_ invalidated.
* The arrows to all its descendants are removed; they are no longer considered descendants, and changing the reactive value again won't have any effect on them. Notice that the arrows are dynamic, not static.


In this case, the only descendant is `output$distPlot`:

![](reactivity_diagrams/01_hello_process_2.png)

Once all the descendants are invalidated, a _flush_ occurs. When this happens, all invalidated observers re-execute.

![](reactivity_diagrams/01_hello_process_3.png)

Remember that the code we assigned to `output$distPlot` makes use of `input$obs`:

{% highlight r %}
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs))
  })
{% endhighlight %}

As `output$distPlot` re-executes, it accesses the reactive value `input$obs`. When it does this, it takes a dependency on that value; in other words, an arrow is drawn from `input$obs` to `output$distPlot`.

![](reactivity_diagrams/01_hello_process_4.png)

As it finishes executing, `output$distPlot` creates a PNG image file, which is sent to the browser, and finally it is marked as clean (not invalidated).

![](reactivity_diagrams/01_hello_process_5.png)

Now the cycle is complete, and the application is ready to accept input again.

When someone first starts a session with a Shiny application, all of the endpoints start out invalidated, triggering this series of events.


### Reactive conductors 

So far we've seen reactive sources and reactive endpoints, and most simple examples use just these two parts, wiring up sources directly to endpoints. It's also possible to put reactive components in between the sources and endpoints. These components are called _reactive conductors_, and they are implemented with _reactive expressions_. Here's a diagram of a simple program with a reactive conductor:

![Reactive conductor](reactivity_diagrams/conductor.png)

A conductor can both be a dependent and have dependents. In other words, it can be both a parent and child in a graph of the reactive structure. Sources can only be parents (they can have dependents), and endpoints can only be children (they can be dependents) in the reactive graph.

Reactive conductors are useful for encapsulating slow or computationally expensive operations. For example, imagine that you have this application that takes a value `input$n` and prints the _n_th value in the Fibonacci sequence, as well as the inverse of _n_th value in the sequence plus one (note the code below is condensed to illustrate reactive concepts, and doesn't necessarily represent best coding practices):

{% highlight r %}
# Calculate nth number in Fibonacci sequence
fib <- function(n) ifelse(n<3, 1, fib(n-1)+fib(n-2))

shinyServer(function(input, output) {
  output$nthValue    <- reactiveText({ fib(as.numeric(input$n)) })
  output$nthValueInv <- reactiveText({ 1 / fib(as.numeric(input$n)) })
})
{% endhighlight %}


The graph structure of this app is:

![Fibonacci app without conductor](reactivity_diagrams/fib_no_conductor.png)


Calculating Fibonacci numbers with the `fib()` algorithm is very computationally expensive, so we don't want to calculate it more times than is absolutely necessary. But in this app we're calculating it twice! On a reasonably fast modern machine, setting `input$n` to 30 takes about 15 seconds to calculate the answer, in part because it runs `fib()` twice.

We can reduce the amount of computation necessary by adding a reactive conductor in between the source and endpoints:

{% highlight r %}
fib <- function(n) ifelse(n<3, 1, fib(n-1)+fib(n-2))

shinyServer(function(input, output) {
  currentFib         <- reactive({ fib(as.numeric(input$n)) })

  output$nthValue    <- reactiveText({ fib(as.numeric(input$n)) })
  output$nthValueInv <- reactiveText({ 1 / fib(as.numeric(input$n)) })
})
{% endhighlight %}


Here is the new graph structure, this time shown in its state after the initial run, with the values and invalidation flags (the starting value for `input$n` is set in `ui.r`, which isn't shown here):

![](reactivity_diagrams/fib_process_1.png)


Suppose the user sets `input$n` to 30. This is a new value, so it immediately invalidates its children, `currentFib`, which in turn invalidates its children, `output$nthValue` and `output$nthValueInv`. As the invalidations are made, the dependency arrows are removed:

![](reactivity_diagrams/fib_process_2.png)

After the invalidations finish, the reactive environment is flushed, so the endpoints re-execute. If a flush occurs when multiple endpoints are invalidated, there isn't a guaranteed order that the endpoints will execute, so `nthValue` may run before `nthValueInv`, or vice versa. Suppose in this case that `nthValue()` executes first. The next several steps are straightforward:

![](reactivity_diagrams/fib_process_3.png)

![](reactivity_diagrams/fib_process_4.png)

![](reactivity_diagrams/fib_process_5.png)

![](reactivity_diagrams/fib_process_6.png)

![](reactivity_diagrams/fib_process_7.png)

![](reactivity_diagrams/fib_process_8.png)

As `output$nthValueInv()` executes, it calls `currentFib()`. If `currentFib()` were an ordinary R expression, it would simply re-execute, taking another several seconds. But it's not an ordinary expression; it's a reactive expression, and it now happens to be marked clean. Because it is clean, Shiny knows that all of `currentFib`'s reactive parents have not changed values since the previous run `currentFib()`. This means that running the function again would simply return the same value as the previous run. (Shiny assumes that the non-reactive objects used by `currentFib()` also have not changed. If, for example, it called `Sys.time()`, then a second run of `currentFib()` could return a different value. If you wanted the changing values of `Sys.time()` to be able to invalidate `currentFib()`, it would have to be wrapped up in an object that acted as a reactive source. If you were to do this, that object would also be added as a node on the reactive graph.)

Acting on this assumption, that clean reactive expressions will return the same value as they did the previous run, when reactive expressions are executed, Shiny caches the return value. On subsequent calls to the reactive expression it simply returns the cached value, without re-executing the expression, as long as it remains clean.

In our example, when `output$nthValueInv()` calls `currentFib()`, Shiny just hands it the cached value, 832040. This happens almost instantaneously, instead of taking several more seconds to re-execute `currentFib()`:

![](reactivity_diagrams/fib_process_9.png)

Finally, `output$nthValueInv()` takes that value, finds the inverse, and then as a side effect, sends the value to the browser.

![](reactivity_diagrams/fib_process_10.png)


#### Other uses of reactive expressions

Reactive expressions can be useful for caching the results of any procedure that happens in response to user input, including:

* accessing a database
* reading data from a file
* downloading data over the network
* performing an expensive computation


#### Differences between conductors and endpoints

Conductors (reactive expressions) and endpoints (reactive observers) are similar in that they have expressions that can be executed, but they have some fundamental differences.

* Endpoints respond to reactive flush events, but conductors don't. If you want a conductor to execute, it must have an endpoint as a descendent on the reactive dependency graph.
* Conductors return values, but endpoints don't. Note that reactive expressions are an implementation of conductors, and they happen to cache their return values. (In an abstract sense, a conductor doesn't necessarily cache its return values, but this implementation -- the reactive expression -- does so.)

If you look at the code for `reactiveText()` and friends, you'll see that they each return a function which returns a value. This might lead you to think that the endpoints _do_ return values. However, this isn't the whole story. The function returned by `reactiveText()` is actually not an observer/endpoint. The function returned by `reactiveText()` gets automatically wrapped into an observer when it is assigned to `output$x`. This is because the observer needs to do special things to send the data to the browser.


### Isolation: avoiding dependency

Sometimes it's useful for an endpoint to access a reactive value or expression, but not to take a dependency on it. For example, if the endpoint performs a long calculation or downloads large data set, you might want it to execute only when a button is clicked. 

For this, we'll use `actionButton` from the `shinyIncubator` package. If you want try this code yourself, you'll have to install the package from Github, using devtools:

{% highlight r %}
install.packages('devtools')
devtools::install_github('shiny-incubator', 'rstudio')
{% endhighlight %}

We'll define a `ui.R` that is a slight modification of the one from 01_hello -- the only difference is that it has an actionButton labeled "Go!". You can see it in action at [http://glimmer.rstudio.com/winston/actionbutton/](http://glimmer.rstudio.com/winston/actionbutton/).

The actionButton includes some JavaScript code that sends numbers to the server. When the web browser first connects, it sends a value of 0, and on each click, it sends an incremented value: 1, 2, 3, and so on.

{% highlight r %}
library(shinyIncubator)

shinyUI(pageWithSidebar(
  headerPanel("Click the button"),
  sidebarPanel(
    sliderInput("obs", "Number of observations:",
                min = 0, max = 1000, value = 500),
    actionButton("goButton", "Go!")
  ),
  mainPanel(
    plotOutput("distPlot")
  )
))
{% endhighlight %}

In our `server.R`, there are two changes to note. First, `output$distPlot` will take a dependency on `input$goButton`, simply by accessing it. When the button is clicked, the value of `input$goButton` increases, and so `output$distPlot` re-executes.

The second change is that the access to `input$obs` is wrapped with `isolate()`. This function takes an R expression, and it tells Shiny that the calling observer or reactive expression should not take a dependency on any reactive objects inside the expression.

{% highlight r %}
shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    # Take a dependency on input$goButton
    input$goButton

    # Use isolate() to avoid dependency on input$obs
    dist <- isolate(rnorm(input$obs))
    hist(dist)
  })
}) 
{% endhighlight %}


The resulting graph looks like this:

![Isolated reactive value](reactivity_diagrams/isolate.png)


And here's a walkthrough of the process when `input$obs` is set to 1000, and then the Go button is clicked: 

![](reactivity_diagrams/isolate_process_1.png)

![](reactivity_diagrams/isolate_process_2.png)

![](reactivity_diagrams/isolate_process_3.png)

![](reactivity_diagrams/isolate_process_4.png)

![](reactivity_diagrams/isolate_process_5.png)

![](reactivity_diagrams/isolate_process_6.png)


In the `actionButton` example, you might want to prevent it from returning a plot the first time, before the button has been clicked. Since the starting value of an `actionButton` is zero, this can be accomplished with the following:

{% highlight r %}
  output$distPlot <- renderPlot({
    if (input$goButton == 0)
      return()

    # plot-making code here
  })
{% endhighlight %}


Reactive values are not the only things that can be isolated; reactive expressions can also be put inside an `isolate()`. It's also possible to put multiple lines of code in `isolate()`, like this:

{% highlight r %}
z <- isolate({
  x <- input$xSlider + 100
  y <- input$ySlider * 2
  x / y
})
{% endhighlight %}

In this piece of code, `x` and `y` are assigned values that will be visible even outside of the `isolate()`, and `z` will be assigned the last value in the expression, `x / y`. The calling function won't take a reactive dependency on either of the `input` variables.
