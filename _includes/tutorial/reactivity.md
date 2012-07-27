

![Reactivity Screenshot](screenshots/reactivity.png)

The Reactivity application is very similar to Hello Text but goes into much more detail about reactive programming concepts. To run the example type: 

<pre><code class="console">&gt; library(shiny)
&gt; runExample(&quot;03_reactivity&quot;)
</code></pre>

The previous examples have given you a good idea of what the code for Shiny applications looks like. We've explained a bit about reactivity but mostly glossed over the finer details. Now before we look at more code we'll explore these concepts more deeply.

### What is Shiny Reactivity?

The Shiny web framework is fundamentally about making it easy to wire up *input values* from a web page, making them easily available to you in R, and have the results of your R code be written as *output values* back out to the web page.

    input values => R code => output values

Since Shiny web apps are interactive, the input values can change at any time, and the output values need to be updated immediately to reflect those changes.

Shiny comes with a **reactive programming** library that you will use to structure your application logic. By using this library, changing input values will naturally cause the right parts of your R code to be reexecuted, which will in turn cause any changed outputs to be updated.

### Reactive Programming Basics

Reactive programming is a coding style that starts with **reactive values**--values that change over time, or in response to the user--and builds on top of them with **reactive functions**--functions that access reactive values and execute other reactive functions.

What's interesting about reactive functions is that whenever they execute, they automatically keep track of what reactive values they read and what reactive functions they called. If those "dependencies" become out of date, then they know that their own return value has also become out of date. Because of this dependency tracking, changing a reactive value will automatically instruct all reactive functions that directly or indirectly depended on that value to re-execute.

The initial way you'll encounter reactive values in Shiny is using the `input` object. The `input` object, which is passed to your `shinyServer` function, lets you access the web page's user input fields using a list-like syntax. Code-wise, it looks like you're grabbing a value from a list or data frame, but you're actually reading a reactive value.

It's also possible to create reactive functions directly (usually computed based on user inputs or even other reactive values), which can then in turn be depended on by other reactive functions. In this application an example of that is the function that returns an R data frame based on the selection the user made in the input form:

<pre><code class="r">datasetInput &lt;- reactive(function() {
   switch(input$dataset,
          &quot;rock&quot; = rock,
          &quot;pressure&quot; = pressure,
          &quot;cars&quot; = cars)
})
</code></pre>

The transormation of reactive values into outputs is then by assignments to the `output` object. Here is an example of an assignment to an output that depends on both the `datasetInput` reactive function we just defined as well as `input$obs`:

<pre><code class="r"> 
output$view &lt;- reactiveTable(function() {
   head(datasetInput(), n = input$obs)
})
</code></pre>

This function will be re-executed (and it's output re-rendered in the browser) whenever either the `datasetInput` or `input$obs` value changes.
 

### Back to the Code

Now that we've taken a deeper loop at some of the core concepts, let's revised the source code and try to understand what's going on at a bit finer level of detail. The user interface definition has been updated to include a text-input field that defines a caption. Other than that it's very similar to the previous example:

#### ui.R

<pre><code class="r">library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel(&quot;Reactivity&quot;),

  # Sidebar with controls to provide a caption, select a dataset, and 
  # specify the number of observations to view. Note that changes made
  # to the caption in the textInput control are updated in the output
  # area immediately as you type
  sidebarPanel(
    textInput(&quot;caption&quot;, &quot;Caption:&quot;, &quot;Data Summary&quot;),

    selectInput(&quot;dataset&quot;, &quot;Choose a dataset:&quot;, 
                choices = c(&quot;rock&quot;, &quot;pressure&quot;, &quot;cars&quot;)),

    numericInput(&quot;obs&quot;, &quot;Number of observations to view:&quot;, 10)
  ),


  # Show the caption, a summary of the dataset and an HTML table with
  # the requested number of observations
  mainPanel(
    h3(textOutput(&quot;caption&quot;)), 

    verbatimTextOutput(&quot;summary&quot;), 

    tableOutput(&quot;view&quot;)
  )
))
</code></pre>

### Server Script

The server script declares the `datasetInput` reactive function as well as three reactive output values. There are detailed comments for each definition that describe how it works within the reactive system:

#### server.R

<pre><code class="r">library(shiny)
library(datasets)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {

  # By declaring databaseInput as a reactive function we ensure that:
  #
  #  1) It is only called when the inputs it depends on changes
  #  2) The computation and result are shared by all the callers (it 
  #     only executes a single time)
  #  3) When the inputs change and the function is re-executed, the
  #     new result is compared to the previous result; if the two are
  #     identical, then the callers are not notified
  #
  datasetInput &lt;- reactive(function() {
    switch(input$dataset,
           &quot;rock&quot; = rock,
           &quot;pressure&quot; = pressure,
           &quot;cars&quot; = cars)
  })

  # The output$caption is computed based on a reactive function that
  # returns input$caption. When the user changes the &quot;caption&quot; field:
  #
  #  1) This function is automatically called to recompute the output 
  #  2) The new caption is pushed back to the browser for re-display
  # 
  # Note that because the data-oriented reactive functions below don&#39;t 
  # depend on input$caption, those functions are NOT called when 
  # input$caption changes.
  output$caption &lt;- reactiveText(function() {
    input$caption
  })

  # The output$summary depends on the datasetInput reactive function, 
  # so will be re-executed whenever datasetInput is re-executed 
  # (i.e. whenever the input$dataset changes)
  output$summary &lt;- reactivePrint(function() {
    dataset &lt;- datasetInput()
    summary(dataset)
  })

  # The output$view depends on both the databaseInput reactive function
  # and input$obs, so will be re-executed whenever input$dataset or 
  # input$obs is changed. 
  output$view &lt;- reactiveTable(function() {
    head(datasetInput(), n = input$obs)
  })
})
</code></pre>

We've reviewed a lot code and covered a lot of conceptual ground in the first three examples. The next section focuses on the mechanics of building a Shiny appliation from the ground up and also covers tips on how to run and debug Shiny applications.
