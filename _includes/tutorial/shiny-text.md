
![Tabsets Screenshot](screenshots/shiny-text.png)

The Shiny Text application demonstrates printing R objects directly, as well as displaying data frames using HTML tables. To run the example, type: 

<pre><code class="console">&gt; library(shiny)
&gt; runExample(&quot;02_text&quot;)
</code></pre>

The first example had a single numeric input specified using a slider and a single plot output. This example has a bit more going on: two inputs and two types of textual output.

If you try changing the number of observations to another value, you'll see a demonstration of one of the most important attributes of Shiny applications: inputs and outputs are connected together "live" and changes are propagated immediately (like a spreadsheet). In this case, rather than the entire page being reloaded, just the table view is updated when the number of observations change.

Here is the user interface definition for the application. Notice in particular that the `sidebarPanel` and `mainPanel` functions are now called with two arguments (corresponding to the two inputs and two outputs displayed):

#### ui.R

<pre><code class="r">library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel(&quot;Shiny Text&quot;),

  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    selectInput(&quot;dataset&quot;, &quot;Choose a dataset:&quot;, 
                choices = c(&quot;rock&quot;, &quot;pressure&quot;, &quot;cars&quot;)),

    numericInput(&quot;obs&quot;, &quot;Number of observations to view:&quot;, 10)
  ),

  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
  mainPanel(
    verbatimTextOutput(&quot;summary&quot;),

    tableOutput(&quot;view&quot;)
  )
))
</code></pre>

The server side of the application has also gotten a bit more complicated. Now we create:

* A reactive function to return the dataset corresponding to the user choice
* Two other reactive functions (`reactivePrint` and `reactiveTable`) that return the output$summary and output$view values

These reactive functions work similarly to the `reactivePlot` function used in the first example: by declaring a reactive function you tell Shiny that it should only be executed when it's dependencies change. In this case that's either one of the user input values (input$dataset or input$n)

#### server.R

<pre><code class="r">library(shiny)
library(datasets)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {

  # Return the requested dataset
  datasetInput &lt;- reactive(function() {
    switch(input$dataset,
           &quot;rock&quot; = rock,
           &quot;pressure&quot; = pressure,
           &quot;cars&quot; = cars)
  })

  # Generate a summary of the dataset
  output$summary &lt;- reactivePrint(function() {
    dataset &lt;- datasetInput()
    summary(dataset)
  })

  # Show the first &quot;n&quot; observations
  output$view &lt;- reactiveTable(function() {
    head(datasetInput(), n = input$obs)
  })
})
</code></pre>

We're introduced more use of reactive functions but haven't really explained how they work yet. The next example will start with this one as a baseline and expand significantly on how reactive functions work in Shiny.
