

![More Widgets Screenshot](screenshots/more-widgets.png)

The More Widgets application demonstrates the helpText and submitButton widgets as well as the use of embedded HTML elements to customized formatting. To run the example type: 

<pre><code class="console">&gt; library(shiny)
&gt; runExample(&quot;07_widgets&quot;)
</code></pre>

#### ui.R

<pre><code class="r">library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(

  # Application title.
  headerPanel(&quot;More Widgets&quot;),

  # Sidebar with controls to select a dataset and specify the number
  # of observations to view. The helpText function is also used to 
  # include clarifying text. Most notably, the inclusion of a 
  # submitButton defers the rendering of output until the user 
  # explicitly clicks the button (rather than doing it immediately
  # when inputs change). This is useful if the computations required
  # to render output are inordinately time-consuming.
  sidebarPanel(
    selectInput(&quot;dataset&quot;, &quot;Choose a dataset:&quot;, 
                choices = c(&quot;rock&quot;, &quot;pressure&quot;, &quot;cars&quot;)),

    numericInput(&quot;obs&quot;, &quot;Number of observations to view:&quot;, 10),

    helpText(&quot;Note: while the data view will show only the specified&quot;,
             &quot;number of observations, the summary will still be based&quot;,
             &quot;on the full dataset.&quot;),

    submitButton(&quot;Update View&quot;)
  ),

  # Show a summary of the dataset and an HTML table with the requested
  # number of observations. Note the use of the h4 function to provide
  # an additional header above each output section.
  mainPanel(
    h4(&quot;Summary&quot;),
    verbatimTextOutput(&quot;summary&quot;),

    h4(&quot;Observations&quot;),
    tableOutput(&quot;view&quot;)
  )
))
</code></pre>

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
    obs &lt;- as.integer(input$obs)
    head(datasetInput(), n = obs)
  })
})
</code></pre>
