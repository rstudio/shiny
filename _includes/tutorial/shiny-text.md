
![Tabsets Screenshot](screenshots/shiny-text.png)

The Shiny Text application demonstrates printing R objects directly as well as displaying data-frames using HTML tables. To run the example type: 

<pre><code class="console">&gt; library(shiny)
&gt; runExample(&quot;02_text&quot;)
</code></pre>

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


