

![Reactivity Screenshot](screenshots/reactivity.png)


To run the example type: 

<pre><code class="console">&gt; library(shiny)
&gt; runExample(&quot;03_reactivity&quot;)
</code></pre>

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
    obs &lt;- as.integer(input$obs)
    head(datasetInput(), n = obs)
  })
})
</code></pre>
