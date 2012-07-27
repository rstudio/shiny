
![Sliders Screenshot](screenshots/sliders.png)

The Sliders application demonstrates the many capabilities of slider controls, including the ability to run an animation sequence. To run the example type: 

<pre><code class="console">&gt; library(shiny)
&gt; runExample(&quot;05_sliders&quot;)
</code></pre>

#### ui.R

<pre><code class="r">library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(

  #  Application title
  headerPanel(&quot;Sliders&quot;),

  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    # Simple integer interval
    sliderInput(&quot;integer&quot;, &quot;Integer:&quot;, 
                min=0, max=1000, value=500),

    # Decimal interval with step value
    sliderInput(&quot;decimal&quot;, &quot;Decimal:&quot;, 
                min = 0, max = 1, value = 0.5, step= 0.1),

    # Specification of range within an interval
    sliderInput(&quot;range&quot;, &quot;Range:&quot;,
                min = 1, max = 1000, value = c(200,500)),

    # Provide a custom currency format for value display
    sliderInput(&quot;format&quot;, &quot;Custom Format:&quot;, 
                min = 0, max = 100000, value = 50000, step = 25000,
                format=&quot;$#,##0&quot;, locale=&quot;us&quot;),  

    # Animation with custom interval (in milliseconds) to control speed
    # (also provide helpText to highlight the possiblity of animation)
    sliderInput(&quot;animation&quot;, &quot;Animation:&quot;, 1, 2000, 1, step = 10,
                animationInterval = 300),
    helpText(&quot;Use the Play button to animate values.&quot;)
  ),

  # Show a table summarizing the values entered
  mainPanel(
    tableOutput(&quot;values&quot;)
  )
))
</code></pre>

#### server.R

<pre><code class="r">library(shiny)

# Define server logic for slider examples
shinyServer(function(input, output) {

  # Reactive function to compose a data frame containing all of the values
  sliderValues &lt;- reactive(function() {

    # Show values using R&#39;s default print format
    printValue &lt;- function(value) {
      capture.output(print(value))
    }

    # Compose data frame
    data.frame(
      Name = c(&quot;Integer&quot;, 
               &quot;Decimal&quot;,
               &quot;Range&quot;,
               &quot;Custom Format&quot;,
               &quot;Animation&quot;),
      Value = c(printValue(input$integer), 
                printValue(input$decimal),
                printValue(input$range),
                printValue(input$format),
                printValue(input$animation)), 
      stringsAsFactors=FALSE)
  }) 

  # Show the values using an HTML table
  output$values &lt;- reactiveTable(function() {
    sliderValues()
  })
})
</code></pre>


