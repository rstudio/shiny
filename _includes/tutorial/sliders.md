
![Sliders Screenshot](screenshots/sliders.png)

The Sliders application demonstrates the many capabilities of slider controls, including the ability to run an animation sequence. To run the example type: 

<pre><code class="console">&gt; library(shiny)
&gt; runExample(&quot;05_sliders&quot;)
</code></pre>

### Customizing Sliders

Shiny slider controls are extremely capable and customizable. Features supported include:

* The ability to input both single values and ranges
* Custom formats for value display (e.g for currency)
* The ability to animate the slider across a range of values

Slider controls are created by calling the `sliderInput` function. The ui.R file demonstrates using sliders with a variety of options:

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

    # Provide a custom currency format for value display, with basic animation
    sliderInput(&quot;format&quot;, &quot;Custom Format:&quot;, 
                min = 0, max = 10000, value = 0, step = 2500,
                format=&quot;$#,##0&quot;, locale=&quot;us&quot;, animate=TRUE),

    # Animation with custom interval (in ms) to control speed, plus looping
    sliderInput(&quot;animation&quot;, &quot;Looping Animation:&quot;, 1, 2000, 1, step = 10, 
                animate=animationOptions(interval=300, loop=T))
  ),

  # Show a table summarizing the values entered
  mainPanel(
    tableOutput(&quot;values&quot;)
  )
))
</code></pre>

### Server Script

The server side of the Slider application is very straightforward: it creates a data frame containing all of the input values and then renders it as an HTML table:

#### server.R

<pre><code class="r">library(shiny)

# Define server logic for slider examples
shinyServer(function(input, output) {

  # Reactive function to compose a data frame containing all of the values
  sliderValues &lt;- reactive(function() {

    # Compose data frame
    data.frame(
      Name = c(&quot;Integer&quot;, 
               &quot;Decimal&quot;,
               &quot;Range&quot;,
               &quot;Custom Format&quot;,
               &quot;Animation&quot;),
      Value = as.character(c(input$integer, 
                             input$decimal,
                             paste(input$range, collapse=&#39; &#39;),
                             input$format,
                             input$animation)), 
      stringsAsFactors=FALSE)
  }) 

  # Show the values using an HTML table
  output$values &lt;- reactiveTable(function() {
    sliderValues()
  })
})
</code></pre>


