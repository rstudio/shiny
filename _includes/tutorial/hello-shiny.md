
![Hello Shiny Screenshot](screenshots/hello-shiny.png)

The Hello Shiny example is a simple application that generates a random distribution with a configurable number of observations and then plots it. To run the example type: 

<pre><code class="console">&gt; library(shiny)
&gt; runExample(&quot;01_hello&quot;)
</code></pre>

#### ui.R

<pre><code class="r">library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(

  # Application title
  headerPanel(&quot;Hello Shiny!&quot;),

  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput(&quot;obs&quot;, 
                &quot;Number of observations:&quot;, 
                min = 0, 
                max = 1000, 
                value = 500)
  ),

  # Show a plot of the generated distribution
  mainPanel(
    plotOutput(&quot;distPlot&quot;)
  )
))
</code></pre>

#### server.R

<pre><code class="r">library(shiny)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

  # Function that generates a plot of the distribution. The function
  # is wrapped in a call to reactivePlot to indicate that:
  #
  #  1) It is &quot;reactive&quot; and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  output$distPlot &lt;- reactivePlot(function() {

    # generate an rnorm distribution and plot it
    dist &lt;- rnorm(input$obs)
    hist(dist)
  })
})
</code></pre>
