

![Tabsets Screenshot](screenshots/tabsets.png)

To run the example type: 

<pre><code class="console">&gt; library(shiny)
&gt; runExample(&quot;06_tabsets&quot;)
</code></pre>

#### ui.R

<pre><code class="r">library(shiny)

# Define UI for random distribution application 
shinyUI(pageWithSidebar(

  # Application title
  headerPanel(&quot;Tabsets&quot;),

  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the br()
  # element to introduce extra vertical spacing
  sidebarPanel(
    radioButtons(&quot;dist&quot;, &quot;Distribution type:&quot;,
                 list(&quot;Normal&quot; = &quot;norm&quot;,
                      &quot;Uniform&quot; = &quot;unif&quot;,
                      &quot;Log-normal&quot; = &quot;lnorm&quot;,
                      &quot;Exponential&quot; = &quot;exp&quot;)),
    br(),

    sliderInput(&quot;n&quot;, 
                &quot;Number of observations:&quot;, 
                 value = 500,
                 min = 1, 
                 max = 1000)
  ),

  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel(&quot;Plot&quot;, plotOutput(&quot;plot&quot;)), 
      tabPanel(&quot;Summary&quot;, verbatimTextOutput(&quot;summary&quot;)), 
      tabPanel(&quot;Table&quot;, tableOutput(&quot;table&quot;))
    )
  )
))
</code></pre>

#### server.R

<pre><code class="r">library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output) {

  # Reactive function to generate the requested distribution. This is 
  # called whenever the inputs change. The output functions defined 
  # below then all use the value computed from this function
  data &lt;- reactive(function() {  
    dist &lt;- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)

    dist(as.integer(input$n))
  })

  # Generate a plot of the data. Also uses the inputs to build the 
  # plot label. Note that the dependencies on both the inputs and
  # the data reactive function are both tracked, and all functions 
  # are called in the sequence implied by the dependency graph
  output$plot &lt;- reactivePlot(function() {
    dist &lt;- input$dist
    n &lt;- input$n

    hist(data(), 
         main=paste(&#39;r&#39;, dist, &#39;(&#39;, n, &#39;)&#39;, sep=&#39;&#39;))
  })

  # Generate a summary of the data
  output$summary &lt;- reactivePrint(function() {
    summary(data())
  })

  # Generate an HTML table view of the data
  output$table &lt;- reactiveTable(function() {
    data.frame(x=data())
  })
})
</code></pre>
