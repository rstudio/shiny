

![HTML UI Screenshot](screenshots/html-ui.png)

The HTML UI application demonstrates defining a Shiny user-interface using a standard HTML page rather than a ui.R script. To run the example type: 

<pre><code class="console">&gt; library(shiny)
&gt; runExample(&quot;08_html&quot;)
</code></pre>

#### www/index.html

<pre style='color:#000000;background:#ffffff;'><code><span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>html</span><span style='color:#7f0055; '>></span>

<span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>head</span><span style='color:#7f0055; '>></span>
  <span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>script src="shared/jquery.js" type="text/javascript"</span><span style='color:#7f0055; '>></span><span style='color:#7f0055; '>&lt;/</span><span style='color:#7f0055; font-weight:bold; '>script</span><span style='color:#7f0055; '>></span>
  <span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>script src="shared/shiny.js" type="text/javascript"</span><span style='color:#7f0055; '>></span><span style='color:#7f0055; '>&lt;/</span><span style='color:#7f0055; font-weight:bold; '>script</span><span style='color:#7f0055; '>></span>
  <span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>link</span> rel=<span style='color:#2a00ff; '>"stylesheet"</span> type=<span style='color:#2a00ff; '>"text/css"</span> href=<span style='color:#2a00ff; '>"shared/shiny.css"</span><span style='color:#7f0055; '>/></span> 
<span style='color:#7f0055; '>&lt;/</span><span style='color:#7f0055; font-weight:bold; '>head</span><span style='color:#7f0055; '>></span>
 
<span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>body</span><span style='color:#7f0055; '>></span>
  <span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>h1</span><span style='color:#7f0055; '>></span>HTML UI<span style='color:#7f0055; '>&lt;/</span><span style='color:#7f0055; font-weight:bold; '>h1</span><span style='color:#7f0055; '>></span>
 
  <span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>p</span><span style='color:#7f0055; '>></span>
    <span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>label</span><span style='color:#7f0055; '>></span>Distribution type:<span style='color:#7f0055; '>&lt;/</span><span style='color:#7f0055; font-weight:bold; '>label</span><span style='color:#7f0055; '>></span><span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>br</span> <span style='color:#7f0055; '>/></span>
    <span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>select</span> name=<span style='color:#2a00ff; '>"dist"</span><span style='color:#7f0055; '>></span>
      <span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>option</span> value=<span style='color:#2a00ff; '>"norm"</span><span style='color:#7f0055; '>></span>Normal<span style='color:#7f0055; '>&lt;/</span><span style='color:#7f0055; font-weight:bold; '>option</span><span style='color:#7f0055; '>></span>
      <span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>option</span> value=<span style='color:#2a00ff; '>"unif"</span><span style='color:#7f0055; '>></span>Uniform<span style='color:#7f0055; '>&lt;/</span><span style='color:#7f0055; font-weight:bold; '>option</span><span style='color:#7f0055; '>></span>
      <span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>option</span> value=<span style='color:#2a00ff; '>"lnorm"</span><span style='color:#7f0055; '>></span>Log-normal<span style='color:#7f0055; '>&lt;/</span><span style='color:#7f0055; font-weight:bold; '>option</span><span style='color:#7f0055; '>></span>
      <span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>option</span> value=<span style='color:#2a00ff; '>"exp"</span><span style='color:#7f0055; '>></span>Exponential<span style='color:#7f0055; '>&lt;/</span><span style='color:#7f0055; font-weight:bold; '>option</span><span style='color:#7f0055; '>></span>
    <span style='color:#7f0055; '>&lt;/</span><span style='color:#7f0055; font-weight:bold; '>select</span><span style='color:#7f0055; '>></span> 
  <span style='color:#7f0055; '>&lt;/</span><span style='color:#7f0055; font-weight:bold; '>p</span><span style='color:#7f0055; '>></span>
 
  <span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>p</span><span style='color:#7f0055; '>></span>
    <span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>label</span><span style='color:#7f0055; '>></span>Number of observations:<span style='color:#7f0055; '>&lt;/</span><span style='color:#7f0055; font-weight:bold; '>label</span><span style='color:#7f0055; '>></span><span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>br</span> <span style='color:#7f0055; '>/></span> 
    <span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>input</span> type=<span style='color:#2a00ff; '>"number"</span> name=<span style='color:#2a00ff; '>"n"</span> value=<span style='color:#2a00ff; '>"500"</span> min=<span style='color:#2a00ff; '>"1"</span> max=<span style='color:#2a00ff; '>"1000"</span> <span style='color:#7f0055; '>/></span>
  <span style='color:#7f0055; '>&lt;/</span><span style='color:#7f0055; font-weight:bold; '>p</span><span style='color:#7f0055; '>></span>
 
  <span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>pre</span> id=<span style='color:#2a00ff; '>"summary"</span> class=<span style='color:#2a00ff; '>"shiny-text-output"</span><span style='color:#7f0055; '>></span><span style='color:#7f0055; '>&lt;/</span><span style='color:#7f0055; font-weight:bold; '>pre</span><span style='color:#7f0055; '>></span> 
  
  <span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>div</span> id=<span style='color:#2a00ff; '>"plot"</span> class=<span style='color:#2a00ff; '>"shiny-plot-output"</span> 
       style=<span style='color:#2a00ff; '>"</span><span style='color:#7f0055; font-weight:bold; '>width</span>: 100%; <span style='color:#7f0055; font-weight:bold; '>height</span>: 400px<span style='color:#2a00ff; '>"</span><span style='color:#7f0055; '>></span><span style='color:#7f0055; '>&lt;/</span><span style='color:#7f0055; font-weight:bold; '>div</span><span style='color:#7f0055; '>></span> 
  
  <span style='color:#7f0055; '>&lt;</span><span style='color:#7f0055; font-weight:bold; '>div</span> id=<span style='color:#2a00ff; '>"table"</span> class=<span style='color:#2a00ff; '>"shiny-html-output"</span><span style='color:#7f0055; '>></span><span style='color:#7f0055; '>&lt;/</span><span style='color:#7f0055; font-weight:bold; '>div</span><span style='color:#7f0055; '>></span>
<span style='color:#7f0055; '>&lt;/</span><span style='color:#7f0055; font-weight:bold; '>body</span><span style='color:#7f0055; '>></span>

<span style='color:#7f0055; '>&lt;/</span><span style='color:#7f0055; font-weight:bold; '>html</span><span style='color:#7f0055; '>></span>
</code></pre>


#### server.R

<pre><code class="r">library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output) {

  # Reactive function to generate the requested distribution. This is 
  # called whenever the inputs change. The output functions defined 
  # below then all used the value computed from this function
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


