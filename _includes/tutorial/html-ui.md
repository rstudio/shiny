

![HTML UI Screenshot](screenshots/html-ui.png)

The HTML UI application demonstrates defining a Shiny user-interface using a standard HTML page rather than a ui.R script. To run the example type: 

{% highlight console %}
> library(shiny)
> runExample("08_html")
{% endhighlight %}

### Defining an HTML UI

The previous examples in this tutorial used a ui.R file to build their user-interfaces. While this is a fast and convenient way to build user-interfaces, some appliations will inevitably require more flexiblity. For this type of application, you can define your user-interface directly in HTML. In this case there is no ui.R file and the directory structure looks like this:

<pre><code>&lt;<em>application-dir</em>&gt;
|-- www
    |-- index.html
|-- server.R
</code></pre>

In this example we re-write the front-end of the Tabsets application using HTML directly. Here is the source code for the new user-interface definition:

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

There are few things to point out regarding how Shiny binds HTML elements back to inputs and outputs:

* HTML form elmements (in this case a select list and a number input) are bound to input slots using their `name` attribute.
* Output is rendered into HTML elements based on matching their `id` attribute to an output slot and by specifying the requisite css class for the element (in this case either shiny-text-output, shiny-plot-output, or shiny-html-output).

With this technique you can create highly customized user-interfaces using whatever HTML, CSS, and JavaScript you like.

### Server Script

All of the changes from the original Tabsets application were to the user-interface, the server script remains the same:

#### server.R

{% highlight r %}
library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output) {

  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The output renderers defined 
  # below then all used the value computed from this expression
  data <- reactive({  
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)

    dist(input$n)
  })

  # Generate a plot of the data. Also uses the inputs to build the 
  # plot label. Note that the dependencies on both the inputs and
  # the data reactive expression are both tracked, and all expressions 
  # are called in the sequence implied by the dependency graph
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n

    hist(data(), 
         main=paste('r', dist, '(', n, ')', sep=''))
  })

  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(data())
  })

  # Generate an HTML table view of the data
  output$table <- renderTable({
    data.frame(x=data())
  })
})
{% endhighlight %}
