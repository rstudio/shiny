## Getting Non-Input Data From the Client

On the server side, Shiny applications use the `input` object to receive user input from the client web browser. The values in `input` are set by UI objects on the client web page. There are also non-input values (in the sense that the user doesn't enter these values through UI components) that are stored in an object called `clientData`. These values include the URL, the pixel ratio (for high-resolution "Retina" displays), the hidden state of output objects, and the height and width of plot outputs.

### Using clientData

To access `clientData` values, you need to pass a function to `shinyServer()` that takes `clientData` as an argument. Once it's in there, you can access `clientData` just as you would `input`.

In the example below, the client browser will display out the components of the URL and also parse and print the query/search string (the part of the URL after a "`?`"):

#### server.R

{% highlight r %}
shinyServer(function(input, output, clientData) {

  # Return the components of the URL in a string:
  output$urlText <- renderText({
    paste(sep = "",
      "protocol: ", clientData$url_protocol, "\n",
      "hostname: ", clientData$url_hostname, "\n",
      "pathname: ", clientData$url_pathname, "\n",
      "port: ",     clientData$url_port,     "\n",
      "search: ",   clientData$url_search,   "\n"
    )
  })

  # Parse the GET query string
  output$queryText <- renderText({
    query <- parseQueryString(clientData$url_search)

    # Return a string with key-value pairs
    paste(names(query), query, sep = "=", collapse=", ")
  })
})
{% endhighlight %}


##### ui.R

{% highlight r %}
shinyUI(bootstrapPage(
  h3("URL components"),
  verbatimTextOutput("urlText"),

  h3("Parsed query string"),
  verbatimTextOutput("queryText")
))
{% endhighlight %}

This app will display the following:

![URL components](screenshots/url-components.png)


### Viewing all available values in clientData

The values in `clientData` will depend to some extent on the outputs. For example, a plot output object will report its height, width, and hidden status. The app below has a plot output, and displays all the values in `clientData`:

{% highlight r %}
shinyServer(function(input, output, clientData) {

  # Values from clientData returned as text
  output$clientdataText <- renderText({
    cnames <- names(clientData)

    allvalues <- lapply(cnames, function(name) {
      paste(name, clientData[[name]], sep=" = ")
    })
    paste(allvalues, collapse = "\n")
  })

  # A histogram
  output$myplot <- renderPlot({
    hist(rnorm(input$obs), main="Generated in renderPlot()")
  })
})
{% endhighlight %}

Notice that, just as with `input`, values in `clientData` can be accessed with `clientData$myvar` or `clientData[['myvar']]`.

##### ui.R

{% highlight r %}
shinyUI(pageWithSidebar(
  headerPanel("Shiny Client Data"),
  sidebarPanel(
    sliderInput("obs", "Number of observations:",
                min = 0, max = 1000, value = 500)
  ),
  mainPanel(
    h3("clientData values"),
    verbatimTextOutput("clientdataText"),
    plotOutput("myplot")
  )
))
{% endhighlight %}

For the plot output `output$myplot`, there are three entries in `clientData`:

* `output_myplot_height`: The height of the plot on the web page, in pixels.
* `output_myplot_width`: The width of the plot on the web page, in pixels.
* `output_myplot_hidden`: If the object is hidden (not visible), this is TRUE. This is used because Shiny will by default suspend the output object when it is hidden. When suspended, the observer will not execute even when its inputs change.

Here is the view from the client, with all the `clientData` values:

![All clientData values](screenshots/clientdata-all.png)
