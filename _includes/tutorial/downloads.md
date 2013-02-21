![Downloading Data Screenshot](screenshots/downloads.png)

The examples so far have demonstrated outputs that appear directly in the page, such as plots, tables, and text boxes. Shiny also has the ability to offer file downloads that are calculated on the fly, which makes it easy to build data exporting features.

To run the example below, type:

{% highlight console %}
> library(shiny)
> runExample("10_download")
{% endhighlight %}

You define a download using the `downloadHandler` function on the server side, and either `downloadButton` or `downloadLink` in the UI:

#### ui.R

{% highlight r %}
shinyUI(pageWithSidebar(
  headerPanel('Download Example'),
  sidebarPanel(
    selectInput("dataset", "Choose a dataset:", 
                choices = c("rock", "pressure", "cars")),
    downloadButton('downloadData', 'Download')
  ),
  mainPanel(
    tableOutput('table')
  )
))
{% endhighlight %}

#### server.R

{% highlight r %}
shinyServer(function(input, output) {
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  output$table <- renderTable({
    datasetInput()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$dataset, '.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
})
{% endhighlight %}

As you can see, `downloadHandler` takes a `filename` argument, which tells the web browser what filename to default to when saving. This argument can either be a simple string, or it can be a function that returns a string (as is the case here).

The `content` argument must be a function that takes a single argument, the file name of a non-existent temp file. The `content` function is responsible for writing the contents of the file download into that temp file.

Both the `filename` and `content` arguments can use reactive values and expressions (although in the case of `filename`, be sure your argument is an actual function; `filename = paste(input$dataset, '.csv')` is not going to work the way you want it to, since it is evaluated only once, when the download handler is being defined).

Generally, those are the only two arguments you'll need. There is an optional `contentType` argument; if it is `NA` or `NULL`, Shiny will attempt to guess the appropriate value based on the filename. Provide your own content type string (e.g. `"text/plain"`) if you want to override this behavior.