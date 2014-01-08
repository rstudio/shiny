This small Shiny application demonstrates Shiny's automatic UI updates. Move the
*Number of observations* slider and notice how the `renderPlot` expression is
automatically re-evaluated when its dependant, `input$obs`, changes, causing a
new distribution to be generated and the plot to be rendered.

