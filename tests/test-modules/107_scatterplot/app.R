library(shiny)
library(ggplot2)


ui <- fixedPage(
  h2("Module example"),
  linkedScatterUI("scatters"),
  textOutput("summary")
)

server <- function(input, output, session) {
  df <- linkedScatterServer(
    "scatters",
    reactive(mpg), # data
    left = reactive(c("cty", "hwy")),
    right = reactive(c("drv", "hwy"))
  )

  output$summary <- renderText({
    sprintf("%d observation(s) selected", sum(df()$selected_))
  })
}

shinyApp(ui, server)
