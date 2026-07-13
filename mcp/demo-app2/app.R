library(shiny)

# Second demo app: served behind the same MCP gateway as demo-app, to show
# one connector exposing multiple Shiny apps. The appId keeps its internal
# tunnel tools and resource URI distinct from the other apps'.
options(shiny.mcp = TRUE)
options(shiny.mcp.appId = "cars")
options(shiny.mcp.tool = list(
  name = "open_cars_app",
  description = paste(
    "Open the mtcars explorer app. Optionally pass `xvar` and `yvar`",
    "(column names of mtcars) to set the initial axes."
  ),
  inputSchema = list(
    type = "object",
    properties = list(
      xvar = list(type = "string", description = "X axis column of mtcars"),
      yvar = list(type = "string", description = "Y axis column of mtcars")
    )
  )
))

vars <- names(mtcars)

shinyApp(
  ui = fluidPage(
    titlePanel("mtcars explorer"),
    sidebarLayout(
      sidebarPanel(
        selectInput("xvar", "X", vars, selected = "wt"),
        selectInput("yvar", "Y", vars, selected = "mpg"),
        checkboxInput("smooth", "Add trend line", TRUE),
        # Dynamic-dependency demo: plotly's JS/CSS are not part of the page
        # until this is checked; in MCP sessions they arrive inlined over
        # the reactive channel (the sandbox can't fetch them over HTTP).
        checkboxInput("show_plotly", "Show interactive (plotly) plot", FALSE)
      ),
      mainPanel(
        uiOutput("plotly_ui"),
        plotOutput("scatter"),
        textOutput("cor")
      )
    )
  ),
  server = function(input, output, session) {
    observe({
      ti <- mcpToolInput()
      if (!is.null(ti$xvar) && ti$xvar %in% vars) {
        updateSelectInput(session, "xvar", selected = ti$xvar)
      }
      if (!is.null(ti$yvar) && ti$yvar %in% vars) {
        updateSelectInput(session, "yvar", selected = ti$yvar)
      }
    })

    output$scatter <- renderPlot({
      x <- mtcars[[input$xvar]]
      y <- mtcars[[input$yvar]]
      plot(x, y, xlab = input$xvar, ylab = input$yvar, pch = 19,
        col = "steelblue")
      if (isTRUE(input$smooth)) {
        abline(stats::lm(y ~ x), col = "tomato", lwd = 2)
      }
    })

    output$plotly_ui <- renderUI({
      req(input$show_plotly)
      plotly::plotlyOutput("plotly_scatter", height = "300px")
    })

    output$plotly_scatter <- plotly::renderPlotly({
      req(input$show_plotly)
      plotly::plot_ly(
        mtcars,
        x = ~ get(input$xvar), y = ~ get(input$yvar),
        type = "scatter", mode = "markers",
        text = rownames(mtcars)
      ) |>
        plotly::layout(
          xaxis = list(title = input$xvar),
          yaxis = list(title = input$yvar)
        )
    })

    output$cor <- renderText({
      sprintf(
        "cor(%s, %s) = %.3f",
        input$xvar, input$yvar,
        stats::cor(mtcars[[input$xvar]], mtcars[[input$yvar]])
      )
    })

    observe({
      mcpUpdateModelContext(
        text = sprintf(
          "The user is viewing mtcars: %s vs %s.",
          input$xvar, input$yvar
        ),
        data = list(xvar = input$xvar, yvar = input$yvar)
      )
    })
  }
)
