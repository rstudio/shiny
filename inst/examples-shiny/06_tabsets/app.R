library(shiny)
library(bslib)

# Define UI for random distribution app ----
# Sidebar layout with input and output definitions ----
ui <- page_sidebar(

  # App title ----
  title = "Tabsets",

  # Sidebar panel for inputs ----
  sidebar = sidebar(

    # Input: Select the random distribution type ----
    radioButtons(
      "dist",
      "Distribution type:",
      c(
        "Normal" = "norm",
        "Uniform" = "unif",
        "Log-normal" = "lnorm",
        "Exponential" = "exp"
      )
    ),
    # br() element to introduce extra vertical spacing ----
    br(),
    # Input: Slider for the number of observations to generate ----
    sliderInput(
      "n",
      "Number of observations:",
      value = 500,
      min = 1,
      max = 1000
    )
  ),

  # Main panel for displaying outputs ----
  # Output: A tabset that combines three panels ----
  navset_card_underline(
    # Panel with plot ----
    nav_panel("Plot", plotOutput("plot")),

    # Panel with summary ----
    nav_panel("Summary", verbatimTextOutput("summary")),

    # Panel with table ----
    nav_panel("Table", tableOutput("table"))
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {

  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  d <- reactive({
    dist <- switch(
      input$dist,
      norm = rnorm,
      unif = runif,
      lnorm = rlnorm,
      exp = rexp,
      rnorm
    )

    dist(input$n)
  })

  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n

    hist(
      d(),
      main = paste("r", dist, "(", n, ")", sep = ""),
      col = "#75AADB",
      border = "white"
    )
  })

  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(d())
  })

  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    d()
  })
}

# Create Shiny app ----
shinyApp(ui, server)
