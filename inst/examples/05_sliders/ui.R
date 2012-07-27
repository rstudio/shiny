library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Sliders"),
  
  sidebarPanel(
    sliderInput("integer", "Integer:", 0, 1000, 500),
    sliderInput("decimal", "Decimal:", 0, 1, 0.5, step= 0.1, round=2),
    sliderInput("range", "Range:", 1, 1000, c(200,500)),
    sliderInput("format", "Custom Format:", 0, 100000, 50000, step = 25000,
                format="$#,##0", locale="us"),   
    sliderInput("animation", "Animation:", 1, 2000, 1, step = 10,
                animationInterval = 300),
    helpText("Use the Play button to animate values.")
  ),
  
  mainPanel(
    tableOutput("values")
  )
))