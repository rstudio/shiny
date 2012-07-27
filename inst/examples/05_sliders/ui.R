library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Sliders"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    # Simple integer interval
    sliderInput("integer", "Integer:", 
                min=0, max=1000, value=500),
    
    # Decimal interval with step value
    sliderInput("decimal", "Decimal:", 
                min = 0, max = 1, value = 0.5, step= 0.1),
   
    # Specification of range within an interval
    sliderInput("range", "Range:",
                min = 1, max = 1000, value = c(200,500)),
    
    # Provide a custom currency format for value display
    sliderInput("format", "Custom Format:", 
                min = 0, max = 100000, value = 50000, step = 25000,
                format="$#,##0", locale="us"),  
    
    # Animation with custom interval (in milliseconds) to control speed
    # (also provide helpText to highlight the possiblity of animation)
    sliderInput("animation", "Animation:", 1, 2000, 1, step = 10,
                animationInterval = 300),
    helpText("Use the Play button to animate values.")
  ),
  
  # Show a table summarizing the values entered
  mainPanel(
    tableOutput("values")
  )
))