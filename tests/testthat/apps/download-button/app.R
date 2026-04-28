library(shiny)

# This app covers the three `enabled` values for both downloadButton and downloadLink,
# plus a simulated shinyjs-disabled case, with toggle switches for each scenario.

# Create a downloadHandler that just serves a simple text file for testing.
handler <- function() {
  downloadHandler(
    filename = "test.txt",
    content = function(file) writeLines("test", file)
  )
}

ui <- fluidPage(
  h3("downloadButton"),

  uiOutput("btn_auto_ui"),
  bslib::input_switch("toggle_btn_auto", "Enabled", value = TRUE),

  uiOutput("btn_off_ui"),
  bslib::input_switch("toggle_btn_off", "Enabled", value = FALSE),

  uiOutput("btn_on_ui"),
  bslib::input_switch("toggle_btn_on", "Enabled", value = TRUE),

  # This mimics what happens when a download button is wrapped in a
  # shinyjs::disabled() call within the UI (and therefore at render time).
  uiOutput("btn_shinyjs_ui"),
  bslib::input_switch("toggle_btn_shinyjs", "Enabled", value = FALSE),

  h3("downloadLink"),

  uiOutput("lnk_auto_ui"),
  bslib::input_switch("toggle_lnk_auto", "Enabled", value = TRUE),

  uiOutput("lnk_off_ui"),
  bslib::input_switch("toggle_lnk_off", "Enabled", value = FALSE),

  uiOutput("lnk_on_ui"),
  bslib::input_switch("toggle_lnk_on", "Enabled", value = TRUE),

  uiOutput("lnk_shinyjs_ui"),
  bslib::input_switch("toggle_lnk_shinyjs", "Enabled", value = FALSE)
)

server <- function(input, output, session) {
  output$btn_auto <- handler()
  output$btn_off <- handler()
  output$btn_on <- handler()
  output$btn_shinyjs <- handler()
  output$lnk_auto <- handler()
  output$lnk_off <- handler()
  output$lnk_on <- handler()
  output$lnk_shinyjs <- handler()

  output$btn_auto_ui <- renderUI({
    if (isTRUE(input$toggle_btn_auto)) {
      downloadButton("btn_auto", "Auto (default)")
    } else {
      downloadButton("btn_auto", "Auto (default)", enabled = FALSE)
    }
  })

  output$btn_off_ui <- renderUI({
    if (isTRUE(input$toggle_btn_off)) {
      downloadButton("btn_off", "Disabled", enabled = TRUE)
    } else {
      downloadButton("btn_off", "Disabled", enabled = FALSE)
    }
  })

  output$btn_on_ui <- renderUI({
    if (isTRUE(input$toggle_btn_on)) {
      downloadButton("btn_on", "Pre-enabled", enabled = TRUE)
    } else {
      downloadButton("btn_on", "Pre-enabled", enabled = FALSE)
    }
  })

  output$btn_shinyjs_ui <- renderUI({
    btn <- downloadButton("btn_shinyjs", "shinyjs-disabled")
    if (!isTRUE(input$toggle_btn_shinyjs)) {
      htmltools::tagAppendAttributes(btn, class = "shinyjs-disabled")
    } else {
      btn
    }
  })

  output$lnk_auto_ui <- renderUI({
    if (isTRUE(input$toggle_lnk_auto)) {
      downloadLink("lnk_auto", "Auto (default)")
    } else {
      downloadLink("lnk_auto", "Auto (default)", enabled = FALSE)
    }
  })

  output$lnk_off_ui <- renderUI({
    if (isTRUE(input$toggle_lnk_off)) {
      downloadLink("lnk_off", "Disabled", enabled = TRUE)
    } else {
      downloadLink("lnk_off", "Disabled", enabled = FALSE)
    }
  })

  output$lnk_on_ui <- renderUI({
    if (isTRUE(input$toggle_lnk_on)) {
      downloadLink("lnk_on", "Pre-enabled", enabled = TRUE)
    } else {
      downloadLink("lnk_on", "Pre-enabled", enabled = FALSE)
    }
  })

  output$lnk_shinyjs_ui <- renderUI({
    lnk <- downloadLink("lnk_shinyjs", "shinyjs-disabled")
    if (!isTRUE(input$toggle_lnk_shinyjs)) {
      htmltools::tagAppendAttributes(lnk, class = "shinyjs-disabled")
    } else {
      lnk
    }
  })
}

shinyApp(ui, server)
