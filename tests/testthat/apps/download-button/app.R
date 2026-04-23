library(shiny)

# This app covers the three `enabled` values for both downloadButton and downloadLink,
# plus a simulated shinyjs-disabled case, with toggle buttons for each scenario.

# Create a downloadHandler that just serves a simple text file for testing.
handler <- function() {
  downloadHandler(
    filename = "test.txt",
    content = function(file) writeLines("test", file)
  )
}

# Mirrors what shinyjs::enable() / shinyjs::disable() does: adds/removes the
# shinyjs-disabled class and the standard disabled attributes. This lets us test
# that the download buttons/links respond to external JS changes to their enabled state,
# without making us require shinyjs.
set_enabled_js <- HTML(
  "
  Shiny.addCustomMessageHandler('setEnabled', function(data) {
    var el = document.getElementById(data.id);
    if (data.enabled) {
      el.classList.remove('disabled');
      el.classList.remove('shinyjs-disabled');
      el.removeAttribute('aria-disabled');
      el.removeAttribute('tabindex');
    } else {
      el.classList.add('disabled');
      el.classList.add('shinyjs-disabled');
      el.setAttribute('aria-disabled', 'true');
      el.setAttribute('tabindex', '-1');
    }
  });
"
)

ui <- fluidPage(
  tags$script(set_enabled_js),
  # Block of download Button tests

  h3("downloadButton"),

  downloadButton("btn_auto", "Auto (default)"),
  actionButton("toggle_btn_auto", "Toggle"),

  downloadButton("btn_off", "Disabled", enabled = FALSE),
  actionButton("toggle_btn_off", "Toggle"),

  downloadButton("btn_on", "Pre-enabled", enabled = TRUE),
  actionButton("toggle_btn_on", "Toggle"),

  # This mimics what happens when a download button is wrapped in a
  # shinyjs::disabled() call within the UI (and therefore at render time).
  htmltools::tagAppendAttributes(
    downloadButton("btn_shinyjs", "shinyjs-disabled"),
    class = "shinyjs-disabled"
  ),
  actionButton("toggle_btn_shinyjs", "Toggle"),

  # Block of download Link tests
  h3("downloadLink"),

  downloadLink("lnk_auto", "Auto (default)"),
  actionButton("toggle_lnk_auto", "Toggle"),

  downloadLink("lnk_off", "Disabled", enabled = FALSE),
  actionButton("toggle_lnk_off", "Toggle"),

  downloadLink("lnk_on", "Pre-enabled", enabled = TRUE),
  actionButton("toggle_lnk_on", "Toggle"),

  htmltools::tagAppendAttributes(
    downloadLink("lnk_shinyjs", "shinyjs-disabled"),
    class = "shinyjs-disabled"
  ),
  actionButton("toggle_lnk_shinyjs", "Toggle")
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

  # Each reactiveVal tracks the current intended state, starting from
  # the post-render-value state (auto/on start enabled; off/shinyjs start disabled).
  make_toggle <- function(id, initial_enabled) {
    enabled <- reactiveVal(initial_enabled)
    observeEvent(input[[paste0("toggle_", id)]], {
      new_state <- !enabled()
      enabled(new_state)
      # This mimics what shinyjs::enable()/disable() would do, which is to send
      # a message to the client to update the button's enabled state via JS.
      session$sendCustomMessage(
        "setEnabled",
        list(id = id, enabled = new_state)
      )
    })
  }

  make_toggle("btn_auto", TRUE)
  make_toggle("btn_off", FALSE)
  make_toggle("btn_on", TRUE)
  make_toggle("btn_shinyjs", FALSE)
  make_toggle("lnk_auto", TRUE)
  make_toggle("lnk_off", FALSE)
  make_toggle("lnk_on", TRUE)
  make_toggle("lnk_shinyjs", FALSE)
}

shinyApp(ui, server)
