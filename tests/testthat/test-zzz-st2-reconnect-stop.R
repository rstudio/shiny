skip_if_not_shinytest2()
library(shinytest2)

test_that("allowReconnect(FALSE) queued behind a slow handler still stops retries", {
  app <- launch_test_app(function() {
    library(shiny)
    ui <- fluidPage(
      tags$script(HTML("
        window.connects = 0;
        $(document).on('shiny:connected', function() { window.connects++; });
        Shiny.addCustomMessageHandler('slow', function(x) {
          return new Promise(function(resolve) { setTimeout(resolve, 4000); });
        });
      ")),
      actionButton("stop", "Stop")
    )
    server <- function(input, output, session) {
      session$allowReconnect("force")
      observeEvent(input$stop, {
        session$sendCustomMessage("slow", 1)
        session$allowReconnect(FALSE)
        session$close()
      })
    }
    shinyApp(ui, server)
  })
  expect_equal(app$get_js("window.connects"), 1)
  app$click("stop", wait_ = FALSE)
  Sys.sleep(7)                                  # past the first retry delay (1.5 s) and the slow handler
  expect_equal(app$get_js("window.connects"), 1)
  expect_false(app$get_js("Shiny.shinyapp.isConnected()"))
  expect_equal(app$get_js("document.querySelectorAll('#shiny-reconnect-text').length"), 0)
})

test_that("a stop arriving while the reconnect notice renders leaves no notice and no retry", {
  app <- launch_test_app(function() {
    library(shiny)
    ui <- fluidPage(
      tags$script(HTML("
        window.connects = 0;
        $(document).on('shiny:connected', function() { window.connects++; });
        // Queued during the synchronous shiny:disconnected trigger inside the
        // socket's onclose: it runs after onDisconnected has scheduled the retry
        // and started the (async) notice, before the notice's first await resumes.
        $(document).on('shiny:disconnected', function() {
          queueMicrotask(function() { Shiny.shinyapp.$allowReconnect = false; });
        });
      ")),
      actionButton("close", "Close")
    )
    server <- function(input, output, session) {
      session$allowReconnect("force")
      observeEvent(input$close, session$close())
    }
    shinyApp(ui, server)
  })
  expect_equal(app$get_js("window.connects"), 1)
  app$click("close", wait_ = FALSE)
  Sys.sleep(3)                                  # past the first retry delay (1.5 s)
  expect_equal(app$get_js("window.connects"), 1)
  expect_equal(app$get_js("document.querySelectorAll('#shiny-reconnect-text').length"), 0)
})
