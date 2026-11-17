skip_if_not_shinytest2()
library(shinytest2)

# library(shiny) must be called explicitly inside the function so that
# shinytest2's library shim loads the dev package via pkgload::load_all().
app <- launch_test_app(function() {
  library(shiny)

  ui <- fluidPage(
    textInput("txt", "Text"),
    selectInput("pick", "Pick", c("a", "b")),
    tagAppendAttributes(
      textInput("locked", "Locked"),
      `data-shiny-drive-access` = "read-only"
    ),
    numericInput("delay", "Delay", 0),
    textOutput("slow")
  )

  server <- function(input, output) {
    output$slow <- renderText({
      Sys.sleep(input$delay)
      paste("delay", input$delay)
    })
  }

  shinyApp(ui, server)
})

# A drive call's error `code`, or "ok" when it doesn't throw
drive_code <- function(call) {
  app$get_js(paste0(
    "(async () => { try { await ", call, "; return 'ok'; }",
    " catch (e) { return e.name + ':' + e.code; } })()"
  ))
}

test_that("Shiny.drive has an API version", {
  expect_equal(app$get_js("Shiny.drive.version"), 1)
})

test_that("ready() resolves once the app has started and updated", {
  expect_equal(drive_code("Shiny.drive.ready()"), "ok")
  expect_equal(
    app$get_js("Shiny.drive.getInput('pick').value"),
    "a"
  )
})

test_that("errors carry a code", {
  expect_equal(
    drive_code("Shiny.drive.setInput('nope', 1)"),
    "ShinyDriveError:not-found"
  )
  expect_equal(
    drive_code("Shiny.drive.setInput('pick', 'z')"),
    "ShinyDriveError:invalid-value"
  )
  expect_equal(
    drive_code("Shiny.drive.setInput('locked', 'x')"),
    "ShinyDriveError:read-only"
  )
  expect_equal(
    drive_code("Shiny.drive.click('txt')"),
    "ShinyDriveError:wrong-type"
  )
  expect_equal(
    drive_code("Shiny.drive.setDefaultTimeout(-5)"),
    "ShinyDriveError:invalid-argument"
  )
})

test_that("not-found messages don't name WebMCP tools", {
  msg <- app$get_js(
    "(async () => { try { await Shiny.drive.setInput('nope', 1) } catch (e) { return e.message } })()"
  )
  expect_no_match(msg, "list_inputs")
})

test_that("waits have no time limit by default", {
  out <- app$get_js(
    "Shiny.drive.setInput('delay', 1.5).then(() => Shiny.drive.getOutput('slow'))"
  )
  expect_equal(out$text, "delay 1.5")
  expect_equal(out$status, "ready")
})

test_that("setDefaultTimeout() makes slow waits reject with a timeout", {
  on.exit(app$run_js("Shiny.drive.setDefaultTimeout(null)"), add = TRUE)
  app$run_js("Shiny.drive.setDefaultTimeout(300)")

  expect_equal(
    drive_code("Shiny.drive.setInput('delay', 2)"),
    "ShinyDriveError:timeout"
  )
  # A `timeout` option wins over the default
  expect_equal(
    drive_code("Shiny.drive.waitForIdle({ timeout: 10000 })"),
    "ok"
  )
  expect_equal(app$get_js("Shiny.drive.getOutput('slow')")$text, "delay 2")
})
