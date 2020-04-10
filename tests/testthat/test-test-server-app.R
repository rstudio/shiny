context("testServer app")

library(shiny)
library(testthat)

test_that("testServer works with dir app", {
  # app.R
  testServer(test_path("..", "test-modules", "06_tabsets"), {
    session$setInputs(dist="norm", n=5)
    expect_length(d(), 5)

    session$setInputs(dist="unif", n=6)
    expect_length(d(), 6)
  })

  # server.R
  testServer(test_path("..", "test-modules", "server_r"), {
    session$setInputs(dist="norm", n=5)
    expect_length(d(), 5)

    session$setInputs(dist="unif", n=6)
    expect_length(d(), 6)
  })
})

test_that("testServer works when referencing external globals", {
  # If global is defined at the top of app.R outside of the server function.
  testServer(test_path("..", "test-modules", "06_tabsets"), {
    expect_equal(get("global", session$env), 123)
  })
})

test_that("runApp works with a dir app that calls modules and uses testServer", {
  app <- test_path("..", "test-modules", "12_counter")
  run <- runTests(app)
  expect_true(all(run$pass))
})

test_that("a Shiny app object with a module inside can be tested", {

  counterUI <- function(id, label = "Counter") {
    ns <- NS(id)
    tagList(
      actionButton(ns("button"), label = label),
      verbatimTextOutput(ns("out"))
    )
  }

  counterServer <- function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        count <- reactiveVal(0)
        observeEvent(input$button, {
          count(count() + 1)
        })
        output$out <- renderText({
          count()
        })
        count
      }
    )
  }

  ui <- fluidPage(
    textInput("number", "A number"),
    textOutput("numberDoubled"),
    counterUI("counter1", "Counter #1"),
    counterUI("counter2", "Counter #2")
  )
  server <- function(input, output, session) {
    counterServer("counter1")
    counterServer("counter2")
    doubled <- reactive( { as.integer(input$number) * 2 })
    output$numberDoubled <- renderText({ doubled() })
  }
  app <- shinyApp(ui, server)

  testServer(app, {
    session$setInputs(number = "42")
    expect_equal(doubled(), 84)
  })
})
