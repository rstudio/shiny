context("testModule-moduleServer")

test_that("New-style modules work", {
  counterServer <- local({
    function(id) {
      moduleServer(id, function(input, output, session) {
        count <- reactiveVal(0)
        observeEvent(input$button, {
          count(count() + 1)
        })
        output$out <- renderText({
          count()
        })
        count
      })
    }
  })
  testModule(counterServer, {
    input$setInputs(button = 0)
    input$setInputs(button = 1)
    expect_equal(count(), 1)
  }, id = "foob")
})
