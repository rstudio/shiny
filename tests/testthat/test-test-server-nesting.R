#test_that("testModule works with nested modules", {
#  outerModule <- function(input, output, session) {
#    r1 <- reactive({ input$x + 1})
#    r2 <- callModule(innerModule, "innerModule", r1)
#    output$someVar <- renderText(r2())
#  }

#  innerModule <- function(input, output, session, r) {
#    reactive(paste("a value:", r()))
#  }

#  testModule(outerModule, {
#    session$setInputs(x = 1)
#    expect_equal(output$someVar, "a value: 2")
#  })
#})

#test_that("testModule calls can be nested", {
#  outerModule <- function(input, output, session) {
#    doubled <- reactive({ input$x * 2 })
#    innerModule <- function(input, output, session) {
#      quadrupled <- reactive({ doubled() * 2 })
#    }
#  }

#  testModule(outerModule, {
#    session$setInputs(x = 1)
#    expect_equal(doubled(), 2)
#    testModule(innerModule, {
#      expect_equal(quadrupled(), 4)
#    })
#  })
#})
