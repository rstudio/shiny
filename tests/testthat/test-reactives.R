test_that("can access reactive values directly", {
  reactiveConsole(TRUE)
  on.exit(reactiveConsole(FALSE))

  x1 <- reactiveVal(1)
  x1(2)
  expect_equal(x1(), 2)

  x2 <- reactiveValues(a = 1)
  x2$a <- 2
  expect_equal(x2$a, 2)

  y <- reactive(x1() + x2$a)
  expect_equal(y(), 4)
})
