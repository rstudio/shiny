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

test_that("errors in throttled/debounced reactives are catchable", {
  reactiveConsole(TRUE)
  on.exit(reactiveConsole(FALSE))

  # In Shiny 1.7 and earlier, if a throttled/debounced reactive threw an error,
  # it would cause internal observers used by the implementations of
  # debounce/throttle to error, which would kill the session. The correct
  # behavior is to only expose the error to consumers of the throttled/debounced
  # reactive.

  r <- reactive({
    stop("boom")
  })

  rd <- r %>% debounce(1000)
  rt <- r %>% throttle(1000)

  observe({
    try(rd(), silent = TRUE)
    try(rt(), silent = TRUE)
  })

  expect_silent(flushReact())
})
