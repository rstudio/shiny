
context("reactlog")

keyValList <- function(key, value) {
  ret <- list()
  ret[[key]] <- value
  ret
}
withOption <- function(key, value, oldVal = NULL, expr) {
  oldVal <- getOption(key, oldVal)
  do.call("options", keyValList(key, value))
  on.exit({
    do.call("options", keyValList(key, oldVal))
  })
  force(expr)
}

withLogging <- function(expr) {
  rLog$reset()
  # reset ctx counter
  reactiveEnvr <- .getReactiveEnvironment()
  reactiveEnvr$.nextId <- 0L

  withOption("shiny.reactlog", TRUE, FALSE, {
    withOption("shiny.reactlog.console", TRUE, FALSE, {
      withOption("shiny.suppressMissingContextError", TRUE, FALSE, {
        force(expr)
      })
    })
  })
}

expect_logs <- function(expr, ...) {
  expected_messages <- unlist(list(...))
  captured_messages <- capture_messages(expr)
  captured_messages <- sub("\n$", "", captured_messages)
  if (length(captured_messages) != length(expected_messages)) {
    cat("\nCaptured: \n"); print(captured_messages)
    cat("Expected: \n"); print(expected_messages)
  }
  expect_equal(
    captured_messages,
    expected_messages
  )
}

test_that("message logger appears", {

  withLogging({

    expect_logs(
      {
        val <- reactiveVal(1, label = "val")
      },
      "- define: r1:val - reactiveVal"
    )
    expect_silent(
      {
        values <- reactiveValues(a = 2, b = 3)
        local({
          values_obj <- .subset2(values, 'impl')
          values_obj$.label <- "values"
        })
      }
    )
    expect_logs(
      {
        react <- reactive(val() + values$a)
      },
      "- define: r3:reactive(val() + values$a) - observable"
    )

    expect_logs(
      {
        react()
      },
      "- createContext: ctxDummy - isolate",
      "- dependsOn: rNoCtxReactId:NoCtxReactId on r3:reactive(val() + values$a) in ctxDummy",
      "- createContext: ctx1 - observable",
      "- enter: r3:reactive(val() + values$a) in ctx1 - observable",
      "= - dependsOn: r3:reactive(val() + values$a) on r1:val in ctx1",
      "= - define: r2$a:values$a - reactiveValuesKey",
      "= - dependsOn: r3:reactive(val() + values$a) on r2$a:values$a in ctx1",
      "- exit: r3:reactive(val() + values$a) in ctx1 - observable"
    )

    expect_logs(
      {
        val(4)
      },
      "- valueChange: r1:val",
      "- invalidateStart: r1:val",
      "= - invalidateStart: r3:reactive(val() + values$a) in ctx1 - observable",
      "= = - isolateInvalidateStart: rNoCtxReactId:NoCtxReactId in ctxDummy",
      "= = = - dependsOnRemove: rNoCtxReactId:NoCtxReactId on r3:reactive(val() + values$a) in ctxDummy",
      "= = - isolateInvalidateEnd: rNoCtxReactId:NoCtxReactId in ctxDummy",
      "= = - dependsOnRemove: r3:reactive(val() + values$a) on r1:val in ctx1",
      "= = - dependsOnRemove: r3:reactive(val() + values$a) on r2$a:values$a in ctx1",
      "= - invalidateEnd: r3:reactive(val() + values$a) in ctx1 - observable",
      "- invalidateEnd: r1:val"
    )

    expect_logs(
      {values$a <- 5},
      "- valueChange: r2$a:values$a",
      "- invalidateStart: r2$a:values$a",
      "- invalidateEnd: r2$a:values$a"
    )

  })

})
