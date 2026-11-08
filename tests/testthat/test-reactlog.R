keyValList <- function(key, value) {
  ret <- list()
  ret[[key]] <- value
  ret
}
# Capture str() output the same way RLog$valueStr() does. str() changed its
# output format in R r90805 (2026-10); computing the expected value strings
# dynamically keeps these tests passing on all R versions.
str_value <- function(value) {
  paste(utils::capture.output(utils::str(value, max.level = 1)), collapse = "\n")
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

test_that("rLog resets when options are FALSE", {

  withOption("shiny.reactlog", FALSE, FALSE, {
    withOption("shiny.reactlog.console", FALSE, FALSE, {
      rLog$reset()

      # check for dummy and no reactid information
      expect_true(!is.null(rLog$noReactId))
      expect_true(!is.null(rLog$dummyReactId))
      expect_equal(rLog$msg$getReact(rLog$noReactId, force = TRUE)$reactId, rLog$noReactId)
      expect_equal(rLog$msg$getReact(rLog$dummyReactId, force = TRUE)$reactId, rLog$dummyReactId)
      expect_equal(length(rLog$msg$reactCache), 2)
    })
  })

})

test_that("message logger appears", {

  withLogging({

    expect_logs(
      {
        val <- reactiveVal(1, label = "val")
      },
      paste0("- define: r1:'val' - reactiveVal '", str_value(1), "'")
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
      paste0("- define: r3:'reactive({\\n    val() + values$a\\n})' - observable '", str_value(NULL), "'")
    )

    expect_logs(
      {
        react()
      },
      "- createContext: ctxDummy - isolate",
      "- dependsOn: rDummyReactId:'DummyReactId' on r3:'reactive({\\n    val() + values$a\\n})' in ctxDummy",
      "- createContext: ctx1 - observable",
      "- enter: r3:'reactive({\\n    val() + values$a\\n})' in ctx1 - observable",
      "= - dependsOn: r3:'reactive({\\n    val() + values$a\\n})' on r1:'val' in ctx1",
      paste0("= - define: r2$a:'values$a' - reactiveValuesKey '", str_value(2), "'"),
      "= - dependsOn: r3:'reactive({\\n    val() + values$a\\n})' on r2$a:'values$a' in ctx1",
      "- exit: r3:'reactive({\\n    val() + values$a\\n})' in ctx1 - observable"
    )

    expect_logs(
      {
        val(4)
      },
      paste0("- valueChange: r1:'val' '", str_value(4), "'"),
      "- invalidateStart: r1:'val'",
      "= - invalidateStart: r3:'reactive({\\n    val() + values$a\\n})' in ctx1 - observable",
      "= = - isolateInvalidateStart: rDummyReactId:'DummyReactId' in ctxDummy",
      "= = = - dependsOnRemove: rDummyReactId:'DummyReactId' on r3:'reactive({\\n    val() + values$a\\n})' in ctxDummy",
      "= = - isolateInvalidateEnd: rDummyReactId:'DummyReactId' in ctxDummy",
      "= = - dependsOnRemove: r3:'reactive({\\n    val() + values$a\\n})' on r1:'val' in ctx1",
      "= = - dependsOnRemove: r3:'reactive({\\n    val() + values$a\\n})' on r2$a:'values$a' in ctx1",
      "= - invalidateEnd: r3:'reactive({\\n    val() + values$a\\n})' in ctx1 - observable",
      "- invalidateEnd: r1:'val'"
    )

    expect_logs(
      {values$a <- 5},
      paste0("- valueChange: r2$a:'values$a' '", str_value(5), "'"),
      "- invalidateStart: r2$a:'values$a'",
      "- invalidateEnd: r2$a:'values$a'"
    )

  })

})


test_that("reactlog_version is as expected", {
  expect_match(
    packageDescription("shiny")$Suggests,
    # The space between reactlog and the version number can include \n
    sprintf("\\breactlog\\s+\\Q(>= %s)\\E", reactlog_min_version)
  )
})
