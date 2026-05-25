test_that("withOtelCollect sets collection level temporarily", {
  # Save original option
  original <- getOption("shiny.otel.collect")
  on.exit(options(shiny.otel.collect = original), add = TRUE)

  # Set a baseline option
  options(shiny.otel.collect = "all")

  # Test that withOtelCollect temporarily changes the option
  result <- withOtelCollect("none", {
    getOption("shiny.otel.collect")
  })

  expect_equal(result, "none")

  # Verify option is restored after expression
  expect_equal(getOption("shiny.otel.collect"), "all")
})

test_that("withOtelCollect returns value of expr", {
  result <- withOtelCollect("none", {
    42
  })

  expect_equal(result, 42)

  # Test with more complex return value
  result <- withOtelCollect("reactivity", {
    list(a = 1, b = "test")
  })

  expect_equal(result, list(a = 1, b = "test"))
})

test_that("withOtelCollect validates collect level", {
  expect_error(
    withOtelCollect("invalid", { 1 }),
    "'arg' should be one of"
  )

  expect_error(
    withOtelCollect(123, { 1 }),
    "`collect` must be a character vector"
  )

  expect_error(
    withOtelCollect(c("all", "none"), { 1 }),
    "'arg' must be of length 1"
  )
})

test_that("withOtelCollect rejects session and reactive_update levels", {
  expect_error(
    withOtelCollect("session", { 1 }),
    "'arg' should be one of"
  )

  expect_error(
    withOtelCollect("reactive_update", { 1 }),
    "'arg' should be one of"
  )
})

test_that("withOtelCollect works with all valid collect levels", {
  for (level in c("none", "reactivity", "all")) {
    result <- withOtelCollect(level, {
      getOption("shiny.otel.collect")
    })
    expect_equal(result, level)
  }
})

test_that("withOtelCollect nests correctly", {
  original <- getOption("shiny.otel.collect")
  on.exit(options(shiny.otel.collect = original), add = TRUE)

  options(shiny.otel.collect = "all")

  result <- withOtelCollect("reactivity", {
    outer <- getOption("shiny.otel.collect")
    inner <- withOtelCollect("none", {
      getOption("shiny.otel.collect")
    })
    restored <- getOption("shiny.otel.collect")

    list(outer = outer, inner = inner, restored = restored)
  })

  expect_equal(result$outer, "reactivity")
  expect_equal(result$inner, "none")
  expect_equal(result$restored, "reactivity")
  expect_equal(getOption("shiny.otel.collect"), "all")
})

test_that("withOtelCollect restores option even on error", {
  original <- getOption("shiny.otel.collect")
  on.exit(options(shiny.otel.collect = original), add = TRUE)

  options(shiny.otel.collect = "all")

  expect_error(
    withOtelCollect("none", {
      stop("test error")
    }),
    "test error"
  )

  # Option should still be restored
  expect_equal(getOption("shiny.otel.collect"), "all")
})

test_that("localOtelCollect sets collection level in function scope", {
  original <- getOption("shiny.otel.collect")
  on.exit(options(shiny.otel.collect = original), add = TRUE)

  options(shiny.otel.collect = "all")

  test_func <- function() {
    localOtelCollect("none")
    getOption("shiny.otel.collect")
  }

  result <- test_func()
  expect_equal(result, "none")

  # Option should be restored after function exits
  expect_equal(getOption("shiny.otel.collect"), "all")
})

test_that("localOtelCollect returns previous collect value invisibly", {
  original <- getOption("shiny.otel.collect")
  on.exit(options(shiny.otel.collect = original), add = TRUE)

  options(shiny.otel.collect = "all")

  result <- withVisible(localOtelCollect("none"))

  # Should return a list with the old option value
  expect_type(result$value, "list")
  expect_equal(result$value$shiny.otel.collect, "all")
  expect_false(result$visible)
})

test_that("localOtelCollect validates collect level", {
  expect_error(
    localOtelCollect("invalid"),
    "'arg' should be one of"
  )

  expect_error(
    localOtelCollect(NULL),
    "`collect` must be a character vector"
  )

  expect_error(
    localOtelCollect(c("all", "none")),
    "'arg' must be of length 1"
  )
})

test_that("localOtelCollect rejects session and reactive_update levels", {
  expect_error(
    localOtelCollect("session"),
    "'arg' should be one of"
  )

  expect_error(
    localOtelCollect("reactive_update"),
    "'arg' should be one of"
  )
})

test_that("localOtelCollect works with all valid collect levels", {
  for (level in c("none", "reactivity", "all")) {
    test_func <- function() {
      localOtelCollect(level)
      getOption("shiny.otel.collect")
    }
    result <- test_func()
    expect_equal(result, level)
  }
})

test_that("localOtelCollect respects envir parameter", {
  original <- getOption("shiny.otel.collect")
  on.exit(options(shiny.otel.collect = original), add = TRUE)

  options(shiny.otel.collect = "all")

  outer_func <- function() {
    env <- environment()

    inner_func <- function() {
      localOtelCollect("none", envir = env)
    }

    inner_func()
    getOption("shiny.otel.collect")
  }

  result <- outer_func()
  expect_equal(result, "none")
  expect_equal(getOption("shiny.otel.collect"), "all")
})

test_that("localOtelCollect scope is limited to function", {
  original <- getOption("shiny.otel.collect")
  on.exit(options(shiny.otel.collect = original), add = TRUE)

  options(shiny.otel.collect = "all")

  func1 <- function() {
    localOtelCollect("reactivity")
    getOption("shiny.otel.collect")
  }

  func2 <- function() {
    localOtelCollect("none")
    getOption("shiny.otel.collect")
  }

  result1 <- func1()
  result2 <- func2()

  expect_equal(result1, "reactivity")
  expect_equal(result2, "none")
  expect_equal(getOption("shiny.otel.collect"), "all")
})

test_that("withOtelCollect and localOtelCollect work together", {
  original <- getOption("shiny.otel.collect")
  on.exit(options(shiny.otel.collect = original), add = TRUE)

  options(shiny.otel.collect = "all")

  result <- withOtelCollect("reactivity", {
    outer <- getOption("shiny.otel.collect")

    test_func <- function() {
      localOtelCollect("none")
      getOption("shiny.otel.collect")
    }

    inner <- test_func()
    restored <- getOption("shiny.otel.collect")

    list(outer = outer, inner = inner, restored = restored)
  })

  expect_equal(result$outer, "reactivity")
  expect_equal(result$inner, "none")
  expect_equal(result$restored, "reactivity")
  expect_equal(getOption("shiny.otel.collect"), "all")
})

test_that("withOtelCollect affects otel_collect_is_enabled", {
  # This tests integration with the otel collection system
  original <- getOption("shiny.otel.collect")
  on.exit(options(shiny.otel.collect = original), add = TRUE)

  options(shiny.otel.collect = "all")

  # With "none", nothing except "none" should be enabled
  result <- withOtelCollect("none", {
    list(
      none = otel_collect_is_enabled("none"),
      session = otel_collect_is_enabled("session"),
      reactivity = otel_collect_is_enabled("reactivity")
    )
  })

  expect_true(result$none)
  expect_false(result$session)
  expect_false(result$reactivity)

  # With "reactivity", reactivity and below should be enabled, but not "all"
  result <- withOtelCollect("reactivity", {
    list(
      none = otel_collect_is_enabled("none"),
      session = otel_collect_is_enabled("session"),
      reactive_update = otel_collect_is_enabled("reactive_update"),
      reactivity = otel_collect_is_enabled("reactivity"),
      all = otel_collect_is_enabled("all")
    )
  })

  expect_true(result$none)
  expect_true(result$session)
  expect_true(result$reactive_update)
  expect_true(result$reactivity)
  expect_false(result$all)
})

test_that("localOtelCollect affects otel_collect_is_enabled", {
  original <- getOption("shiny.otel.collect")
  on.exit(options(shiny.otel.collect = original), add = TRUE)

  options(shiny.otel.collect = "all")

  test_func <- function() {
    localOtelCollect("reactivity")
    list(
      session = otel_collect_is_enabled("session"),
      reactive_update = otel_collect_is_enabled("reactive_update"),
      reactivity = otel_collect_is_enabled("reactivity"),
      all = otel_collect_is_enabled("all")
    )
  }

  result <- test_func()

  expect_true(result$session)
  expect_true(result$reactive_update)
  expect_true(result$reactivity)
  expect_false(result$all)
})
