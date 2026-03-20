test_that("otel_collect_is_enabled works with valid collect levels", {
  # Test with default "all" option
  expect_true(otel_collect_is_enabled("none"))
  expect_true(otel_collect_is_enabled("session"))
  expect_true(otel_collect_is_enabled("reactive_update"))
  expect_true(otel_collect_is_enabled("reactivity"))
  expect_true(otel_collect_is_enabled("all"))
})

test_that("otel_collect_is_enabled respects hierarchy with 'none' option", {
  # With "none" option, nothing should be enabled
  expect_false(otel_collect_is_enabled("session", "none"))
  expect_false(otel_collect_is_enabled("reactive_update", "none"))
  expect_false(otel_collect_is_enabled("reactivity", "none"))
  expect_false(otel_collect_is_enabled("all", "none"))
  expect_true(otel_collect_is_enabled("none", "none"))
})

test_that("otel_collect_is_enabled respects hierarchy with 'session' option", {
  # With "session" option, only "none" and "session" should be enabled
  expect_true(otel_collect_is_enabled("none", "session"))
  expect_true(otel_collect_is_enabled("session", "session"))
  expect_false(otel_collect_is_enabled("reactive_update", "session"))
  expect_false(otel_collect_is_enabled("reactivity", "session"))
  expect_false(otel_collect_is_enabled("all", "session"))
})

test_that("otel_collect_is_enabled respects hierarchy with 'reactive_update' option", {
  # With "reactive_update" option, "none", "session", and "reactive_update" should be enabled
  expect_true(otel_collect_is_enabled("none", "reactive_update"))
  expect_true(otel_collect_is_enabled("session", "reactive_update"))
  expect_true(otel_collect_is_enabled("reactive_update", "reactive_update"))
  expect_false(otel_collect_is_enabled("reactivity", "reactive_update"))
  expect_false(otel_collect_is_enabled("all", "reactive_update"))
})

test_that("otel_collect_is_enabled respects hierarchy with 'reactivity' option", {
  # With "reactivity" option, all except "all" should be enabled
  expect_true(otel_collect_is_enabled("none", "reactivity"))
  expect_true(otel_collect_is_enabled("session", "reactivity"))
  expect_true(otel_collect_is_enabled("reactive_update", "reactivity"))
  expect_true(otel_collect_is_enabled("reactivity", "reactivity"))
  expect_false(otel_collect_is_enabled("all", "reactivity"))
})

test_that("otel_collect_is_enabled respects hierarchy with 'all' option", {
  # With "all" option (default), everything should be enabled
  expect_true(otel_collect_is_enabled("none", "all"))
  expect_true(otel_collect_is_enabled("session", "all"))
  expect_true(otel_collect_is_enabled("reactive_update", "all"))
  expect_true(otel_collect_is_enabled("reactivity", "all"))
  expect_true(otel_collect_is_enabled("all", "all"))
})

test_that("otel_collect_is_enabled uses shiny.otel.collect option", {
  # Test that option is respected
  withr::with_options(
    list(shiny.otel.collect = "session"),
    {
      expect_true(otel_collect_is_enabled("none"))
      expect_true(otel_collect_is_enabled("session"))
      expect_false(otel_collect_is_enabled("reactive_update"))
    }
  )

  withr::with_options(
    list(shiny.otel.collect = "reactivity"),
    {
      expect_true(otel_collect_is_enabled("reactive_update"))
      expect_true(otel_collect_is_enabled("reactivity"))
      expect_false(otel_collect_is_enabled("all"))
    }
  )
})

test_that("otel_collect_is_enabled falls back to SHINY_OTEL_COLLECT env var", {
  # Remove option to test env var fallback
  withr::local_options(list(shiny.otel.collect = NULL))

  # Test env var is respected
  withr::local_envvar(list(SHINY_OTEL_COLLECT = "session"))
  expect_true(otel_collect_is_enabled("none"))
  expect_true(otel_collect_is_enabled("session"))
  expect_false(otel_collect_is_enabled("reactive_update"))

  withr::local_envvar(list(SHINY_OTEL_COLLECT = "none"))
  expect_true(otel_collect_is_enabled("none"))
  expect_false(otel_collect_is_enabled("session"))
})

test_that("otel_collect_is_enabled option takes precedence over env var", {
  # Set conflicting option and env var
  withr::local_options(shiny.otel.collect = "session")
  withr::local_envvar(SHINY_OTEL_COLLECT = "all")

  # Option should take precedence
  expect_true(otel_collect_is_enabled("session"))
  expect_false(otel_collect_is_enabled("reactive_update"))
})

test_that("otel_collect_is_enabled defaults to 'all' when no option or env var", {
  # Remove both option and env var
  withr::local_options(list(shiny.otel.collect = NULL))
  withr::local_envvar(list(SHINY_OTEL_COLLECT = NA))

  # Should default to "all"
  expect_true(otel_collect_is_enabled("all"))
  expect_true(otel_collect_is_enabled("reactivity"))
  expect_true(otel_collect_is_enabled("none"))
})

# Tests for as_otel_collect()
test_that("as_otel_collect validates and returns valid collect levels", {
  expect_equal(as_otel_collect("none"), "none")
  expect_equal(as_otel_collect("session"), "session")
  expect_equal(as_otel_collect("reactive_update"), "reactive_update")
  expect_equal(as_otel_collect("reactivity"), "reactivity")
  expect_equal(as_otel_collect("all"), "all")
})

test_that("as_otel_collect uses default value", {
  expect_equal(as_otel_collect(), "all")
})

test_that("as_otel_collect errors on invalid input types", {
  expect_error(as_otel_collect(123), "`collect` must be a character vector.")
  expect_error(as_otel_collect(NULL), "`collect` must be a character vector.")
  expect_error(as_otel_collect(TRUE), "`collect` must be a character vector.")
  expect_error(as_otel_collect(list("all")), "`collect` must be a character vector.")
})

test_that("as_otel_collect errors on invalid collect levels", {
  expect_error(as_otel_collect("invalid"), "'arg' should be one of")
  expect_error(as_otel_collect("unknown"), "'arg' should be one of")
  expect_error(as_otel_collect(""), "'arg' should be one of")
})

test_that("as_otel_collect errors on multiple values", {
  # match.arg with several.ok = FALSE should error on multiple values
  expect_error(as_otel_collect(c("all", "none")), "'arg' must be of length 1")
  expect_error(as_otel_collect(c("session", "reactivity")), "'arg' must be of length 1")
})
