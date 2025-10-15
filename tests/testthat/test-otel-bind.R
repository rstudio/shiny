test_that("otel_bind_is_enabled works with valid bind levels", {
  # Test with default "all" option
  expect_true(otel_bind_is_enabled("none"))
  expect_true(otel_bind_is_enabled("session"))
  expect_true(otel_bind_is_enabled("reactive_update"))
  expect_true(otel_bind_is_enabled("reactivity"))
  expect_true(otel_bind_is_enabled("all"))
})

test_that("otel_bind_is_enabled respects hierarchy with 'none' option", {
  # With "none" option, nothing should be enabled
  expect_false(otel_bind_is_enabled("session", "none"))
  expect_false(otel_bind_is_enabled("reactive_update", "none"))
  expect_false(otel_bind_is_enabled("reactivity", "none"))
  expect_false(otel_bind_is_enabled("all", "none"))
  expect_true(otel_bind_is_enabled("none", "none"))
})

test_that("otel_bind_is_enabled respects hierarchy with 'session' option", {
  # With "session" option, only "none" and "session" should be enabled
  expect_true(otel_bind_is_enabled("none", "session"))
  expect_true(otel_bind_is_enabled("session", "session"))
  expect_false(otel_bind_is_enabled("reactive_update", "session"))
  expect_false(otel_bind_is_enabled("reactivity", "session"))
  expect_false(otel_bind_is_enabled("all", "session"))
})

test_that("otel_bind_is_enabled respects hierarchy with 'reactive_update' option", {
  # With "reactive_update" option, "none", "session", and "reactive_update" should be enabled
  expect_true(otel_bind_is_enabled("none", "reactive_update"))
  expect_true(otel_bind_is_enabled("session", "reactive_update"))
  expect_true(otel_bind_is_enabled("reactive_update", "reactive_update"))
  expect_false(otel_bind_is_enabled("reactivity", "reactive_update"))
  expect_false(otel_bind_is_enabled("all", "reactive_update"))
})

test_that("otel_bind_is_enabled respects hierarchy with 'reactivity' option", {
  # With "reactivity" option, all except "all" should be enabled
  expect_true(otel_bind_is_enabled("none", "reactivity"))
  expect_true(otel_bind_is_enabled("session", "reactivity"))
  expect_true(otel_bind_is_enabled("reactive_update", "reactivity"))
  expect_true(otel_bind_is_enabled("reactivity", "reactivity"))
  expect_false(otel_bind_is_enabled("all", "reactivity"))
})

test_that("otel_bind_is_enabled respects hierarchy with 'all' option", {
  # With "all" option (default), everything should be enabled
  expect_true(otel_bind_is_enabled("none", "all"))
  expect_true(otel_bind_is_enabled("session", "all"))
  expect_true(otel_bind_is_enabled("reactive_update", "all"))
  expect_true(otel_bind_is_enabled("reactivity", "all"))
  expect_true(otel_bind_is_enabled("all", "all"))
})

test_that("otel_bind_is_enabled uses shiny.otel.bind option", {
  # Test that option is respected
  withr::with_options(
    list(shiny.otel.bind = "session"),
    {
      expect_true(otel_bind_is_enabled("none"))
      expect_true(otel_bind_is_enabled("session"))
      expect_false(otel_bind_is_enabled("reactive_update"))
    }
  )

  withr::with_options(
    list(shiny.otel.bind = "reactivity"),
    {
      expect_true(otel_bind_is_enabled("reactive_update"))
      expect_true(otel_bind_is_enabled("reactivity"))
      expect_false(otel_bind_is_enabled("all"))
    }
  )
})

test_that("otel_bind_is_enabled falls back to SHINY_OTEL_BIND env var", {
  # Remove option to test env var fallback
  withr::local_options(list(shiny.otel.bind = NULL))

  # Test env var is respected
  withr::local_envvar(list(SHINY_OTEL_BIND = "session"))
  expect_true(otel_bind_is_enabled("none"))
  expect_true(otel_bind_is_enabled("session"))
  expect_false(otel_bind_is_enabled("reactive_update"))

  withr::local_envvar(list(SHINY_OTEL_BIND = "none"))
  expect_true(otel_bind_is_enabled("none"))
  expect_false(otel_bind_is_enabled("session"))
})

test_that("otel_bind_is_enabled option takes precedence over env var", {
  # Set conflicting option and env var
  withr::local_options(shiny.otel.bind = "session")
  withr::local_envvar(SHINY_OTEL_BIND = "all")

  # Option should take precedence
  expect_true(otel_bind_is_enabled("session"))
  expect_false(otel_bind_is_enabled("reactive_update"))
})

test_that("otel_bind_is_enabled defaults to 'all' when no option or env var", {
  # Remove both option and env var
  withr::local_options(list(shiny.otel.bind = NULL))
  withr::local_envvar(list(SHINY_OTEL_BIND = NA))

  # Should default to "all"
  expect_true(otel_bind_is_enabled("all"))
  expect_true(otel_bind_is_enabled("reactivity"))
  expect_true(otel_bind_is_enabled("none"))
})

# Tests for as_otel_bind()
test_that("as_otel_bind validates and returns valid bind levels", {
  expect_equal(as_otel_bind("none"), "none")
  expect_equal(as_otel_bind("session"), "session")
  expect_equal(as_otel_bind("reactive_update"), "reactive_update")
  expect_equal(as_otel_bind("reactivity"), "reactivity")
  expect_equal(as_otel_bind("all"), "all")
})

test_that("as_otel_bind uses default value", {
  expect_equal(as_otel_bind(), "all")
})

test_that("as_otel_bind errors on invalid input types", {
  expect_error(as_otel_bind(123), "`bind` must be a character vector.")
  expect_error(as_otel_bind(NULL), "`bind` must be a character vector.")
  expect_error(as_otel_bind(TRUE), "`bind` must be a character vector.")
  expect_error(as_otel_bind(list("all")), "`bind` must be a character vector.")
})

test_that("as_otel_bind errors on invalid bind levels", {
  expect_error(as_otel_bind("invalid"), "'arg' should be one of")
  expect_error(as_otel_bind("unknown"), "'arg' should be one of")
  expect_error(as_otel_bind(""), "'arg' should be one of")
})

test_that("as_otel_bind errors on multiple values", {
  # match.arg with several.ok = FALSE should error on multiple values
  expect_error(as_otel_bind(c("all", "none")), "'arg' must be of length 1")
  expect_error(as_otel_bind(c("session", "reactivity")), "'arg' must be of length 1")
})
