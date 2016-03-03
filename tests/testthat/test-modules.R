context("modules")

test_that("Namespace qualifying", {
  expect_equivalent(NS("one", "two"), "one-two")
  expect_equivalent(NS(c("one", "two"))(NULL), "one-two")
  expect_equivalent(NS(NULL)(c("one", "two")), "one-two")
  expect_equivalent(NS(c("one", "two"), c("three", "four")), "one-two-three-four")
  expect_equivalent(NS(c("one", "two"))(c("three", "four")), "one-two-three-four")
  expect_equivalent(NS(c("one", "two"))("three four"), "one-two-three four")
  expect_equivalent(NS(c("one", "two"))("three-four"), "one-two-three-four")
})

test_that("reactiveValues with namespace", {
  values <- ReactiveValues$new()

  rv <- .createReactiveValues(values)
  rv$foo <- 10
  rv$baz <- 11
  expect_equivalent(isolate(values$get("foo")), 10)
  expect_equivalent(isolate(values$get("baz")), 11)

  rv1 <- .createReactiveValues(values, ns = NS("bar"))
  rv1$baz <- 20
  expect_equivalent(isolate(rv1$baz), 20)
  expect_equivalent(isolate(rv[["bar-baz"]]), 20)

  rv2 <- .createReactiveValues(values, ns = NS(c("bar", "qux")))
  rv2$quux <- 30
  expect_equivalent(isolate(rv2$quux), 30)
  expect_equivalent(isolate(rv1[["qux-quux"]]), 30)
  expect_equivalent(isolate(rv[["bar-qux-quux"]]), 30)

  # Namespaced reactive values objects only get their own names,
  # minus the namespace prefix, when names() is called.
  # Unnamespaced (root) reactive values objects get all names.
  expect_equivalent(isolate(names(rv)), c("bar-baz", "bar-qux-quux", "baz", "foo"))
  expect_equivalent(isolate(names(rv1)), c("baz", "qux-quux"))
  expect_equivalent(isolate(names(rv2)), c("quux"))
})
