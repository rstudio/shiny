test_that("Namespace qualifying", {
  expect_equal(NS(NULL, "one"), "one")
  expect_equal(NS(NULL)("one"), "one")

  expect_equal(NS("one", NULL), "one")
  expect_equal(NS("one")(NULL), "one")

  expect_equal(NS("one", "two"), "one-two")
  expect_equal(NS("one")("two"), "one-two")

  expect_equal(NS(c("one", "two"))(NULL), "one-two")
  expect_equal(NS(c("one", "two"), NULL), "one-two")

  expect_equal(NS(NULL)(c("one", "two")), c("one", "two"))
  expect_equal(NS(NULL, c("one", "two")), c("one", "two"))

  expect_equal(NS("one", c("two", "three")), c("one-two", "one-three"))
  expect_equal(NS("one")(c("two", "three")), c("one-two", "one-three"))

  expect_equal(NS(c("one", "two"), "three"), "one-two-three")
  expect_equal(NS(c("one", "two"))("three"), "one-two-three")

  expect_equal(NS(c("one", "two"), c("three", "four")), c("one-two-three", "one-two-four"))
  expect_equal(NS(c("one", "two"))(c("three", "four")), c("one-two-three", "one-two-four"))

  expect_equal(NS(c("one", "two"))("three four"), "one-two-three four")
  expect_equal(NS(c("one", "two"))("three-four"), "one-two-three-four")
})

test_that("reactiveValues with namespace", {
  values <- ReactiveValues$new()

  rv <- .createReactiveValues(values)
  rv$foo <- 10
  rv$baz <- 11
  expect_equal(isolate(values$get("foo")), 10)
  expect_equal(isolate(values$get("baz")), 11)

  rv1 <- .createReactiveValues(values, ns = NS("bar"))
  rv1$baz <- 20
  expect_equal(isolate(rv1$baz), 20)
  expect_equal(isolate(rv[["bar-baz"]]), 20)

  rv2 <- .createReactiveValues(values, ns = NS(c("bar", "qux")))
  rv2$quux <- 30
  expect_equal(isolate(rv2$quux), 30)
  expect_equal(isolate(rv1[["qux-quux"]]), 30)
  expect_equal(isolate(rv[["bar-qux-quux"]]), 30)

  # Namespaced reactive values objects only get their own names,
  # minus the namespace prefix, when names() is called.
  # Unnamespaced (root) reactive values objects get all names.
  expect_setequal(isolate(names(rv)), c("bar-baz", "bar-qux-quux", "baz", "foo"))
  expect_setequal(isolate(names(rv1)), c("baz", "qux-quux"))
  expect_setequal(isolate(names(rv2)), c("quux"))
})

test_that("implicit output respects module namespace", {
  output <- new.env(parent = emptyenv())
  ns <- NS("test")
  result <- withReactiveDomain(list(output = output, ns = ns),
    as.tags(renderText("hi"))
  )
  # Does the automatically-generated output id include the correct namespace qualifier?
  # (See issue #2000)
  expect_equal(result$attribs$id, ns(ls(output)))
})
