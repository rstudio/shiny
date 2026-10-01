test_that("plain data is serializable", {
  expect_null(snapshot_unserializable_path(1:3))
  expect_null(snapshot_unserializable_path(list(a = "x", b = list(c = TRUE))))
  expect_null(snapshot_unserializable_path(data.frame(x = 1)))
  expect_null(snapshot_unserializable_path(factor("a")))
  expect_null(snapshot_unserializable_path(Sys.time()))
  expect_null(snapshot_unserializable_path(NULL))
})

test_that("closures, external pointers, connections, promises and R6 objects are not", {
  expect_identical(snapshot_unserializable_path(function() 1), "value")
  expect_identical(snapshot_unserializable_path(list(f = function() 1)), "value$f")
  con <- textConnection("x"); on.exit(close(con))
  expect_identical(snapshot_unserializable_path(list(con)), "value[[1]]")
  expect_identical(snapshot_unserializable_path(promises::promise_resolve(1)), "value")
  expect_identical(snapshot_unserializable_path(MockShinySession$new()), "value")
  expect_identical(snapshot_unserializable_path(structure(list(), class = "mirai")), NULL)
  expect_identical(snapshot_unserializable_path(new.env()), "value")
})

test_that("special environments are allowed", {
  expect_null(snapshot_unserializable_path(globalenv()))
  expect_null(snapshot_unserializable_path(baseenv()))
  expect_null(snapshot_unserializable_path(emptyenv()))
  expect_null(snapshot_unserializable_path(asNamespace("stats")))
  expect_null(snapshot_unserializable_path(as.environment("package:base")))
})

test_that("an lm fit made inside a function is rejected at its terms environment", {
  fit <- local({ d <- data.frame(x = 1:5, y = 1:5); lm(y ~ x, data = d) })
  expect_identical(snapshot_unserializable_path(fit), "value$terms@.Environment")
})

test_that("a ggplot captures its environment and is rejected; data.table is allowlisted", {
  skip_if_not_installed("ggplot2")
  p <- local({ d <- data.frame(x = 1); ggplot2::ggplot(d, ggplot2::aes(x)) })
  expect_match(snapshot_unserializable_path(p), "^value")
  skip_if_not_installed("data.table")
  expect_null(snapshot_unserializable_path(data.table::data.table(x = 1)))
})

test_that("S4 slots are walked", {
  setClass("SnapTestS4", representation(f = "function"), where = environment())
  obj <- new("SnapTestS4", f = function() 1)
  expect_identical(snapshot_unserializable_path(obj), "value@f")
})

test_that("serialized size is measured as bytes", {
  expect_identical(snapshot_serialized_size(1:10), length(serialize(1:10, NULL, xdr = FALSE)))
})
