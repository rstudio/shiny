test_that("plain data is serializable", {
  expect_null(snapshot_unserializable_path(1:10))
  expect_null(snapshot_unserializable_path(list(a = 1, b = list(c = "x"))))
  expect_null(snapshot_unserializable_path(data.frame(x = 1:3, y = letters[1:3])))
  expect_null(snapshot_unserializable_path(factor(c("a", "b"))))
  expect_null(snapshot_unserializable_path(NULL))
})

test_that("closures, external pointers, connections, promises and R6 objects are not", {
  expect_identical(snapshot_unserializable_path(function() 1), "value")
  expect_identical(snapshot_unserializable_path(list(f = function() 1)), "value$f")
  con <- file(tempfile(), "w"); withr::defer(close(con))
  expect_identical(snapshot_unserializable_path(list(a = 1, con = con)), "value$con")
  expect_identical(snapshot_unserializable_path(promises::promise_resolve(1)), "value")
  # fastmap >= 1.2.0 is a classed list of closures; the walk rejects it at the first.
  expect_identical(snapshot_unserializable_path(fastmap::fastmap()), "value$reset")
  expect_identical(snapshot_unserializable_path(list(1, list(e = new.env()))), "value[[2]]$e")
})

test_that("special environments are allowed", {
  expect_null(snapshot_unserializable_path(globalenv()))
  expect_null(snapshot_unserializable_path(baseenv()))
  expect_null(snapshot_unserializable_path(emptyenv()))
  expect_null(snapshot_unserializable_path(asNamespace("stats")))
})

test_that("an lm fit made inside a function is rejected at its terms environment", {
  fit <- local({ d <- data.frame(x = 1:5, y = 1:5); lm(y ~ x, d) })
  expect_identical(snapshot_unserializable_path(fit), "value$terms@.Environment")
})

test_that("S4 slots are walked and data.table is allowlisted", {
  setClass("ResumeTestS4", representation(f = "function"), where = environment())
  obj <- new("ResumeTestS4", f = function() 1)
  expect_identical(snapshot_unserializable_path(obj), "value@f")
  skip_if_not_installed("data.table")
  expect_null(snapshot_unserializable_path(data.table::data.table(x = 1)))
})

test_that("the walk fails closed past the depth limit", {
  deep <- 1
  for (i in 1:60) deep <- list(deep)
  expect_identical(snapshot_unserializable_path(deep), "too large to check")
  shallow <- 1
  for (i in 1:40) shallow <- list(shallow)
  expect_null(snapshot_unserializable_path(shallow))
})

test_that("the walk fails closed past the element budget, and atomic vectors are cheap", {
  wide <- as.list(seq_len(100001))
  expect_identical(snapshot_unserializable_path(wide), "too large to check")
  # A long atomic vector costs one step, a data frame one step per column.
  expect_null(snapshot_unserializable_path(rnorm(1e6)))
  big_df <- as.data.frame(matrix(1, nrow = 10, ncol = 50000))
  expect_null(snapshot_unserializable_path(big_df))
})

test_that("serialized size is measured as bytes", {
  expect_identical(snapshot_serialized_size(raw(100)), length(serialize(raw(100), NULL, xdr = FALSE)))
})

test_that("labels come from an explicit label, then the assignment, else none", {
  expect_identical(snapshot_source_label(NULL, explicit = "lbl"), list(label = "lbl", source = "explicit"))
  expect_identical(snapshot_source_label(NULL, explicit = c("a", "b")), list(label = "a, b", source = "explicit"))
  # Without a srcref (installed package, keep.source = FALSE) there is no identity.
  expect_identical(snapshot_source_label(NULL, explicit = NULL), list(label = NULL, source = "none"))
  # With one, the assignment names the source (as rassignSrcrefToLabel() does for display).
  withr::local_options(keep.source = TRUE)
  srcref <- attr(parse(text = "count <- reactiveVal(0)", keep.source = TRUE), "srcref")[[1]]
  expect_identical(snapshot_source_label(srcref, explicit = NULL), list(label = "count", source = "srcref"))
})

test_that("snapshot_hazards() finds <<- and assign() targets", {
  expr <- quote({ a <<- 1; b$c <<- 2; assign("d", 3); e <- 4; f(assign("g", 5)) })
  expect_identical(snapshot_hazards(expr), c("a", "b", "d", "g"))
  expect_identical(snapshot_hazards(quote(x <- 1)), character(0))
})
