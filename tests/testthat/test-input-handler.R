context("Parse Shiny Input")

test_that("A new type can be registered successfully", {
  expect_error(
    registerInputHandler("shiny.someType", function(){}),
    NA
  )
})

test_that("A duplicated type throws", {
  expect_error({
    registerInputHandler("shiny.dupType", function(){})
    registerInputHandler("shiny.dupType", function(){})
  })
})

test_that("Date converts to date", {
  x <- "2013/01/01"
  class(x) <- "shiny.date"
  handler <- inputHandlers$get('shiny.date')
  expect_identical(
    handler(x), as.Date(unclass(x))
  )
})

test_that("List of dates converts to vector", {
  x <- list("2013/01/01", "2014/01/01")
  class(x) <- "shiny.date"
  handler <- inputHandlers$get('shiny.date')
  expect_identical(
    handler(x), as.Date(unlist(x))
  )
})

test_that("Matrix converts list of lists to matrix", {
  x <- list(a=1:3,b=4:6)
  class(x) <- "shiny.matrix"
  handler <- inputHandlers$get('shiny.matrix')
  expect_identical(
    handler(x), matrix(c(1:3,4:6), byrow=FALSE, ncol=2)
  )
})

test_that("Nulls are not converted to NAs in parsing", {
  msg <- charToRaw("{\"method\":\"init\",\"data\":{\"obs\":500,\"nullObs\":null}}")
  expect_identical(
    decodeMessage(msg),
    list(method="init", data=list(obs=500L, nullObs=NULL))
  )
})
