context("Parse Shiny Input")

test_that("Default function is a pass-through", {
  x <- "2013/01/01"   
  expect_identical(     
    parseShinyInput(x), x
  )
})

test_that("Date converts to date", {
  x <- "2013/01/01"
  expect_identical(    
    parseShinyInput.date(x), as.Date(x)
  )
})

test_that("Matrix converts list of lists to matrix", {
  x <- data.frame(a=1:3,b=4:6)
  expect_identical(
    parseShinyInput.matrix(x), matrix(c(1:3,4:6), byrow=FALSE, ncol=2)
  )
})

test_that("Nulls are converted to NAs in parsing", {
  msg <- charToRaw("{\"method\":\"init\",\"data\":{\"obs\":500,\"nullObs\":null}}")
  expect_identical(
    decodeMessage(msg), 
    list(method="init", data=list(obs=500, nullObs=NA)) 
  )
})


