with_several_promise_domains <- function(expr) {
  withReactiveDomain(MockShinySession$new(), {
    with_promise_domain(reactivePromiseDomain(), {
      captureStackTraces({
        expr
      })
    })
  })
}

recursive_promise <- function(n, callback = identity) {
  if (n <= 0) {
    return(promise_resolve(0))
  }

  p <- promises::promise_resolve(TRUE)
  promises::then(p, ~{
    callback(n)
    recursive_promise(n - 1, callback = callback)
  })
}

test_that("Stack trace doesn't grow (resolution within domain)", {

  depths <- list()
  with_several_promise_domains({
    recursive_promise(10, function(n) {
      depths <<- c(depths, list(length(sys.calls())))
    })
    while (!later::loop_empty()) {
      later::run_now()
    }
  })
  expect_equal(diff(range(depths)), 0)
})

test_that("Stack trace doesn't grow (resolution outside domain)", {

  depths <- list()
  with_several_promise_domains({
    recursive_promise(10, function(n) {
      depths <<- c(depths, list(length(sys.calls())))
    })
  })
  while (!later::loop_empty()) {
    later::run_now()
  }
  expect_equal(diff(range(depths)), 0)
})
