foo <- function() {
  capture <- function() {
    list(
      calls = sys.calls(),
      parents = sys.parents()
    )
  }

  capture_1 <- function() {
    capture()
  }

  capture_2 <- function() {
    capture_1()
  }

  do.call(
    identity,
    list(
      identity(capture_2())
    )
  )
}
res <- foo()
res$calls <- tail(res$calls, 6)
res$parents <- tail(res$parents - (length(res$parents) - 6), 6)

describe("stack pruning", {
  it("passes basic example", {
    expect_equal(pruneStackTrace(res$parents), c(T, F, F, T, T, T))
    expect_equal(lapply(list(res$parents), pruneStackTrace), list(c(T, F, F, T, T, T)))
  })
})
