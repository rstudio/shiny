context("stack pruning")

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

res <- do.call(
  identity,
  list(
    identity(capture_2())
  )
)
res$calls <- tail(res$calls, 5)
res$parents <- tail(res$parents - (length(res$parents) - 5), 5)

describe("stack pruning", {
  it("passes basic example", {
    expect_equal(pruneStackTrace(res$parents), c(F, F, T, T, T))
    expect_equal(lapply(list(res$parents), pruneStackTrace), list(c(F, F, T, T, T)))
  })
})
