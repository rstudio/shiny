context("deepstacks")

describe("deep stack trace filtering", {
  it("passes smoke test", {
    st <- list(
      c(
        common <- c("1", "2", "..stacktraceoff..", "3", "..stacktracefloor.."),
        "4", "..stacktraceon..", "5"
      ),
      c(common, "6", "..stacktraceoff..", "7"),
      c(common, "8", "..stacktraceon.."),
      c(common, "9")
    )
    
    expect_equal(
      stripStackTraces(values = TRUE, st),
      jsonlite::fromJSON('[["1", "2", "5"],["6"],[],["9"]]')
    )
  })
  
  it("handles null cases", {
    expect_equal(
      stripStackTraces(values = TRUE, list(c())),
      list(character(0))
    )
  })
  
  it("handles various edge cases", {
    expect_equal(
      stripStackTraces(values = TRUE, list(
        c("..stacktraceoff..", "..stacktraceoff..")
      )),
      list(character(0))
    )

    expect_equal(
      stripStackTraces(values = TRUE, list(
        c("..stacktraceoff..", "..stacktraceoff.."),
        c(),
        c("..stacktraceon.."),
        c("..stacktraceon.."),
        c("1")
      )),
      list(character(0), character(0), character(0), character(0), "1")
    )
  })
})
