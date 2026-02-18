test_that("can access reactive values directly", {
  reactiveConsole(TRUE)
  on.exit(reactiveConsole(FALSE))

  x1 <- reactiveVal(1)
  x1(2)
  expect_equal(x1(), 2)

  x2 <- reactiveValues(a = 1)
  x2$a <- 2
  expect_equal(x2$a, 2)

  y <- reactive(x1() + x2$a)
  expect_equal(y(), 4)
})

describe("srcfilealias in reactive labels", {
  # When a #line directive specifies a path that differs from the srcfilecopy
  # filename, R's parser wraps the srcfile in a srcfilealias whose $lines is
  # NULL. This is exactly what happens in sourceUTF8() when the normalized path
  # differs from the original.
  parse_as_srcfilealias <- function(user_code) {
    code <- c('#line 1 "/absolute/path/to/app.R"', user_code)
    src <- base::srcfilecopy("app.R", code, isFile = TRUE)
    exprs <- parse(text = code, keep.source = TRUE, srcfile = src)
    list(code = code, exprs = exprs, srcrefs = attr(exprs, "srcref"))
  }

  it("getSrcfileLines() resolves lines from srcfilealias", {
    parsed <- parse_as_srcfilealias("my_val <- reactiveVal(1)")

    srcref <- parsed$srcrefs[[1]]
    srcfile <- attr(srcref, "srcfile", exact = TRUE)

    expect_s3_class(srcfile, "srcfilealias")
    expect_null(srcfile$lines)

    result <- getSrcfileLines(srcfile, srcref)
    expect_false(is.null(result$lines))
    expect_equal(result$lines, parsed$code)
    expect_match(result$lines[result$line_num], "my_val <- reactiveVal")
  })

  it("getSrcfileLines() works with regular srcfile", {
    code <- c("x <- 1", "y <- 2")
    src <- base::srcfilecopy("test.R", code, isFile = TRUE)
    exprs <- parse(text = code, keep.source = TRUE, srcfile = src)

    srcref <- attr(exprs, "srcref")[[1]]
    srcfile <- attr(srcref, "srcfile", exact = TRUE)

    expect_false(inherits(srcfile, "srcfilealias"))

    result <- getSrcfileLines(srcfile, srcref)
    expect_equal(result$lines, code)
    expect_equal(result$line_num, 1L)
  })

  it("rassignSrcrefToLabel() extracts label from srcfilealias", {
    parsed <- parse_as_srcfilealias("my_val <- reactiveVal(1)")
    srcref <- parsed$srcrefs[[1]]

    label <- rassignSrcrefToLabel(srcref, defaultLabel = "fallback")
    expect_equal(label, "my_val")
  })

  it("rexprSrcrefToLabel() extracts label from srcfilealias", {
    parsed <- parse_as_srcfilealias("my_r <- reactive({ 1 + 1 })")

    # rexprSrcrefToLabel() expects the srcref of the reactive body (the { }),
    # not the entire assignment. This mirrors how exprToLabel() calls it with
    # the srcref from the body of the expression created by installExprFunction.
    assign_expr <- parsed$exprs[[1]]
    reactive_body <- assign_expr[[3]][[2]]  # reactive( <body> )
    body_srcrefs <- attr(reactive_body, "srcref")
    srcref <- body_srcrefs[[1]]

    label <- rexprSrcrefToLabel(srcref, defaultLabel = "fallback", fnName = "reactive")
    expect_equal(label, "my_r")
  })
})

test_that("sourceUTF8() auto-labels reactives despite srcfilealias", {
  # sourceUTF8() uses normalizePath() in its #line directive but the original
  # path for srcfilecopy. When these differ (e.g. macOS /tmp -> /private/tmp),
  # R creates a srcfilealias whose $lines is NULL. When they match (e.g.
  # Ubuntu), the #line directive still remaps line numbers. getSrcfileLines()
  # handles both cases by using srcref[7] (the pre-remap line number).
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)

  reactiveConsole(TRUE)
  on.exit(reactiveConsole(FALSE), add = TRUE)

  writeLines(c(
    "my_val <- reactiveVal(1)",
    "my_react <- reactive({ my_val() + 1 })"
  ), tmp)

  env <- new.env(parent = globalenv())
  sourceUTF8(tmp, envir = env)

  # reactiveVal label (uses rassignSrcrefToLabel)
  rv_impl <- attr(env$my_val, ".impl", exact = TRUE)
  expect_equal(
    rv_impl$.__enclos_env__$private$label,
    "my_val"
  )

  # reactive label (uses rexprSrcrefToLabel via exprToLabel)
  r_observable <- attr(env$my_react, "observable", exact = TRUE)
  expect_equal(as.character(r_observable$.label), "my_react")
})

describe("srcfilealias filename selection", {
  parse_as_srcfilealias <- function(user_code, alias_path = "/absolute/path/to/app.R") {
    code <- c(sprintf('#line 1 "%s"', alias_path), user_code)
    src <- base::srcfilecopy("app.R", code, isFile = TRUE)
    exprs <- parse(text = code, keep.source = TRUE, srcfile = src)
    list(code = code, exprs = exprs, srcrefs = attr(exprs, "srcref"))
  }

  it("getSrcfileFilename() prefers original unless package file", {
    lib <- normalizePath(.libPaths()[[1]], winslash = "/", mustWork = FALSE)
    pkg_path <- file.path(lib, "pkg", "R", "foo.R")

    parsed_pkg <- parse_as_srcfilealias("x <- 1", alias_path = pkg_path)
    srcref_pkg <- parsed_pkg$srcrefs[[1]]
    srcfile_pkg <- attr(srcref_pkg, "srcfile", exact = TRUE)
    expect_equal(getSrcfileFilename(srcfile_pkg), pkg_path)

    parsed_user <- parse_as_srcfilealias("y <- 2", alias_path = "/tmp/user.R")
    srcref_user <- parsed_user$srcrefs[[1]]
    srcfile_user <- attr(srcref_user, "srcfile", exact = TRUE)
    expect_equal(getSrcfileFilename(srcfile_user), "app.R")
  })
})

test_that("isPackageFile() uses path-boundary matching", {
  lib <- normalizePath(.libPaths()[[1]], winslash = "/", mustWork = FALSE)

  # A path like "{lib}Extra/foo.R" shares the prefix but is NOT inside the lib
  fake_path <- paste0(lib, "Extra/foo.R")
  expect_false(isPackageFile(fake_path))

  # A path actually inside the library SHOULD match
  real_path <- file.path(lib, "pkg", "R", "foo.R")
  expect_true(isPackageFile(real_path))
})

test_that("errors in throttled/debounced reactives are catchable", {
  reactiveConsole(TRUE)
  on.exit(reactiveConsole(FALSE))

  # In Shiny 1.7 and earlier, if a throttled/debounced reactive threw an error,
  # it would cause internal observers used by the implementations of
  # debounce/throttle to error, which would kill the session. The correct
  # behavior is to only expose the error to consumers of the throttled/debounced
  # reactive.

  r <- reactive({
    stop("boom")
  })

  rd <- r %>% debounce(1000)
  rt <- r %>% throttle(1000)

  observe({
    try(rd(), silent = TRUE)
    try(rt(), silent = TRUE)
  })

  expect_silent(flushReact())
})
