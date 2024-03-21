test_that("invalidateLater supported", {
  session <- MockShinySession$new()
  i <- 0
  isolate({
    observe({
      invalidateLater(10, session)
      i <<- i + 1
    })
  })
  session$flushReact()
  expect_equal(i, 1)
  session$elapse(10)
  expect_equal(i, 2)
})

test_that("reactiveTimer supported", {
  session <- MockShinySession$new()
  i <- 0
  isolate({
    rt <- reactiveTimer(10, session)
    observe({
      rt()
      i <<- i + 1
    })
  })
  session$flushReact()
  expect_equal(i, 1)
  session$elapse(10)
  expect_equal(i, 2)
})

test_that("getOutput should auto-flush if needed", {
  session <- MockShinySession$new()
  session$defineOutput("n", function(...){ 123 })
  # There's no flushing in between these lines, so getOutput may need to instigate a flush
  # in order to get the requisite observer to run.
  expect_equal(session$getOutput("n"), 123)
})

test_that("reactivePoll supported", {
  session <- MockShinySession$new()
  i <- 0
  isolate({
    rp <- reactivePoll(10, session, function(){ rnorm(1) }, function(){ i <<- i + 1 })
    observe({
      # rnorm(1) as the check function will cause it to (almost) always run the update.
      rp()
    })
  })
  session$flushReact()
  expect_equal(i, 1)
  session$elapse(10)
  session$flushReact()
  expect_equal(i, 2)
})

# `renderCachedPlot()` is now implemented with `bindCache()`, and won't work by
# calling `f(session, "name")`, because the key computation function is not
# called with session and name.
# test_that("renderCachedPlot supported", {
#   session <- MockShinySession$new()
#   isolate({
#     # renderCachedPlot is sensitive to having the cache set for it before entering.
#     origCache <- getShinyOption("cache")
#     shinyOptions(cache = cachem::cache_mem())
#     on.exit(shinyOptions(cache = origCache), add=TRUE)
#
#     p <- renderCachedPlot({ plot(1,1) }, { Sys.time() })
#     plt <- p(session, "name")
#
#     # Should have a size defined
#     expect_equal(plt$coordmap$dims$width, 692) #FIXME: why isn't this respecting the clientdata sizes?
#     expect_equal(plt$coordmap$dims$height, 400)
#   })
# })

test_that("renderDataTable supported", {
  session <- MockShinySession$new()
  lifecycle::expect_deprecated(
    isolate({
      rt <- renderDataTable({
        head(iris)
      })
      res <- rt(session, "name")
      expect_equal(res$colnames, colnames(iris))
    })
  )
})

test_that("renderImage supported", {
  session <- MockShinySession$new()
  isolate({
    ri <- renderImage({
      # A temp file to save the output. It will be deleted after renderImage
      # sends it, because deleteFile=TRUE.
      outfile <- tempfile(fileext='.png')

      # Generate a png
      png(outfile, width=400, height=400)
      plot(1,1)
      dev.off()

      # Return a list
      list(src = outfile,
           alt = "Alt text here")
    }, deleteFile = TRUE)
    img <- ri(session, "name")
    expect_match(img$src, "^data:image/png;base64,")
    expect_equal(img$alt, "Alt text here")
  })
})

test_that("renderPlot supported", {
  session <- MockShinySession$new()
  isolate({
    p <- renderPlot({ plot(1,1) })
    plt <- p(session, "name")

    # Should have a size defined
    expect_equal(plt$width, 600)
    expect_equal(plt$height, 400)
  })
})

test_that("renderPrint supported", {
  session <- MockShinySession$new()
  isolate({
    p <- renderPrint({ print("hi") })
    pt <- p(session, "name")

    expect_equal(pt, "[1] \"hi\"")
  })
})

test_that("renderTable supported", {
  session <- MockShinySession$new()
  isolate({
    rt <- renderTable({
      head(iris)
    })
    ren <- rt(session, "name")
    expect_match(ren, "^<table")
  })
})

test_that("renderText supported", {
  session <- MockShinySession$new()
  isolate({
    rt <- renderText({
      "text here"
    })
    ren <- rt(session, "name")
    expect_equal(ren, "text here")
  })
})

test_that("renderUI supported", {
  session <- MockShinySession$new()
  isolate({
    ui <- renderUI({
      tags$a(href="https://rstudio.com", "link")
    })
    ren <- ui(session, "name")
    expect_equal(ren$deps, list())
    expect_equal(as.character(ren$html), "<a href=\"https://rstudio.com\">link</a>")
  })
})

test_that("session supports allowReconnect", {
  session <- MockShinySession$new()
  session$allowReconnect(TRUE)
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports clientData", {
  session <- MockShinySession$new()
  expect_equal(session$clientData$pixelratio, 1)
  expect_equal(session$clientData$url_protocol, "http:")
  expect_equal(session$clientData$url_hostname, "mocksession")
  expect_equal(session$clientData$url_port, 1234)
  expect_equal(session$clientData$url_pathname, "/mockpath")
  expect_equal(session$clientData$url_hash, "#mockhash")
  expect_equal(session$clientData$url_hash_initial, "#mockhash")
  expect_equal(session$clientData$url_search, "?mocksearch=1")

  # Arbitrary names have width, height, and hidden
  expect_equal(session$clientData$output_arbitrary_width, 600)
  expect_equal(session$clientData$output_arbitrary_height, 400)
  expect_equal(session$clientData$output_arbitrary_hidden, FALSE)
})

test_that("session supports ns", {
  session <- MockShinySession$new()
  expect_equal(session$ns("hi"), "mock-session-hi")
})

test_that("session supports reload", {
  session <- MockShinySession$new()
  session$reload()
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports close", {
  session <- MockShinySession$new()
  session$close()
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports request", {
  session <- MockShinySession$new()
  expect_warning(session$request, "doesn't currently simulate a realistic request")
  expect_error(session$request <- "blah", "can't be assigned to")
})

test_that("session supports userData", {
  session <- MockShinySession$new()
  expect_length(ls(session$userData), 0)
  session$userData$x <- 123
  expect_length(ls(session$userData), 1)
  expect_equal(session$userData$x, 123)
})

test_that("session supports resetBrush", {
  session <- MockShinySession$new()
  withr::with_options(list("shiny.mocksession.warn" = TRUE), {
    expect_warning(session$resetBrush(1))
  })
})

test_that("generated methods signal unused argument errors", {
  expect_error(session$resetBrush(1,2,3))
  expect_error(session$resetBrush(1,2))
})

test_that("session supports sendCustomMessage", {
  session <- MockShinySession$new()
  session$sendCustomMessage(type=1, message=2)
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports sendBinaryMessage", {
  session <- MockShinySession$new()
  session$sendBinaryMessage(type=1, message=2)
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports sendInputMessage", {
  session <- MockShinySession$new()
  session$sendInputMessage(inputId=1, message=2)
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports setBookmarkExclude", {
  session <- MockShinySession$new()
  withr::with_options(list("shiny.mocksession.warn" = TRUE), {
    expect_warning(session$setBookmarkExclude(names=1))
  })
})

test_that("session supports getBookmarkExclude", {
  session <- MockShinySession$new()
  withr::with_options(list("shiny.mocksession.warn" = TRUE), {
    expect_warning(session$getBookmarkExclude())
  })
})

test_that("session supports onBookmark", {
  session <- MockShinySession$new()
  session$onBookmark(fun=1)
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports onBookmarked", {
  session <- MockShinySession$new()
  session$onBookmarked(fun=1)
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports doBookmark", {
  session <- MockShinySession$new()
  withr::with_options(list("shiny.mocksession.warn" = TRUE), {
    expect_warning(session$doBookmark())
  })
})

test_that("session supports onRestore", {
  session <- MockShinySession$new()
  session$onRestore(fun=1)
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports onRestored", {
  session <- MockShinySession$new()
  session$onRestored(fun=1)
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports exportTestValues", {
  session <- MockShinySession$new()
  session$exportTestValues()
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports getTestSnapshotUrl", {
  session <- MockShinySession$new()
  session$getTestSnapshotUrl(input=1, output=1, export=1, format=1)
  expect_true(TRUE) # testthat insists that every test must have an expectation
})
