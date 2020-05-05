context("MockShinySession")

test_that("invalidateLater supported", {
  session <- makeMockSession()
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
  session <- makeMockSession()
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
  session <- makeMockSession()
  session$defineOutput("n", function(...){ 123 })
  # There's no flushing in between these lines, so getOutput may need to instigate a flush
  # in order to get the requisite observer to run.
  expect_equal(session$getOutput("n"), 123)
})

test_that("reactivePoll supported", {
  session <- makeMockSession()
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

test_that("renderCachedPlot supported", {
  session <- makeMockSession()
  isolate({
    # renderCachedPlot is sensitive to having the cache set for it before entering.
    origCache <- getShinyOption("cache")
    shinyOptions(cache = MemoryCache$new())
    on.exit(shinyOptions(cache = origCache), add=TRUE)

    p <- renderCachedPlot({ plot(1,1) }, { Sys.time() })
    plt <- p(session, "name")

    # Should have a size defined
    expect_equal(plt$coordmap$dims$width, 692) #FIXME: why isn't this respecting the clientdata sizes?
    expect_equal(plt$coordmap$dims$height, 400)
  })
})

test_that("renderDataTable supported", {
  session <- makeMockSession()
  isolate({
    rt <- renderDataTable({
      head(iris)
    })
    res <- rt(session, "name")
    expect_equal(res$colnames, colnames(iris))
  })
})

test_that("renderImage supported", {
  session <- makeMockSession()
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
  session <- makeMockSession()
  isolate({
    p <- renderPlot({ plot(1,1) })
    plt <- p(session, "name")

    # Should have a size defined
    expect_equal(plt$width, 600)
    expect_equal(plt$height, 400)
  })
})

test_that("renderPrint supported", {
  session <- makeMockSession()
  isolate({
    p <- renderPrint({ print("hi") })
    pt <- p(session, "name")

    expect_equal(pt, "[1] \"hi\"")
  })
})

test_that("renderTable supported", {
  session <- makeMockSession()
  isolate({
    rt <- renderTable({
      head(iris)
    })
    ren <- rt(session, "name")
    expect_match(ren, "^<table")
  })
})

test_that("renderText supported", {
  session <- makeMockSession()
  isolate({
    rt <- renderText({
      "text here"
    })
    ren <- rt(session, "name")
    expect_equal(ren, "text here")
  })
})

test_that("renderUI supported", {
  session <- makeMockSession()
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
  session <- makeMockSession()
  session$allowReconnect(TRUE)
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports clientData", {
  session <- makeMockSession()
  expect_equal(session$clientData$allowDataUriScheme, TRUE)
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
  session <- makeMockSession()
  expect_equal(session$ns("hi"), "mock-session-hi")
})

test_that("session supports reload", {
  session <- makeMockSession()
  session$reload()
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports close", {
  session <- makeMockSession()
  session$close()
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports request", {
  session <- makeMockSession()
  expect_warning(session$request, "doesn't currently simulate a realistic request")
  expect_error(session$request <- "blah", "can't be assigned to")
})

test_that("session supports userData", {
  session <- makeMockSession()
  expect_length(ls(session$userData), 0)
  session$userData$x <- 123
  expect_length(ls(session$userData), 1)
  expect_equal(session$userData$x, 123)
})

test_that("session supports resetBrush", {
  session <- makeMockSession()
  expect_warning(session$resetBrush(1), "isn't meaningfully mocked")
})

test_that("session supports sendCustomMessage", {
  session <- makeMockSession()
  session$sendCustomMessage(type=1, message=2)
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports sendBinaryMessage", {
  session <- makeMockSession()
  session$sendBinaryMessage(type=1, message=2)
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports sendInputMessage", {
  session <- makeMockSession()
  session$sendInputMessage(inputId=1, message=2)
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports setBookmarkExclude", {
  session <- makeMockSession()
  expect_warning(session$setBookmarkExclude(names=1), "Bookmarking isn't meaningfully mocked")
})

test_that("session supports getBookmarkExclude", {
  session <- makeMockSession()
  expect_warning(session$getBookmarkExclude(), "Bookmarking isn't meaningfully mocked")
})

test_that("session supports onBookmark", {
  session <- makeMockSession()
  session$onBookmark(fun=1)
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports onBookmarked", {
  session <- makeMockSession()
  session$onBookmarked(fun=1)
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports doBookmark", {
  session <- makeMockSession()
  expect_warning(session$doBookmark(), "Bookmarking isn't meaningfully mocked")
})

test_that("session supports onRestore", {
  session <- makeMockSession()
  session$onRestore(fun=1)
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports onRestored", {
  session <- makeMockSession()
  session$onRestored(fun=1)
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports exportTestValues", {
  session <- makeMockSession()
  session$exportTestValues()
  expect_true(TRUE) # testthat insists that every test must have an expectation
})

test_that("session supports getTestSnapshotUrl", {
  session <- makeMockSession()
  session$getTestSnapshotUrl(input=1, output=1, export=1, format=1)
  expect_true(TRUE) # testthat insists that every test must have an expectation
})
