context("MockShinySession")

test_that("invalidateLater supported", {
  session <- MockShinySession$new()
  i <- 0
  isolate({
    observe({
      invalidateLater(10, session)
      i <<- i + 1
    })
  })
  flushReact()
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
  flushReact()
  expect_equal(i, 1)
  session$elapse(10)
  expect_equal(i, 2)
})

test_that("reactivePoll supported", {
  session <- MockShinySession$new()
  i <- 0
  isolate({
    rp <- reactivePoll(10, session, Sys.time, function(){ i <<- i + 1 })
    observe({
      # Sys.time as the check function will cause it to always run the update.
      rp()
    })
  })
  flushReact()
  expect_equal(i, 1)
  session$elapse(10)
  flushReact()
  expect_equal(i, 2)
})

test_that("renderCachedPlot supported", {
  session <- MockShinySession$new()
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
  session <- MockShinySession$new()
  isolate({
    rt <- renderDataTable({
      head(iris)
    })
    res <- rt(session, "name")
    expect_equal(res$colnames, colnames(iris))
  })
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
  isolate({
    #' \item{clientData}{
    #'   A [reactiveValues()] object that contains information about the client.
    #'   \itemize{
    #'     \item{`allowDataUriScheme` is a logical value that indicates whether
    #'       the browser is able to handle URIs that use the `data:` scheme.
    #'     }
    #'     \item{`pixelratio` reports the "device pixel ratio" from the web browser,
    #'       or 1 if none is reported. The value is 2 for Apple Retina displays.
    #'     }
    #'     \item{`singletons` - for internal use}
    #'     \item{`url_protocol`, `url_hostname`, `url_port`,
    #'       `url_pathname`, `url_search`, `url_hash_initial`
    #'       and `url_hash` can be used to get the components of the URL
    #'       that was requested by the browser to load the Shiny app page.
    #'       These values are from the browser's perspective, so neither HTTP
    #'       proxies nor Shiny Server will affect these values. The
    #'       `url_search` value may be used with [parseQueryString()]
    #'       to access query string parameters.
    #'     }
    #'   }
    #'   `clientData` also contains information about each output.
    #'   \code{output_\var{outputId}_width} and \code{output_\var{outputId}_height}
    #'   give the dimensions (using `offsetWidth` and `offsetHeight`) of
    #'   the DOM element that is bound to \code{\var{outputId}}, and
    #'   \code{output_\var{outputId}_hidden} is a logical that indicates whether
    #'   the element is hidden. These values may be `NULL` if the output is
    #'   not bound.
    #' }
    testthat::skip("NYI")
  })
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
  expect_warning(session$resetBrush(1), "isn't meaningfully mocked")
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
  expect_warning(session$setBookmarkExclude(names=1), "Bookmarking isn't meaningfully mocked")
})

test_that("session supports getBookmarkExclude", {
  session <- MockShinySession$new()
  expect_warning(session$getBookmarkExclude(), "Bookmarking isn't meaningfully mocked")
})

test_that("session supports onBookmark", {
  session <- MockShinySession$new()
  expect_warning(session$onBookmark(fun=1), "Bookmarking isn't meaningfully mocked")
})

test_that("session supports onBookmarked", {
  session <- MockShinySession$new()
  expect_warning(session$onBookmarked(fun=1), "Bookmarking isn't meaningfully mocked")
})

test_that("session supports doBookmark", {
  session <- MockShinySession$new()
  expect_warning(session$doBookmark(), "Bookmarking isn't meaningfully mocked")
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
