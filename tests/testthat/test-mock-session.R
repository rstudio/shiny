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
  isolate({
    #' \item{allowReconnect(value)}{
    #'   If `value` is `TRUE` and run in a hosting environment (Shiny
    #'   Server or Connect) with reconnections enabled,  then when the session ends
    #'   due to the network connection closing, the client will attempt to
    #'   reconnect to the server. If a reconnection is successful, the browser will
    #'   send all the current input values to the new session on the server, and
    #'   the server will recalculate any outputs and send them back to the client.
    #'   If `value` is `FALSE`, reconnections will be disabled (this is
    #'   the default state). If `"force"`, then the client browser will always
    #'   attempt to reconnect. The only reason to use `"force"` is for testing
    #'   on a local connection (without Shiny Server or Connect).
    #' }
    testthat::skip("NYI")
  })
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
  isolate({
    #' \item{ns(id)}{
    #'   Server-side version of [`ns <- NS(id)`][NS]. If bare IDs need to be
    #'   explicitly namespaced for the current module, `session$ns("name")`
    #'   will return the fully-qualified ID.
    #' }
    testthat::skip("NYI")
  })
})

test_that("session supports reload", {
  session <- MockShinySession$new()
  isolate({
    #' \item{reload()}{
    #'   The equivalent of hitting the browser's Reload button. Only works if the
    #'   session is actually connected.
    #' }
    #'
    testthat::skip("NYI")
  })
})

test_that("session supports request", {
  session <- MockShinySession$new()
  isolate({
    #' \item{request}{
    #'   An environment that implements the Rook specification for HTTP requests.
    #'   This is the request that was used to initiate the websocket connection
    #'   (as opposed to the request that downloaded the web page for the app).
    #' }
    testthat::skip("NYI")
  })
})

test_that("session supports userData", {
  session <- MockShinySession$new()
  isolate({
    #' \item{userData}{
    #'   An environment for app authors and module/package authors to store whatever
    #'   session-specific data they want.
    #' }
    testthat::skip("NYI")
  })
})

test_that("session supports resetBrush", {
  session <- MockShinySession$new()
  isolate({
    #' \item{resetBrush(brushId)}{
    #'   Resets/clears the brush with the given `brushId`, if it exists on
    #'   any `imageOutput` or `plotOutput` in the app.
    #' }
    testthat::skip("NYI")
  })
})

test_that("session supports sendCustomMessage", {
  session <- MockShinySession$new()
  isolate({
    #' \item{sendCustomMessage(type, message)}{
    #'   Sends a custom message to the web page. `type` must be a
    #'   single-element character vector giving the type of message, while
    #'   `message` can be any jsonlite-encodable value. Custom messages
    #'   have no meaning to Shiny itself; they are used soley to convey information
    #'   to custom JavaScript logic in the browser. You can do this by adding
    #'   JavaScript code to the browser that calls
    #'   \code{Shiny.addCustomMessageHandler(type, function(message){...})}
    #'   as the page loads; the function you provide to
    #'   `addCustomMessageHandler` will be invoked each time
    #'   `sendCustomMessage` is called on the server.
    #' }
    testthat::skip("NYI")
  })
})

test_that("session supports sendBinaryMessage", {
  session <- MockShinySession$new()
  isolate({
    #' \item{sendBinaryMessage(type, message)}{
    #'   Similar to `sendCustomMessage`, but the message must be a raw vector
    #'   and the registration method on the client is
    #'   \code{Shiny.addBinaryMessageHandler(type, function(message){...})}. The
    #'   message argument on the client will be a
    #'   [DataView](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView).
    #' }
    testthat::skip("NYI")
  })
})

test_that("session supports sendInputMessage", {
  session <- MockShinySession$new()
  isolate({
    #' \item{sendInputMessage(inputId, message)}{
    #'   Sends a message to an input on the session's client web page; if the input
    #'   is present and bound on the page at the time the message is received, then
    #'   the input binding object's `receiveMessage(el, message)` method will
    #'   be called. `sendInputMessage` should generally not be called directly
    #'   from Shiny apps, but through friendlier wrapper functions like
    #'   [updateTextInput()].
    #' }
    testthat::skip("NYI")
  })
})

test_that("session supports setBookmarkExclude", {
  session <- MockShinySession$new()
  isolate({
    #' \item{setBookmarkExclude(names)}{
    #'   Set input names to be excluded from bookmarking.
    #' }
    testthat::skip("NYI")
  })
})

test_that("session supports getBookmarkExclude", {
  session <- MockShinySession$new()
  isolate({
    #' \item{getBookmarkExclude()}{
    #'   Returns the set of input names to be excluded from bookmarking.
    #' }
    testthat::skip("NYI")
  })
})

test_that("session supports onBookmark", {
  session <- MockShinySession$new()
  isolate({
    #' \item{onBookmark(fun)}{
    #'   Registers a function that will be called just before bookmarking state.
    #' }
    testthat::skip("NYI")
  })
})

test_that("session supports onBookmarked", {
  session <- MockShinySession$new()
  isolate({
    #' \item{onBookmarked(fun)}{
    #'   Registers a function that will be called just after bookmarking state.
    #' }
    testthat::skip("NYI")
  })
})

test_that("session supports onRestore", {
  session <- MockShinySession$new()
  isolate({
    #' \item{onRestore(fun)}{
    #'   Registers a function that will be called when a session is restored, before
    #'   all other reactives, observers, and render functions are run.
    #' }
    testthat::skip("NYI")
  })
})

test_that("session supports onRestored", {
  session <- MockShinySession$new()
  isolate({
    #' \item{onRestored(fun)}{
    #'   Registers a function that will be called when a session is restored, after
    #'   all other reactives, observers, and render functions are run.
    #' }
    testthat::skip("NYI")
  })
})

test_that("session supports doBookmark", {
  session <- MockShinySession$new()
  isolate({
    #' \item{doBookmark()}{
    #'   Do bookmarking and invoke the onBookmark and onBookmarked callback functions.
    #' }
    testthat::skip("NYI")
  })
})

test_that("session supports exportTestValues", {
  session <- MockShinySession$new()
  isolate({
    #' \item{exportTestValues()}{
    #'   Registers expressions for export in test mode, available at the test
    #'   snapshot URL.
    #' }
    testthat::skip("NYI")
  })
})

test_that("session supports getTestSnapshotUrl", {
  session <- MockShinySession$new()
  isolate({
    #' \item{getTestSnapshotUrl(input=TRUE, output=TRUE, export=TRUE,
    #'   format="json")}{
    #'   Returns a URL for the test snapshots. Only has an effect when the
    #'   `shiny.testmode` option is set to TRUE. For the input, output, and
    #'   export arguments, TRUE means to return all of these values. It is also
    #'   possible to specify by name which values to return by providing a
    #'   character vector, as in `input=c("x", "y")`. The format can be
    #'   "rds" or "json".
    #' }
    testthat::skip("NYI")
  })
})
