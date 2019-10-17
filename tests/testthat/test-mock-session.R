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

test_that("session supports all the documented values in ?session", {
  testthat::fail("It doesn't")
})
