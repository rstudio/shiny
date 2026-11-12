skip_if_not_shinytest2()
library(shinytest2)

# library(shiny) must be called explicitly inside the function so that
# shinytest2's library shim loads the dev package via pkgload::load_all().
app <- launch_test_app(function() {
  library(shiny)
  options(shiny.webmcp = TRUE)

  dot <- "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="

  # Served by this app, but loaded from `localhost` while the page is on
  # `127.0.0.1`: a different origin, without CORS headers.
  dir <- tempfile()
  dir.create(dir)
  writeBin(
    jsonlite::base64_dec(sub("^data:image/png;base64,", "", dot)),
    file.path(dir, "dot.png")
  )
  addResourcePath("xorigin", dir)

  canvases <- tags$script(HTML(
    "
    const xsrc = location.origin.replace('127.0.0.1', 'localhost') + '/xorigin/dot.png';
    document.getElementById('xurl').src = xsrc;

    const c2d = document.getElementById('c2d').getContext('2d');
    c2d.fillStyle = 'red'; c2d.fillRect(10, 10, 50, 30);

    // Not preserved: cleared after the frame is shown
    const gl = document.getElementById('cgl').getContext('webgl');
    gl.clearColor(0, 0, 1, 1); gl.clear(gl.COLOR_BUFFER_BIT);

    const glk = document.getElementById('cglkeep')
      .getContext('webgl', { preserveDrawingBuffer: true });
    glk.clearColor(0, 1, 0, 1); glk.clear(glk.COLOR_BUFFER_BIT);
    glk.enable(glk.SCISSOR_TEST); glk.scissor(0, 0, 20, 20);
    glk.clearColor(1, 0, 0, 1); glk.clear(glk.COLOR_BUFFER_BIT);

    const img = new Image();
    img.onload = () => {
      document.getElementById('ctaint').getContext('2d').drawImage(img, 0, 0);
      window.imagesReady = true;
    };
    img.src = xsrc;
    "
  ))

  ui <- fluidPage(
    plotOutput("plot", height = 200),
    uiOutput("mixed"),
    uiOutput("gallery")
  )

  server <- function(input, output) {
    output$plot <- renderPlot(plot(1:10))
    output$nested <- renderPlot(plot(10:1))
    output$mixed <- renderUI(tagList(
      plotOutput("nested", height = 150),
      tags$img(id = "xurl"),
      tags$canvas(id = "c2d", width = 80, height = 50),
      tags$canvas(id = "cgl", width = 80, height = 50),
      tags$canvas(id = "cglkeep", width = 80, height = 50),
      tags$canvas(id = "ctaint", width = 20, height = 20),
      tags$canvas(id = "czero", width = 0, height = 0),
      div(style = "display: none", tags$img(src = dot)),
      canvases
    ))
    output$gallery <- renderUI(lapply(1:25, function(i) tags$img(src = dot)))
  }

  shinyApp(ui, server)
})

app$wait_for_js("window.imagesReady === true")

types <- function(images) vapply(images, `[[`, "", "type")

test_that("listOutputs() counts each output's own visible images", {
  counts <- app$get_js(
    "Shiny.drive.listOutputs().map((o) => [o.id, o.imageCount])"
  )
  expect_equal(
    counts,
    list(list("plot", 1L), list("mixed", 6L), list("nested", 1L), list("gallery", 25L))
  )
})

test_that("getOutput() returns data, url, and error entries in page order", {
  plot <- app$get_js("Shiny.drive.getOutput('plot')")
  expect_equal(types(plot$images), "data")
  expect_match(plot$images[[1]]$value, "^data:image/png;base64,")

  mixed <- app$get_js("Shiny.drive.getOutput('mixed')")
  expect_equal(
    types(mixed$images),
    c("url", "data", "error", "data", "error", "error")
  )
  expect_match(mixed$images[[1]]$value, "^http://localhost:.*/xorigin/dot.png$")
  expect_match(mixed$images[[3]]$reason, "WebGL")
  expect_match(mixed$images[[5]]$reason, "another site")
  expect_match(mixed$images[[6]]$reason, "zero width or height")
})

test_that("getOutput() pages through images", {
  page <- app$get_js("Shiny.drive.getOutput('gallery', { imageOffset: 20 })")
  expect_equal(page$imageCount, 25L)
  expect_equal(page$imageOffset, 20L)
  expect_equal(vapply(page$images, `[[`, 0L, "index"), 20:24)

  expect_equal(
    app$get_js("Shiny.drive.getOutput('gallery').then((x) => x.images.length)"),
    10L
  )
  # get_js() returns NULL for a rejected promise, so read the message
  expect_match(
    app$get_js(
      "Shiny.drive.getOutput('gallery', { imageOffset: -1 }).catch((e) => e.message)"
    ),
    "`imageOffset` must be a non-negative integer"
  )
})

test_that("get_output attaches data images as image content", {
  res <- app$get_js("Shiny.webmcp.callTool('get_output', { id: 'mixed' })")
  expect_equal(types(res$content), c("text", "image", "image"))
  expect_equal(res$content[[2]]$mimeType, "image/png")

  json <- jsonlite::fromJSON(res$content[[1]]$text, simplifyVector = FALSE)
  expect_null(json$images[[2]]$value)
  expect_equal(json$images[[1]]$type, "url")

  # The tool caps imageLimit at 10
  big <- app$get_js(
    "Shiny.webmcp.callTool('get_output', { id: 'gallery', imageLimit: 50 })"
  )
  expect_length(big$content, 11)
})

test_that("callTool() accepts JSON strings and rejects unknown arguments", {
  res <- app$get_js(
    "Shiny.webmcp.callTool('get_output', '{\"id\": \"plot\"}')"
  )
  expect_null(res$isError)
  expect_equal(res$content[[2]]$type, "image")

  bad <- app$get_js(
    "Shiny.webmcp.callTool('get_output', { id: 'gallery', offset: 20 })"
  )
  expect_true(bad$isError)
  expect_match(bad$content[[1]]$text, 'unknown parameter "offset"', fixed = TRUE)
  expect_match(bad$content[[1]]$text, "Expected input schema:", fixed = TRUE)
  expect_match(bad$content[[1]]$text, 'Example: {"id":"..."', fixed = TRUE)

  unknown <- app$get_js("Shiny.webmcp.callTool('nope')")
  expect_match(unknown$content[[1]]$text, "Tools: list_inputs, set_input")
})

test_that("<html data-shiny-webmcp> reports registration", {
  # The polyfill (from a CDN) provides document.modelContext
  app$wait_for_js(
    "document.documentElement.dataset.shinyWebmcp === 'registered'"
  )
  expect_equal(
    app$get_js("document.documentElement.dataset.shinyWebmcp"),
    "registered"
  )
})
