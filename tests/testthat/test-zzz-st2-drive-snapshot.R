skip_if_not_shinytest2()
library(shinytest2)

# library(shiny) must be called explicitly inside the function so that
# shinytest2's library shim loads the dev package via pkgload::load_all().
app <- launch_test_app(function() {
  library(shiny)
  options(shiny.webmcp = TRUE, shiny.webmcp.polyfill = FALSE)

  dot <- "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="

  # One output binding per getSnapshot() behavior, keyed by class
  bindings <- tags$script(HTML(sprintf(
    "
    const dot = '%s';
    function make(cls, getSnapshot) {
      const b = new Shiny.OutputBinding();
      b.name = 'test.' + cls;
      b.find = (scope) => $(scope).find('.' + cls);
      // Keeps the element's <img>s
      b.renderValue = (el, data) => {
        let span = el.querySelector('span');
        if (!span) span = el.insertBefore(document.createElement('span'), el.firstChild);
        span.textContent = data;
      };
      if (getSnapshot) b.getSnapshot = getSnapshot;
      Shiny.outputBindings.register(b, b.name);
    }
    make('snap', async () => ({ images: [dot, dot], data: { rows: [[1, 'a']] } }));
    make('nosnap', () => undefined);
    make('throws', () => { throw new Error('boom'); });
    make('badimages', () => ({ images: 'nope' }));
    ",
    dot
  )))

  ui <- fluidPage(
    bindings,
    div(id = "snap", class = "snap"),
    div(id = "nosnap", class = "nosnap", tags$img(src = dot)),
    div(id = "throws", class = "throws", tags$img(src = dot)),
    div(id = "badimages", class = "badimages")
  )

  server <- function(input, output) {
    for (id in c("snap", "nosnap", "throws", "badimages")) {
      local({
        id <- id
        output[[id]] <- renderText(paste("text of", id))
      })
    }
  }

  shinyApp(ui, server)
})

get_output <- function(id, opts = "{}") {
  app$get_js(sprintf("Shiny.drive.getOutput('%s', %s)", id, opts))
}

test_that("getSnapshot() images replace the page's, and data is returned", {
  out <- get_output("snap")
  expect_equal(out$text, "text of snap")
  expect_equal(out$imageCount, 2)
  expect_equal(vapply(out$images, `[[`, "", "type"), c("data", "data"))
  expect_equal(out$data, list(rows = list(list(1L, "a"))))
  expect_null(out$snapshotError)

  # Paged like the page's images
  out <- get_output("snap", "{ imageOffset: 1, imageLimit: 5 }")
  expect_length(out$images, 1)
  expect_equal(out$images[[1]]$index, 1)

  # listOutputs() never calls getSnapshot(): it counts the page's images
  rows <- app$get_js("Shiny.drive.listOutputs({ ids: ['snap'] })")
  expect_equal(rows[[1]]$imageCount, 0)
})

test_that("undefined from getSnapshot() reads the page", {
  out <- get_output("nosnap")
  expect_equal(out$imageCount, 1)
  expect_null(out$data)
  expect_null(out$snapshotError)
})

test_that("a failing getSnapshot() reads the page and says why", {
  out <- get_output("throws")
  expect_equal(out$imageCount, 1)
  expect_equal(out$snapshotError, "boom")

  out <- get_output("badimages")
  expect_match(out$snapshotError, "array of strings")
})

test_that("get_output sends snapshot images as image content and keeps data", {
  res <- app$get_js("Shiny.webmcp.callTool('get_output', { id: 'snap' })")
  types <- vapply(res$content, `[[`, "", "type")
  expect_equal(types, c("text", "image", "image"))
  expect_equal(res$structuredContent$data, list(rows = list(list(1L, "a"))))
})
