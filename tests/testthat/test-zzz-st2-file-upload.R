skip_if_not_shinytest2()
library(shinytest2)

test_that("starting a second upload keeps the first file's progress", {
  app <- launch_test_app(function() {
    library(shiny)
    ui <- fluidPage(fileInput("files", "Files", multiple = TRUE), textOutput("count"))
    server <- function(input, output, session) {
      output$count <- renderText(nrow(input$files))
    }
    shinyApp(ui, server)
  })

  # Read the actual bar when each request starts. A small upload can finish
  # before polling the DOM between files would observe either width.
  app$run_js("
    window.uploadWidths = [];
    $(document).on('ajaxSend', function(event, xhr, options) {
      if (options.data instanceof File) {
        window.uploadWidths.push($('#files_progress .progress-bar')[0].style.width);
      }
    });
  ")
  files <- c(withr::local_tempfile(), withr::local_tempfile())
  for (path in files) writeBin(as.raw(1), path)
  app$upload_file(files = files)

  expect_equal(app$get_value(output = "count"), "2")
  expect_equal(unlist(app$get_js("window.uploadWidths")), c("0%", "50%"))
})
