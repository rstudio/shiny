test_that("resources/list and resources/read serve a self-contained app page", {
  ui <- fluidPage(
    sliderInput("n", "N", 1, 10, 5),
    plotOutput("p")
  )
  app <- shinyApp(ui, function(input, output) {})
  withr::with_options(list(shiny.mcp = TRUE), {
    handlers <- createAppHandlers(
      app$httpHandler,
      function() function(input, output) {}
    )
    h <- handlers$http

    out <- mcp_post(h, "resources/list")
    res <- out$result$resources[[1]]
    expect_equal(res$uri, "ui://shiny/app")
    expect_equal(res$mimeType, "text/html;profile=mcp-app")

    out <- mcp_post(h, "resources/read", params = list(uri = "ui://shiny/app"))
    content <- out$result$contents[[1]]
    expect_equal(content$mimeType, "text/html;profile=mcp-app")
    html <- content$text

    # All scripts and stylesheets inlined: no external file refs remain
    expect_false(grepl("<script[^>]+src=", html))
    expect_false(grepl("<link[^>]+href=", html))
    # Bridge script injected
    expect_match(html, "shiny-mcp-bridge", fixed = TRUE)
    # App UI present
    expect_match(html, "sliderInput|shiny-plot-output|slider")
    # jquery + shiny.js payloads inlined
    expect_match(html, "jQuery", fixed = TRUE)

    out <- mcp_post(h, "resources/read", params = list(uri = "ui://nope"))
    expect_equal(out$error$code, -32602L)
  })
})

test_that("resolveAssetFile maps registered resource paths and shared dir", {
  # createWebDependency() registers e.g. jquery-<version> -> local dir
  jq <- resolveAssetFile("shared/jquery.min.js")
  skip_if(is.null(jq)) # only if shared assets aren't installed
  expect_true(file.exists(jq))

  expect_null(resolveAssetFile("nonexistent-prefix/some.js"))
  expect_null(resolveAssetFile("noslash"))
})

test_that("escapeScriptContent defuses closing script tags", {
  expect_equal(
    escapeScriptContent('var a = "</script>";'),
    'var a = "<\\/script>";'
  )
})
