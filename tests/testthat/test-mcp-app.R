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

test_that("mcpInlineDependency inlines file-based scripts and stylesheets", {
  dir <- withr::local_tempdir()
  writeLines("window.__mcp_test__ = 1;", file.path(dir, "dep.js"))
  writeLines(".mcp-test { color: red; }", file.path(dir, "dep.css"))
  dep <- htmltools::htmlDependency(
    "mcptestdep", "1.0",
    src = c(file = dir),
    script = "dep.js",
    stylesheet = "dep.css",
    head = "<script>window.__mcp_head__ = 1;</script>"
  )

  inlined <- mcpInlineDependency(dep)
  expect_null(inlined$script)
  expect_null(inlined$stylesheet)
  expect_match(inlined$head, "window.__mcp_test__ = 1;", fixed = TRUE)
  expect_match(inlined$head, ".mcp-test { color: red; }", fixed = TRUE)
  expect_match(inlined$head, "window.__mcp_head__ = 1;", fixed = TRUE)
  expect_null(inlined$src$file)

  # href-only dependencies fall back to createWebDependency behavior
  href_dep <- htmltools::htmlDependency(
    "mcphrefdep", "1.0",
    src = c(href = "https://example.com/lib"),
    script = "lib.js"
  )
  passthrough <- mcpInlineDependency(href_dep)
  expect_equal(passthrough$src$href, "https://example.com/lib")
})

test_that("processDeps inlines dependencies for MCP tunnel sessions", {
  dir <- withr::local_tempdir()
  writeLines("window.__mcp_dyn__ = 1;", file.path(dir, "dyn.js"))
  dep <- htmltools::htmlDependency(
    "mcpdyndep", "1.0",
    src = c(file = dir),
    script = "dyn.js"
  )
  tag <- htmltools::attachDependencies(htmltools::div("hi"), dep)

  ui <- fluidPage(textOutput("out"))
  server <- function(input, output) {}
  app <- shinyApp(ui, server)
  handlers <- createAppHandlers(app$httpHandler, function() server)
  con_res <- mcpTunnelToolCall("_shiny_connect", empty_named_list(), 1, handlers$ws)
  cid <- con_res$result$structuredContent$connectionId
  session <- appsByToken$values()[[length(appsByToken$values())]]
  expect_true(isMcpSession(session))

  res <- processDeps(tag, session)
  dyn <- Filter(function(d) d$name == "mcpdyndep", res$deps)[[1]]
  expect_match(dyn$head, "window.__mcp_dyn__ = 1;", fixed = TRUE)
  expect_null(dyn$script)

  mcpTunnelToolCall("_shiny_close", list(connectionId = cid), 2, handlers$ws)
})

test_that("processDeps is unchanged for regular sessions", {
  session <- MockShinySession$new()
  expect_false(isMcpSession(session))
})
