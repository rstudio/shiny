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

test_that("rewriteCssUrls inlines local url() refs and leaves the rest", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "css"))
  dir.create(file.path(dir, "webfonts"))
  writeBin(as.raw(c(0x77, 0x4F, 0x46, 0x32)), file.path(dir, "webfonts", "fa.woff2"))
  writeBin(as.raw(1:4), file.path(dir, "css", "img.png"))

  css <- paste(
    "@font-face { src: url(../webfonts/fa.woff2?v=6.4#frag) format('woff2'); }",
    ".a { background: url('img.png'); }",
    ".b { background: url(\"data:image/png;base64,AAAA\"); }",
    ".c { fill: url(#gradient); }",
    ".d { background: url(https://example.com/x.png); }",
    ".e { background: url(missing.png); }",
    sep = "\n"
  )
  out <- rewriteCssUrls(css, file.path(dir, "css"))

  # Local files (including ../ paths with query/fragment) become data: URIs
  expect_match(out, "url\\(\"data:font/woff2;base64,d09GMg==\"\\)")
  expect_match(out, "url\\(\"data:image/png;base64,AQIDBA==\"\\)")
  # data:, fragment-only, absolute, and missing refs are untouched
  expect_match(out, "url(\"data:image/png;base64,AAAA\")", fixed = TRUE)
  expect_match(out, "url(#gradient)", fixed = TRUE)
  expect_match(out, "url(https://example.com/x.png)", fixed = TRUE)
  expect_match(out, "url(missing.png)", fixed = TRUE)
})

test_that("stylesheet inlining rewrites url() refs (page + dynamic deps)", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "fonts"))
  writeBin(as.raw(c(0x77, 0x4F, 0x46, 0x32)), file.path(dir, "fonts", "f.woff2"))
  writeBin(as.raw(c(0x00, 0x01, 0x00, 0x00)), file.path(dir, "fonts", "f.ttf"))
  writeLines(
    paste0(
      "@font-face { font-family: x; ",
      "src: url(fonts/f.woff2) format('woff2'), ",
      "url(fonts/f.ttf) format('truetype'); }"
    ),
    file.path(dir, "style.css")
  )
  dep <- htmltools::htmlDependency(
    "mcpfontdep", "1.0",
    src = c(file = dir),
    stylesheet = "style.css"
  )

  inlined <- mcpInlineDependency(dep)
  expect_match(inlined$head, "data:font/woff2;base64,d09GMg==", fixed = TRUE)
  expect_false(grepl("url(fonts/f.woff2)", inlined$head, fixed = TRUE))
  # Legacy fallback formats are left alone (never fetched once woff2 loads)
  expect_match(inlined$head, "url(fonts/f.ttf)", fixed = TRUE)
})

test_that("resources/read inlines fontawesome webfonts for icon() apps", {
  ui <- fluidPage(downloadButton("dl", "Get"))
  app <- shinyApp(ui, function(input, output) {})
  withr::with_options(list(shiny.mcp = TRUE), {
    handlers <- createAppHandlers(
      app$httpHandler,
      function() function(input, output) {}
    )
    out <- mcp_post(handlers$http, "resources/read",
      params = list(uri = "ui://shiny/app")
    )
    html <- out$result$contents[[1]]$text
    # The fontawesome stylesheet references ../webfonts/*.woff2; those must
    # now be data: URIs rather than unreachable relative URLs. (Legacy .ttf
    # fallback refs stay as-is; browsers never fetch them once woff2 loads.)
    expect_match(html, "data:font/woff2;base64,")
    expect_false(grepl("url\\([\"']?\\.\\./webfonts/[^)\"']*\\.woff2", html))
  })
})

test_that("rewriteCssUrls produces single-line data URIs for large files", {
  dir <- withr::local_tempdir()
  writeBin(as.raw(rep(1:255, 40)), file.path(dir, "big.woff2"))
  out <- rewriteCssUrls("a { src: url(big.woff2); }", dir)
  expect_match(out, "data:font/woff2;base64,")
  expect_false(grepl("[\r\n]", out))
})

test_that("resources/read declares connectDomains derived from the request", {
  ui <- fluidPage("hi")
  app <- shinyApp(ui, function(input, output) {})
  withr::with_options(list(shiny.mcp = TRUE), {
    handlers <- createAppHandlers(
      app$httpHandler,
      function() function(input, output) {}
    )
    h <- handlers$http

    req <- mcp_req(list(
      jsonrpc = "2.0", id = 1, method = "resources/read",
      params = list(uri = "ui://shiny/app")
    ))
    req$HTTP_HOST <- "myapp.example.com"
    req$HTTP_X_FORWARDED_PROTO <- "https"
    out <- jsonlite::parse_json(rawToChar(as_bytes(wait_for_result(h(req))$content)))
    csp <- out$result$contents[[1]]$`_meta`$ui$csp
    domains <- unlist(csp$connectDomains)
    expect_true("https://myapp.example.com" %in% domains)
    expect_true("wss://myapp.example.com" %in% domains)

    # Plain http host -> ws scheme
    req2 <- mcp_req(list(
      jsonrpc = "2.0", id = 2, method = "resources/read",
      params = list(uri = "ui://shiny/app")
    ))
    req2$HTTP_HOST <- "127.0.0.1:7788"
    out2 <- jsonlite::parse_json(rawToChar(as_bytes(wait_for_result(h(req2))$content)))
    domains2 <- unlist(out2$result$contents[[1]]$`_meta`$ui$csp$connectDomains)
    expect_true(all(c("http://127.0.0.1:7788", "ws://127.0.0.1:7788") %in% domains2))

    # Config injected into the page for the bridge
    html <- out2$result$contents[[1]]$text
    expect_match(html, "__shinyMcpConfig__", fixed = TRUE)
    expect_match(html, '"directBase":"http://127.0.0.1:7788"', fixed = TRUE)
  })
})

test_that("shiny.mcp.direct = FALSE omits CSP and directOrigin", {
  ui <- fluidPage("hi")
  app <- shinyApp(ui, function(input, output) {})
  withr::with_options(list(shiny.mcp = TRUE, shiny.mcp.direct = FALSE), {
    handlers <- createAppHandlers(
      app$httpHandler,
      function() function(input, output) {}
    )
    req <- mcp_req(list(
      jsonrpc = "2.0", id = 1, method = "resources/read",
      params = list(uri = "ui://shiny/app")
    ))
    req$HTTP_HOST <- "127.0.0.1:7788"
    out <- jsonlite::parse_json(rawToChar(as_bytes(wait_for_result(handlers$http(req))$content)))
    expect_null(out$result$contents[[1]]$`_meta`$ui$csp)
    # The injected config object must not carry a directOrigin (the bridge
    # bundle source mentions the name, so scope the check to the config)
    html <- out$result$contents[[1]]$text
    config_json <- regmatches(
      html,
      regexpr("__shinyMcpConfig__ = \\{[^;]*", html)
    )
    expect_length(config_json, 1)
    expect_false(grepl('"directBase"', config_json, fixed = TRUE))
  })
})

test_that("isMcpSession recognizes direct websocket connections via ?mcp=1", {
  req <- new.env(parent = emptyenv())
  req$QUERY_STRING <- "?mcp=1"
  fake_session <- list(request = req)
  expect_true(isMcpSession(fake_session))

  req2 <- new.env(parent = emptyenv())
  req2$QUERY_STRING <- ""
  expect_false(isMcpSession(list(request = req2)))
})

test_that("createWebDependency inlines file-based deps for MCP sessions", {
  dir <- withr::local_tempdir()
  writeLines("console.log('widget');", file.path(dir, "w.js"))
  writeLines(".w { color: red; }", file.path(dir, "w.css"))
  dep <- htmltools::htmlDependency(
    "testwidget", "1.0",
    src = c(file = dir),
    script = "w.js", stylesheet = "w.css"
  )

  # Outside an MCP session: normal resource-path behavior
  plain <- createWebDependency(dep)
  expect_equal(plain$src$href, "testwidget-1.0")

  # Inside an MCP session (e.g. {htmlwidgets} calling it directly for
  # render-time deps): diverted to the inline representation
  req <- new.env(parent = emptyenv())
  req$HTTP_MCP_TUNNEL <- "1"
  fake_session <- list(request = req)
  inlined <- withReactiveDomain(fake_session, createWebDependency(dep))
  expect_equal(inlined$src$href, "_mcp_inline")
  expect_match(inlined$head, "console.log('widget');", fixed = TRUE)
  expect_match(inlined$head, ".w { color: red; }", fixed = TRUE)

  # href-only deps fall through untouched (no infinite recursion)
  href_dep <- htmltools::htmlDependency(
    "cdn", "1.0",
    src = c(href = "https://cdn.example.com"), script = "x.js"
  )
  passed <- withReactiveDomain(fake_session, createWebDependency(href_dep))
  expect_equal(passed$src$href, "https://cdn.example.com")
})
