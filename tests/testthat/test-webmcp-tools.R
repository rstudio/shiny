skip_if_not_installed("ellmer")
skip_if_not_installed("S7")

summarize_tool <- function(...) {
  webMcpTool(
    "summarize",
    function(column, digits = 1) {
      list(column = column, mean = round(mean(mtcars[[column]]), digits))
    },
    "Mean of one mtcars column.",
    arguments = list(
      column = ellmer::type_enum(c("mpg", "hp"), "Column."),
      digits = ellmer::type_integer(required = FALSE)
    ),
    value = ellmer::type_object(
      column = ellmer::type_string(),
      mean = ellmer::type_number()
    ),
    ...
  )
}

# The shape of `arguments` and `value` (defining, JSON Schema, checking) is
# covered in test-webmcp-tools-types.R.

test_that("webMcpTool() checks its other arguments", {
  f <- function() 1
  expect_error(webMcpTool("t", "x", "d"), "`fun` must be a function")
  for (name in list("bad name", "", 1, c("a", "b"), strrep("a", 129))) {
    expect_error(webMcpTool(name, f, "d"), "`name` must be", info = format(name))
  }
  expect_s3_class(webMcpTool("a.b-c_1", f, "d"), "shiny_webmcp_tool")
  expect_error(webMcpTool("t", f, 1), "`description` must be")
  expect_error(webMcpTool("t", f, "d", annotations = list(TRUE)), "`annotations` must be")
  # Optional arguments must be named
  expect_error(webMcpTool("t", f, "d", list()), class = "rlib_error_dots_nonempty")
  # `input` and `session` are filled in, so they need no type
  expect_s3_class(webMcpTool("t", function(input, session) 1, "d"), "shiny_webmcp_tool")
})

test_that("webMcpTool() refuses a `fun` that takes `...`", {
  # Agents can only send the keys listed in `arguments`: the input schema has
  # `additionalProperties: false`, and both the browser and R reject unknown
  # keys. So `...` could only ever receive typed arguments that `fun` doesn't
  # name, which hides them from its signature. Free-form data goes in a
  # declared argument: an array of name-value pairs, as ellmer recommends.
  # (`ellmer::type_object(.additional_properties = TRUE)` is deprecated as of
  # ellmer 0.5.0 because providers' structured output can't fill open objects.)
  expect_error(webMcpTool("t", function(...) 1, "d"), "can't take `...`")
  expect_error(
    webMcpTool("t", function(x, ...) x, "d", arguments = list(x = ellmer::type_string())),
    "can't take `...`. WebMCP can only send the keys listed in `arguments`. If free-form data is needed, wrap it in a defined argument.",
    fixed = TRUE
  )
})

test_that("webMcpToolInfo() describes the tool", {
  info <- webMcpToolInfo(summarize_tool(annotations = list(readOnlyHint = TRUE)), "ns-summarize")
  expect_equal(info$name, "ns-summarize")
  expect_equal(info$description, "Mean of one mtcars column.")
  expect_named(info$inputSchema$properties, c("column", "digits"))
  expect_equal(info$outputSchema$required, list("column", "mean"))
  expect_equal(info$annotations, list(readOnlyHint = TRUE))

  # No `value` or annotations: left out
  info <- webMcpToolInfo(webMcpTool("t", function() "hi", "d"), "t")
  expect_named(info, c("name", "description", "inputSchema"))
})

test_that("registerWebMcpTool() registers for all sessions or one session", {
  withr::local_options(shiny.webmcp = TRUE)
  sent <- list()
  local_mocked_bindings(sendWebMcpTools = function(session, add = list(), remove = list()) {
    sent[[length(sent) + 1]] <<- list(add = add, remove = remove)
  })

  # Outside a session: for all sessions, sent when one starts
  unregister <- registerWebMcpTool(summarize_tool(), session = NULL)
  withr::defer(unregister())
  expect_named(.globals$webMcpTools, "summarize")
  session <- MockShinySession$new()
  sendGlobalWebMcpTools(session)
  expect_equal(sent[[1]]$add[[1]]$name, "summarize")

  # Inside a module: namespaced, and sent right away
  scope <- session$makeScope("mod")
  off <- registerWebMcpTool(summarize_tool(), session = scope)
  expect_equal(sent[[2]]$add[[1]]$name, "mod-summarize")
  expect_named(webMcpSessionTools[[session$token]], "mod-summarize")
  off()
  expect_equal(sent[[3]]$remove, list("mod-summarize"))
  expect_length(webMcpSessionTools[[session$token]], 0)

  # Removed when the session ends
  registerWebMcpTool(summarize_tool(), session = session)
  session$close()
  expect_null(webMcpSessionTools[[session$token]])

  expect_error(
    registerWebMcpTool(webMcpTool("set_input", function() 1, "d"), NULL),
    "built-in"
  )
  expect_error(registerWebMcpTool(list()), "made by `webMcpTool\\(\\)`")
})

test_that("registerWebMcpTool() does nothing in a session when WebMCP is off", {
  withr::local_options(shiny.webmcp = FALSE)
  session <- MockShinySession$new()
  registerWebMcpTool(summarize_tool(), session = session)
  expect_null(webMcpSessionTools[[session$token]])
})

test_that("webMcpCall() checks arguments, runs the tool, and checks its value", {
  withr::local_options(shiny.webmcp = TRUE)
  local_mocked_bindings(sendWebMcpTools = function(...) NULL)
  session <- MockShinySession$new()
  withr::defer(session$close())

  registerWebMcpTool(summarize_tool(), session = session)
  expect_equal(
    webMcpCall(session, session$ns("summarize"), list(column = "mpg")),
    list(column = "mpg", mean = 20.1)
  )
  expect_equal(
    webMcpCall(session, session$ns("summarize"), list(column = "mpg", digits = 0))$mean,
    20
  )
  expect_error(webMcpCall(session, session$ns("summarize"), list()), "Invalid input: Missing required parameter `column`")
  expect_error(webMcpCall(session, "nope", list()), 'No R tool named "nope"')

  # The tool runs isolated, with the calling session as the reactive domain
  session$setInputs(n = 3)
  registerWebMcpTool(session = session, webMcpTool(
    "read_n",
    function() list(n = getDefaultReactiveDomain()$input$n),
    "d",
    value = ellmer::type_object(n = ellmer::type_integer())
  ))
  expect_equal(webMcpCall(session, session$ns("read_n"), list()), list(n = 3L))

  # `input` and `session` are filled in when `fun` takes them, in
  # any position, and agents can't set them
  registerWebMcpTool(session = session, webMcpTool(
    "add_n",
    function(session, by, input) {
      list(total = input$n + by, same = identical(session, getDefaultReactiveDomain()))
    },
    "d",
    arguments = list(by = ellmer::type_integer())
  ))
  expect_equal(
    webMcpCall(session, session$ns("add_n"), list(by = 2)),
    list(total = 5, same = TRUE)
  )
  expect_error(
    webMcpCall(session, session$ns("add_n"), list(by = 2, session = "x")),
    "Unknown parameter `session`"
  )
  # In a module, they are the module's
  scope <- session$makeScope("mod")
  session$setInputs(`mod-n` = 10)
  registerWebMcpTool(session = scope, webMcpTool(
    "mod_n", function(input) list(n = input$n), "d"
  ))
  expect_equal(webMcpCall(session, scope$ns("mod_n"), list()), list(n = 10))

  # A value that doesn't match `value`
  registerWebMcpTool(session = session, webMcpTool(
    "bad_value", function() list(n = "three"), "d",
    value = ellmer::type_object(n = ellmer::type_integer())
  ))
  expect_error(webMcpCall(session, session$ns("bad_value"), list()), "does not match its `value` type: `value.n` must be an integer")

  # Errors are hidden with shiny.sanitize.errors, unless they're safeError()s
  registerWebMcpTool(session = session, webMcpTool(
    "boom", function() stop("secret"), "d"
  ))
  expect_error(webMcpCall(session, session$ns("boom"), list()), "secret")
  withr::local_options(shiny.sanitize.errors = TRUE)
  expect_message(
    expect_error(webMcpCall(session, session$ns("boom"), list()), "failed. Check the app logs"),
    "secret"
  )
  expect_error(webMcpCall(session, session$ns("summarize"), list()), "Missing required parameter")

  withr::local_options(shiny.webmcp = FALSE)
  expect_error(webMcpCall(session, session$ns("summarize"), list(column = "mpg")), "not enabled")
})

test_that("the calling session is the default reactive domain, wherever the tool was registered", {
  withr::local_options(shiny.webmcp = TRUE)
  local_mocked_bindings(sendWebMcpTools = function(...) NULL)
  whoami <- function(name) {
    webMcpTool(name, function() {
      domain <- getDefaultReactiveDomain()
      list(token = domain$token, ns = domain$ns("x"))
    }, "d")
  }

  # For every session: each call runs in the session that made it
  unregister <- registerWebMcpTool(whoami("global"), session = NULL)
  withr::defer(unregister())
  s1 <- MockShinySession$new()
  s2 <- MockShinySession$new()
  withr::defer({ s1$close(); s2$close() })
  expect_equal(webMcpCall(s1, "global", list())$token, s1$token)
  expect_equal(webMcpCall(s2, "global", list())$token, s2$token)

  # For one session, and in a module: the module's scope
  registerWebMcpTool(whoami("own"), session = s1)
  expect_equal(webMcpCall(s1, s1$ns("own"), list()), list(token = s1$token, ns = s1$ns("x")))

  scope <- s1$makeScope("mod")
  registerWebMcpTool(whoami("own"), session = scope)
  expect_equal(webMcpCall(s1, scope$ns("own"), list()), list(token = s1$token, ns = scope$ns("x")))
})

test_that("a tool changes what the app shows through a reactive value", {
  withr::local_options(shiny.webmcp = TRUE)
  local_mocked_bindings(sendWebMcpTools = function(...) NULL)

  testServer(
    function(input, output, session) {
      label <- reactiveVal("start")
      output$label <- renderText(label())
      registerWebMcpTool(webMcpTool(
        "set_label",
        function(text) {
          label(text)
          "done"
        },
        "Set the label.",
        arguments = list(text = ellmer::type_string())
      ))
    },
    {
      expect_equal(output$label, "start")
      expect_equal(webMcpCall(session, session$ns("set_label"), list(text = "agent")), "done")
      session$flushReact()
      expect_equal(output$label, "agent")
    }
  )
})

test_that("webMcpCall() supports promises", {
  skip_if_not_installed("later")
  withr::local_options(shiny.webmcp = TRUE)
  local_mocked_bindings(sendWebMcpTools = function(...) NULL)
  session <- MockShinySession$new()
  withr::defer(session$close())
  register <- function(name, fun, ...) {
    registerWebMcpTool(session = session, webMcpTool(name, fun, "d", ...))
  }
  # The promise's value, or its error message
  settle <- function(name) {
    done <- FALSE
    out <- NULL
    promises::then(
      webMcpCall(session, session$ns(name), list()),
      onFulfilled = function(x) out <<- x,
      onRejected = function(e) out <<- paste("error:", conditionMessage(e))
    ) |> promises::finally(function() done <<- TRUE)
    while (!done) later::run_now()
    out
  }

  register(
    "later_ok", function() promises::promise_resolve(list(ok = TRUE)),
    value = ellmer::type_object(ok = ellmer::type_boolean())
  )
  expect_equal(settle("later_ok"), list(ok = TRUE))

  # The resolved value is checked against `value`
  register(
    "later_bad", function() promises::promise_resolve(list(ok = "yes")),
    value = ellmer::type_object(ok = ellmer::type_boolean())
  )
  expect_match(settle("later_bad"), "does not match its `value` type: `value.ok` must be a boolean")

  # Rejections are sanitized like synchronous errors
  register("later_boom", function() promises::promise_reject(simpleError("secret")))
  expect_match(settle("later_boom"), "error: secret")
  withr::local_options(shiny.sanitize.errors = TRUE)
  # capture_messages(): expect_message() lets a message from a later callback
  # leak into the test output
  logged <- capture_messages(out <- settle("later_boom"))
  expect_match(out, "failed. Check the app logs")
  expect_length(logged, 1)
  expect_match(logged, "secret")
})

test_that("registering a name again replaces the tool", {
  withr::local_options(shiny.webmcp = TRUE)
  sent <- list()
  local_mocked_bindings(sendWebMcpTools = function(session, add = list(), remove = list()) {
    sent[[length(sent) + 1]] <<- list(add = add, remove = remove)
  })
  answer <- function(x) webMcpTool("answer", function() x, "d")
  session <- MockShinySession$new()
  withr::defer(session$close())

  # For every session: the last one wins
  off <- registerWebMcpTool(answer("first"), session = NULL)
  registerWebMcpTool(answer("second"), session = NULL)
  withr::defer(off())
  expect_equal(webMcpCall(session, "answer", list()), "second")
  expect_length(.globals$webMcpTools, 1)

  # In one session: the last one wins, and each is sent to the browser
  # (which replaces a tool registered under the same name)
  name <- session$ns("answer")
  registerWebMcpTool(answer("third"), session = session)
  registerWebMcpTool(answer("fourth"), session = session)
  expect_equal(webMcpCall(session, name, list()), "fourth")
  expect_equal(vapply(sent, function(m) m$add[[1]]$name, ""), c(name, name))
})

test_that("a session's own tool comes before a tool for every session", {
  withr::local_options(shiny.webmcp = TRUE)
  local_mocked_bindings(sendWebMcpTools = function(...) NULL)
  session <- MockShinySession$new()
  withr::defer(session$close())
  # The mock session prefixes its own ids, so register for every session under
  # the name this session's tool will have
  name <- session$ns("answer")

  off <- registerWebMcpTool(webMcpTool(name, function() "everyone", "d"), session = NULL)
  withr::defer(off())
  expect_equal(webMcpCall(session, name, list()), "everyone")

  off_own <- registerWebMcpTool(webMcpTool("answer", function() "own", "d"), session = session)
  expect_equal(webMcpCall(session, name, list()), "own")
  # Unregistering the session's tool falls back to the one for every session
  off_own()
  expect_equal(webMcpCall(session, name, list()), "everyone")
})

test_that("unregistering removes the tool once", {
  withr::local_options(shiny.webmcp = TRUE)
  sent <- list()
  local_mocked_bindings(sendWebMcpTools = function(session, add = list(), remove = list()) {
    sent[[length(sent) + 1]] <<- list(add = add, remove = remove)
  })
  session <- MockShinySession$new()
  withr::defer(session$close())
  tool <- webMcpTool("t", function() 1, "d")

  off <- registerWebMcpTool(tool, session = NULL)
  off()
  expect_null(.globals$webMcpTools[["t"]])
  expect_error(webMcpCall(session, "t", list()), 'No R tool named "t"')

  off <- registerWebMcpTool(tool, session = session)
  off()
  off()
  removes <- Filter(function(m) length(m$remove) > 0, sent)
  expect_length(removes, 1)
  expect_error(webMcpCall(session, session$ns("t"), list()), "No R tool named")
})

test_that("tools for every session are sent only when there are some and WebMCP is on", {
  sent <- 0
  local_mocked_bindings(sendWebMcpTools = function(...) sent <<- sent + 1)
  session <- MockShinySession$new()
  withr::defer(session$close())

  withr::local_options(shiny.webmcp = TRUE)
  sendGlobalWebMcpTools(session)
  expect_equal(sent, 0)

  off <- registerWebMcpTool(webMcpTool("t", function() 1, "d"), session = NULL)
  withr::defer(off())
  withr::local_options(shiny.webmcp = FALSE)
  sendGlobalWebMcpTools(session)
  expect_equal(sent, 0)

  withr::local_options(shiny.webmcp = TRUE)
  sendGlobalWebMcpTools(session)
  expect_equal(sent, 1)
})

test_that("the tools message always has `add` and `remove` arrays", {
  message <- NULL
  fake_session <- list(sendCustomMessage = function(type, msg) message <<- list(type = type, msg = msg))
  info <- webMcpToolInfo(webMcpTool("t", function() 1, "d"), "t")

  sendWebMcpTools(fake_session, add = list(info))
  expect_equal(message$type, "shiny.webmcp.tools")
  json <- as.character(toJSON(message$msg))
  expect_match(json, '^\\{"add":\\[\\{"name":"t",')
  expect_match(json, '"remove":\\[\\]\\}$')

  sendWebMcpTools(fake_session, remove = list("t"))
  expect_equal(as.character(toJSON(message$msg)), '{"add":[],"remove":["t"]}')
})
