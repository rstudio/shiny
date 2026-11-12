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

test_that("webMcpTool() checks its arguments", {
  f <- function(x) x
  x_str <- list(x = ellmer::type_string())
  expect_error(webMcpTool("t", "x", "d"), "`fun` must be a function")
  expect_error(webMcpTool("bad name", f, "d"), "`name` must be")
  expect_error(webMcpTool("t", f, 1), "`description` must be")
  expect_error(webMcpTool("t", f, "d", arguments = list(ellmer::type_string())), "named list")
  expect_error(webMcpTool("t", f, "d", arguments = list(x = "string")), "named list")
  expect_error(
    webMcpTool("t", f, "d", arguments = x_str, value = ellmer::type_string()),
    "`value` must be"
  )
  # Optional arguments must be named
  expect_error(webMcpTool("t", f, "d", x_str), class = "rlib_error_dots_nonempty")
  expect_error(
    webMcpTool("t", f, "d", arguments = c(x_str, y = ellmer::type_string())),
    "no argument for: y"
  )
  expect_error(webMcpTool("t", f, "d"), "no type for `fun`'s argument: x")
  # `input`, `output` and `session` are filled in, so they need no type and
  # can't have one
  expect_s3_class(webMcpTool("t", function(input, output, session) 1, "d"), "shiny_webmcp_tool")
  expect_error(
    webMcpTool("t", function(session) 1, "d", arguments = list(session = ellmer::type_string())),
    "can't describe `session`"
  )
  # Defaults and `...` don't need types
  expect_s3_class(webMcpTool("t", function(x = 1) x, "d"), "shiny_webmcp_tool")
  expect_s3_class(
    webMcpTool("t", function(...) 1, "d", arguments = list(y = ellmer::type_string())),
    "shiny_webmcp_tool"
  )
})

test_that("ellmer types become JSON Schema", {
  info <- webMcpToolInfo(summarize_tool(annotations = list(readOnlyHint = TRUE)), "ns-summarize")
  expect_equal(info$name, "ns-summarize")
  expect_equal(
    info$inputSchema,
    list(
      type = "object",
      properties = list(
        column = list(type = "string", enum = list("mpg", "hp"), description = "Column."),
        digits = list(type = "integer")
      ),
      required = list("column"),
      additionalProperties = FALSE
    )
  )
  expect_equal(info$outputSchema$required, list("column", "mean"))
  expect_equal(info$annotations, list(readOnlyHint = TRUE))

  arr <- webMcpTypeSchema(ellmer::type_array(ellmer::type_boolean()))
  expect_equal(arr, list(type = "array", items = list(type = "boolean")))

  # No arguments: `properties` is an empty JSON object
  empty <- webMcpToolInfo(webMcpTool("t", function() "hi", "d"), "t")
  expect_equal(
    as.character(toJSON(empty$inputSchema)),
    '{"type":"object","properties":{},"required":[],"additionalProperties":false}'
  )
})

test_that("values are checked against ellmer types", {
  conform <- function(type, x, simplify = TRUE) webMcpConform(type, x, "x", simplify)

  expect_equal(conform(ellmer::type_integer(), 3), 3L)
  expect_error(conform(ellmer::type_integer(), 3.5), "`x` must be an integer; got 3.5")
  expect_error(conform(ellmer::type_string(), 1), "must be a string")
  expect_error(conform(ellmer::type_boolean(), NA), "must be a boolean")
  expect_error(conform(ellmer::type_enum(c("a", "b")), "z"), 'one of "a", "b"')

  # Arrays: atomic vectors for R, lists for JSON (so length 1 stays an array)
  nums <- ellmer::type_array(ellmer::type_number())
  expect_equal(conform(nums, list(1, 2)), c(1, 2))
  expect_equal(conform(nums, list()), numeric(0))
  expect_equal(conform(nums, 5, simplify = FALSE), list(5))
  expect_error(conform(nums, list(1, "a")), "`x\\[2\\]` must be a number")

  obj <- ellmer::type_object(
    a = ellmer::type_string(),
    b = ellmer::type_array(ellmer::type_string(), required = FALSE)
  )
  expect_equal(conform(obj, list(a = "1", b = list("x"))), list(a = "1", b = "x"))
  expect_equal(conform(obj, list(a = "1")), list(a = "1"))
  expect_error(conform(obj, list(b = list())), "Missing required parameter `x.a`")
  expect_error(conform(obj, list(a = "1", c = 2)), "Unknown parameter `x.c`")
  expect_error(conform(obj, "a"), "`x` must be an object")
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
  expect_error(webMcpCall(session, session$ns("summarize"), list(column = "cyl")), 'Invalid input: `column` must be one of "mpg", "hp"')
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

  # `input`, `output` and `session` are filled in when `fun` takes them, in
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
    "mod_n", function(input, output) list(n = input$n), "d"
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

test_that("webMcpCall() supports promises", {
  skip_if_not_installed("later")
  withr::local_options(shiny.webmcp = TRUE)
  local_mocked_bindings(sendWebMcpTools = function(...) NULL)
  session <- MockShinySession$new()
  withr::defer(session$close())

  registerWebMcpTool(session = session, webMcpTool(
    "later_ok", function() promises::promise_resolve(list(ok = TRUE)), "d",
    value = ellmer::type_object(ok = ellmer::type_boolean())
  ))
  result <- NULL
  promises::then(webMcpCall(session, session$ns("later_ok"), list()), function(x) result <<- x)
  while (is.null(result) && !later::loop_empty()) later::run_now()
  expect_equal(result, list(ok = TRUE))
})
