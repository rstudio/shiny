#' Define a WebMCP tool in R
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Describes an R function that AI agents can call through WebMCP when
#' `options(shiny.webmcp = TRUE)`. Pass the result to [registerWebMcpTool()].
#'
#' Like a module server function, `fun` is called with the session's `input`,
#' `output` and `session`, followed by the arguments the agent sent. It runs
#' inside [isolate()], so it can read reactive values and inputs.
#'
#' The argument and value types use
#' [ellmer type specifications](https://ellmer.tidyverse.org/reference/type_boolean.html)
#' (`ellmer::type_string()`, `ellmer::type_object()`, ...). They are sent to
#' the agent as JSON Schema, and checked on every call: arguments before `fun`
#' runs, and the return value before it is sent back. Arrays of scalars
#' arrive as atomic vectors.
#'
#' @param fun A function taking `input`, `output` and `session`, then one
#'   argument per entry of `arguments`. It returns a string (sent as text),
#'   a list (sent as JSON), or a promise of either.
#' @param name The tool's name: letters, numbers, `_`, `.` or `-`. Inside a
#'   module, [registerWebMcpTool()] prefixes it with the module's namespace.
#' @param description What the tool does and when an agent should use it.
#' @param arguments A named list of ellmer types, one per argument of `fun`.
#'   Arguments with `required = FALSE` may be missing, in which case `fun`'s
#'   default is used.
#' @param value An `ellmer::type_object()` describing the list `fun` returns.
#'   It becomes the tool's `outputSchema`. `NULL` (the default) skips the
#'   check.
#' @param annotations A named list of WebMCP tool annotations, e.g.
#'   `list(readOnlyHint = TRUE)`.
#' @return A `shiny_webmcp_tool` object.
#'
#' @seealso [registerWebMcpTool()], [webMcpAccess()]
#' @examplesIf rlang::is_installed("ellmer")
#' summarize_column <- webMcpTool(
#'   function(input, output, session, column) {
#'     x <- mtcars[[column]]
#'     list(mean = mean(x), min = min(x), max = max(x))
#'   },
#'   name = "summarize_column",
#'   description = "Summary statistics for one column of the mtcars data.",
#'   arguments = list(
#'     column = ellmer::type_enum(names(mtcars), "Column to summarize.")
#'   ),
#'   value = ellmer::type_object(
#'     mean = ellmer::type_number(),
#'     min = ellmer::type_number(),
#'     max = ellmer::type_number()
#'   ),
#'   annotations = list(readOnlyHint = TRUE)
#' )
#' @export
webMcpTool <- function(
  fun,
  name,
  description,
  arguments = list(),
  value = NULL,
  annotations = list()
) {
  if (!is.function(fun)) {
    stop("`fun` must be a function.", call. = FALSE)
  }
  if (!rlang::is_string(name) || !grepl("^[A-Za-z0-9_.-]{1,128}$", name)) {
    stop(
      "`name` must be a string of letters, numbers, `_`, `.` or `-`.",
      call. = FALSE
    )
  }
  if (!rlang::is_string(description)) {
    stop("`description` must be a string.", call. = FALSE)
  }
  if (length(arguments) > 0 || !is.null(value)) {
    rlang::check_installed(c("ellmer", "S7"), "to describe WebMCP tool types.")
  }
  if (
    !is.list(arguments) ||
      (length(arguments) > 0 && !rlang::is_named(arguments)) ||
      anyDuplicated(names(arguments)) ||
      !all(vapply(arguments, inherits, logical(1), "ellmer::Type"))
  ) {
    stop("`arguments` must be a named list of ellmer types.", call. = FALSE)
  }
  if (!is.null(value) && !inherits(value, "ellmer::TypeObject")) {
    stop("`value` must be `NULL` or an `ellmer::type_object()`.", call. = FALSE)
  }
  if (!is.list(annotations) || (length(annotations) > 0 && !rlang::is_named(annotations))) {
    stop("`annotations` must be a named list.", call. = FALSE)
  }

  formalNames <- names(formals(fun))
  if (!identical(formalNames[1:3], c("input", "output", "session"))) {
    stop(
      "`fun` must take `input`, `output` and `session` as its first three ",
      "arguments, like a module server function.",
      call. = FALSE
    )
  }
  if (!"..." %in% formalNames) {
    missingFormals <- setdiff(names(arguments), formalNames)
    if (length(missingFormals) > 0) {
      stop(
        "`fun` has no argument for: ", paste(missingFormals, collapse = ", "),
        call. = FALSE
      )
    }
  }

  structure(
    list(
      fun = fun,
      name = name,
      description = description,
      arguments = arguments,
      value = value,
      annotations = annotations
    ),
    class = "shiny_webmcp_tool"
  )
}

#' @export
print.shiny_webmcp_tool <- function(x, ...) {
  cat("<shiny_webmcp_tool> ", x$name, "\n", x$description, "\n", sep = "")
  if (length(x$arguments) > 0) {
    cat("Arguments: ", paste(names(x$arguments), collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

#' Register a WebMCP tool defined in R
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Makes a [webMcpTool()] available to AI agents in the browser, next to the
#' built-in WebMCP tools. Does nothing unless `options(shiny.webmcp = TRUE)`.
#'
#' * **Outside the server function** (e.g. at the top of `app.R`), the tool is
#'   registered for every session that starts afterwards.
#' * **Inside the server function or a module**, it is registered for that
#'   session only. In a module, the tool's name is prefixed with the module's
#'   namespace (`"filters-summarize"`), and `fun` gets the module's `input`,
#'   `output` and `session`.
#'
#' When an agent calls the tool, the browser sends the arguments to the server,
#' where `fun` runs. Treat the arguments like inputs: anyone with the browser's
#' developer tools can call the tool with any value that passes the argument
#' types.
#'
#' @param tool A [webMcpTool()].
#' @param session The session to register the tool in, or `NULL` to register
#'   it for all future sessions. The default is the current session when
#'   called inside the server function, otherwise `NULL`.
#' @return Invisibly, a function that unregisters the tool.
#'
#' @seealso [webMcpTool()]
#' @examplesIf rlang::is_installed("ellmer") && interactive()
#' options(shiny.webmcp = TRUE)
#'
#' ui <- fluidPage(
#'   selectInput("cyl", "Cylinders", c(4, 6, 8)),
#'   tableOutput("cars")
#' )
#'
#' server <- function(input, output, session) {
#'   cars <- reactive(mtcars[mtcars$cyl == input$cyl, ])
#'   output$cars <- renderTable(cars())
#'
#'   registerWebMcpTool(webMcpTool(
#'     function(input, output, session, column) {
#'       x <- cars()[[column]]
#'       list(mean = mean(x), n = length(x))
#'     },
#'     name = "summarize_shown_cars",
#'     description = "Mean of one column for the cars shown in the table.",
#'     arguments = list(column = ellmer::type_enum(names(mtcars)))
#'   ))
#' }
#'
#' shinyApp(ui, server)
#' @export
registerWebMcpTool <- function(tool, session = getDefaultReactiveDomain()) {
  if (!inherits(tool, "shiny_webmcp_tool")) {
    stop("`tool` must be made by `webMcpTool()`.", call. = FALSE)
  }

  if (is.null(session)) {
    checkWebMcpToolName(tool$name)
    .globals$webMcpTools[[tool$name]] <- tool
    return(invisible(function() {
      .globals$webMcpTools[[tool$name]] <- NULL
      invisible()
    }))
  }

  if (!webmcp_enabled()) {
    return(invisible(function() invisible()))
  }

  name <- session$ns(tool$name)
  checkWebMcpToolName(name)
  token <- session$token
  if (is.null(webMcpSessionTools[[token]])) {
    webMcpSessionTools[[token]] <- list()
    session$onSessionEnded(function() {
      rm(list = token, envir = webMcpSessionTools)
    })
  }
  webMcpSessionTools[[token]][[name]] <- list(tool = tool, session = session)
  sendWebMcpTools(session, add = list(webMcpToolInfo(tool, name)))

  invisible(function() {
    if (is.null(webMcpSessionTools[[token]][[name]])) return(invisible())
    webMcpSessionTools[[token]][[name]] <- NULL
    sendWebMcpTools(session, remove = list(name))
    invisible()
  })
}

# Per-session tools, keyed by session token, then by (namespaced) tool name
webMcpSessionTools <- new.env(parent = emptyenv())

webMcpBuiltinToolNames <- c("list_inputs", "set_input", "list_outputs", "get_output")

checkWebMcpToolName <- function(name) {
  if (name %in% webMcpBuiltinToolNames) {
    stop("`", name, "` is a built-in WebMCP tool name.", call. = FALSE)
  }
}

sendWebMcpTools <- function(session, add = list(), remove = list()) {
  session$sendCustomMessage("shiny.webmcp.tools", list(add = add, remove = remove))
}

# Called when a session starts: send the tools registered for all sessions
sendGlobalWebMcpTools <- function(session) {
  tools <- .globals$webMcpTools
  if (!webmcp_enabled() || length(tools) == 0) return()
  sendWebMcpTools(session, add = unname(lapply(tools, function(tool) {
    webMcpToolInfo(tool, tool$name)
  })))
}

# The tool's WebMCP descriptor (everything but `execute`)
webMcpToolInfo <- function(tool, name) {
  info <- list(
    name = name,
    description = tool$description,
    inputSchema = webMcpObjectSchema(tool$arguments, additional = FALSE)
  )
  if (!is.null(tool$value)) info$outputSchema <- webMcpTypeSchema(tool$value)
  if (length(tool$annotations) > 0) info$annotations <- tool$annotations
  info
}

# Run a tool for the `@webmcpCall` RPC. Errors become the RPC's error message;
# with `options(shiny.sanitize.errors = TRUE)`, only `safeError()`s (and the
# argument checks) keep their message.
webMcpCall <- function(session, name, args) {
  if (!webmcp_enabled()) stop("WebMCP is not enabled for this app.")
  entry <- webMcpSessionTools[[session$token]][[name]]
  if (is.null(entry) && !is.null(.globals$webMcpTools[[name]])) {
    entry <- list(tool = .globals$webMcpTools[[name]], session = session)
  }
  if (is.null(entry)) stop(safeError(paste0('No R tool named "', name, '".')))

  tool <- entry$tool
  scope <- entry$session
  args <- tryCatch(
    webMcpConformObject(tool$arguments, FALSE, args %||% list(), "", simplify = TRUE),
    error = function(e) stop(safeError(paste("Invalid input:", conditionMessage(e))))
  )

  sanitize <- function(e) {
    if (inherits(e, "shiny.custom.error") || !getOption("shiny.sanitize.errors", FALSE)) {
      stop(e)
    }
    message('Error in WebMCP tool "', name, '": ', conditionMessage(e))
    stop(paste0('Tool "', name, '" failed. Check the app logs for details.'))
  }

  hybrid_chain(
    withReactiveDomain(scope, isolate(
      do.call(tool$fun, c(
        list(input = scope$input, output = scope$output, session = scope),
        args
      ))
    )),
    function(result) {
      if (is.null(tool$value)) return(result)
      tryCatch(
        webMcpConform(tool$value, result, "value", simplify = FALSE),
        error = function(e) {
          stop(safeError(paste0(
            'Tool "', name, '" returned a value that does not match its `value` type: ',
            conditionMessage(e)
          )))
        }
      )
    },
    catch = sanitize
  )
}

# ---- ellmer types -> JSON Schema ---------------------------------------------

webMcpTypeSchema <- function(type) {
  prop <- function(name) S7::prop(type, name)

  schema <- if (inherits(type, "ellmer::TypeBasic")) {
    list(type = prop("type"))
  } else if (inherits(type, "ellmer::TypeEnum")) {
    list(type = "string", enum = as.list(prop("values")))
  } else if (inherits(type, "ellmer::TypeArray")) {
    list(type = "array", items = webMcpTypeSchema(prop("items")))
  } else if (inherits(type, "ellmer::TypeObject")) {
    webMcpObjectSchema(prop("properties"), prop("additional_properties"))
  } else if (inherits(type, "ellmer::TypeJsonSchema")) {
    prop("json")
  } else {
    stop("Unsupported ellmer type: ", class(type)[[1]], call. = FALSE)
  }
  description <- prop("description")
  if (!is.null(description)) schema$description <- description
  schema
}

webMcpObjectSchema <- function(properties, additional) {
  required <- names(properties)[
    vapply(properties, function(p) isTRUE(S7::prop(p, "required")), logical(1))
  ]
  list(
    type = "object",
    # A named empty list, so it serializes as `{}`
    properties = stats::setNames(
      lapply(properties, webMcpTypeSchema),
      names(properties) %||% character(0)
    ),
    required = as.list(required),
    additionalProperties = isTRUE(additional)
  )
}

# ---- Checking values against ellmer types -------------------------------------

# Checks `x` against `type` and returns it ready for R (`simplify = TRUE`:
# arrays of scalars become atomic vectors) or for JSON (`simplify = FALSE`:
# arrays become lists, so a length-1 array stays an array).
webMcpConform <- function(type, x, path, simplify) {
  prop <- function(name) S7::prop(type, name)
  fail <- function(what) {
    got <- if (is.null(x)) "null" else as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
    stop("`", path, "` must be ", what, "; got ", got, call. = FALSE)
  }
  scalar <- function(ok) is.atomic(x) && length(x) == 1 && !is.na(x) && ok

  if (inherits(type, "ellmer::TypeBasic")) {
    switch(
      prop("type"),
      string = if (!scalar(is.character(x))) fail("a string"),
      boolean = if (!scalar(is.logical(x))) fail("a boolean"),
      number = if (!scalar(is.numeric(x))) fail("a number"),
      integer = {
        if (!scalar(is.numeric(x) && x == round(x))) fail("an integer")
        x <- as.integer(x)
      }
    )
    return(x)
  }
  if (inherits(type, "ellmer::TypeEnum")) {
    values <- prop("values")
    if (!scalar(is.character(x) && x %in% values)) {
      fail(paste0("one of ", paste0('"', values, '"', collapse = ", ")))
    }
    return(x)
  }
  if (inherits(type, "ellmer::TypeArray")) {
    if (!(is.list(x) && is.null(names(x))) && !(is.atomic(x) && is.null(dim(x)))) {
      fail("an array")
    }
    items <- prop("items")
    out <- lapply(seq_along(x), function(i) {
      webMcpConform(items, x[[i]], paste0(path, "[", i, "]"), simplify)
    })
    scalarItems <- inherits(items, c("ellmer::TypeBasic", "ellmer::TypeEnum"))
    if (simplify && scalarItems) {
      return(if (length(out) == 0) vector(atomicMode(items)) else unlist(out))
    }
    return(out)
  }
  if (inherits(type, "ellmer::TypeObject")) {
    return(webMcpConformObject(
      prop("properties"), prop("additional_properties"), x, path, simplify
    ))
  }
  # ellmer::TypeJsonSchema and anything else: not checked
  x
}

atomicMode <- function(type) {
  if (inherits(type, "ellmer::TypeEnum")) return("character")
  switch(
    S7::prop(type, "type"),
    string = "character", boolean = "logical", integer = "integer", "numeric"
  )
}

webMcpConformObject <- function(properties, additional, x, path, simplify) {
  key <- function(k) if (nzchar(path)) paste0(path, ".", k) else k
  isObject <- is.list(x) && (length(x) == 0 || rlang::is_named(x))
  if (!isObject) {
    got <- as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
    stop(
      if (nzchar(path)) paste0("`", path, "` must be an object") else "Input must be an object",
      "; got ", got,
      call. = FALSE
    )
  }
  if (!isTRUE(additional)) {
    unknown <- setdiff(names(x), names(properties))
    if (length(unknown) > 0) {
      stop("Unknown parameter `", key(unknown[[1]]), "`.", call. = FALSE)
    }
  }
  for (k in names(properties)) {
    if (is.null(x[[k]])) {
      if (isTRUE(S7::prop(properties[[k]], "required"))) {
        stop("Missing required parameter `", key(k), "`.", call. = FALSE)
      }
      x[[k]] <- NULL
      next
    }
    x[[k]] <- webMcpConform(properties[[k]], x[[k]], key(k), simplify)
  }
  x
}
