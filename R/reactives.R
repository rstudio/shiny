#' @include utils.R
NULL

Dependents <- R6Class(
  'Dependents',
  portable = FALSE,
  class = FALSE,
  public = list(
    .reactId = character(0),
    .dependents = 'Map',

    initialize = function(reactId = NULL) {
      .reactId <<- reactId
      .dependents <<- Map$new()
    },
    # ... ignored, use to be depLabel and depId, not used anymore
    register = function(...) {
      ctx <- getCurrentContext()
      if (!.dependents$containsKey(ctx$id)) {

        # must wrap in if statement as ctx react id could be NULL
        #   if options(shiny.suppressMissingContextError = TRUE)
        if (is.character(.reactId) && is.character(ctx$.reactId)) {
          rLog$dependsOn(ctx$.reactId, .reactId, ctx$id, ctx$.domain)
        }

        if (ctx$isWeak()) {
          .dependents$set(ctx$id, rlang::new_weakref(ctx))
        } else {
          .dependents$set(ctx$id, ctx)
        }

        ctx$onInvalidate(function() {
          rLog$dependsOnRemove(ctx$.reactId, .reactId, ctx$id, ctx$.domain)
          .dependents$remove(ctx$id)
        })
      }
    },
    # at times, the context is run in a ctx$onInvalidate(...) which has no runtime context
    invalidate = function(log = TRUE) {
      if (isTRUE(log)) {

        domain <- getDefaultReactiveDomain()
        rLog$invalidateStart(.reactId, NULL, "other", domain)
        on.exit(
          rLog$invalidateEnd(.reactId, NULL, "other", domain),
          add = TRUE
        )
      }
      lapply(
        .dependents$values(sort = TRUE),
        function(ctx) {
          if (rlang::is_weakref(ctx)) {
            ctx <- rlang::wref_key(ctx)
            if (is.null(ctx)) {
              # Can get here if weakref target was GC'd
              return()
            }
          }
          ctx$invalidate()
          NULL
        }
      )
    }
  )
)


# ReactiveVal ---------------------------------------------------------------

ReactiveVal <- R6Class(
  'ReactiveVal',
  portable = FALSE,
  private = list(
    reactId = character(0),
    value = NULL,
    label = NULL,
    frozen = FALSE,
    dependents = NULL
  ),
  public = list(
    .isRecordingOtel = FALSE, # needs to be accessed/set within Shiny
    .otelLabel = NULL,
    .otelAttrs = NULL, # needs to be accessed/set within Shiny

    initialize = function(value, label = NULL) {
      reactId <- nextGlobalReactId()
      private$reactId <- reactId
      private$value <- value
      private$label <- label
      private$dependents <- Dependents$new(reactId = private$reactId)

      domain <- getDefaultReactiveDomain()
      rLog$define(private$reactId, value, private$label, type = "reactiveVal", domain)
      .otelLabel <<- otel_label_set_reactive_val(private$label, domain = domain)
    },
    get = function() {
      private$dependents$register()

      if (private$frozen)
      reactiveStop()

      private$value
    },
    set = function(value) {
      if (identical(private$value, value)) {
        return(invisible(FALSE))
      }

      domain <- getDefaultReactiveDomain()
      if ((!is.null(domain)) && .isRecordingOtel) {
        # [mymod] Set reactiveVal: x
        otel_log(
          .otelLabel,
          severity = "info",
          attributes = private$.otelAttrs
        )
      }
      rLog$valueChange(private$reactId, value, domain)
      private$value <- value
      private$dependents$invalidate()
      invisible(TRUE)
    },
    freeze = function(session = getDefaultReactiveDomain()) {
      checkReactiveDomain(session)
      rLog$freezeReactiveVal(private$reactId, session)
      session$onFlushed(function() {
        self$thaw(session)
      })
      private$frozen <- TRUE
    },
    thaw = function(session = getDefaultReactiveDomain()) {
      rLog$thawReactiveVal(private$reactId, session)
      private$frozen <- FALSE
    },
    isFrozen = function() {
      private$frozen
    },
    format = function(...) {
      # capture.output(print()) is necessary because format() doesn't
      # necessarily return a character vector, e.g. data.frame.
      label <- utils::capture.output(print(base::format(private$value, ...)))
      if (length(label) == 1) {
        paste0("reactiveVal: ", label)
      } else {
        c("reactiveVal:", label)
      }
    }
  )
)

#' Create a (single) reactive value
#'
#' The `reactiveVal` function is used to construct a "reactive value"
#' object. This is an object used for reading and writing a value, like a
#' variable, but with special capabilities for reactive programming. When you
#' read the value out of a reactiveVal object, the calling reactive expression
#' takes a dependency, and when you change the value, it notifies any reactives
#' that previously depended on that value.
#'
#' `reactiveVal` is very similar to [reactiveValues()], except
#' that the former is for a single reactive value (like a variable), whereas the
#' latter lets you conveniently use multiple reactive values by name (like a
#' named list of variables). For a one-off reactive value, it's more natural to
#' use `reactiveVal`. See the Examples section for an illustration.
#'
#' @param value An optional initial value.
#' @param label An optional label, for debugging purposes (see
#'   [reactlog()]). If missing, a label will be automatically
#'   created.
#'
#' @return A function. Call the function with no arguments to (reactively) read
#'   the value; call the function with a single argument to set the value.
#'
#' @examples
#'
#' \dontrun{
#'
#' # Create the object by calling reactiveVal
#' r <- reactiveVal()
#'
#' # Set the value by calling with an argument
#' r(10)
#'
#' # Read the value by calling without arguments
#' r()
#'
#' }
#'
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   actionButton("minus", "-1"),
#'   actionButton("plus", "+1"),
#'   br(),
#'   textOutput("value")
#' )
#'
#' # The comments below show the equivalent logic using reactiveValues()
#' server <- function(input, output, session) {
#'   value <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
#'
#'   observeEvent(input$minus, {
#'     newValue <- value() - 1     # newValue <- rv$value - 1
#'     value(newValue)             # rv$value <- newValue
#'   })
#'
#'   observeEvent(input$plus, {
#'     newValue <- value() + 1     # newValue <- rv$value + 1
#'     value(newValue)             # rv$value <- newValue
#'   })
#'
#'   output$value <- renderText({
#'     value()                     # rv$value
#'   })
#' }
#'
#' shinyApp(ui, server)
#'
#' }
#'
#' @export
reactiveVal <- function(value = NULL, label = NULL) {
  call_srcref <- attr(sys.call(), "srcref", exact = TRUE)
  if (missing(label)) {
    label <- rassignSrcrefToLabel(
      call_srcref,
      defaultLabel = paste0("reactiveVal", createUniqueId(4)),
      fnName = "reactiveVal"
    )
  }

  rv <- ReactiveVal$new(value, label)
  if (!is.null(call_srcref)) {
    rv$.otelAttrs <- otel_srcref_attributes(call_srcref)
  }

  ret <- structure(
    function(x) {
      if (missing(x)) {
        rv$get()
      } else {
        force(x)
        rv$set(x)
      }
    },
    class = c("reactiveVal", "reactive", "function"),
    label = label,
    .impl = rv
  )

  if (has_otel_bind("reactivity")) {
    ret <- bind_otel_reactive_val(ret)
  }

  ret
}

#' @rdname freezeReactiveValue
#' @export
freezeReactiveVal <- function(x) {
  if (getOption("shiny.deprecation.messages", TRUE) && getOption("shiny.deprecation.messages.freeze", TRUE)) {
    rlang::warn(
      "freezeReactiveVal() is soft-deprecated, and may be removed in a future version of Shiny. (See https://github.com/rstudio/shiny/issues/3063)",
      .frequency = "once", .frequency_id = "freezeReactiveVal")
  }

  domain <- getDefaultReactiveDomain()
  checkReactiveDomain(domain)

  if (!inherits(x, "reactiveVal")) {
    rlang::abort("`x` must be a reactiveVal.")
  }

  attr(x, ".impl", exact = TRUE)$freeze(domain)
  invisible()
}

checkReactiveDomain <- function(x) {
  if (is.null(x)) {
    rlang::abort("Can't freeze reactive values without a reactive domain.")
  }
}


#' @export
format.reactiveVal <- function(x, ...) {
  attr(x, ".impl", exact = TRUE)$format(...)
}

# Attempts to extract the variable name that the reactiveVal object is being
# assigned to (e.g. for `a <- reactiveVal()`, the result should be "a"). This
# is a fragile, error-prone operation, so we default to a random label if
# necessary.
rassignSrcrefToLabel <- function(
  srcref,
  defaultLabel,
  fnName
) {

  if (is.null(srcref))
    return(defaultLabel)

  srcfile <- attr(srcref, "srcfile", exact = TRUE)
  if (is.null(srcfile))
    return(defaultLabel)

  if (is.null(srcfile$lines))
    return(defaultLabel)

  lines <- srcfile$lines
  # When pasting at the Console, srcfile$lines is not split
  if (length(lines) == 1) {
    lines <- strsplit(lines, "\n")[[1]]
  }

  if (length(lines) < srcref[1]) {
    return(defaultLabel)
  }

  firstLine <- substring(lines[srcref[1]], srcref[2] - 1)

  m <- regexec(
    paste0("\\s*([^[:space:]]+)\\s*(<-|=)\\s*", fnName, "\\b"),
    firstLine
  )
  if (m[[1]][1] == -1) {
    return(defaultLabel)
  }

  sym <- regmatches(firstLine, m)[[1]][2]
  res <- try(parse(text = sym), silent = TRUE)
  if (inherits(res, "try-error"))
    return(defaultLabel)

  if (length(res) != 1)
    return(defaultLabel)

  return(as.character(res))
}


# ReactiveValues ------------------------------------------------------------

ReactiveValues <- R6Class(
  'ReactiveValues',
  portable = FALSE,
  public = list(
    # For debug purposes
    .reactId = character(0),
    .label = character(0),
    .values = 'Map',
    .metadata = 'Map',
    # A map of Dependents objects, one for each key
    .dependents = 'Map',
    # Dependents for the list of all names, including hidden
    .namesDeps = 'Dependents',
    # Dependents for all values, including hidden
    .allValuesDeps = 'Dependents',
    # Dependents for all values
    .valuesDeps = 'Dependents',
    .dedupe = logical(0),
    # Key, asList(), or names() have been retrieved
    .hasRetrieved = list(),
    # All names, in insertion order. The names are also stored in the .values
    # object, but it does not preserve order.
    .nameOrder = character(0),

    .isRecordingOtel = FALSE, # Needs to be set by Shiny
    .otelAttrs = NULL, # Needs to be set by Shiny


    initialize = function(
      dedupe = TRUE,
      label = paste0('reactiveValues', p_randomInt(1000, 10000))
    ) {
      .reactId <<- nextGlobalReactId()
      .label <<- label
      .values <<- Map$new()
      .metadata <<- Map$new()
      .dependents <<- Map$new()
      .hasRetrieved <<- list(names = FALSE, asListAll = FALSE, asList = FALSE)
      .namesDeps <<- Dependents$new(reactId = rLog$namesIdStr(.reactId))
      .allValuesDeps <<- Dependents$new(reactId = rLog$asListAllIdStr(.reactId))
      .valuesDeps <<- Dependents$new(reactId = rLog$asListIdStr(.reactId))
      .dedupe <<- dedupe
    },

    get = function(key) {
      # get value right away to use for logging
      keyValue <- .values$get(key)

      if (!.dependents$containsKey(key)) {
        # If we got here, this is the first time someone has tried to access
        # this key.
        rLog$defineKey(.reactId, keyValue, key, .label, getCurrentContext()$.domain)

        reactKeyId <- rLog$keyIdStr(.reactId, key)
        .dependents$set(key, Dependents$new(reactKeyId))
      }

      # Register the "downstream" reactive which is accessing this value, so
      # that we know to invalidate them when this value changes.
      .dependents$get(key)$register()

      if (isFrozen(key))
        reactiveStop()

      keyValue
    },

    set = function(key, value, force = FALSE) {
      # if key exists
      #   if it is the same value, return
      #
      # update value of `key`
      #
      # if key exists
      #   if `key` has been read,
      #     log `update key`
      #     ## (invalidate key later in code)
      # else # if new key
      #   if `names()` have been read,
      #     log `update names()`
      #     invalidate `names()`
      #
      # if hidden
      #   if asListAll has been read,
      #     log `update asList(all.names = TRUE)`
      #     invalidate `asListAll`
      # else # not hidden
      #   if asList has been read,
      #     log `update asList()`
      #     invalidate `asList`
      #
      # update value of `key`
      # invalidate all deps of `key`

      domain <- getDefaultReactiveDomain()
      hidden <- substr(key, 1, 1) == "."

      key_exists <- .values$containsKey(key)

      if (key_exists && !isTRUE(force) && .dedupe && identical(.values$get(key), value)) {
        return(invisible())
      }

      if ((!is.null(domain)) && .isRecordingOtel) {
        # Do not include updates to input or clientData unless _some_ reactivity has occured
        if (has_reactive_ospan_cleanup(domain) || !(.label == "input" || .label == "clientData")) {
          # [mymod] Set reactiveValues: x$key
          otel_log(
            otel_label_set_reactive_values(.label, key, domain = domain),
            severity = "info",
            attributes = .otelAttrs
          )
        }
      }

      # If it's new, append key to the name order
      if (!key_exists) {
        .nameOrder[length(.nameOrder) + 1] <<- key
      }

      # set the value for better logging
      .values$set(key, value)

      # key has been depended upon
      if (.dependents$containsKey(key)) {
        rLog$valueChangeKey(.reactId, key, value, domain)
        .dependents$get(key)$invalidate()
      }

      # only invalidate if there are deps
      if (!key_exists && isTRUE(.hasRetrieved$names)) {
        rLog$valueChangeNames(.reactId, .values$keys(), domain)
        .namesDeps$invalidate()
      }

      if (hidden) {
        if (isTRUE(.hasRetrieved$asListAll)) {
          rLog$valueChangeAsListAll(.reactId, .values$values(), domain)
          .allValuesDeps$invalidate()
        }
      } else {
        if (isTRUE(.hasRetrieved$asList)) {
          react_vals <- .values$values()
          react_vals <- react_vals[!grepl("^\\.", base::names(react_vals))]
          # leave as is. both object would be registered to the listening object
          rLog$valueChangeAsList(.reactId, react_vals, domain)
          .valuesDeps$invalidate()
        }
      }

      invisible()
    },

    mset = function(lst) {
      lapply(base::names(lst),
             function(name) {
               self$set(name, lst[[name]])
             })
    },

    names = function() {
      if (!isTRUE(.hasRetrieved$names)) {
        domain <- getDefaultReactiveDomain()
        rLog$defineNames(.reactId, .nameOrder, .label, domain)
        .hasRetrieved$names <<- TRUE
      }
      .namesDeps$register()
      return(.nameOrder)
    },

    # Get a metadata value. Does not trigger reactivity.
    getMeta = function(key, metaKey) {
      # Make sure to use named (not numeric) indexing into list.
      metaKey <- as.character(metaKey)
      .metadata$get(key)[[metaKey]]
    },

    # Set a metadata value. Does not trigger reactivity.
    setMeta = function(key, metaKey, value) {
      # Make sure to use named (not numeric) indexing into list.
      metaKey <- as.character(metaKey)

      if (!.metadata$containsKey(key)) {
        .metadata$set(key, list())
      }

      m <- .metadata$get(key)
      m[[metaKey]] <- value
      .metadata$set(key, m)
    },

    # Mark a value as frozen If accessed while frozen, a shiny.silent.error will
    # be thrown.
    freeze = function(key, invalidate = FALSE) {
      domain <- getDefaultReactiveDomain()
      rLog$freezeReactiveKey(.reactId, key, domain)
      setMeta(key, "frozen", TRUE)

      if (invalidate) {
        # Force an invalidation
        self$set(key, NULL, force = TRUE)
      }
    },

    thaw = function(key) {
      domain <- getDefaultReactiveDomain()
      rLog$thawReactiveKey(.reactId, key, domain)
      setMeta(key, "frozen", NULL)
    },

    isFrozen = function(key) {
      isTRUE(getMeta(key, "frozen"))
    },

    toList = function(all.names=FALSE) {
      listValue <- .values$mget(.nameOrder)
      if (!all.names) {
        listValue <- listValue[!grepl("^\\.", base::names(listValue))]
      }
      if (all.names) {
        if (!isTRUE(.hasRetrieved$asListAll)) {
          domain <- getDefaultReactiveDomain()
          rLog$defineAsListAll(.reactId, listValue, .label, domain)
          .hasRetrieved$asListAll <<- TRUE
        }
        .allValuesDeps$register()
      }

      if (!isTRUE(.hasRetrieved$asList)) {
        domain <- getDefaultReactiveDomain()
        # making sure the value being recorded is with `all.names = FALSE`
        rLog$defineAsList(.reactId, listValue[!grepl("^\\.", base::names(listValue))], .label, domain)
        .hasRetrieved$asList <<- TRUE
      }
      .valuesDeps$register()

      return(listValue)
    }

  )
)


# reactivevalues ------------------------------------------------------------
# S3 wrapper class for ReactiveValues reference class

#' Create an object for storing reactive values
#'
#' This function returns an object for storing reactive values. It is similar to
#' a list, but with special capabilities for reactive programming. When you read
#' a value from it, the calling reactive expression takes a reactive dependency
#' on that value, and when you write to it, it notifies any reactive functions
#' that depend on that value. Note that values taken from the reactiveValues
#' object are reactive, but the reactiveValues object itself is not.
#'
#' @examples
#' # Create the object with no values
#' values <- reactiveValues()
#'
#' # Assign values to 'a' and 'b'
#' values$a <- 3
#' values[['b']] <- 4
#'
#' \dontrun{
#' # From within a reactive context, you can access values with:
#' values$a
#' values[['a']]
#' }
#'
#' # If not in a reactive context (e.g., at the console), you can use isolate()
#' # to retrieve the value:
#' isolate(values$a)
#' isolate(values[['a']])
#'
#' # Set values upon creation
#' values <- reactiveValues(a = 1, b = 2)
#' isolate(values$a)
#'
#' @param ... Objects that will be added to the reactivevalues object. All of
#'   these objects must be named.
#'
#' @seealso [isolate()] and [is.reactivevalues()].
#' @export
reactiveValues <- function(...) {
  args <- list2(...)
  if ((length(args) > 0) && (is.null(names(args)) || any(names(args) == "")))
    rlang::abort("All arguments passed to reactiveValues() must be named.")

  values <- .createReactiveValues(ReactiveValues$new())

  # Use .subset2() instead of [[, to avoid method dispatch
  impl <- .subset2(values, 'impl')
  impl$mset(args)

  call_srcref <- attr(sys.call(), "srcref", exact = TRUE)
  if (!is.null(call_srcref)) {
    impl$.label <- rassignSrcrefToLabel(
      call_srcref,
      # Pass through the random default label created in ReactiveValues$new()
      defaultLabel = impl$.label,
      fnName = "reactiveValues"
    )

    impl$.otelAttrs <- otel_srcref_attributes(call_srcref)
  }

  if (has_otel_bind("reactivity")) {
    values <- bind_otel_reactive_values(values)
  }

  values
}

checkName <- function(x) {
  if (!is.character(x) || length(x) != 1) {
    rlang::abort("Must use single string to index into reactivevalues.")
  }
}

# Create a reactivevalues object
#
# @param values A ReactiveValues object
# @param readonly Should this object be read-only?
# @param ns A namespace function (either `identity` or `NS(namespace)`)
.createReactiveValues <- function(values = NULL, readonly = FALSE,
  ns = identity) {

  structure(
    list(
      impl = values,
      readonly = readonly,
      ns = ns
    ),
    class='reactivevalues'
  )
}

#' @export
print.reactivevalues <- function(x, ...) {
  impl <- .subset2(x, "impl")
  cat_line("<ReactiveValues>")
  cat_line("  Values:   ", paste0(impl$.values$keys(sort = TRUE), collapse = ", "))
  cat_line("  Readonly: ", .subset2(x, "readonly"))
}

#' Checks whether an object is a reactivevalues object
#'
#' Checks whether its argument is a reactivevalues object.
#'
#' @param x The object to test.
#' @seealso [reactiveValues()].
#' @export
is.reactivevalues <- function(x) inherits(x, 'reactivevalues')

#' @export
`$.reactivevalues` <- function(x, name) {
  checkName(name)

  if (!hasCurrentContext()) {
    rlang::abort(c(
      paste0("Can't access reactive value '", name, "' outside of reactive consumer."),
      i = "Do you need to wrap inside reactive() or observe()?"
    ))
  }

  .subset2(x, 'impl')$get(.subset2(x, 'ns')(name))
}

#' @export
`[[.reactivevalues` <- `$.reactivevalues`

#' @export
`$<-.reactivevalues` <- function(x, name, value) {
  if (.subset2(x, 'readonly')) {
    rlang::abort(paste0("Can't modify read-only reactive value '", name, "'"))
  }
  checkName(name)
  .subset2(x, 'impl')$set(.subset2(x, 'ns')(name), value)
  x
}

#' @export
`[[<-.reactivevalues` <- `$<-.reactivevalues`

#' @export
`[.reactivevalues` <- function(values, name) {
  rlang::abort("Can't index reactivevalues with `[`.")
}

#' @export
`[<-.reactivevalues` <- function(values, name, value) {
  rlang::abort("Can't index reactivevalues with `[`.")
}

#' @export
names.reactivevalues <- function(x) {
  prefix <- .subset2(x, 'ns')("")
  results <- .subset2(x, 'impl')$names()
  if (nzchar(prefix)) {
    results <- results[substring(results, 1, nchar(prefix)) == prefix]
    results <- substring(results, nchar(prefix) + 1)
  }
  results
}

#' @export
`names<-.reactivevalues` <- function(x, value) {
  rlang::abort("Can't assign names to reactivevalues.")
}

#' @export
as.list.reactivevalues <- function(x, all.names=FALSE, ...) {
  shinyDeprecated(
    "0.4.0", "as.list.reactivevalues()", "reactiveValuesToList()",
    details = "Please see ?reactiveValuesToList for more information."
  )

  reactiveValuesToList(x, all.names)
}

#' Convert a reactivevalues object to a list
#'
#' This function does something similar to what you might want or expect
#' [base::as.list()] to do. The difference is that the calling context will take
#' dependencies on every object in the `reactivevalue`s object. To avoid taking
#' dependencies on all the objects, you can wrap the call with [isolate()].
#'
#' @param x A `reactivevalues` object.
#' @param all.names If `TRUE`, include objects with a leading dot. If `FALSE`
#'   (the default) don't include those objects.
#' @examples
#' values <- reactiveValues(a = 1)
#' \dontrun{
#' reactiveValuesToList(values)
#' }
#'
#' # To get the objects without taking dependencies on them, use isolate().
#' # isolate() can also be used when calling from outside a reactive context (e.g.
#' # at the console)
#' isolate(reactiveValuesToList(values))
#' @export
reactiveValuesToList <- function(x, all.names=FALSE) {
  # Default case
  res <- .subset2(x, 'impl')$toList(all.names)

  prefix <- .subset2(x, 'ns')("")
  # Special handling for namespaces
  if (nzchar(prefix)) {
    fullNames <- names(res)

    # Filter out items that match namespace
    fullNames <- fullNames[substring(fullNames, 1, nchar(prefix)) == prefix]
    res <- res[fullNames]

    # Remove namespace prefix
    names(res) <- substring(fullNames, nchar(prefix) + 1)
  }

  res
}

# This function is needed because str() on a reactivevalues object will call
# [[.reactivevalues(), which will give an error when it tries to access
# x[['impl']].
#' @export
str.reactivevalues <- function(object, indent.str = " ", ...) {
  utils::str(unclass(object), indent.str = indent.str, ...)
  # Need to manually print out the class field,
  cat(indent.str, '- attr(*, "class")=', sep = "")
  utils::str(class(object))
}


#' Freeze a reactive value
#'
#' These functions freeze a [reactiveVal()], or an element of a
#' [reactiveValues()]. If the value is accessed while frozen, a
#' "silent" exception is raised and the operation is stopped. This is the same
#' thing that happens if `req(FALSE)` is called. The value is thawed
#' (un-frozen; accessing it will no longer raise an exception) when the current
#' reactive domain is flushed. In a Shiny application, this occurs after all of
#' the observers are executed. **NOTE:** We are considering deprecating
#' `freezeReactiveVal`, and `freezeReactiveValue` except when `x` is `input`.
#' If this affects your app, please let us know by leaving a comment on
#' [this GitHub issue](https://github.com/rstudio/shiny/issues/3063).
#'
#' @param x For `freezeReactiveValue`, a [reactiveValues()]
#'   object (like `input`); for `freezeReactiveVal`, a
#'   [reactiveVal()] object.
#' @param name The name of a value in the [reactiveValues()] object.
#'
#' @seealso [req()]
#' @examples
#' ## Only run this examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   selectInput("data", "Data Set", c("mtcars", "pressure")),
#'   checkboxGroupInput("cols", "Columns (select 2)", character(0)),
#'   plotOutput("plot")
#' )
#'
#' server <- function(input, output, session) {
#'   observe({
#'     data <- get(input$data)
#'     # Sets a flag on input$cols to essentially do req(FALSE) if input$cols
#'     # is accessed. Without this, an error will momentarily show whenever a
#'     # new data set is selected.
#'     freezeReactiveValue(input, "cols")
#'     updateCheckboxGroupInput(session, "cols", choices = names(data))
#'   })
#'
#'   output$plot <- renderPlot({
#'     # When a new data set is selected, input$cols will have been invalidated
#'     # above, and this will essentially do the same as req(FALSE), causing
#'     # this observer to stop and raise a silent exception.
#'     cols <- input$cols
#'     data <- get(input$data)
#'
#'     if (length(cols) == 2) {
#'       plot(data[[ cols[1] ]], data[[ cols[2] ]])
#'     }
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @export
freezeReactiveValue <- function(x, name) {
  domain <- getDefaultReactiveDomain()
  checkReactiveDomain(domain)

  domain$freezeValue(x, name)
  invisible()
}


# Observable ----------------------------------------------------------------

Observable <- R6Class(
  'Observable',
  portable = FALSE,
  public = list(
    .reactId = character(0),
    .origFunc = 'function',
    .func = 'function',
    .label = character(0),
    .domain = NULL,
    .dependents = 'Dependents',
    .invalidated = logical(0),
    .running = logical(0),
    .value = NULL,
    .error = FALSE,
    .visible = logical(0),
    .execCount = integer(0),
    .mostRecentCtxId = character(0),
    .ctx = 'Context',

    .isRecordingOtel = FALSE, # Needs to be set by Shiny
    .otelLabel = NULL, # Needs to be set by Shiny
    .otelAttrs = NULL, # Needs to be set by Shiny

    initialize = function(func, label = deparse(substitute(func)),
                          domain = getDefaultReactiveDomain(),
                          ..stacktraceon = TRUE) {
      if (length(formals(func)) > 0)
        rlang::abort(c(
          "Can't make a reactive expression from a function that takes arguments.",
          "Only functions without parameters can become reactive expressions."
        ))

      # This is to make sure that the function labels that show in the profiler
      # and in stack traces doesn't contain whitespace. See
      # https://github.com/rstudio/profvis/issues/58
      if (grepl("\\s", label, perl = TRUE)) {
        funcLabel <- "<reactive>"
      } else {
        funcLabel <- paste0("<reactive:", label, ">")
      }

      .reactId <<- nextGlobalReactId()
      .origFunc <<- func
      .func <<- wrapFunctionLabel(func, funcLabel,
        ..stacktraceon = ..stacktraceon)
      .label <<- label
      .domain <<- domain
      .dependents <<- Dependents$new(reactId = .reactId)
      .invalidated <<- TRUE
      .running <<- FALSE
      .execCount <<- 0L
      .mostRecentCtxId <<- ""
      .ctx <<- NULL
      rLog$define(.reactId, .value, .label, type = "observable", .domain)
    },
    getValue = function() {
      .dependents$register()

      if (.invalidated || .running) {
        ..stacktraceoff..(
          self$.updateValue()
        )
      }

      if (.error) {
        stop(.value)
      }

      if (.visible)
        .value
      else
        invisible(.value)
    },
    format = function() {
      simpleExprToFunction(fn_body(.origFunc), "reactive")
    },
    .updateValue = function() {
      ctx <- Context$new(.domain, .label, type = 'observable',
                         prevId = .mostRecentCtxId, reactId = .reactId,
                         weak = TRUE)
      .mostRecentCtxId <<- ctx$id

      ctx$setOspanInfo(
        isRecordingOtel = .isRecordingOtel,
        otelLabel = .otelLabel,
        otelAttrs = .otelAttrs
      )

      # A Dependency object will have a weak reference to the context, which
      # doesn't prevent it from being GC'd. However, as long as this
      # Observable object is reachable and not invalidated, we need to make
      # sure the context isn't GC'd. To do that we need a strong reference to
      # the context.
      .ctx <<- ctx

      ctx$onInvalidate(function() {
        .invalidated <<- TRUE
        .value <<- NULL # Value can be GC'd, it won't be read once invalidated
        .dependents$invalidate(log = FALSE)
        .ctx <<- NULL   # No longer need to prevent the context from being GC'd.
      })
      .execCount <<- .execCount + 1L

      .invalidated <<- FALSE

      wasRunning <- .running
      .running <<- TRUE
      on.exit(.running <<- wasRunning)

      ctx$run(function() {
        result <- withCallingHandlers(

          {
            .error <<- FALSE
            withVisible(.func())
          },

          error = function(cond) {
            # If an error occurs, we want to propagate the error, but we also
            # want to save a copy of it, so future callers of this reactive will
            # get the same error (i.e. the error is cached).
            .value <<- cond
            .error <<- TRUE
            .visible <<- FALSE
          }
        )
        .value <<- result$value
        .visible <<- result$visible
      })
    }
  )
)

#' Create a reactive expression
#'
#' Wraps a normal expression to create a reactive expression. Conceptually, a
#' reactive expression is a expression whose result will change over time.
#'
#' Reactive expressions are expressions that can read reactive values and call
#' other reactive expressions. Whenever a reactive value changes, any reactive
#' expressions that depended on it are marked as "invalidated" and will
#' automatically re-execute if necessary. If a reactive expression is marked as
#' invalidated, any other reactive expressions that recently called it are also
#' marked as invalidated. In this way, invalidations ripple through the
#' expressions that depend on each other.
#'
#' See the [Shiny tutorial](https://shiny.rstudio.com/tutorial/) for
#' more information about reactive expressions.
#'
#' @param x For `is.reactive()`, an object to test. For `reactive()`, an
#'   expression. When passing in a [`rlang::quo()`]sure with `reactive()`,
#'   remember to use [`rlang::inject()`] to distinguish that you are passing in
#'   the content of your quosure, not the expression of the quosure.
#' @template param-env
#' @templateVar x x
#' @templateVar env env
#' @templateVar quoted quoted
#' @template param-quoted
#' @templateVar x x
#' @templateVar quoted quoted

#' @param label A label for the reactive expression, useful for debugging.
#' @param domain See [domains].
#' @param ..stacktraceon Advanced use only. For stack manipulation purposes; see
#'   [stacktrace()].
#' @param ... Not used.
#' @return a function, wrapped in a S3 class "reactive"
#'
#' @examples
#' library(rlang)
#' values <- reactiveValues(A=1)
#'
#' reactiveB <- reactive({
#'   values$A + 1
#' })
#' # View the values from the R console with isolate()
#' isolate(reactiveB())
#' # 2
#'
#' # To store expressions for later conversion to reactive, use quote()
#' myquo <- rlang::quo(values$A + 2)
#' # Unexpected value! Sending a quosure directly will not work as expected.
#' reactiveC <- reactive(myquo)
#' # We'd hope for `3`, but instead we get the quosure that was supplied.
#' isolate(reactiveC())
#'
#' # Instead, the quosure should be `rlang::inject()`ed
#' reactiveD <- rlang::inject(reactive(!!myquo))
#' isolate(reactiveD())
#' # 3
#'
#' # (Legacy) Can use quoted expressions
#' expr <- quote({ values$A + 3 })
#' reactiveE <- reactive(expr, quoted = TRUE)
#' isolate(reactiveE())
#' # 4
#'
#' @export
reactive <- function(
  x,
  env = parent.frame(),
  quoted = FALSE,
  ...,
  label = NULL,
  domain = getDefaultReactiveDomain(),
  ..stacktraceon = TRUE
) {
  check_dots_empty()

  func <- installExprFunction(x, "func", env, quoted, wrappedWithLabel = FALSE)
  # Attach a label and a reference to the original user source for debugging
  userExpr <- fn_body(func)
  label <- exprToLabel(userExpr, "reactive", label)

  o <- Observable$new(func, label, domain, ..stacktraceon = ..stacktraceon)

  call_srcref <- attr(sys.call(), "srcref", exact = TRUE)
  if (!is.null(call_srcref)) {
    o$.otelAttrs <- otel_srcref_attributes(call_srcref)
  }

  ret <- structure(
    o$getValue,
    observable = o,
    cacheHint = list(userExpr = zap_srcref(userExpr)),
    class = c("reactiveExpr", "reactive", "function")
  )

  if (has_otel_bind("reactivity")) {
    ret <- bind_otel_reactive_expr(ret)
  }

  ret
}

# Given the srcref to a reactive expression, attempts to figure out what the
# name of the reactive expression is. This isn't foolproof, as it literally
# scans the line of code that started the reactive block and looks for something
# that looks like assignment. If we fail, fall back to a default value (likely
# the block of code in the body of the reactive).
rexprSrcrefToLabel <- function(srcref, defaultLabel, fnName) {
  if (is.null(srcref))
    return(defaultLabel)

  srcfile <- attr(srcref, "srcfile", exact = TRUE)
  if (is.null(srcfile))
    return(defaultLabel)

  if (is.null(srcfile$lines))
    return(defaultLabel)

  lines <- srcfile$lines
  # When pasting at the Console, srcfile$lines is not split
  if (length(lines) == 1) {
    lines <- strsplit(lines, "\n")[[1]]
  }

  if (length(lines) < srcref[1]) {
    return(defaultLabel)
  }

  firstLine <- substring(lines[srcref[1]], 1, srcref[2] - 1)

  m <- regexec(paste0("(.*)(<-|=)\\s*", fnName, "\\s*\\($"), firstLine)
  if (m[[1]][1] == -1) {
    return(defaultLabel)
  }
  sym <- regmatches(firstLine, m)[[1]][2]
  res <- try(parse(text = sym), silent = TRUE)
  if (inherits(res, "try-error"))
    return(defaultLabel)

  if (length(res) != 1)
    return(defaultLabel)

  return(as.character(res))
}

#' @export
format.reactiveExpr <- function(x, ...) {
  attr(x, "observable", exact = TRUE)$format()
}

#' @export
print.reactive <- function(x, ...) {
  cat(paste(format(x), collapse = "\n"), "\n")
}

#' @export
#' @rdname reactive
is.reactive <- function(x) {
  inherits(x, "reactive")
}

# Return the number of times that a reactive expression or observer has been run
execCount <- function(x) {
  if (inherits(x, "reactiveExpr"))
    return(attr(x, "observable", exact = TRUE)$.execCount)
  else if (inherits(x, 'Observer'))
    return(x$.execCount)
  else
    rlang::abort("Unexpected argument to execCount().")
}

# Internal utility functions for extracting things out of reactives.
reactive_get_value_func <- function(x) {
  attr(x, "observable", exact = TRUE)$.origFunc
}
reactive_get_domain <- function(x) {
  attr(x, "observable", exact = TRUE)$.domain
}

# Observer ------------------------------------------------------------------

Observer <- R6Class(
  'Observer',
  portable = FALSE,
  public = list(
    .reactId = character(0),
    .func = 'function',
    .label = character(0),
    .domain = 'ANY',
    .priority = numeric(0),
    .autoDestroy = logical(0),
    # A function that, when invoked, unsubscribes the autoDestroy
    # listener (or NULL if autodestroy is disabled for this observer).
    # We must unsubscribe when this observer is destroyed, or else
    # the observer cannot be garbage collected until the session ends.
    .autoDestroyHandle = 'ANY',
    .invalidateCallbacks = list(),
    .execCount = integer(0),
    .onResume = 'function',
    .suspended = logical(0),
    .destroyed = logical(0),
    .prevId = character(0),
    .ctx = NULL,

    .isRecordingOtel = FALSE, # Needs to be set by Shiny
    .otelLabel = NULL, # Needs to be set by Shiny
    .otelAttrs = NULL, # Needs to be set by Shiny

    initialize = function(observerFunc, label, suspended = FALSE, priority = 0,
                          domain = getDefaultReactiveDomain(),
                          autoDestroy = TRUE, ..stacktraceon = TRUE) {
      if (length(formals(observerFunc)) > 0)
        rlang::abort(c(
          "Can't make an observer from a function that takes arguments.",
          "Only functions without arguments can become observers."
        ))
      if (grepl("\\s", label, perl = TRUE)) {
        funcLabel <- "<observer>"
      } else {
        funcLabel <- paste0("<observer:", label, ">")
      }
      .func <<- wrapFunctionLabel(observerFunc, funcLabel, ..stacktraceon = ..stacktraceon)
      .label <<- label
      .domain <<- domain
      .priority <<- normalizePriority(priority)
      .execCount <<- 0L
      .suspended <<- suspended
      .onResume <<- function() NULL
      .destroyed <<- FALSE
      .prevId <<- ''

      .autoDestroy <<- FALSE
      .autoDestroyHandle <<- NULL
      setAutoDestroy(autoDestroy)

      .reactId <<- nextGlobalReactId()
      rLog$defineObserver(.reactId, .label, .domain)

      # Defer the first running of this until flushReact is called
      .createContext()$invalidate()
    },
    .createContext = function() {
      ctx <- Context$new(.domain, .label, type='observer', prevId=.prevId, reactId = .reactId)
      .prevId <<- ctx$id

      if (!is.null(.ctx)) {
        # If this happens, something went wrong.
        warning("Created a new context without invalidating previous context.")
      }
      # Store the context explicitly in the Observer object. This is necessary
      # to make sure that when the observer is destroyed, it also gets
      # invalidated. Otherwise the upstream reactive (on which the observer
      # depends) will hold a (indirect) reference to this context until the
      # reactive is invalidated, which may not happen immediately or at all.
      # This can lead to a memory leak (#1253).
      .ctx <<- ctx

      ctx$setOspanInfo(
        isRecordingOtel = .isRecordingOtel,
        otelLabel = .otelLabel,
        otelAttrs = .otelAttrs
      )

      ctx$onInvalidate(function() {
        # Context is invalidated, so we don't need to store a reference to it
        # anymore.
        .ctx <<- NULL

        lapply(.invalidateCallbacks, function(invalidateCallback) {
          invalidateCallback()
          NULL
        })

        continue <- function() {
          ctx$addPendingFlush(.priority)
          if (!is.null(.domain)) {
            .domain$incrementBusyCount()
          }
        }

        if (.suspended == FALSE)
          continue()
        else
          .onResume <<- continue
      })

      ctx$onFlush(function() {

        hybrid_chain(
          {
            if (!.destroyed) {
              shinyCallingHandlers(run())
            }
          },
          catch = function(e) {
            # It's OK for shiny.silent.error errors to cause an observer to stop running
            # shiny.silent.error = function(e) NULL
            # validation = function(e) NULL,
            # shiny.output.cancel = function(e) NULL

            if (cnd_inherits(e, "shiny.silent.error")) {
              return()
            }

            printError(e)
            if (!is.null(.domain)) {
              .domain$unhandledError(e, close = TRUE)
            }
          },
          finally = .domain$decrementBusyCount
        )
      })

      return(ctx)
    },
    run = function() {
      ctx <- .createContext()
      .execCount <<- .execCount + 1L
      ctx$run(.func)
    },
    onInvalidate = function(callback) {
      "Register a callback function to run when this observer is invalidated.
      No arguments will be provided to the callback function when it is
      invoked."
      .invalidateCallbacks <<- c(.invalidateCallbacks, callback)
    },
    setPriority = function(priority = 0) {
      "Change this observer's priority. Note that if the observer is currently
      invalidated, then the change in priority will not take effect until the
      next invalidation--unless the observer is also currently suspended, in
      which case the priority change will be effective upon resume."
      .priority <<- normalizePriority(priority)
    },
    setAutoDestroy = function(autoDestroy) {
      "Sets whether this observer should be automatically destroyed when its
      domain (if any) ends. If autoDestroy is TRUE and the domain already
      ended, then destroy() is called immediately."

      if (.autoDestroy == autoDestroy) {
        return(.autoDestroy)
      }

      oldValue <- .autoDestroy
      .autoDestroy <<- autoDestroy

      if (autoDestroy) {
        if (!.destroyed && !is.null(.domain)) { # Make sure to not try to destroy twice.
          if (.domain$isEnded()) {
            destroy()
          } else {
            .autoDestroyHandle <<- onReactiveDomainEnded(.domain, .onDomainEnded)
          }
        }
      } else {
        if (!is.null(.autoDestroyHandle))
          .autoDestroyHandle()
        .autoDestroyHandle <<- NULL
      }

      invisible(oldValue)
    },
    suspend = function() {
      "Causes this observer to stop scheduling flushes (re-executions) in
      response to invalidations. If the observer was invalidated prior to this
      call but it has not re-executed yet (because it waits until onFlush is
      called) then that re-execution will still occur, because the flush is
      already scheduled."
      .suspended <<- TRUE
    },
    resume = function() {
      "Causes this observer to start re-executing in response to invalidations.
      If the observer was invalidated while suspended, then it will schedule
      itself for re-execution (pending flush)."
      if (.suspended) {
        .suspended <<- FALSE
        .onResume()
        .onResume <<- function() NULL
      }
      invisible()
    },
    destroy = function() {
      "Prevents this observer from ever executing again (even if a flush has
      already been scheduled)."

      # Make sure to not try to destory twice.
      if (.destroyed)
        return()

      suspend()
      .destroyed <<- TRUE

      if (!is.null(.autoDestroyHandle)) {
        .autoDestroyHandle()
      }
      .autoDestroyHandle <<- NULL

      if (!is.null(.ctx)) {
        .ctx$invalidate()
      }
    },
    .onDomainEnded = function() {
      if (isTRUE(.autoDestroy)) {
        destroy()
      }
    }
  )
)

#' Create a reactive observer
#'
#' Creates an observer from the given expression.
#'
#' An observer is like a reactive expression in that it can read reactive values
#' and call reactive expressions, and will automatically re-execute when those
#' dependencies change. But unlike reactive expressions, it doesn't yield a
#' result and can't be used as an input to other reactive expressions. Thus,
#' observers are only useful for their side effects (for example, performing
#' I/O).
#'
#' Another contrast between reactive expressions and observers is their
#' execution strategy. Reactive expressions use lazy evaluation; that is, when
#' their dependencies change, they don't re-execute right away but rather wait
#' until they are called by someone else. Indeed, if they are not called then
#' they will never re-execute. In contrast, observers use eager evaluation; as
#' soon as their dependencies change, they schedule themselves to re-execute.
#'
#' Starting with Shiny 0.10.0, observers are automatically destroyed by default
#' when the [domain][domains] that owns them ends (e.g. when a Shiny
#' session ends).
#'
#' @param x An expression (quoted or unquoted). Any return value will be
#'   ignored.
#' @inheritParams reactive
#' @param label A label for the observer, useful for debugging.
#' @param suspended If `TRUE`, start the observer in a suspended state. If
#'   `FALSE` (the default), start in a non-suspended state.
#' @param priority An integer or numeric that controls the priority with which
#'   this observer should be executed. A higher value means higher priority: an
#'   observer with a higher priority value will execute before all observers
#'   with lower priority values. Positive, negative, and zero values are
#'   allowed.
#' @param domain See [domains].
#' @param autoDestroy If `TRUE` (the default), the observer will be
#'   automatically destroyed when its domain (if any) ends.
#' @param ..stacktraceon Advanced use only. For stack manipulation purposes; see
#'   [stacktrace()].
#' @param ... Not used.
#'
#' @return An observer reference class object. This object has the following
#'   methods:
#'   \describe{
#'     \item{`suspend()`}{
#'       Causes this observer to stop scheduling flushes (re-executions) in
#'       response to invalidations. If the observer was invalidated prior to
#'       this call but it has not re-executed yet then that re-execution will
#'       still occur, because the flush is already scheduled.
#'     }
#'     \item{`resume()`}{
#'       Causes this observer to start re-executing in response to
#'       invalidations. If the observer was invalidated while suspended, then it
#'       will schedule itself for re-execution.
#'     }
#'     \item{`destroy()`}{
#'       Stops the observer from executing ever again, even if it is currently
#'       scheduled for re-execution.
#'     }
#'     \item{`setPriority(priority = 0)`}{
#'       Change this observer's priority. Note that if the observer is currently
#'       invalidated, then the change in priority will not take effect until the
#'       next invalidation--unless the observer is also currently suspended, in
#'       which case the priority change will be effective upon resume.
#'     }
#'     \item{`setAutoDestroy(autoDestroy)`}{
#'       Sets whether this observer should be automatically destroyed when its
#'       domain (if any) ends. If autoDestroy is TRUE and the domain already
#'       ended, then destroy() is called immediately."
#'     }
#'     \item{`onInvalidate(callback)`}{
#'       Register a callback function to run when this observer is invalidated.
#'       No arguments will be provided to the callback function when it is
#'       invoked.
#'     }
#'   }
#'
#' @examples
#' values <- reactiveValues(A=1)
#'
#' obsB <- observe({
#'   print(values$A + 1)
#' })
#'
#' # To store expressions for later conversion to observe, use rlang::quo()
#' myquo <- rlang::quo({ print(values$A + 3) })
#' obsC <- rlang::inject(observe(!!myquo))
#'
#' # (Legacy) Can use quoted expressions
#' obsD <- observe(quote({ print(values$A + 2) }), quoted = TRUE)
#'
#' # In a normal Shiny app, the web client will trigger flush events. If you
#' # are at the console, you can force a flush with flushReact()
#' shiny:::flushReact()
#' @export
observe <- function(
  x,
  env = parent.frame(),
  quoted = FALSE,
  ...,
  label = NULL,
  suspended = FALSE,
  priority = 0,
  domain = getDefaultReactiveDomain(),
  autoDestroy = TRUE,
  ..stacktraceon = TRUE)
{
  check_dots_empty()

  func <- installExprFunction(x, "func", env, quoted)
  label <- funcToLabel(func, "observe", label)

  o <- Observer$new(
    func,
    label = label,
    suspended = suspended,
    priority = priority,
    domain = domain,
    autoDestroy = autoDestroy,
    ..stacktraceon = ..stacktraceon
  )
  call_srcref <- attr(sys.call(), "srcref", exact = TRUE)
  if (!is.null(call_srcref)) {
    o$.otelAttrs <- otel_srcref_attributes(call_srcref)
  }

  if (has_otel_bind("reactivity")) {
    o <- bind_otel_observe(o)
  }

  invisible(o)
}

#' Make a reactive variable
#'
#' Turns a normal variable into a reactive variable, that is, one that has
#' reactive semantics when assigned or read in the usual ways. The variable may
#' already exist; if so, its value will be used as the initial value of the
#' reactive variable (or `NULL` if the variable did not exist).
#'
#' @param symbol Name of variable to make reactive, as a string.
#' @param env Environment in which to create binding. Expert use only.
#' @return None.
#' @keywords internal
#' @examples
#' reactiveConsole(TRUE)
#'
#' a <- 10
#' makeReactiveBinding("a")
#'
#' b <- reactive(a * -1)
#' observe(print(b()))
#'
#' a <- 20
#' a <- 30
#'
#' reactiveConsole(FALSE)
#' @export
makeReactiveBinding <- function(symbol, env = parent.frame()) {
  if (exists(symbol, envir = env, inherits = FALSE)) {
    initialValue <- env[[symbol]]
    rm(list = symbol, envir = env, inherits = FALSE)
  } else {
    initialValue <- NULL
  }

  val <- reactiveVal(initialValue, label = symbol)
  makeActiveBinding(symbol, val, env = env)

  invisible()
}

# `%<-reactive%` <- function(name, value) {
#   sym <- deparse(substitute(name))
#   assign(sym, value, pos = parent.frame())
#   makeReactiveBinding(sym, env=parent.frame())
#   invisible(NULL)
# }

# Causes flushReact to be called every time an expression is
# entered into the top-level prompt
setAutoflush <- local({
  callbackId <- NULL

  function(enable) {
    if (xor(is.null(callbackId), isTRUE(enable))) {
      return(invisible())
    }

    if (isTRUE(enable)) {
      callbackId <<- addTaskCallback(function(expr, value, ok, visible) {
        timerCallbacks$executeElapsed()
        flushReact()
        return(TRUE)
      })
    } else {
      removeTaskCallback(callbackId)
      callbackId <<- NULL
    }
    invisible()
  }
})


#' Activate reactivity in the console
#'
#' This is an experimental feature that allows you to enable reactivity
#' at the console, for the purposes of experimentation and learning.
#'
#' @keywords internal
#' @param enabled Turn console reactivity on or off?
#' @export
#' @examples
#' reactiveConsole(TRUE)
#' x <- reactiveVal(10)
#' y <- observe({
#'   message("The value of x is ", x())
#' })
#' x(20)
#' x(30)
#' reactiveConsole(FALSE)
reactiveConsole <- function(enabled) {
  options(shiny.suppressMissingContextError = enabled)
  setAutoflush(enabled)
}

# ---------------------------------------------------------------------------

#' Timer
#'
#' Creates a reactive timer with the given interval. A reactive timer is like a
#' reactive value, except reactive values are triggered when they are set, while
#' reactive timers are triggered simply by the passage of time.
#'
#' [Reactive expressions][reactive] and observers that want to be
#' invalidated by the timer need to call the timer function that
#' `reactiveTimer` returns, even if the current time value is not actually
#' needed.
#'
#' See [invalidateLater()] as a safer and simpler alternative.
#'
#' @param intervalMs How often to fire, in milliseconds
#' @param session A session object. This is needed to cancel any scheduled
#'   invalidations after a user has ended the session. If `NULL`, then
#'   this invalidation will not be tied to any session, and so it will still
#'   occur.
#' @return A no-parameter function that can be called from a reactive context,
#'   in order to cause that context to be invalidated the next time the timer
#'   interval elapses. Calling the returned function also happens to yield the
#'   current time (as in [base::Sys.time()]).
#' @seealso [invalidateLater()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   sliderInput("n", "Number of observations", 2, 1000, 500),
#'   plotOutput("plot")
#' )
#'
#' server <- function(input, output) {
#'
#'   # Anything that calls autoInvalidate will automatically invalidate
#'   # every 2 seconds.
#'   autoInvalidate <- reactiveTimer(2000)
#'
#'   observe({
#'     # Invalidate and re-execute this reactive expression every time the
#'     # timer fires.
#'     autoInvalidate()
#'
#'     # Do something each time this is invalidated.
#'     # The isolate() makes this observer _not_ get invalidated and re-executed
#'     # when input$n changes.
#'     print(paste("The value of input$n is", isolate(input$n)))
#'   })
#'
#'   # Generate a new histogram each time the timer fires, but not when
#'   # input$n changes.
#'   output$plot <- renderPlot({
#'     autoInvalidate()
#'     hist(rnorm(isolate(input$n)))
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @export
reactiveTimer <- function(intervalMs=1000, session = getDefaultReactiveDomain()) {
  # Need to make sure that session is resolved at creation, not when the
  # callback below is fired (see #1621).
  force(session)

  # TODO-barret - ## leave alone for now
  # reactId <- nextGlobalReactId()
  # rLog$define(reactId, paste0("timer(", intervalMs, ")"))

  scheduler <- defineScheduler(session)

  dependents <- Map$new()
  timerHandle <- scheduler(intervalMs, function() {
    # Quit if the session is closed
    if (!is.null(session) && session$isClosed()) {
      return(invisible())
    }

    timerHandle <<- scheduler(intervalMs, sys.function())

    doInvalidate <- function() {
      lapply(
        dependents$values(),
        function(dep.ctx) {
          dep.ctx$invalidate()
          NULL
        })
    }

    if (!is.null(session)) {
      # If this timer belongs to a session, we must wait until the next cycle is
      # ready to invalidate.
      session$cycleStartAction(doInvalidate)
    } else {
      # If this timer doesn't belong to a session, we invalidate right away.
      doInvalidate()
    }
  })

  if (!is.null(session)) {
    session$onEnded(timerHandle)
  }

  return(function() {
    newValue <- Sys.time()
    ctx <- getCurrentContext()
    if (!dependents$containsKey(ctx$id)) {
      dependents$set(ctx$id, ctx)
      ctx$onInvalidate(function() {
        dependents$remove(ctx$id)
      })
    }
    return(newValue)
  })
}

#' Scheduled Invalidation
#'
#' Schedules the current reactive context to be invalidated in the given number
#' of milliseconds.
#'
#' If this is placed within an observer or reactive expression, that object will
#' be invalidated (and re-execute) after the interval has passed. The
#' re-execution will reset the invalidation flag, so in a typical use case, the
#' object will keep re-executing and waiting for the specified interval. It's
#' possible to stop this cycle by adding conditional logic that prevents the
#' `invalidateLater` from being run.
#'
#' @param millis Approximate milliseconds to wait before invalidating the
#'   current reactive context.
#' @param session A session object. This is needed to cancel any scheduled
#'   invalidations after a user has ended the session. If `NULL`, then
#'   this invalidation will not be tied to any session, and so it will still
#'   occur.
#'
#' @seealso [reactiveTimer()] is a slightly less safe alternative.
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   sliderInput("n", "Number of observations", 2, 1000, 500),
#'   plotOutput("plot")
#' )
#'
#' server <- function(input, output, session) {
#'
#'   observe({
#'     # Re-execute this reactive expression after 1000 milliseconds
#'     invalidateLater(1000, session)
#'
#'     # Do something each time this is invalidated.
#'     # The isolate() makes this observer _not_ get invalidated and re-executed
#'     # when input$n changes.
#'     print(paste("The value of input$n is", isolate(input$n)))
#'   })
#'
#'   # Generate a new histogram at timed intervals, but not when
#'   # input$n changes.
#'   output$plot <- renderPlot({
#'     # Re-execute this reactive expression after 2000 milliseconds
#'     invalidateLater(2000)
#'     hist(rnorm(isolate(input$n)))
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @export
invalidateLater <- function(millis, session = getDefaultReactiveDomain()) {
  force(session)

  ctx <- getCurrentContext()
  rLog$invalidateLater(ctx$.reactId, ctx$id, millis, session)

  clear_on_ended_callback <- function() {}

  scheduler <- defineScheduler(session)

  timerHandle <- scheduler(millis, function() {
    if (is.null(session)) {
      ctx$invalidate()
      return(invisible())
    }

    clear_on_ended_callback()

    if (!session$isClosed()) {
      session$cycleStartAction(function() {
        ctx$invalidate()
      })
    }

    invisible()
  })

  if (!is.null(session)) {
    # timerHandle is a callback that clears the scheduled task. It gets
    # registered with session$onEnded() each time invalidateLater() is called.
    # So, to prevent these callbacks from building up and leaking memory, we
    # need to deregister the onEnded(timerHandle) callback each time when the
    # scheduled task executes; after the task executes, the timerHandle()
    # function is essentially a no-op, so we can deregister it.
    clear_on_ended_callback <- session$onEnded(timerHandle)
  }

  invisible()
}

coerceToFunc <- function(x) {
  force(x);
  if (is.function(x))
    return(x)
  else
    return(function() x)
}

#' Reactive polling
#'
#' Used to create a reactive data source, which works by periodically polling a
#' non-reactive data source.
#'
#' `reactivePoll` works by pairing a relatively cheap "check" function with
#' a more expensive value retrieval function. The check function will be
#' executed periodically and should always return a consistent value until the
#' data changes. When the check function returns a different value, then the
#' value retrieval function will be used to re-populate the data.
#'
#' Note that the check function doesn't return `TRUE` or `FALSE` to
#' indicate whether the underlying data has changed. Rather, the check function
#' indicates change by returning a different value from the previous time it was
#' called.
#'
#' For example, `reactivePoll` is used to implement
#' `reactiveFileReader` by pairing a check function that simply returns the
#' last modified timestamp of a file, and a value retrieval function that
#' actually reads the contents of the file.
#'
#' As another example, one might read a relational database table reactively by
#' using a check function that does `SELECT MAX(timestamp) FROM table` and
#' a value retrieval function that does `SELECT * FROM table`.
#'
#' The `intervalMillis`, `checkFunc`, and `valueFunc` functions
#' will be executed in a reactive context; therefore, they may read reactive
#' values and reactive expressions.
#'
#' @param intervalMillis Approximate number of milliseconds to wait between
#'   calls to `checkFunc`. This can be either a numeric value, or a
#'   function that returns a numeric value.
#' @param session The user session to associate this file reader with, or
#'   `NULL` if none. If non-null, the reader will automatically stop when
#'   the session ends.
#' @param checkFunc A relatively cheap function whose values over time will be
#'   tested for equality; inequality indicates that the underlying value has
#'   changed and needs to be invalidated and re-read using `valueFunc`. See
#'   Details.
#' @param valueFunc A function that calculates the underlying value. See
#'   Details.
#'
#' @return A reactive expression that returns the result of `valueFunc`,
#'   and invalidates when `checkFunc` changes.
#'
#' @seealso [reactiveFileReader()]
#'
#' @examples
#' function(input, output, session) {
#'
#'   data <- reactivePoll(1000, session,
#'     # This function returns the time that log_file was last modified
#'     checkFunc = function() {
#'       if (file.exists(log_file))
#'         file.info(log_file)$mtime[1]
#'       else
#'         ""
#'     },
#'     # This function returns the content of log_file
#'     valueFunc = function() {
#'       read.csv(log_file)
#'     }
#'   )
#'
#'   output$dataTable <- renderTable({
#'     data()
#'   })
#' }
#' @export
reactivePoll <- function(intervalMillis, session, checkFunc, valueFunc) {
  intervalMillis <- coerceToFunc(intervalMillis)

  cookie <- reactiveVal(
    isolate(checkFunc()),
    label = "reactivePoll cookie"
  )

  re_finalized <- FALSE
  env <- environment()

  o <- observe({
    # When no one holds a reference to the reactive returned from
    # reactivePoll, destroy and remove the observer so that it doesn't keep
    # firing and hold onto resources.
    if (re_finalized) {
      o$destroy()
      rm(o, envir = env)
      return()
    }

    cookie(checkFunc())
    invalidateLater(intervalMillis(), session)
  }, label = "reactivePoll cleanup")

  re <- reactive(label = "reactivePoll value", {
    # Take a dependency on the cookie, so that when it changes, this
    # reactive expression is invalidated.
    cookie()

    valueFunc()
  })

  reg.finalizer(attr(re, "observable"), function(e) {
    re_finalized <<- TRUE
  })

  # So that the observer and finalizer function don't (indirectly) hold onto a
  # reference to `re` and thus prevent it from getting GC'd.
  on.exit(rm(re))

  return(re)
}

#' Reactive file reader
#'
#' Given a file path and read function, returns a reactive data source for the
#' contents of the file.
#'
#' `reactiveFileReader` works by periodically checking the file's last
#' modified time; if it has changed, then the file is re-read and any reactive
#' dependents are invalidated.
#'
#' The `intervalMillis`, `filePath`, and `readFunc` functions
#' will each be executed in a reactive context; therefore, they may read
#' reactive values and reactive expressions.
#'
#' @param intervalMillis Approximate number of milliseconds to wait between
#'   checks of the file's last modified time. This can be a numeric value, or a
#'   function that returns a numeric value.
#' @param session The user session to associate this file reader with, or
#'   `NULL` if none. If non-null, the reader will automatically stop when
#'   the session ends.
#' @param filePath The file path to poll against and to pass to `readFunc`.
#'   This can either be a single-element character vector, or a function that
#'   returns one.
#' @param readFunc The function to use to read the file; must expect the first
#'   argument to be the file path to read. The return value of this function is
#'   used as the value of the reactive file reader.
#' @param ... Any additional arguments to pass to `readFunc` whenever it is
#'   invoked.
#'
#' @return A reactive expression that returns the contents of the file, and
#'   automatically invalidates when the file changes on disk (as determined by
#'   last modified time).
#'
#' @seealso [reactivePoll()]
#'
#' @examples
#' \dontrun{
#' # Per-session reactive file reader
#' function(input, output, session) {
#'   fileData <- reactiveFileReader(1000, session, 'data.csv', read.csv)
#'
#'   output$data <- renderTable({
#'     fileData()
#'   })
#' }
#'
#' # Cross-session reactive file reader. In this example, all sessions share
#' # the same reader, so read.csv only gets executed once no matter how many
#' # user sessions are connected.
#' fileData <- reactiveFileReader(1000, NULL, 'data.csv', read.csv)
#' function(input, output, session) {
#'   output$data <- renderTable({
#'     fileData()
#'   })
#' }
#' }
#' @export
reactiveFileReader <- function(intervalMillis, session, filePath, readFunc, ...) {
  filePath <- coerceToFunc(filePath)
  extraArgs <- list2(...)

  reactivePoll(
    intervalMillis, session,
    function() {
      path <- filePath()
      info <- file.info(path)
      return(paste(path, info$mtime, info$size))
    },
    function() {
      do.call(readFunc, c(filePath(), extraArgs))
    }
  )
}

#' Create a non-reactive scope for an expression
#'
#' Executes the given expression in a scope where reactive values or expression
#' can be read, but they cannot cause the reactive scope of the caller to be
#' re-evaluated when they change.
#'
#' Ordinarily, the simple act of reading a reactive value causes a relationship
#' to be established between the caller and the reactive value, where a change
#' to the reactive value will cause the caller to re-execute. (The same applies
#' for the act of getting a reactive expression's value.) The `isolate`
#' function lets you read a reactive value or expression without establishing this
#' relationship.
#'
#' The expression given to `isolate()` is evaluated in the calling
#' environment. This means that if you assign a variable inside the
#' `isolate()`, its value will be visible outside of the `isolate()`.
#' If you want to avoid this, you can use [base::local()] inside the
#' `isolate()`.
#'
#' This function can also be useful for calling reactive expression at the
#' console, which can be useful for debugging. To do so, simply wrap the
#' calls to the reactive expression with `isolate()`.
#'
#' @param expr An expression that can access reactive values or expressions.
#'
#' @examples
#' \dontrun{
#' observe({
#'   input$saveButton  # Do take a dependency on input$saveButton
#'
#'   # isolate a simple expression
#'   data <- get(isolate(input$dataset))  # No dependency on input$dataset
#'   writeToDatabase(data)
#' })
#'
#' observe({
#'   input$saveButton  # Do take a dependency on input$saveButton
#'
#'   # isolate a whole block
#'   data <- isolate({
#'     a <- input$valueA   # No dependency on input$valueA or input$valueB
#'     b <- input$valueB
#'     c(a=a, b=b)
#'   })
#'   writeToDatabase(data)
#' })
#'
#' observe({
#'   x <- 1
#'   # x outside of isolate() is affected
#'   isolate(x <- 2)
#'   print(x) # 2
#'
#'   y <- 1
#'   # Use local() to avoid affecting calling environment
#'   isolate(local(y <- 2))
#'   print(y) # 1
#' })
#'
#' }
#'
#' # Can also use isolate to call reactive expressions from the R console
#' values <- reactiveValues(A=1)
#' fun <- reactive({ as.character(values$A) })
#' isolate(fun())
#' # "1"
#'
#' # isolate also works if the reactive expression accesses values from the
#' # input object, like input$x
#' @export
isolate <- function(expr) {
  if (hasCurrentContext()) {
    reactId <- getCurrentContext()$.reactId
  } else {
    reactId <- rLog$noReactId
  }

  # Do not track ospans for `isolate()`
  ctx <- Context$new(getDefaultReactiveDomain(), '[isolate]', type='isolate', reactId = reactId)
  on.exit(ctx$invalidate())
  # Matching ..stacktraceon../..stacktraceoff.. pair
  ..stacktraceoff..(ctx$run(function() {
    ..stacktraceon..(expr)
  }))
}

#' Evaluate an expression without a reactive context
#'
#' Temporarily blocks the current reactive context and evaluates the given
#' expression. Any attempt to directly access reactive values or expressions in
#' `expr` will give the same results as doing it at the top-level (by
#' default, an error).
#'
#' @param expr An expression to evaluate.
#' @return The value of `expr`.
#'
#' @seealso [isolate()]
#' @export
maskReactiveContext <- function(expr) {
  .getReactiveEnvironment()$runWith(NULL, function() {
    expr
  })
}

#' Event handler
#'
#' Respond to "event-like" reactive inputs, values, and expressions. As of Shiny
#' 1.6.0, we recommend using [bindEvent()] instead of `eventReactive()` and
#' `observeEvent()`. This is because `bindEvent()` can be composed with
#' [bindCache()], and because it can also be used with `render` functions (like
#' [renderText()] and [renderPlot()]).
#'
#' Shiny's reactive programming framework is primarily designed for calculated
#' values (reactive expressions) and side-effect-causing actions (observers)
#' that respond to *any* of their inputs changing. That's often what is
#' desired in Shiny apps, but not always: sometimes you want to wait for a
#' specific action to be taken from the user, like clicking an
#' [actionButton()], before calculating an expression or taking an
#' action. A reactive value or expression that is used to trigger other
#' calculations in this way is called an *event*.
#'
#' These situations demand a more imperative, "event handling" style of
#' programming that is possible--but not particularly intuitive--using the
#' reactive programming primitives [observe()] and
#' [isolate()]. `observeEvent` and `eventReactive` provide
#' straightforward APIs for event handling that wrap `observe` and
#' `isolate`.
#'
#' Use `observeEvent` whenever you want to *perform an action* in
#' response to an event. (Note that "recalculate a value" does not generally
#' count as performing an action--see `eventReactive` for that.) The first
#' argument is the event you want to respond to, and the second argument is a
#' function that should be called whenever the event occurs. Note that
#' `observeEvent()` is equivalent to using `observe() %>% bindEvent()` and as of
#' Shiny 1.6.0, we recommend the latter.
#'
#' Use `eventReactive` to create a *calculated value* that only
#' updates in response to an event. This is just like a normal
#' [reactive expression][reactive] except it ignores all the usual
#' invalidations that come from its reactive dependencies; it only invalidates
#' in response to the given event. Note that
#' `eventReactive()` is equivalent to using `reactive() %>% bindEvent()` and as of
#' Shiny 1.6.0, we recommend the latter.
#'
#' @section ignoreNULL and ignoreInit:
#'
#' Both `observeEvent` and `eventReactive` take an `ignoreNULL`
#' parameter that affects behavior when the `eventExpr` evaluates to
#' `NULL` (or in the special case of an [actionButton()],
#' `0`). In these cases, if `ignoreNULL` is `TRUE`, then an
#' `observeEvent` will not execute and an `eventReactive` will raise a
#' silent [validation][validate] error. This is useful behavior if you
#' don't want to do the action or calculation when your app first starts, but
#' wait for the user to initiate the action first (like a "Submit" button);
#' whereas `ignoreNULL=FALSE` is desirable if you want to initially perform
#' the action/calculation and just let the user re-initiate it (like a
#' "Recalculate" button).
#'
#' Likewise, both `observeEvent` and `eventReactive` also take in an
#' `ignoreInit` argument. By default, both of these will run right when they
#' are created (except if, at that moment, `eventExpr` evaluates to `NULL`
#' and `ignoreNULL` is `TRUE`). But when responding to a click of an action
#' button, it may often be useful to set `ignoreInit` to `TRUE`. For
#' example, if you're setting up an `observeEvent` for a dynamically created
#' button, then `ignoreInit = TRUE` will guarantee that the action (in
#' `handlerExpr`) will only be triggered when the button is actually clicked,
#' instead of also being triggered when it is created/initialized. Similarly,
#' if you're setting up an `eventReactive` that responds to a dynamically
#' created button used to refresh some data (then returned by that `eventReactive`),
#' then you should use `eventReactive([...], ignoreInit = TRUE)` if you want
#' to let the user decide if/when they want to refresh the data (since, depending
#' on the app, this may be a computationally expensive operation).
#'
#' Even though `ignoreNULL` and `ignoreInit` can be used for similar
#' purposes they are independent from one another. Here's the result of combining
#' these:

#'
#' \describe{
#'   \item{`ignoreNULL = TRUE` and `ignoreInit = FALSE`}{
#'      This is the default. This combination means that `handlerExpr`/
#'      `valueExpr` will run every time that `eventExpr` is not
#'      `NULL`. If, at the time of the creation of the
#'      `observeEvent`/`eventReactive`, `eventExpr` happens
#'      to *not* be `NULL`, then the code runs.
#'   }
#'   \item{`ignoreNULL = FALSE` and `ignoreInit = FALSE`}{
#'      This combination means that `handlerExpr`/`valueExpr` will
#'      run every time no matter what.
#'   }
#'   \item{`ignoreNULL = FALSE` and `ignoreInit = TRUE`}{
#'      This combination means that `handlerExpr`/`valueExpr` will
#'      *not* run when the `observeEvent`/`eventReactive` is
#'      created (because `ignoreInit = TRUE`), but it will run every
#'      other time.
#'   }
#'   \item{`ignoreNULL = TRUE` and `ignoreInit = TRUE`}{
#'      This combination means that `handlerExpr`/`valueExpr` will
#'      *not* run when the `observeEvent`/`eventReactive` is
#'      created (because  `ignoreInit = TRUE`). After that,
#'      `handlerExpr`/`valueExpr` will run every time that
#'      `eventExpr` is not `NULL`.
#'   }
#' }
#'
#' @param eventExpr A (quoted or unquoted) expression that represents the event;
#'   this can be a simple reactive value like `input$click`, a call to a
#'   reactive expression like `dataset()`, or even a complex expression
#'   inside curly braces
#' @param handlerExpr The expression to call whenever `eventExpr` is
#'   invalidated. This should be a side-effect-producing action (the return
#'   value will be ignored). It will be executed within an [isolate()]
#'   scope.
#' @param valueExpr The expression that produces the return value of the
#'   `eventReactive`. It will be executed within an [isolate()]
#'   scope.
#' @param event.env The parent environment for the reactive expression. By default,
#'   this is the calling environment, the same as when defining an ordinary
#'   non-reactive expression. If `eventExpr` is a quosure and `event.quoted` is `TRUE`,
#'   then `event.env` is ignored.
#' @param event.quoted If it is `TRUE`, then the [`quote()`]ed value of `eventExpr`
#'   will be used when `eventExpr` is evaluated. If `eventExpr` is a quosure and you
#'   would like to use its expression as a value for `eventExpr`, then you must set
#'  `event.quoted` to `TRUE`.
#' @param handler.env The parent environment for the reactive expression. By default,
#'   this is the calling environment, the same as when defining an ordinary
#'   non-reactive expression. If `handlerExpr` is a quosure and `handler.quoted` is `TRUE`,
#'   then `handler.env` is ignored.
#' @param handler.quoted If it is `TRUE`, then the [`quote()`]ed value of `handlerExpr`
#'   will be used when `handlerExpr` is evaluated. If `handlerExpr` is a quosure and you
#'   would like to use its expression as a value for `handlerExpr`, then you must set
#'  `handler.quoted` to `TRUE`.
#' @param value.env The parent environment for the reactive expression. By default,
#'   this is the calling environment, the same as when defining an ordinary
#'   non-reactive expression. If `valueExpr` is a quosure and `value.quoted` is `TRUE`,
#'   then `value.env` is ignored.
#' @param value.quoted If it is `TRUE`, then the [`quote()`]ed value of `valueExpr`
#'   will be used when `valueExpr` is evaluated. If `valueExpr` is a quosure and you
#'   would like to use its expression as a value for `valueExpr`, then you must set
#'  `value.quoted` to `TRUE`.
#' @param label A label for the observer or reactive, useful for debugging.
#' @param suspended If `TRUE`, start the observer in a suspended state. If
#'   `FALSE` (the default), start in a non-suspended state.
#' @param priority An integer or numeric that controls the priority with which
#'   this observer should be executed. An observer with a given priority level
#'   will always execute sooner than all observers with a lower priority level.
#'   Positive, negative, and zero values are allowed.
#' @param domain See [domains].
#' @param autoDestroy If `TRUE` (the default), the observer will be
#'   automatically destroyed when its domain (if any) ends.
#' @param ignoreNULL Whether the action should be triggered (or value
#'   calculated, in the case of `eventReactive`) when the input event expression
#'   is `NULL`. See Details.
#' @param ignoreInit If `TRUE`, then, when this `observeEvent` is
#'   first created/initialized, ignore the `handlerExpr` (the second
#'   argument), whether it is otherwise supposed to run or not. The default is
#'   `FALSE`. See Details.
#' @param once Whether this `observeEvent` should be immediately destroyed
#'   after the first time that the code in `handlerExpr` is run. This
#'   pattern is useful when you want to subscribe to a event that should only
#'   happen once.
#' @param ... Currently not used.
#'
#' @return `observeEvent` returns an observer reference class object (see
#'   [observe()]). `eventReactive` returns a reactive expression
#'   object (see [reactive()]).
#'
#' @seealso [actionButton()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#'   ## App 1: Sample usage
#'   shinyApp(
#'     ui = fluidPage(
#'       column(4,
#'         numericInput("x", "Value", 5),
#'         br(),
#'         actionButton("button", "Show")
#'       ),
#'       column(8, tableOutput("table"))
#'     ),
#'     server = function(input, output) {
#'       # Take an action every time button is pressed;
#'       # here, we just print a message to the console
#'       observeEvent(input$button, {
#'         cat("Showing", input$x, "rows\n")
#'       })
#'       # The observeEvent() above is equivalent to:
#'       # observe({
#'       #    cat("Showing", input$x, "rows\n")
#'       #   }) %>%
#'       #   bindEvent(input$button)
#'
#'       # Take a reactive dependency on input$button, but
#'       # not on any of the stuff inside the function
#'       df <- eventReactive(input$button, {
#'         head(cars, input$x)
#'       })
#'       output$table <- renderTable({
#'         df()
#'       })
#'     }
#'   )
#'
#'   ## App 2: Using `once`
#'   shinyApp(
#'     ui = basicPage( actionButton("go", "Go")),
#'     server = function(input, output, session) {
#'       observeEvent(input$go, {
#'         print(paste("This will only be printed once; all",
#'               "subsequent button clicks won't do anything"))
#'       }, once = TRUE)
#'       # The observeEvent() above is equivalent to:
#'       # observe({
#'       #   print(paste("This will only be printed once; all",
#'       #         "subsequent button clicks won't do anything"))
#'       #   }) %>%
#'       #   bindEvent(input$go, once = TRUE)
#'     }
#'   )
#'
#'   ## App 3: Using `ignoreInit` and `once`
#'   shinyApp(
#'     ui = basicPage(actionButton("go", "Go")),
#'     server = function(input, output, session) {
#'       observeEvent(input$go, {
#'         insertUI("#go", "afterEnd",
#'                  actionButton("dynamic", "click to remove"))
#'
#'         # set up an observer that depends on the dynamic
#'         # input, so that it doesn't run when the input is
#'         # created, and only runs once after that (since
#'         # the side effect is remove the input from the DOM)
#'         observeEvent(input$dynamic, {
#'           removeUI("#dynamic")
#'         }, ignoreInit = TRUE, once = TRUE)
#'       })
#'     }
#'   )
#' }
#' @export
observeEvent <- function(eventExpr, handlerExpr,
  event.env = parent.frame(), event.quoted = FALSE,
  handler.env = parent.frame(), handler.quoted = FALSE,
  ...,
  label = NULL, suspended = FALSE, priority = 0,
  domain = getDefaultReactiveDomain(), autoDestroy = TRUE,
  ignoreNULL = TRUE, ignoreInit = FALSE, once = FALSE)
{
  check_dots_empty()

  eventQ <- exprToQuo(eventExpr, event.env, event.quoted)
  handlerQ <- exprToQuo(handlerExpr, handler.env, handler.quoted)
  label <- quoToLabel(eventQ, "observeEvent", label)

  without_otel_bind({
    handler <- inject(observe(
      !!handlerQ,
      label = label,
      suspended = suspended,
      priority = priority,
      domain = domain,
      autoDestroy = TRUE,
      ..stacktraceon = TRUE
    ))
  })

  o <- inject(bindEvent(
    ignoreNULL = ignoreNULL,
    ignoreInit = ignoreInit,
    once = once,
    label = label,
    !!eventQ,
    x = handler
  ))

  invisible(o)
}

#' @rdname observeEvent
#' @export
eventReactive <- function(eventExpr, valueExpr,
  event.env = parent.frame(), event.quoted = FALSE,
  value.env = parent.frame(), value.quoted = FALSE,
  ...,
  label = NULL, domain = getDefaultReactiveDomain(),
  ignoreNULL = TRUE, ignoreInit = FALSE)
{
  check_dots_empty()

  eventQ <- exprToQuo(eventExpr, event.env, event.quoted)
  valueQ <- exprToQuo(valueExpr, value.env, value.quoted)

  func <- installExprFunction(eventExpr, "func", event.env, event.quoted, wrappedWithLabel = FALSE)
  # Attach a label and a reference to the original user source for debugging
  userEventExpr <- fn_body(func)
  label <- exprToLabel(userEventExpr, "eventReactive", label)

  without_otel_bind({
    value_r <- inject(reactive(!!valueQ, domain = domain, label = label))
  })

  invisible(inject(bindEvent(
    ignoreNULL = ignoreNULL,
    ignoreInit = ignoreInit,
    label = label,
    !!eventQ,
    x = value_r
  )))
}

isNullEvent <- function(value) {
  is.null(value) || (inherits(value, 'shinyActionButtonValue') && value == 0)
}

#' Slow down a reactive expression with debounce/throttle
#'
#' Transforms a reactive expression by preventing its invalidation signals from
#' being sent unnecessarily often. This lets you ignore a very "chatty" reactive
#' expression until it becomes idle, which is useful when the intermediate
#' values don't matter as much as the final value, and the downstream
#' calculations that depend on the reactive expression take a long time.
#' `debounce` and `throttle` use different algorithms for slowing down
#' invalidation signals; see Details.
#'
#' @section Limitations:
#'
#'   Because R is single threaded, we can't come close to guaranteeing that the
#'   timing of debounce/throttle (or any other timing-related functions in
#'   Shiny) will be consistent or accurate; at the time we want to emit an
#'   invalidation signal, R may be performing a different task and we have no
#'   way to interrupt it (nor would we necessarily want to if we could).
#'   Therefore, it's best to think of the time windows you pass to these
#'   functions as minimums.
#'
#'   You may also see undesirable behavior if the amount of time spent doing
#'   downstream processing for each change approaches or exceeds the time
#'   window: in this case, debounce/throttle may not have any effect, as the
#'   time each subsequent event is considered is already after the time window
#'   has expired.
#'
#' @details
#'
#' This is not a true debounce/throttle in that it will not prevent `r`
#' from being called many times (in fact it may be called more times than
#' usual), but rather, the reactive invalidation signal that is produced by
#' `r` is debounced/throttled instead. Therefore, these functions should be
#' used when `r` is cheap but the things it will trigger (downstream
#' outputs and reactives) are expensive.
#'
#' Debouncing means that every invalidation from `r` will be held for the
#' specified time window. If `r` invalidates again within that time window,
#' then the timer starts over again. This means that as long as invalidations
#' continually arrive from `r` within the time window, the debounced
#' reactive will not invalidate at all. Only after the invalidations stop (or
#' slow down sufficiently) will the downstream invalidation be sent.
#'
#' `ooo-oo-oo---- => -----------o-`
#'
#' (In this graphical depiction, each character represents a unit of time, and
#' the time window is 3 characters.)
#'
#' Throttling, on the other hand, delays invalidation if the *throttled*
#' reactive recently (within the time window) invalidated. New `r`
#' invalidations do not reset the time window. This means that if invalidations
#' continually come from `r` within the time window, the throttled reactive
#' will invalidate regularly, at a rate equal to or slower than the time
#' window.
#'
#' `ooo-oo-oo---- => o--o--o--o---`
#'
#' @param r A reactive expression (that invalidates too often).
#' @param millis The debounce/throttle time window. You may optionally pass a
#'   no-arg function or reactive expression instead, e.g. to let the end-user
#'   control the time window.
#' @param priority Debounce/throttle is implemented under the hood using
#'   [observers][observe]. Use this parameter to set the priority of
#'   these observers. Generally, this should be higher than the priorities of
#'   downstream observers and outputs (which default to zero).
#' @param domain See [domains].
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#' options(device.ask.default = FALSE)
#'
#' library(shiny)
#' library(magrittr)
#'
#' ui <- fluidPage(
#'   plotOutput("plot", click = clickOpts("hover")),
#'   helpText("Quickly click on the plot above, while watching the result table below:"),
#'   tableOutput("result")
#' )
#'
#' server <- function(input, output, session) {
#'   hover <- reactive({
#'     if (is.null(input$hover))
#'       list(x = NA, y = NA)
#'     else
#'       input$hover
#'   })
#'   hover_d <- hover %>% debounce(1000)
#'   hover_t <- hover %>% throttle(1000)
#'
#'   output$plot <- renderPlot({
#'     plot(cars)
#'   })
#'
#'   output$result <- renderTable({
#'     data.frame(
#'       mode = c("raw", "throttle", "debounce"),
#'       x = c(hover()$x, hover_t()$x, hover_d()$x),
#'       y = c(hover()$y, hover_t()$y, hover_d()$y)
#'     )
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @export
debounce <- function(r, millis, priority = 100, domain = getDefaultReactiveDomain()) {

  # TODO: make a nice label for the observer(s)

  force(r)
  force(millis)

  call_srcref <- attr(sys.call(), "srcref", exact = TRUE)
  label <- rassignSrcrefToLabel(
    call_srcref,
    defaultLabel = "<anonymous>",
    fnName = "debounce"
  )

  if (!is.function(millis)) {
    origMillis <- millis
    millis <- function() origMillis
  }

  trigger <- reactiveVal(NULL, label = sprintf("debounce %s trigger", label))
  # the deadline for the timer to fire; NULL if not scheduled
  when <- reactiveVal(NULL, label = sprintf("debounce %s when", label))

  # Responsible for tracking when r() changes.
  firstRun <- TRUE
  observe(
    label = sprintf("debounce %s tracker", label),
    domain = domain,
    priority = priority,
    {
      if (firstRun) {
        # During the first run we don't want to set `when`, as this will kick
        # off the timer. We only want to do that when we see `r()` change.
        firstRun <<- FALSE

        # Ensure r() is called only after setting firstRun to FALSE since r()
        # may throw an error
        try(r(), silent = TRUE)
        return()
      }
      # This ensures r() is still tracked after firstRun
      try(r(), silent = TRUE)

      # The value (or possibly millis) changed. Start or reset the timer.
      when(
        getDomainTimeMs(domain) + millis()
      )
    }
  )

  # This observer is the timer. It rests until `when` elapses, then touches
  # `trigger`.
  observe(
    label = sprintf("debounce %s timer", label),
    domain = domain,
    priority = priority,
    {
      if (is.null(when()))
        return()

      now <- getDomainTimeMs(domain)
      if (now >= when()) {
        # Mod by 999999999 to get predictable overflow behavior
        trigger(
          isolate(trigger() %||% 0) %% 999999999 + 1
        )
        when(NULL)
      } else {
        invalidateLater(when() - now)
      }
    }
  )

  # This is the actual reactive that is returned to the user. It returns the
  # value of r(), but only invalidates/updates when `trigger` is touched.
  er <- eventReactive(
    {trigger()}, {r()},
    label = sprintf("debounce %s result", label), ignoreNULL = FALSE, domain = domain
  )

  # Force the value of er to be immediately cached upon creation. It's very hard
  # to explain why this observer is needed, but if you want to understand, try
  # commenting it out and studying the unit test failure that results.
  primer <- observe({
    primer$destroy()
    try(er(), silent = TRUE)
  }, label = sprintf("debounce %s primer", label), domain = domain, priority = priority)

  er
}

#' @rdname debounce
#' @export
throttle <- function(r, millis, priority = 100, domain = getDefaultReactiveDomain()) {

  # TODO: make a nice label for the observer(s)

  force(r)
  force(millis)


  call_srcref <- attr(sys.call(), "srcref", exact = TRUE)
  label <- rassignSrcrefToLabel(
    call_srcref,
    defaultLabel = "<anonymous>",
    fnName = "throttle"
  )

  if (!is.function(millis)) {
    origMillis <- millis
    millis <- function() origMillis
  }

  trigger <- reactiveVal(0, label = sprintf("throttle %s trigger", label))
  # Last time we fired; NULL if never
  lastTriggeredAt <- reactiveVal(NULL, label = sprintf("throttle %s last triggered at", label))
  # If TRUE, trigger again when timer elapses
  pending <- reactiveVal(FALSE, label = sprintf("throttle %s pending", label))
  blackoutMillisLeft <- function() {
    if (is.null(lastTriggeredAt())) {
      0
    } else {
      max(0, lastTriggeredAt() + millis() - getDomainTimeMs(domain))
    }
  }

  update_trigger <- function() {
    lastTriggeredAt(getDomainTimeMs(domain))
    # Mod by 999999999 to get predictable overflow behavior
    trigger(isolate(trigger()) %% 999999999 + 1)
    pending(FALSE)
  }

  # Responsible for tracking when f() changes.
  observeEvent(try(r(), silent = TRUE), {
    if (pending()) {
      # In a blackout period and someone already scheduled; do nothing
    } else if (blackoutMillisLeft() > 0) {
      # In a blackout period but this is the first change in that period; set
      # pending so that a trigger will be scheduled at the end of the period
      pending(TRUE)
    } else {
      # Not in a blackout period. Trigger, which will start a new blackout
      # period.
      update_trigger()
    }
  }, label = sprintf("throttle %s tracker", label), ignoreNULL = FALSE, priority = priority, domain = domain)

  observe({
    if (!pending()) {
      return()
    }

    timeout <- blackoutMillisLeft()
    if (timeout > 0) {
      invalidateLater(timeout)
    } else {
      update_trigger()
    }
  }, label = sprintf("throttle %s trigger", label), priority = priority, domain = domain)

  # This is the actual reactive that is returned to the user. It returns the
  # value of r(), but only invalidates/updates when trigger is touched.
  eventReactive({trigger()}, {
    r()
  }, label = sprintf("throttle %s result", label), ignoreNULL = FALSE, domain = domain)
}
