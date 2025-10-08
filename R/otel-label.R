# observe mymod:<anonymous>
# observe <anonymous>
# observe mylabel (edited)

# -- Reactives --------------------------------------------------------------

#' OpenTelemetry Label Generation Functions
#'
#' Functions for generating formatted labels for OpenTelemetry tracing spans
#' in Shiny applications. These functions handle module namespacing and
#' cache/event modifiers for different Shiny reactive constructs.
#'
#' @param x The object to generate a label for (reactive, observer, etc.)
#' @param label Character string label for reactive values
#' @param key Character string key for reactiveValues operations
#' @param ... Additional arguments (unused)
#' @param domain Shiny domain object containing namespace information
#'
#' @return Character string formatted for OpenTelemetry span labels
#' @name otel_label
#' @noRd
NULL

ospan_label_reactive <- function(x, ..., domain) {
  fn_name <- otel_label_with_modifiers(
    x,
    "reactive",
    cache_class = "reactive.cache",
    event_class = "reactive.event"
  )

  label <- attr(x, "observable", exact = TRUE)[[".label"]]
  ospan_label <- otel_label_upgrade(label, domain = domain)

  sprintf("%s %s", fn_name, ospan_label)
}

ospan_label_render_function <- function(x, ..., domain) {
  fn_name <- otel_label_with_modifiers(
    x,
    "output",
    cache_class = "shiny.render.function.cache",
    event_class = "shiny.render.function.event"
  )

  ospan_label <- otel_label_upgrade(
    getCurrentOutputInfo(session = domain)$name,
    domain = domain
  )

  sprintf("%s %s", fn_name, ospan_label)
}

ospan_label_observer <- function(x, ..., domain) {
  fn_name <- otel_label_with_modifiers(
    x,
    "observe",
    cache_class = NULL, # Do not match a cache class here
    event_class = "Observer.event"
  )

  ospan_label <- otel_label_upgrade(x$.label, domain = domain)

  sprintf("%s %s", fn_name, ospan_label)
}

# -- Set reactive value(s) ----------------------------------------------------

otel_label_set_reactive_val <- function(label, ..., domain) {
  sprintf(
    "Set reactiveVal %s",
    otel_label_upgrade(label, domain = domain)
  )
}

otel_label_set_reactive_values <- function(label, key, ..., domain) {
  sprintf(
    "Set reactiveValues %s$%s",
    otel_label_upgrade(label, domain = domain),
    key
  )
}


# -- Helpers --------------------------------------------------------------

#' Modify function name based on object class modifiers
#'
#' @param x Object to check class of
#' @param fn_name Base function name
#' @param cache_class Optional class name that indicates cache operation
#' @param event_class Optional class name that indicates event operation
#'
#' @return Modified function name with "cache" or "event" suffix if applicable
#' @noRd
otel_label_with_modifiers <- function(
  x,
  fn_name,
  cache_class = NULL,
  event_class = NULL
) {
  for (x_class in rev(class(x))) {
    if (!is.null(cache_class) && x_class == cache_class) {
      fn_name <- sprintf("%s cache", fn_name)
    } else if (!is.null(event_class) && x_class == event_class) {
      fn_name <- sprintf("%s event", fn_name)
    }
  }

  fn_name
}


#' Upgrade and format OpenTelemetry labels with module namespacing
#'
#' Processes labels for OpenTelemetry tracing, replacing default verbose labels
#' with cleaner alternatives and prepending module namespaces when available.
#'
#' @param label Character string label to upgrade
#' @param ... Additional arguments (unused)
#' @param domain Shiny domain object containing namespace information
#'
#' @return Modified label string with module prefix if applicable
#' @noRd
#'
#' @details
#' Module prefix examples:
#' - "" -> ""
#' - "my-nested-mod-" -> "my-nested-mod"
otel_label_upgrade <- function(label, ..., domain) {
  # By default, `observe()` sets the label to `observe(CODE)`
  # This label is too big and inconsistent.
  # Replace it with `<anonymous>`
  # (Similar with `eventReactive()` and `observeEvent()`)
  if (is_default_label(label) && grepl("(", label, fixed = TRUE)) {
    label <- "<anonymous>"
    # label <- sprintf("<anonymous> - %s", label)
  }

  if (is.null(domain)) {
    return(label)
  }

  namespace <- domain$ns("")

  if (!nzchar(namespace)) {
    return(label)
  }

  # Remove trailing module separator
  mod_ns <- sub(sprintf("%s$", ns.sep), "", namespace)

  # Prepend the module name to the label
  # Ex: `"mymod:x"`
  sprintf("%s:%s", mod_ns, label)
}
