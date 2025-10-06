# observe mymod:<anonymous>
# observe <anonymous>
# observe mylabel (edited)

# -- Reactives --------------------------------------------------------------

# reactive MOD:LABEL
# reactive x
# reactive mymod:x
ospan_label_reactive <- function(x, ..., domain) {
  label <- attr(x, "observable", exact = TRUE)[[".label"]]

  fn_name <-
    if (inherits(x, "reactive.event")) {
      "reactive event"
    } else {
      "reactive"
    }

  sprintf(
    "%s %s",
    fn_name,
    otel_label_upgrade(label, domain = domain)
  )
}

# output MOD:LABEL
# output txt
# output mymod:txt
ospan_label_render_function <- function(..., domain) {
  sprintf(
    "output %s",
    otel_label_upgrade(
      getCurrentOutputInfo(session = domain)$name,
      domain = domain
    )
  )
}

# observe MOD:LABEL
# observe customlabel
# observe mymod:customlabel
# observe mymod:<anonymous>
ospan_label_observer <- function(x, ..., domain) {
  span_label <- x$.label

  fn_name <-
    if (inherits(x, "Observer.event")) {
      "observe event"
    } else {
      "observe"
    }

  sprintf(
    "%s %s",
    fn_name,
    otel_label_upgrade(span_label, domain = domain)
  )
}

# -- Set reactive value(s) ----------------------------------------------------

# [MOD] Set reactiveVal LABEL
# Set reactiveVal x
# Set reactiveVal mymod:x
otel_label_set_reactive_val <- function(label, ..., domain) {
  sprintf(
    "Set reactiveVal %s",
    otel_label_upgrade(label, domain = domain)
  )
}

# Set reactiveValues MOD:LABEL$KEY
# Set reactiveValues x$key
# Set reactiveValues mymod:x$key
otel_label_set_reactive_values <- function(label, key, ..., domain) {
  sprintf(
    "Set reactiveValues %s$%s",
    otel_label_upgrade(label, domain = domain),
    key
  )
}




# -- Helpers --------------------------------------------------------------
# Prefix:
# "" -> ""
# "my-nested-mod-" -> "my-nested-mod"
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
  mod_ns <- sub(sprintf("%s$", ns.sep), "", namespace)

  # Prepend the module name to the label
  # Ex: `"mymod:x"`
  sprintf("%s:%s", mod_ns, label)
}
