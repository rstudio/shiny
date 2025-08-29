# observe mymod:<anonymous>
# observe <anonymous>
# observe mylabel (edited)

# -- Reactives --------------------------------------------------------------

# reactive MOD:LABEL
# reactive x
# reactive mymod:x
ospan_label_reactive <- function(x, ..., domain) {
  label <- attr(x, "observable", exact = TRUE)[[".label"]]

    sprintf(
      "reactive %s",
      otel_label_module_prefix(label, domain = domain)
    )
}

# output MOD:LABEL
# output txt
# output mymod:txt
ospan_label_render_function <- function(..., domain) {
  sprintf(
    "output %s",
    otel_label_module_prefix(
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

  # By default, observe() sets the label to `observe(CODE)`
  # This label is too big and inconsistent.
  # Replace it with `<anonymous>`
  if (isDefaultLabel(span_label)) {
    span_label <- "<anonymous>"
  }

  sprintf(
    "observe %s",
    otel_label_module_prefix(span_label, domain = domain)
  )
}

# -- Set reactive value(s) ----------------------------------------------------

# [MOD] Set reactiveVal LABEL
# Set reactiveVal x
# Set reactiveVal mymod:x
otel_label_set_reactive_val <- function(x, ..., domain) {
  label <- attr(x, "observable", exact = TRUE)[[".label"]]

  sprintf(
    "Set reactiveVal %s",
    otel_label_module_prefix(label, domain = domain)
  )
}

# Set reactiveValues MOD:LABEL$KEY
# Set reactiveValues x$key
# Set reactiveValues mymod:x$key
otel_label_set_reactive_values <- function(x, key, ..., domain) {
  label <- attr(x, "observable", exact = TRUE)[[".label"]]

  sprintf(
    "Set reactiveValues %s",
    otel_label_module_prefix(label, domain = domain)
  )
}




# -- Helpers --------------------------------------------------------------
# Prefix:
# "" -> ""
# "my-nested-mod-" -> "my-nested-mod"
otel_label_module_prefix <- function(label, ..., domain) {
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
