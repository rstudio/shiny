# -- Reactives --------------------------------------------------------------

# [MOD] reactive: LABEL
# reactive: x
# [mymod] reactive: x
ospan_label_reactive <- function(x, ..., domain) {
  label <- attr(x, "observable", exact = TRUE)[[".label"]]

  ospan_label_module_prefix(
    sprintf("reactive: %s", label),
    domain = domain
  )
}

# [MOD] output: LABEL
# output: txt
# [mymod] output: txt
ospan_label_render_function <- function(..., domain) {
  ospan_label_module_prefix(
    sprintf(
      "output$%s",
      getCurrentOutputInfo(session = domain)$name
    ),
    domain = domain
  )
}

# [MOD] observe: LABEL
# observe: customlabel
# [mymod] observe: customlabel
# [mymod] observe: CODE...[truncated; Please set `observe(label=)`]
ospan_label_observer <- function(x, ..., domain) {
  span_label <- x$.label

  # By default, observe() sets the label to `observe(CODE)`
  # Extract the code and provide a hint on how to label
  if (isDefaultLabel(span_label)) {
    # Is default `observe(CODE)` label
    # Turn into: `"[mymod] observe: CODE...[truncated; Please set `observe(label=)`]"`
    span_label <- sub("observe\\((.*)\\)", "\\1", span_label)
    if (nchar(span_label) > 25) {
      span_label <- sprintf(
        "%s... [truncated; Please set `observe(label=)`]",
        substr(span_label, 1, 25)
      )
    }
  }

  ospan_label_module_prefix(
    sprintf("observe: %s", span_label),
    domain = domain
  )
}

# -- Set reactive value(s) ----------------------------------------------------

# [MOD] Set reactiveVal: LABEL
# Set reactiveVal: x
# [mymod] Set reactiveVal: x
ospan_label_set_reactive_val <- function(x, ..., domain) {
  label <- attr(x, "observable", exact = TRUE)[[".label"]]

  ospan_label_module_prefix(
    sprintf("Set reactiveVal: %s", label),
    domain = domain
  )
}

# [MOD] Set reactiveValues: LABEL$KEY
# Set reactiveValues: x$key
# [mymod] Set reactiveValues: x$key
ospan_label_set_reactive_values <- function(x, key, ..., domain) {
  label <- attr(x, "observable", exact = TRUE)[[".label"]]

  ospan_label_module_prefix(
    sprintf("Set reactiveValues: %s", label),
    domain = domain
  )
}




# -- Helpers --------------------------------------------------------------
# Prefix:
# "" -> ""
# "my-nested-mod-" -> "my-nested-mod"
ospan_label_module_prefix <- function(label, ..., domain) {
  if (is.null(domain)) {
    return(label)
  }

  namespace <- domain$ns("")

  if (!nzchar(namespace)) {
    return(label)
  }
  mod_ns <- sub(sprintf("%s$", ns.sep), "", namespace)

  # Prepend the module name to the label
  # Ex: `"[mymod] reactive: x"`
  sprintf("[%s] %s", mod_ns, label)
}
