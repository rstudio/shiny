controlLabel <- function(controlName, label) {
  label %AND% tags$label(class = "control-label", `for` = controlName, label)
}

normalizeChoicesArgs <- function(choices, choicesNames, choicesValues) {
  # if-else to check that either choices OR (choicesNames + choicesValues)
  # were correctly provided
  if (is.null(choices)) {
    if (length(choicesNames) == 0 || length(choicesValues) == 0) {
      stop("Please specify a non-empty vector for `choices` (or,
           alternatively, for both `choicesNames` and `choicesValues`).")
    }
    if (length(choicesNames) != length(choicesValues)) {
      stop("`choicesNames` and `choicesValues` must have the same length.")
    }
    if (anyNamed(choicesNames) || anyNamed(choicesValues)) {
      stop("`choicesNames` and `choicesValues` must not be named.")
    }
  } else {
    if (!is.null(choicesNames) || !is.null(choicesValues)) {
      warning("Using `choices` argument; ignoring `choicesNames` and
               `choicesValues`.")
    }
    choices <- choicesWithNames(choices) # resolve names if not specified
    choicesNames <- names(choices)
    choicesValues <- unname(choices)
  }

  return(list(choicesNames = choicesNames, choicesValues = choicesValues))
}

# Before shiny 0.9, `selected` refers to names/labels of `choices`; now it
# refers to values. Below is a function for backward compatibility. It also
# coerces the value to `character`.
normalizeSelected <- function(selected, inputId, choicesNames, choicesValues) {
  # this line accomplishes two tings:
  #   - coerces selected to character
  #   - drops name, otherwise toJSON() keeps it too
  selected <- as.character(selected)

  # if you are using optgroups, you're using shiny > 0.10.0, and you should
  # already know that `selected` must be a value instead of a label
  if (needOptgroup(choicesValues)) return(selected)

  if (is.list(choicesNames)) choicesNames <- unlist(choicesNames)
  if (is.list(choicesValues)) choicesValues <- unlist(choicesValues)

  # when selected labels instead of values
  i <- (selected %in% choicesNames) & !(selected %in% choicesValues)
  if (any(i)) {
    warnFun <- if (all(i)) {
      # replace names with values
      selected <- choicesValues[[which(choicesNames == selected)]]
      warning
    } else stop  # stop when it is ambiguous (some labels == values)
    warnFun("'selected' must be the values instead of names of 'choices' ",
            "for the input '", inputId, "'")
  }
  selected
}

# generate options for radio buttons and checkbox groups (type = 'checkbox' or
# 'radio')
generateOptions <- function(inputId, selected, inline, type = 'checkbox',
                            choicesNames, choicesValues,
                            session = getDefaultReactiveDomain()) {
  # generate a list of <input type=? [checked] />
  options <- mapply(
    choicesValues, choicesNames,
    FUN = function(value, name) {
      inputTag <- tags$input(
        type = type, name = inputId, value = value
      )
      if (value %in% selected)
        inputTag$attribs$checked <- "checked"

      # in case, the options include UI code other than text
      # (arbitrary HTML using the tags() function or equivalent)
      pd <- processDeps(name, session)

      # If inline, there's no wrapper div, and the label needs a class like
      # checkbox-inline.
      if (inline) {
        tags$label(class = paste0(type, "-inline"), inputTag,
                   tags$span(pd$html, pd$dep))
      } else {
        tags$div(class = type, tags$label(inputTag,
                 tags$span(pd$html, pd$dep)))
      }
    },
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  )

  div(class = "shiny-options-group", options)
}


# Takes a vector or list, and adds names (same as the value) to any entries
# without names. Coerces all leaf nodes to `character`.
choicesWithNames <- function(choices) {
  # Take a vector or list, and convert to list. Also, if any children are
  # vectors with length > 1, convert those to list. If the list is unnamed,
  # convert it to a named list with blank names.
  listify <- function(obj) {
    # If a list/vector is unnamed, give it blank names
    makeNamed <- function(x) {
      if (is.null(names(x))) names(x) <- character(length(x))
      x
    }

    res <- lapply(obj, function(val) {
      if (is.list(val))
        listify(val)
      else if (length(val) == 1 && is.null(names(val)))
        as.character(val)
      else
        makeNamed(as.list(val))
    })

    makeNamed(res)
  }

  choices <- listify(choices)
  if (length(choices) == 0) return(choices)

  # Recurse into any subgroups
  choices <- mapply(choices, names(choices), FUN = function(choice, name) {
    if (!is.list(choice)) return(choice)
    if (name == "") stop('All sub-lists in "choices" must be named.')
    choicesWithNames(choice)
  }, SIMPLIFY = FALSE)

  # default missing names to choice values
  missing <- names(choices) == ""
  names(choices)[missing] <- as.character(choices)[missing]

  choices
}
