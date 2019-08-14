shinyInputLabel <- function(inputId, label = NULL) {
  tags$label(
    label,
    class = "control-label",
    class = if (is.null(label)) "shiny-label-null",
    `for` = inputId
  )
}

# This function takes in either a list or vector for `choices` (and
# `choiceNames` and `choiceValues` are passed in as NULL) OR it takes
# in a list or vector for both `choiceNames` and `choiceValues` (and
# `choices` is passed as NULL) and returns a list of two elements:
#    - `choiceNames` is a vector or list that holds the options names
#      (each element can be arbitrary UI, or simple text)
#    - `choiceValues` is a vector or list that holds the options values
#       (each element must be simple text)
normalizeChoicesArgs <- function(choices, choiceNames, choiceValues,
  mustExist = TRUE) {
  # if-else to check that either choices OR (choiceNames + choiceValues)
  # were correctly provided
  if (is.null(choices)) {
    if (is.null(choiceNames) || is.null(choiceValues)) {
      if (mustExist) {
        stop("Please specify a non-empty vector for `choices` (or, ",
             "alternatively, for both `choiceNames` AND `choiceValues`).")
      } else {
        if (is.null(choiceNames) && is.null(choiceValues)) {
          # this is useful when we call this function from `updateInputOptions()`
          # in which case, all three `choices`, `choiceNames` and `choiceValues`
          # may legitimately be NULL
          return(list(choiceNames = NULL, choiceValues = NULL))
        } else {
          stop("One of `choiceNames` or `choiceValues` was set to ",
               "NULL, but either both or none should be NULL.")
        }
      }
    }
    if (length(choiceNames) != length(choiceValues)) {
      stop("`choiceNames` and `choiceValues` must have the same length.")
    }
    if (anyNamed(choiceNames) || anyNamed(choiceValues)) {
      stop("`choiceNames` and `choiceValues` must not be named.")
    }
  } else {
    if (!is.null(choiceNames) || !is.null(choiceValues)) {
      warning("Using `choices` argument; ignoring `choiceNames` and `choiceValues`.")
    }
    choices <- choicesWithNames(choices) # resolve names if not specified
    choiceNames <- names(choices)
    choiceValues <- unname(choices)
  }

  return(list(choiceNames = as.list(choiceNames),
              choiceValues = as.list(as.character(choiceValues))))
}

# generate options for radio buttons and checkbox groups (type = 'checkbox' or
# 'radio')
generateOptions <- function(inputId, selected, inline, type = 'checkbox',
                            choiceNames, choiceValues,
                            session = getDefaultReactiveDomain()) {
  # generate a list of <input type=? [checked] />
  options <- mapply(
    choiceValues, choiceNames,
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
                   tags$span(pd$html, pd$deps))
      } else {
        tags$div(class = type, tags$label(inputTag,
                 tags$span(pd$html, pd$deps)))
      }
    },
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  )

  div(class = "shiny-options-group", options)
}

# Walks tree and coerces any encountered atomic types to possibly-named
# character vectors.
coerceAtomicToCharacter <- function(tree) {
  if (is.atomic(tree)) {
    # Names are preserved explicitly here to preserve names on factors, which
    # are atomic
    stats::setNames(as.character(tree), names(tree))
  } else {
    lapply(tree, coerceAtomicToCharacter)
  }
}

# Walks tree and converts any unnamed lists or vectors of length 1 to a vector
# of length 1.
coerceLeavesToVectors <- function(tree) {
  if (length(tree) == 1 && is.null(names(tree))) {
    tree[[1]]
  } else {
    lapply(tree, coerceLeavesToVectors)
  }
}

# Walks tree and ensures all lists are named. Lists without names are given
# blank names.
nameBranches <- function(tree) {
  if(is.list(tree)) {
    asNamed(lapply(tree, nameBranches))
  } else {
    tree
  }
}

# Takes a vector or list, and adds names (same as the value) to any entries
# without names. Coerces all leaf nodes to `character`.
choicesWithNames <- function(choices) {

  choices <- coerceAtomicToCharacter(choices)
  choices <- coerceLeavesToVectors(choices)
  choices <- nameBranches(choices)
  choices <- asNamed(choices)

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
