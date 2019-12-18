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

# True when a choice list item represents a group of related inputs.
isGroup <- function(choice) {
  is.list(choice) ||
    !is.null(names(choice)) ||
    length(choice) > 1 ||
    length(choice) == 0
}

# True when choices is a list and contains at least one group of related inputs.
hasGroups <- function(choices) {
  is.list(choices) && any(vapply(choices, isGroup, logical(1)))
}

# Assigns empty names to x if it's unnamed, and then fills any empty names with
# the corresponding value coerced to a character(1).
setDefaultNames <- function(x) {
  x <- asNamed(x)
  emptyNames <- names(x) == ""
  names(x)[emptyNames] <- as.character(x)[emptyNames]
  x
}

# Makes a character vector out of x in a way that preserves names.
asCharacter <- function(x) {
  stats::setNames(as.character(x), names(x))
}

# Processes a "flat" set of choices, or a collection of choices not containing
# any named groups. choices should be a list without any list children, or an
# atomic vector. choices may be named or unnamed. Any empty names are replaced
# with the corresponding value coerced to a character.
processFlatChoices <- function(choices) {
  choices <- setDefaultNames(asCharacter(choices))
  as.list(choices)
}

# Processes a "nested" set of choices, or a collection of choices that contains
# one or more named groups of related choices and zero or more "flat" choices.
# choices should be a named list, and any choice group must have a non-empty
# name. Empty names of remaining "flat" choices are replaced with that choice's
# value coerced to a character.
processGroupedChoices <- function(choices) {
  # We assert choices is a list, since only a list may contain a group.
  stopifnot(is.list(choices))
  # The list might be unnamed by this point. We add default names of "" so that
  # names(choices) is not zero-length and mapply can work. Within mapply, we
  # error if any group's name is ""
  choices <- asNamed(choices)
  choices <- mapply(function(name, choice) {
    choiceIsGroup <- isGroup(choice)
    if (choiceIsGroup && name == "") {
      # If the choice is a group, and if its name is empty, produce an error. We
      # error here because the composite nature of the choice prevents us from
      # meaningfully automatically naming it. Note that while not documented,
      # groups are not necessarily lists (aka generic vectors) but can also be
      # any named atomic vector, or any atomic vector of length > 1.
      stop('All sub-lists in "choices" must be named.')
    } else if (choiceIsGroup) {
      # The choice is a group, but it is named. Process it using the same
      # function we use for "top level" choices.
      processFlatChoices(choice)
    } else {
      # The choice was not named and is not a group; it is a "leaf".
      as.character(choice)
    }
  }, names(choices), choices, SIMPLIFY = FALSE)
  # By this point, any leaves in the choices list might still have empty names,
  # so we're sure to automatically name them.
  setDefaultNames(choices)
}

# Takes a vector/list/factor, and adds names (same as the value) to any entries
# without names. Coerces all leaf nodes to `character`.
choicesWithNames <- function(choices) {
  if (hasGroups(choices)) {
    processGroupedChoices(choices)
  } else {
    processFlatChoices(choices)
  }
}
