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

# This pass accepts a compound structure and returns a "normalized" structure
# that simplifies subsequent passes.
#
# Input: A tree consisting of named and unnamed lists and atomic vectors.
#
# Output: A tree consisting of possibly-named lists and unnamed character(1)
# vectors. Lists are branches and character(1) vectors are leaves.
passNormalizeTree <- function(tree, root = TRUE) {
  if (is.atomic(tree)) {
    # Names are copied in this manner to preserve names from factors.
    chr <- stats::setNames(as.character(tree), names(tree))
    if (is.null(names(chr)) && length(chr) == 1 && !root) {
      chr
    } else {
      as.list(chr)
    }
  } else {
    lapply(tree, passNormalizeTree, FALSE)
  }
}

# This pass applies to branches containing unnamed leaves. Leaves are named with
# a default based on their value.
#
# If one branch contains another, the child branch is given the name "" in the
# parent branch.
#
# Input: A tree consisting of possibly-named lists and unnamed character(1)
# vectors.
#
# Output: A tree consisting of named lists and unnamed character(1) vectors.
passNameLeaves <- function(tree, errorMsg) {
  if (is.list(tree)) {
    # asNamed preserves existing names, but they still might be empty
    tree <- asNamed(tree)

    # Here we compute the set of children that are both atomic (character
    # vectors) and have empty names, because those are the only ones we can
    # meaningfully automatically name. Then, we set the names of those children
    # to their values.
    toRename <- sapply(tree, is.atomic) & (names(tree) == "")
    names(tree)[toRename] <- tree[toRename]

    lapply(tree, passNameLeaves)
  } else {
    tree
  }
}

# Computes the depth of a tree given by the isBranch and children functions.
# - isBranch() should return TRUE if the given tree is a branch and FALSE
#   otherwise
# - children() should return a branch's children as a list.
getDepth <- function(tree, isBranch = is.list, children = identity, depth = 0) {
  if (isBranch(tree)) {
    max(sapply(children(tree), getDepth, isBranch, children, depth + 1))
  } else {
    depth
  }
}

# Returns TRUE when every item in the list `branch` is named and no name is "",
# and FALSE otherwise.
branchHasNames <- function(branch) {
  stopifnot(is.list(branch))
  ns <- names(branch)
  !is.null(ns) && !length(ns[ns == ""])
}

# Takes a vector or list, and adds names (same as the value) to any entries
# without names. Coerces all leaf nodes to `character`.
choicesWithNames <- function(choices) {

  if (length(choices) == 0) return(choices)

  choices <- passNormalizeTree(choices)
  choices <- passNameLeaves(choices)

  # Only choices of depth 1 and 2 are currently meaningful in Shiny. Earlier versions of this
  # code did not produce an error if the choices tree was deeper than 2, and so we don't here.
  #
  # - radioButtons() and checkboxGroupInput() use a depth=1 tree via normalizeChoicesArgs()
  # - selectInput() and selectizeInput() use either depth=1 or depth=2 trees. depth=2 trees
  #   are used for "grouped" inputs.
  if (getDepth(choices) >= 2 && !branchHasNames(choices)) {
    stop('All sub-lists in "choices" must be named.')
  }

  choices
}
