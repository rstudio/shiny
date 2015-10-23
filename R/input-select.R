#' Create a select list input control
#'
#' Create a select list that can be used to choose a single or multiple items
#' from a list of values.
#'
#' By default, \code{selectInput()} and \code{selectizeInput()} use the
#' JavaScript library \pkg{selectize.js}
#' (\url{https://github.com/brianreavis/selectize.js}) to instead of the basic
#' select input element. To use the standard HTML select input element, use
#' \code{selectInput()} with \code{selectize=FALSE}.
#'
#' In selectize mode, if the first element in \code{choices} has a value of
#' \code{""}, its name will be treated as a placeholder prompt. For example:
#' \code{selectInput("letter", "Letter", c("Choose one" = "", LETTERS))}
#'
#' @inheritParams textInput
#' @param choices List of values to select from. If elements of the list are
#'   named then that name rather than the value is displayed to the user.
#' @param selected The initially selected value (or multiple values if
#'   \code{multiple = TRUE}). If not specified then defaults to the first value
#'   for single-select lists and no values for multiple select lists.
#' @param multiple Is selection of multiple items allowed?
#' @param selectize Whether to use \pkg{selectize.js} or not.
#' @param size Number of items to show in the selection box; a larger number
#'   will result in a taller box. Not compatible with \code{selectize=TRUE}.
#'   Normally, when \code{multiple=FALSE}, a select input will be a drop-down
#'   list, but when \code{size} is set, it will be a box instead.
#' @return A select list control that can be added to a UI definition.
#'
#' @family input elements
#' @seealso \code{\link{updateSelectInput}}
#'
#' @examples
#' selectInput("variable", "Variable:",
#'             c("Cylinders" = "cyl",
#'               "Transmission" = "am",
#'               "Gears" = "gear"))
#' @export
selectInput <- function(inputId, label, choices, selected = NULL,
                        multiple = FALSE, selectize = TRUE, width = NULL,
                        size = NULL) {
  # resolve names
  choices <- choicesWithNames(choices)

  # default value if it's not specified
  if (is.null(selected)) {
    if (!multiple) selected <- firstChoice(choices)
  } else selected <- validateSelected(selected, choices, inputId)

  if (!is.null(size) && selectize) {
    stop("'size' argument is incompatible with 'selectize=TRUE'.")
  }

  # create select tag and add options
  selectTag <- tags$select(
    id = inputId,
    class = if (!selectize) "form-control",
    size = size,
    selectOptions(choices, selected)
  )
  if (multiple)
    selectTag$attribs$multiple <- "multiple"

  # return label and select tag
  res <- div(
    class = "form-group shiny-input-container",
    style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
    controlLabel(inputId, label),
    div(selectTag)
  )

  if (!selectize) return(res)

  selectizeIt(inputId, res, NULL, nonempty = !multiple && !("" %in% choices))
}

firstChoice <- function(choices) {
  if (length(choices) == 0L) return()
  choice <- choices[[1]]
  if (is.list(choice)) firstChoice(choice) else choice
}

# Create tags for each of the options; use <optgroup> if necessary.
# This returns a HTML string instead of tags, because of the 'selected'
# attribute.
selectOptions <- function(choices, selected = NULL) {
  html <- mapply(choices, names(choices), FUN = function(choice, label) {
    if (is.list(choice)) {
      # If sub-list, create an optgroup and recurse into the sublist
      sprintf(
        '<optgroup label="%s">\n%s\n</optgroup>',
        htmlEscape(label, TRUE),
        selectOptions(choice, selected)
      )

    } else {
      # If single item, just return option string
      sprintf(
        '<option value="%s"%s>%s</option>',
        htmlEscape(choice, TRUE),
        if (choice %in% selected) ' selected' else '',
        htmlEscape(label)
      )
    }
  })

  HTML(paste(html, collapse = '\n'))
}

# need <optgroup> when choices contains sub-lists
needOptgroup <- function(choices) {
  any(vapply(choices, is.list, logical(1)))
}

#' @rdname selectInput
#' @param ... Arguments passed to \code{selectInput()}.
#' @param options A list of options. See the documentation of \pkg{selectize.js}
#'   for possible options (character option values inside \code{\link{I}()} will
#'   be treated as literal JavaScript code; see \code{\link{renderDataTable}()}
#'   for details).
#' @param width The width of the input, e.g. \code{'400px'}, or \code{'100\%'};
#'   see \code{\link{validateCssUnit}}.
#' @note The selectize input created from \code{selectizeInput()} allows
#'   deletion of the selected option even in a single select input, which will
#'   return an empty string as its value. This is the default behavior of
#'   \pkg{selectize.js}. However, the selectize input created from
#'   \code{selectInput(..., selectize = TRUE)} will ignore the empty string
#'   value when it is a single choice input and the empty string is not in the
#'   \code{choices} argument. This is to keep compatibility with
#'   \code{selectInput(..., selectize = FALSE)}.
#' @export
selectizeInput <- function(inputId, ..., options = NULL, width = NULL) {
  selectizeIt(
    inputId,
    selectInput(inputId, ..., selectize = FALSE, width = width),
    options
  )
}

# given a select input and its id, selectize it
selectizeIt <- function(inputId, select, options, nonempty = FALSE) {
  res <- checkAsIs(options)

  selectizeDep <- htmlDependency(
    "selectize", "0.11.2", c(href = "shared/selectize"),
    stylesheet = "css/selectize.bootstrap3.css",
    head = format(tagList(
      HTML('<!--[if lt IE 9]>'),
      tags$script(src = 'shared/selectize/js/es5-shim.min.js'),
      HTML('<![endif]-->'),
      tags$script(src = 'shared/selectize/js/selectize.min.js')
    ))
  )

  if ('drag_drop' %in% options$plugins) {
    selectizeDep <- list(selectizeDep, htmlDependency(
      'jqueryui', '1.11.4', c(href = 'shared/jqueryui'),
      script = 'jquery-ui.min.js'
    ))
  }

  # Insert script on same level as <select> tag
  select$children[[2]] <- tagAppendChild(
    select$children[[2]],
    tags$script(
      type = 'application/json',
      `data-for` = inputId, `data-nonempty` = if (nonempty) '',
      `data-eval` = if (length(res$eval)) HTML(toJSON(res$eval)),
      if (length(res$options)) HTML(toJSON(res$options)) else '{}'
    )
  )

  attachDependencies(select, selectizeDep)
}
