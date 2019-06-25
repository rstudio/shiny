#' Create a select list input control
#'
#' Create a select list that can be used to choose a single or multiple items
#' from a list of values.
#'
#' By default, `selectInput()` and `selectizeInput()` use the
#' JavaScript library \pkg{selectize.js}
#' (<https://github.com/selectize/selectize.js>) instead of the basic
#' select input element. To use the standard HTML select input element, use
#' `selectInput()` with `selectize=FALSE`.
#'
#' In selectize mode, if the first element in `choices` has a value of
#' `""`, its name will be treated as a placeholder prompt. For example:
#' `selectInput("letter", "Letter", c("Choose one" = "", LETTERS))`
#'
#' @inheritParams textInput
#' @param choices List of values to select from. If elements of the list are
#'   named, then that name --- rather than the value --- is displayed to the
#'   user. It's also possible to group related inputs by providing a named list
#'   whose elements are (either named or unnamed) lists or vectors. In this
#'   case, the outermost names will be used as the group labels (leveraging the
#'   `<optgroup>` HTML tag) for the elements in the respective sublist. See the
#'   example section for a small demo of this feature.
#' @param selected The initially selected value (or multiple values if
#'   `multiple = TRUE`). If not specified then defaults to the first value
#'   for single-select lists and no values for multiple select lists.
#' @param multiple Is selection of multiple items allowed?
#' @param selectize Whether to use \pkg{selectize.js} or not.
#' @param size Number of items to show in the selection box; a larger number
#'   will result in a taller box. Not compatible with `selectize=TRUE`.
#'   Normally, when `multiple=FALSE`, a select input will be a drop-down
#'   list, but when `size` is set, it will be a box instead.
#' @return A select list control that can be added to a UI definition.
#'
#' @family input elements
#' @seealso [updateSelectInput()] [varSelectInput()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' # basic example
#' shinyApp(
#'   ui = fluidPage(
#'     selectInput("variable", "Variable:",
#'                 c("Cylinders" = "cyl",
#'                   "Transmission" = "am",
#'                   "Gears" = "gear")),
#'     tableOutput("data")
#'   ),
#'   server = function(input, output) {
#'     output$data <- renderTable({
#'       mtcars[, c("mpg", input$variable), drop = FALSE]
#'     }, rownames = TRUE)
#'   }
#' )
#'
#' # demoing group support in the `choices` arg
#' shinyApp(
#'   ui = fluidPage(
#'     selectInput("state", "Choose a state:",
#'       list(`East Coast` = list("NY", "NJ", "CT"),
#'            `West Coast` = list("WA", "OR", "CA"),
#'            `Midwest` = list("MN", "WI", "IA"))
#'     ),
#'     textOutput("result")
#'   ),
#'   server = function(input, output) {
#'     output$result <- renderText({
#'       paste("You chose", input$state)
#'     })
#'   }
#' )
#' }
#' @export
selectInput <- function(inputId, label, choices, selected = NULL,
  multiple = FALSE, selectize = TRUE, width = NULL,
  size = NULL) {

  selected <- restoreInput(id = inputId, default = selected)

  # resolve names
  choices <- choicesWithNames(choices)

  # default value if it's not specified
  if (is.null(selected)) {
    if (!multiple) selected <- firstChoice(choices)
  } else selected <- as.character(selected)

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
    shinyInputLabel(inputId, label),
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
#' @param ... Arguments passed to `selectInput()`.
#' @param options A list of options. See the documentation of \pkg{selectize.js}
#'   for possible options (character option values inside [base::I()] will
#'   be treated as literal JavaScript code; see [renderDataTable()]
#'   for details).
#' @param width The width of the input, e.g. `'400px'`, or `'100%'`;
#'   see [validateCssUnit()].
#' @note The selectize input created from `selectizeInput()` allows
#'   deletion of the selected option even in a single select input, which will
#'   return an empty string as its value. This is the default behavior of
#'   \pkg{selectize.js}. However, the selectize input created from
#'   `selectInput(..., selectize = TRUE)` will ignore the empty string
#'   value when it is a single choice input and the empty string is not in the
#'   `choices` argument. This is to keep compatibility with
#'   `selectInput(..., selectize = FALSE)`.
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
      'jqueryui', '1.12.1', c(href = 'shared/jqueryui'),
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








#' Select variables from a data frame
#'
#' Create a select list that can be used to choose a single or multiple items
#' from the column names of a data frame.
#'
#' The resulting server `input` value will be returned as:
#' \itemize{
#'   \item a symbol if `multiple = FALSE`.  The `input` value should be
#'         used with rlang's [rlang::!!()]. For example,
#'         `ggplot2::aes(!!input$variable)`.
#'   \item a list of symbols if `multiple = TRUE`. The `input` value
#'         should be used with rlang's [rlang::!!!()] to expand
#'         the symbol list as individual arguments. For example,
#'         `dplyr::select(mtcars, !!!input$variabls)` which is
#'         equivalent to `dplyr::select(mtcars, !!input$variabls[[1]], !!input$variabls[[2]], ..., !!input$variabls[[length(input$variabls)]])`.
#' }
#'
#' By default, `varSelectInput()` and `selectizeInput()` use the
#' JavaScript library \pkg{selectize.js}
#' (<https://github.com/selectize/selectize.js>) to instead of the basic
#' select input element. To use the standard HTML select input element, use
#' `selectInput()` with `selectize=FALSE`.
#'
#' @inheritParams selectInput
#' @param data A data frame. Used to retrieve the column names as choices for a [selectInput()]
#' @return A variable select list control that can be added to a UI definition.
#'
#' @family input elements
#' @seealso [updateSelectInput()]
#' @examples
#'
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' library(ggplot2)
#'
#' # single selection
#' shinyApp(
#'   ui = fluidPage(
#'     varSelectInput("variable", "Variable:", mtcars),
#'     plotOutput("data")
#'   ),
#'   server = function(input, output) {
#'     output$data <- renderPlot({
#'       ggplot(mtcars, aes(!!input$variable)) + geom_histogram()
#'     })
#'   }
#' )
#'
#'
#' # multiple selections
#' \dontrun{
#' shinyApp(
#'  ui = fluidPage(
#'    varSelectInput("variables", "Variable:", mtcars, multiple = TRUE),
#'    tableOutput("data")
#'  ),
#'  server = function(input, output) {
#'    output$data <- renderTable({
#'       if (length(input$variables) == 0) return(mtcars)
#'       mtcars %>% dplyr::select(!!!input$variables)
#'    }, rownames = TRUE)
#'  }
#' )}
#'
#' }
#' @export
varSelectInput <- function(
  inputId, label, data, selected = NULL,
  multiple = FALSE, selectize = TRUE, width = NULL,
  size = NULL
) {
  # no place holders
  choices <- colnames(data)

  selectInputVal <- selectInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    multiple = multiple,
    selectize = selectize,
    width = width,
    size = size
  )

  # set the select tag class to be "symbol"
  selectClass <- selectInputVal$children[[2]]$children[[1]]$attribs$class
  if (is.null(selectClass)) {
    newClass <- "symbol"
  } else {
    newClass <- paste(selectClass, "symbol", sep = " ")
  }
  selectInputVal$children[[2]]$children[[1]]$attribs$class <- newClass

  selectInputVal
}



#' @rdname varSelectInput
#' @param ... Arguments passed to `varSelectInput()`.
#' @param options A list of options. See the documentation of \pkg{selectize.js}
#'   for possible options (character option values inside [base::I()] will
#'   be treated as literal JavaScript code; see [renderDataTable()]
#'   for details).
#' @param width The width of the input, e.g. `'400px'`, or `'100%'`;
#'   see [validateCssUnit()].
#' @note The variable selectize input created from `varSelectizeInput()` allows
#'   deletion of the selected option even in a single select input, which will
#'   return an empty string as its value. This is the default behavior of
#'   \pkg{selectize.js}. However, the selectize input created from
#'   `selectInput(..., selectize = TRUE)` will ignore the empty string
#'   value when it is a single choice input and the empty string is not in the
#'   `choices` argument. This is to keep compatibility with
#'   `selectInput(..., selectize = FALSE)`.
#' @export
varSelectizeInput <- function(inputId, ..., options = NULL, width = NULL) {
  selectizeIt(
    inputId,
    varSelectInput(inputId, ..., selectize = FALSE, width = width),
    options
  )
}
