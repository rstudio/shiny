#' Change the value of a text input on the client
#'
#' @template update-input
#' @inheritParams textInput
#'
#' @seealso [textInput()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   sliderInput("controller", "Controller", 0, 20, 10),
#'   textInput("inText", "Input text"),
#'   textInput("inText2", "Input text 2")
#' )
#'
#' server <- function(input, output, session) {
#'   observe({
#'     # We'll use the input$controller variable multiple times, so save it as x
#'     # for convenience.
#'     x <- input$controller
#'
#'     # This will change the value of input$inText, based on x
#'     updateTextInput(session, "inText", value = paste("New text", x))
#'
#'     # Can also set the label, this time for input$inText2
#'     updateTextInput(session, "inText2",
#'       label = paste("New label", x),
#'       value = paste("New text", x))
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @export
updateTextInput <- function(session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL, placeholder = NULL) {
  validate_session_object(session)

  message <- dropNulls(list(label=label %convert% as.character, value=value, placeholder=placeholder))
  session$sendInputMessage(inputId, message)
}

#' Change the value of a textarea input on the client
#'
#' @template update-input
#' @inheritParams updateTextInput
#'
#' @seealso [textAreaInput()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   sliderInput("controller", "Controller", 0, 20, 10),
#'   textAreaInput("inText", "Input textarea"),
#'   textAreaInput("inText2", "Input textarea 2")
#' )
#'
#' server <- function(input, output, session) {
#'   observe({
#'     # We'll use the input$controller variable multiple times, so save it as x
#'     # for convenience.
#'     x <- input$controller
#'
#'     # This will change the value of input$inText, based on x
#'     updateTextAreaInput(session, "inText", value = paste("New text", x))
#'
#'     # Can also set the label, this time for input$inText2
#'     updateTextAreaInput(session, "inText2",
#'       label = paste("New label", x),
#'       value = paste("New text", x))
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @export
updateTextAreaInput <- updateTextInput


#' Change the value of a checkbox input on the client
#'
#' @template update-input
#' @inheritParams checkboxInput
#'
#' @seealso [checkboxInput()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   sliderInput("controller", "Controller", 0, 1, 0, step = 1),
#'   checkboxInput("inCheckbox", "Input checkbox")
#' )
#'
#' server <- function(input, output, session) {
#'   observe({
#'     # TRUE if input$controller is odd, FALSE if even.
#'     x_even <- input$controller %% 2 == 1
#'
#'     updateCheckboxInput(session, "inCheckbox", value = x_even)
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @export
updateCheckboxInput <- function(session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL) {
  validate_session_object(session)

  message <- dropNulls(list(label=label %convert% as.character, value=value))
  session$sendInputMessage(inputId, message)
}


#' Change the label or icon of an action button on the client
#'
#' @template update-input
#' @param disabled If `TRUE`, the button will not be clickable; if `FALSE`, it
#'   will be.
#' @inheritParams actionButton
#'
#' @seealso [actionButton()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   actionButton("update", "Update other buttons and link"),
#'   br(),
#'   actionButton("goButton", "Go"),
#'   br(),
#'   actionButton("goButton2", "Go 2", icon = icon("area-chart")),
#'   br(),
#'   actionButton("goButton3", "Go 3"),
#'   br(),
#'   actionLink("goLink", "Go Link")
#' )
#'
#' server <- function(input, output, session) {
#'   observe({
#'     req(input$update)
#'
#'     # Updates goButton's label and icon
#'     updateActionButton(session, "goButton",
#'       label = "New label",
#'       icon = icon("calendar"))
#'
#'     # Leaves goButton2's label unchanged and
#'     # removes its icon
#'     updateActionButton(session, "goButton2",
#'       icon = character(0))
#'
#'     # Leaves goButton3's icon, if it exists,
#'     # unchanged and changes its label
#'     updateActionButton(session, "goButton3",
#'       label = "New label 3")
#'
#'     # Updates goLink's label and icon
#'     updateActionButton(session, "goLink",
#'       label = "New link label",
#'       icon = icon("link"))
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @rdname updateActionButton
#' @export
updateActionButton <- function(session = getDefaultReactiveDomain(), inputId, label = NULL, icon = NULL, disabled = NULL) {
  validate_session_object(session)

  if (!is.null(icon)) icon <- as.character(validateIcon(icon))
  message <- dropNulls(list(label=label %convert% as.character, icon=icon, disabled=disabled))
  session$sendInputMessage(inputId, message)
}
#' @rdname updateActionButton
#' @export
updateActionLink <- function(session = getDefaultReactiveDomain(), inputId, label = NULL, icon = NULL) {
  updateActionButton(session, inputId=inputId, label=label %convert% as.character, icon=icon)
}


#' Change the value of a date input on the client
#'
#' @template update-input
#' @inheritParams dateInput
#'
#' @seealso [dateInput()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   sliderInput("n", "Day of month", 1, 30, 10),
#'   dateInput("inDate", "Input date")
#' )
#'
#' server <- function(input, output, session) {
#'   observe({
#'     date <- as.Date(paste0("2013-04-", input$n))
#'     updateDateInput(session, "inDate",
#'       label = paste("Date label", input$n),
#'       value = date,
#'       min   = date - 3,
#'       max   = date + 3
#'     )
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @export
updateDateInput <- function(session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL,
                            min = NULL, max = NULL)
{
  validate_session_object(session)

  value <- dateYMD(value, "value")
  min   <- dateYMD(min, "min")
  max   <- dateYMD(max, "max")

  message <- dropNulls(list(label=label %convert% as.character, value=value, min=min, max=max))
  session$sendInputMessage(inputId, message)
}


#' Change the start and end values of a date range input on the client
#'
#' @template update-input
#' @inheritParams dateRangeInput
#'
#' @seealso [dateRangeInput()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   sliderInput("n", "Day of month", 1, 30, 10),
#'   dateRangeInput("inDateRange", "Input date range")
#' )
#'
#' server <- function(input, output, session) {
#'   observe({
#'     date <- as.Date(paste0("2013-04-", input$n))
#'
#'     updateDateRangeInput(session, "inDateRange",
#'       label = paste("Date range label", input$n),
#'       start = date - 1,
#'       end = date + 1,
#'       min = date - 5,
#'       max = date + 5
#'     )
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @export
updateDateRangeInput <- function(session = getDefaultReactiveDomain(), inputId, label = NULL,
                                 start = NULL, end = NULL, min = NULL,
                                 max = NULL)
{
  validate_session_object(session)

  start <- dateYMD(start, "start")
  end <- dateYMD(end, "end")
  min <- dateYMD(min, "min")
  max <- dateYMD(max, "max")

  message <- dropNulls(list(
    label = label %convert% as.character,
    value = dropNulls(list(start = start, end = end)),
    min = min,
    max = max
  ))

  session$sendInputMessage(inputId, message)
}

#' Change the selected tab on the client
#'
#' @param session The `session` object passed to function given to
#'   `shinyServer`. Default is `getDefaultReactiveDomain()`.
#' @param inputId The id of the `tabsetPanel`, `navlistPanel`,
#' or `navbarPage` object.
#' @inheritParams tabsetPanel
#'
#' @seealso [tabsetPanel()], [navlistPanel()],
#' [navbarPage()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(sidebarLayout(
#'   sidebarPanel(
#'     sliderInput("controller", "Controller", 1, 3, 1)
#'   ),
#'   mainPanel(
#'     tabsetPanel(id = "inTabset",
#'       tabPanel(title = "Panel 1", value = "panel1", "Panel 1 content"),
#'       tabPanel(title = "Panel 2", value = "panel2", "Panel 2 content"),
#'       tabPanel(title = "Panel 3", value = "panel3", "Panel 3 content")
#'     )
#'   )
#' ))
#'
#' server <- function(input, output, session) {
#'   observeEvent(input$controller, {
#'     updateTabsetPanel(session, "inTabset",
#'       selected = paste0("panel", input$controller)
#'     )
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @export
updateTabsetPanel <- function(session = getDefaultReactiveDomain(), inputId, selected = NULL) {
  validate_session_object(session)

  message <- dropNulls(list(value = selected))
  session$sendInputMessage(inputId, message)
}

#' @rdname updateTabsetPanel
#' @export
updateNavbarPage <- updateTabsetPanel

#' @rdname updateTabsetPanel
#' @export
updateNavlistPanel <- updateTabsetPanel

#' Change the value of a number input on the client
#'
#' @template update-input
#' @inheritParams numericInput
#'
#' @seealso [numericInput()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   sliderInput("controller", "Controller", 0, 20, 10),
#'   numericInput("inNumber", "Input number", 0),
#'   numericInput("inNumber2", "Input number 2", 0)
#' )
#'
#' server <- function(input, output, session) {
#'
#'   observeEvent(input$controller, {
#'     # We'll use the input$controller variable multiple times, so save it as x
#'     # for convenience.
#'     x <- input$controller
#'
#'     updateNumericInput(session, "inNumber", value = x)
#'
#'     updateNumericInput(session, "inNumber2",
#'       label = paste("Number label ", x),
#'       value = x, min = x-10, max = x+10, step = 5)
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @export
updateNumericInput <- function(session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL,
    min = NULL, max = NULL, step = NULL) {

  validate_session_object(session)

  message <- dropNulls(list(
    label = label %convert% as.character, value = formatNoSci(value),
    min = formatNoSci(min), max = formatNoSci(max), step = formatNoSci(step)
  ))
  session$sendInputMessage(inputId, message)
}

#' Update Slider Input Widget
#'
#' Change the value of a slider input on the client.
#'
#' @template update-input
#' @inheritParams sliderInput
#'
#' @seealso [sliderInput()]
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   shinyApp(
#'     ui = fluidPage(
#'       sidebarLayout(
#'         sidebarPanel(
#'           p("The first slider controls the second"),
#'           sliderInput("control", "Controller:", min=0, max=20, value=10,
#'                        step=1),
#'           sliderInput("receive", "Receiver:", min=0, max=20, value=10,
#'                        step=1)
#'         ),
#'         mainPanel()
#'       )
#'     ),
#'     server = function(input, output, session) {
#'       observe({
#'         val <- input$control
#'         # Control the value, min, max, and step.
#'         # Step size is 2 when input value is even; 1 when value is odd.
#'         updateSliderInput(session, "receive", value = val,
#'           min = floor(val/2), max = val+4, step = (val+1)%%2 + 1)
#'       })
#'     }
#'   )
#' }
#' @export
updateSliderInput <- function(session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL,
  min = NULL, max = NULL, step = NULL, timeFormat = NULL, timezone = NULL)
{
  validate_session_object(session)

  if (!is.null(value)) {
    if (!is.null(min) && !is.null(max)) {
      # Validate value/min/max together if all three are provided
      tryCatch(
        validate_slider_value(min, max, value, "updateSliderInput"),
        error = function(err) warning(conditionMessage(err), call. = FALSE)
      )
    } else if (length(value) <  1 || length(value) > 2 || any(is.na(value))) {
      # Otherwise ensure basic assumptions about value are met
      warning(
        "In updateSliderInput(): value must be a single value or a length-2 ",
        "vector and cannot contain NA values.",
        call. = FALSE
      )
    }
  }

  # If no min/max/value is provided, we won't know the
  # type, and this will return an empty string
  dataType <- getSliderType(min, max, value)

  if (is.null(timeFormat)) {
    timeFormat <- switch(dataType, date = "%F", datetime = "%F %T", number = NULL)
  }

  if (isTRUE(dataType %in% c("date", "datetime"))) {
    to_ms <- function(x) 1000 * as.numeric(as.POSIXct(x))
    if (!is.null(min))   min   <- to_ms(min)
    if (!is.null(max))   max   <- to_ms(max)
    if (!is.null(value)) value <- to_ms(value)
  }

  message <- dropNulls(list(
    label = label %convert% as.character,
    value = formatNoSci(value),
    min = formatNoSci(min),
    max = formatNoSci(max),
    step = formatNoSci(step),
    `data-type` = dataType,
    `time-format` = timeFormat,
    timezone = timezone
  ))
  session$sendInputMessage(inputId, message)
}


updateInputOptions <- function(session, inputId, label = NULL, choices = NULL,
                               selected = NULL, inline = FALSE, type = NULL,
                               choiceNames = NULL, choiceValues = NULL) {
  validate_session_object(session)

  if (is.null(type)) stop("Please specify the type ('checkbox' or 'radio')")

  args <- normalizeChoicesArgs(choices, choiceNames, choiceValues, mustExist = FALSE)

  if (!is.null(selected)) selected <- as.character(selected)

  options <- if (!is.null(args$choiceValues)) {
    format(tagList(
      generateOptions(session$ns(inputId), selected, inline, type,
        args$choiceNames, args$choiceValues)
    ))
  }

  message <- dropNulls(list(label = label %convert% as.character, options = options, value = selected))

  session$sendInputMessage(inputId, message)
}

#' Change the value of a checkbox group input on the client
#'
#' @template update-input
#' @inheritParams checkboxGroupInput
#'
#' @seealso [checkboxGroupInput()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   p("The first checkbox group controls the second"),
#'   checkboxGroupInput("inCheckboxGroup", "Input checkbox",
#'     c("Item A", "Item B", "Item C")),
#'   checkboxGroupInput("inCheckboxGroup2", "Input checkbox 2",
#'     c("Item A", "Item B", "Item C"))
#' )
#'
#' server <- function(input, output, session) {
#'   observe({
#'     x <- input$inCheckboxGroup
#'
#'     # Can use character(0) to remove all choices
#'     if (is.null(x))
#'       x <- character(0)
#'
#'     # Can also set the label and select items
#'     updateCheckboxGroupInput(session, "inCheckboxGroup2",
#'       label = paste("Checkboxgroup label", length(x)),
#'       choices = x,
#'       selected = x
#'     )
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @export
updateCheckboxGroupInput <- function(session = getDefaultReactiveDomain(), inputId, label = NULL,
  choices = NULL, selected = NULL, inline = FALSE,
  choiceNames = NULL, choiceValues = NULL)
{
  validate_session_object(session)

  updateInputOptions(session, inputId, label, choices, selected,
                     inline, "checkbox", choiceNames, choiceValues)
}


#' Change the value of a radio input on the client
#'
#' @template update-input
#' @inheritParams radioButtons
#'
#' @seealso [radioButtons()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   p("The first radio button group controls the second"),
#'   radioButtons("inRadioButtons", "Input radio buttons",
#'     c("Item A", "Item B", "Item C")),
#'   radioButtons("inRadioButtons2", "Input radio buttons 2",
#'     c("Item A", "Item B", "Item C"))
#' )
#'
#' server <- function(input, output, session) {
#'   observe({
#'     x <- input$inRadioButtons
#'
#'     # Can also set the label and select items
#'     updateRadioButtons(session, "inRadioButtons2",
#'       label = paste("radioButtons label", x),
#'       choices = x,
#'       selected = x
#'     )
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @export
updateRadioButtons <- function(session = getDefaultReactiveDomain(), inputId, label = NULL, choices = NULL,
                               selected = NULL, inline = FALSE,
                               choiceNames = NULL, choiceValues = NULL)
{
  validate_session_object(session)

  # you must select at least one radio button
  if (is.null(selected)) {
    if (!is.null(choices)) selected <- choices[[1]]
    else if (!is.null(choiceValues)) selected <- choiceValues[[1]]
  }
  updateInputOptions(session, inputId, label, choices, selected,
    inline, 'radio', choiceNames, choiceValues)
}


#' Change the value of a select input on the client
#'
#' @template update-input
#' @inheritParams selectInput
#'
#' @seealso [selectInput()] [varSelectInput()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   p("The checkbox group controls the select input"),
#'   checkboxGroupInput("inCheckboxGroup", "Input checkbox",
#'     c("Item A", "Item B", "Item C")),
#'   selectInput("inSelect", "Select input",
#'     c("Item A", "Item B", "Item C"))
#' )
#'
#' server <- function(input, output, session) {
#'   observe({
#'     x <- input$inCheckboxGroup
#'
#'     # Can use character(0) to remove all choices
#'     if (is.null(x))
#'       x <- character(0)
#'
#'     # Can also set the label and select items
#'     updateSelectInput(session, "inSelect",
#'       label = paste("Select input label", length(x)),
#'       choices = x,
#'       selected = tail(x, 1)
#'     )
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @export
updateSelectInput <- function(session = getDefaultReactiveDomain(), inputId, label = NULL, choices = NULL,
                              selected = NULL)
{
  validate_session_object(session)

  choices <- if (!is.null(choices)) choicesWithNames(choices)
  if (!is.null(selected)) selected <- as.character(selected)
  options <- if (!is.null(choices)) selectOptions(choices, selected, inputId, FALSE)
  message <- dropNulls(list(label = label %convert% as.character, options = options, value = selected))
  session$sendInputMessage(inputId, message)
}

#' @rdname updateSelectInput
#' @inheritParams selectizeInput
#' @param server whether to store `choices` on the server side, and load
#'   the select options dynamically on searching, instead of writing all
#'   `choices` into the page at once (i.e., only use the client-side
#'   version of \pkg{selectize.js})
#' @export
updateSelectizeInput <- function(session = getDefaultReactiveDomain(), inputId, label = NULL, choices = NULL,
                                 selected = NULL, options = list(),
                                 server = FALSE)
{
  validate_session_object(session)

  if (length(options)) {
    res <- checkAsIs(options)
    cfg <- tags$script(
      type = 'application/json',
      `data-for` = session$ns(inputId),
      `data-eval` = if (length(res$eval)) HTML(toJSON(res$eval)),
      HTML(toJSON(res$options))
    )
    session$sendInputMessage(inputId, list(config = as.character(cfg)))
  }
  if (!server) {
    return(updateSelectInput(session, inputId, label, choices, selected))
  }

  noOptGroup <- TRUE
  if (is.list(choices)) {
    # check if list is nested
    for (i in seq_along(choices)) {
      if (is.list(choices[[i]]) || length(choices[[i]]) > 1) {
        noOptGroup <- FALSE
        break()
      }
    }
  }
  # convert choices to a data frame so it returns [{label: , value: , optgroup: },...]
  choices <- if (is.data.frame(choices)) {
    # jcheng 2018/09/25: I don't think we ever said data frames were OK to pass
    # to updateSelectInput, but one of the example apps does this and at least
    # one user noticed when we broke it.
    # https://github.com/rstudio/shiny/issues/2172
    # https://github.com/rstudio/shiny/issues/2192
    as.data.frame(choices, stringsAsFactors = FALSE)
  } else if (is.atomic(choices) || noOptGroup) {
    # fast path for vectors and flat lists
    if (is.list(choices)) {
      choices <- unlist(choices)
    }
    if (is.null(names(choices))) {
      lab <- as.character(choices)
    } else {
      lab <- names(choices)
      # replace empty names like: choices = c(a = 1, 2)
      # in this case: names(choices) = c("a", "")
      # with replacement below choices will be: lab = c("a", "2")
      empty_names_indices <- lab == ""
      lab[empty_names_indices] <- as.character(choices[empty_names_indices])
    }
    data.frame(label = lab, value = choices, stringsAsFactors = FALSE)
  } else {
    # slow path for nested lists/optgroups
    list_names <- names(choices)
    if (is.null(list_names)) {
      list_names <- rep("", length(choices))
    }

    choice_list <- mapply(choices, list_names, FUN = function (choice, name) {
      group <- ""
      lab <- name
      if (lab == "") lab <- as.character(choice)

      if (is.list(choice) || length(choice) > 1) {
        group <- rep(name, length(choice))
        choice <- unlist(choice)

        if (is.null(names(choice))) {
          lab <- as.character(choice)
        } else {
          lab <- names(choice)
          # replace empty names like: choices = c(a = 1, 2)
          # in this case: names(choices) = c("a", "")
          # with replacement below choices will be: lab = c("a", "2")
          empty_names_indices <- lab == ""
          lab[empty_names_indices] <- as.character(choice[empty_names_indices])
        }
      }

      list(
        label = lab,
        value = as.character(choice),
        # The name "optgroup" is because this is the default field where
        # selectize will look for group IDs
        optgroup = group
      )
    }, SIMPLIFY = FALSE)


    extract_vector <- function(x, name) {
      vecs <- lapply(x, `[[`, name)
      do.call(c, vecs)
    }

    data.frame(
      label = extract_vector(choice_list, "label"),
      value = extract_vector(choice_list, "value"),
      optgroup = extract_vector(choice_list, "optgroup"),
      stringsAsFactors = FALSE, row.names = NULL
    )
  }

  value <- unname(selected)
  attr(choices, 'selected_value') <- value

  message <- dropNulls(list(
    label = label,
    value = value,
    url = session$registerDataObj(inputId, choices, selectizeJSON)
  ))
  session$sendInputMessage(inputId, message)
}
#' @rdname updateSelectInput
#' @inheritParams varSelectInput
#' @export
updateVarSelectInput <- function(session = getDefaultReactiveDomain(), inputId, label = NULL, data = NULL, selected = NULL) {
  validate_session_object(session)

  if (is.null(data)) {
    choices <- NULL
  } else {
    choices <- colnames(data)
  }

  updateSelectInput(
    session = session,
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected
  )
}
#' @rdname updateSelectInput
#' @export
updateVarSelectizeInput <- function(session = getDefaultReactiveDomain(), inputId, label = NULL,
                                    data = NULL, selected = NULL, options = list(), server = FALSE)
{
  validate_session_object(session)

  if (is.null(data)) {
    choices <- NULL
  } else {
    choices <- colnames(data)
  }
  updateSelectizeInput(
    session = session,
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    options = options,
    server = server
  )
}



selectizeJSON <- function(data, req) {
  query <- parseQueryString(req$QUERY_STRING)

  # extract the query variables, conjunction (and/or), search string, maximum options
  var <- c(safeFromJSON(query$field))

  # all keywords in lower-case, for case-insensitive matching
  key <- unique(strsplit(tolower(query$query), '\\s+')[[1]])

  if (identical(key, '')) key <- character(0)
  mop <- as.numeric(query$maxop)
  vfd <- query$value  # the value field name
  sel <- attr(data, 'selected_value', exact = TRUE)

  # start searching for keywords in all specified columns
  idx <- logical(nrow(data))
  if (length(key)) {
    for (v in var) {
      matches <- do.call(
        cbind,
        lapply(key, function(k) {
          grepl(k, tolower(as.character(data[[v]])), fixed = TRUE)
        })
      )
      # merge column matches using OR, and match multiple keywords in one column
      # using the conjunction setting (AND or OR)
      matches <- rowSums(matches)
      if (query$conju == 'and')
        idx <- idx | (matches == length(key))
      else
        idx <- idx | matches
    }
  }
  # only return the first n rows (n = maximum options in configuration)
  idx <- utils::head(if (length(key)) which(idx) else seq_along(idx), mop)
  # make sure the selected value is in the data
  if (length(sel)) {
    i <- stats::na.omit(match(sel, data[, vfd]))
    if (length(i)) idx <- sort(utils::head(unique(c(i, idx)), mop))
  }
  data <- data[idx, ]

  res <- toJSON(columnToRowData(data))
  httpResponse(200, 'application/json', enc2utf8(res))
}
