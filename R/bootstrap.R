#' @include utils.R
NULL

#' Create a Bootstrap page
#'
#' Create a Shiny UI page that loads the CSS and JavaScript for
#' \href{http://getbootstrap.com/2.3.2/}{Bootstrap}, and has no content in the
#' page body (other than what you provide).
#'
#' This function is primarily intended for users who are proficient in
#' HTML/CSS, and know how to lay out pages in Bootstrap. Most applications
#' should use \code{\link{fluidPage}} along with layout functions like
#' \code{\link{fluidRow}} and \code{\link{sidebarLayout}}.
#'
#' @param ... The contents of the document body.
#' @param title The browser window title (defaults to the host URL of the page)
#' @param responsive \code{TRUE} to use responsive layout (automatically adapt
#'   and resize page elements based on the size of the viewing device)
#' @param theme Alternative Bootstrap stylesheet (normally a css file within the
#'   www directory, e.g. \code{www/bootstrap.css})
#'
#' @return A UI defintion that can be passed to the \link{shinyUI} function.
#'
#' @note The \code{basicPage} function is deprecated, you should use the
#'   \code{\link{fluidPage}} function instead.
#'
#' @seealso \code{\link{fluidPage}}, \code{\link{fixedPage}}
#'
#' @export
bootstrapPage <- function(..., title = NULL, responsive = TRUE, theme = NULL) {

  # required head tags for boostrap
  importBootstrap <- function(min = TRUE) {

    ext <- function(ext) {
      ifelse(min, paste(".min", ext, sep=""), ext)
    }
    cssExt <- ext(".css")
    jsExt = ext(".js")
    bs <- c(
      href = "shared/bootstrap",
      file = system.file("www/shared/bootstrap", package = "shiny")
    )

    list(
      htmlDependency("bootstrap", "2.3.2", bs,
        script = sprintf("js/bootstrap%s", jsExt),
        stylesheet = if (is.null(theme))
          sprintf("css/bootstrap%s", cssExt)
      ),
      if (responsive) {
        htmlDependency("bootstrap-responsive", "2.3.2", bs,
          stylesheet = sprintf("css/bootstrap-responsive%s", cssExt),
          meta = list(viewport = "width=device-width, initial-scale=1.0")
        )
      }
    )
  }

  attachDependencies(
    tagList(
      if (!is.null(title)) tags$head(tags$title(title)),
      if (!is.null(theme)) {
        tags$head(tags$link(rel="stylesheet", type="text/css", href = theme))
      },

      # remainder of tags passed to the function
      list(...)
    ),
    importBootstrap()
  )
}

#' @rdname bootstrapPage
#' @export
basicPage <- function(...) {
  bootstrapPage(div(class="container-fluid", list(...)))
}

#' Create a page with a sidebar
#'
#' Create a Shiny UI that contains a header with the application title, a
#' sidebar for input controls, and a main area for output.
#'
#' @param headerPanel The \link{headerPanel} with the application title
#' @param sidebarPanel The \link{sidebarPanel} containing input controls
#' @param mainPanel The \link{mainPanel} containing outputs

#' @return A UI defintion that can be passed to the \link{shinyUI} function
#'
#' @note This function is deprecated. You should use \code{\link{fluidPage}}
#' along with \code{\link{sidebarLayout}} to implement a page with a sidebar.
#'
#' @examples
#' # Define UI
#' shinyUI(pageWithSidebar(
#'
#'   # Application title
#'   headerPanel("Hello Shiny!"),
#'
#'   # Sidebar with a slider input
#'   sidebarPanel(
#'     sliderInput("obs",
#'                 "Number of observations:",
#'                 min = 0,
#'                 max = 1000,
#'                 value = 500)
#'   ),
#'
#'   # Show a plot of the generated distribution
#'   mainPanel(
#'     plotOutput("distPlot")
#'   )
#' ))
#'
#' @export
pageWithSidebar <- function(headerPanel,
                            sidebarPanel,
                            mainPanel) {

  bootstrapPage(
    # basic application container divs
    div(
      class="container-fluid",
      div(class="row-fluid",
          headerPanel
      ),
      div(class="row-fluid",
          sidebarPanel,
          mainPanel
      )
    )
  )
}

#' Create a page with a top level navigation bar
#'
#' Create a page that contains a top level navigation bar that can be used to
#' toggle a set of \code{\link{tabPanel}} elements.
#'
#' @param title The title to display in the navbar
#' @param ... \code{\link{tabPanel}} elements to include in the page
#' @param id If provided, you can use \code{input$}\emph{\code{id}} in your
#'   server logic to determine which of the current tabs is active. The value
#'   will correspond to the \code{value} argument that is passed to
#'   \code{\link{tabPanel}}.
#' @param header Tag of list of tags to display as a common header above all
#'   tabPanels.
#' @param footer Tag or list of tags to display as a common footer below all
#'   tabPanels
#' @param inverse \code{TRUE} to use a dark background and light text for the
#'   navigation bar
#' @param collapsable \code{TRUE} to automatically collapse the navigation
#'   elements into a menu when the width of the browser is less than 940 pixels
#'   (useful for viewing on smaller touchscreen device)
#' @param fluid \code{TRUE} to use a fluid layout. \code{FALSE} to use a fixed
#'   layout.
#' @param responsive \code{TRUE} to use responsive layout (automatically adapt
#'   and resize page elements based on the size of the viewing device)
#' @param theme Alternative Bootstrap stylesheet (normally a css file within the
#'   www directory). For example, to use the theme located at
#'   \code{www/bootstrap.css} you would use \code{theme = "bootstrap.css"}.
#' @param windowTitle The title that should be displayed by the browser window.
#'   Useful if \code{title} is not a string.
#' @param icon Optional icon to appear on a \code{navbarMenu} tab.
#'
#' @return A UI defintion that can be passed to the \link{shinyUI} function.
#'
#' @details The \code{navbarMenu} function can be used to create an embedded
#'   menu within the navbar that in turns includes additional tabPanels (see
#'   example below).
#'
#' @seealso \code{\link{tabPanel}}, \code{\link{tabsetPanel}}
#'
#' @examples
#' shinyUI(navbarPage("App Title",
#'   tabPanel("Plot"),
#'   tabPanel("Summary"),
#'   tabPanel("Table")
#' ))
#'
#' shinyUI(navbarPage("App Title",
#'   tabPanel("Plot"),
#'   navbarMenu("More",
#'     tabPanel("Summary"),
#'     tabPanel("Table")
#'   )
#' ))
#' @export
navbarPage <- function(title,
                       ...,
                       id = NULL,
                       header = NULL,
                       footer = NULL,
                       inverse = FALSE,
                       collapsable = FALSE,
                       fluid = TRUE,
                       responsive = TRUE,
                       theme = NULL,
                       windowTitle = title) {

  # alias title so we can avoid conflicts w/ title in withTags
  pageTitle <- title

  # navbar class based on options
  navbarClass <- "navbar navbar-static-top"
  if (inverse)
    navbarClass <- paste(navbarClass, "navbar-inverse")

  # build the tabset
  tabs <- list(...)
  tabset <- buildTabset(tabs, "nav", NULL, id)

  # built the container div dynamically to support optional collapsability
  if (collapsable) {
    navId <- paste("navbar-", p_randomInt(1000, 10000), sep="")
    containerDiv <- div(class="container",
                        tags$button(type="button",
                                    class="btn btn-navbar",
                                    `data-toggle`="collapse",
                                    `data-target`=".nav-collapse",
                          span(class="icon-bar"),
                          span(class="icon-bar"),
                          span(class="icon-bar")
                        ),
                        span(class="brand pull-left", pageTitle),
                        div(class="nav-collapse collapse",
                            id=navId,
                            tabset$navList),
                            tags$script(paste(
                              "$('#", navId, " a:not(.dropdown-toggle)').click(function (e) {
                                  e.preventDefault();
                                  $(this).tab('show');
                                  if ($('.navbar .btn-navbar').is(':visible'))
                                    $('.navbar .btn-navbar').click();
                               });", sep="")))
  } else {
    containerDiv <- div(class="container",
                        span(class="brand pull-left", pageTitle),
                        tabset$navList)
  }

  # create a default header if necessary
  if (length(header) == 0)
    header <- HTML("&nbsp;")

  # function to return plain or fluid class name
  className <- function(name) {
    if (fluid)
      paste(name, "-fluid", sep="")
    else
      name
  }

  # build the main tab content div
  contentDiv <- div(class=className("container"))
  if (!is.null(header))
    contentDiv <- tagAppendChild(contentDiv,div(class=className("row"), header))
  contentDiv <- tagAppendChild(contentDiv, tabset$content)
  if (!is.null(footer))
    contentDiv <- tagAppendChild(contentDiv,div(class=className("row"), footer))

  # build the page
  bootstrapPage(
    title = windowTitle,
    responsive = responsive,
    theme = theme,
    div(class=navbarClass,
      div(class="navbar-inner", containerDiv)
    ),
    contentDiv
  )
}

#' @rdname navbarPage
#' @export
navbarMenu <- function(title, ..., icon = NULL) {
  structure(list(title = title,
                 tabs = list(...),
                 iconClass = iconClass(icon)),
            class = "shiny.navbarmenu")
}

#' Create a header panel
#'
#' Create a header panel containing an application title.
#'
#' @param title An application title to display
#' @param windowTitle The title that should be displayed by the browser window.
#'   Useful if \code{title} is not a string.
#' @return A headerPanel that can be passed to \link{pageWithSidebar}
#'
#' @examples
#' headerPanel("Hello Shiny!")
#' @export
headerPanel <- function(title, windowTitle=title) {
  tagList(
    tags$head(tags$title(windowTitle)),
    div(class="span12", style="padding: 10px 0px;",
      h1(title)
    )
  )
}

#' Create a well panel
#'
#' Creates a panel with a slightly inset border and grey background. Equivalent
#' to Bootstrap's \code{well} CSS class.
#'
#' @param ... UI elements to include inside the panel.
#' @return The newly created panel.
#'
#' @export
wellPanel <- function(...) {
  div(class="well", ...)
}

#' Create a sidebar panel
#'
#' Create a sidebar panel containing input controls that can in turn be passed
#' to \code{\link{sidebarLayout}}.
#'
#' @param ... UI elements to include on the sidebar
#' @param width The width of the sidebar. For fluid layouts this is out of 12
#'   total units; for fixed layouts it is out of whatever the width of the
#'   sidebar's parent column is.
#' @return A sidebar that can be passed to \code{\link{sidebarLayout}}
#'
#' @examples
#' # Sidebar with controls to select a dataset and specify
#' # the number of observations to view
#' sidebarPanel(
#'   selectInput("dataset", "Choose a dataset:",
#'               choices = c("rock", "pressure", "cars")),
#'
#'   numericInput("obs", "Observations:", 10)
#' )
#' @export
sidebarPanel <- function(..., width = 4) {
  div(class=paste0("span", width),
    tags$form(class="well",
      ...
    )
  )
}

#' Create a main panel
#'
#' Create a main panel containing output elements that can in turn be passed to
#' \code{\link{sidebarLayout}}.
#'
#' @param ... Output elements to include in the main panel
#' @param width The width of the main panel. For fluid layouts this is out of 12
#'   total units; for fixed layouts it is out of whatever the width of the main
#'   panel's parent column is.
#' @return A main panel that can be passed to \code{\link{sidebarLayout}}.
#'
#' @examples
#' # Show the caption and plot of the requested variable against mpg
#' mainPanel(
#'    h3(textOutput("caption")),
#'    plotOutput("mpgPlot")
#' )
#' @export
mainPanel <- function(..., width = 8) {
  div(class=paste0("span", width),
    ...
  )
}

#' Conditional Panel
#'
#' Creates a panel that is visible or not, depending on the value of a
#' JavaScript expression. The JS expression is evaluated once at startup and
#' whenever Shiny detects a relevant change in input/output.
#'
#' In the JS expression, you can refer to \code{input} and \code{output}
#' JavaScript objects that contain the current values of input and output. For
#' example, if you have an input with an id of \code{foo}, then you can use
#' \code{input.foo} to read its value. (Be sure not to modify the input/output
#' objects, as this may cause unpredictable behavior.)
#'
#' @param condition A JavaScript expression that will be evaluated repeatedly to
#'   determine whether the panel should be displayed.
#' @param ... Elements to include in the panel.
#'
#' @note You are not recommended to use special JavaScript characters such as a
#'   period \code{.} in the input id's, but if you do use them anyway, for
#'   example, \code{inputId = "foo.bar"}, you will have to use
#'   \code{input["foo.bar"]} instead of \code{input.foo.bar} to read the input
#'   value.
#' @examples
#' sidebarPanel(
#'    selectInput(
#'       "plotType", "Plot Type",
#'       c(Scatter = "scatter",
#'         Histogram = "hist")),
#'
#'    # Only show this panel if the plot type is a histogram
#'    conditionalPanel(
#'       condition = "input.plotType == 'hist'",
#'       selectInput(
#'          "breaks", "Breaks",
#'          c("Sturges",
#'            "Scott",
#'            "Freedman-Diaconis",
#'            "[Custom]" = "custom")),
#'
#'       # Only show this panel if Custom is selected
#'       conditionalPanel(
#'          condition = "input.breaks == 'custom'",
#'          sliderInput("breakCount", "Break Count", min=1, max=1000, value=10)
#'       )
#'    )
#' )
#'
#' @export
conditionalPanel <- function(condition, ...) {
  div('data-display-if'=condition, ...)
}

#' Create a text input control
#'
#' Create an input control for entry of unstructured text values
#'
#' @param inputId Input variable to assign the control's value to
#' @param label Display label for the control
#' @param value Initial value
#' @return A text input control that can be added to a UI definition.
#'
#' @family input elements
#' @seealso \code{\link{updateTextInput}}
#'
#' @examples
#' textInput("caption", "Caption:", "Data Summary")
#' @export
textInput <- function(inputId, label, value = "") {
  tagList(
    label %AND% tags$label(label, `for` = inputId),
    tags$input(id = inputId, type="text", value=value)
  )
}

#' Create a numeric input control
#'
#' Create an input control for entry of numeric values
#'
#' @param inputId Input variable to assign the control's value to
#' @param label Display label for the control
#' @param value Initial value
#' @param min Minimum allowed value
#' @param max Maximum allowed value
#' @param step Interval to use when stepping between min and max
#' @return A numeric input control that can be added to a UI definition.
#'
#' @family input elements
#' @seealso \code{\link{updateNumericInput}}
#'
#' @examples
#' numericInput("obs", "Observations:", 10,
#'              min = 1, max = 100)
#' @export
numericInput <- function(inputId, label, value, min = NA, max = NA, step = NA) {

  # build input tag
  inputTag <- tags$input(id = inputId, type = "number", value = formatNoSci(value))
  if (!is.na(min))
    inputTag$attribs$min = min
  if (!is.na(max))
    inputTag$attribs$max = max
  if (!is.na(step))
    inputTag$attribs$step = step

  tagList(
    label %AND% tags$label(label, `for` = inputId),
    inputTag
  )
}


#' File Upload Control
#'
#' Create a file upload control that can be used to upload one or more files.
#' \bold{Does not work on older browsers, including Internet Explorer 9 and
#' earlier.}
#'
#' Whenever a file upload completes, the corresponding input variable is set
#' to a dataframe. This dataframe contains one row for each selected file, and
#' the following columns:
#' \describe{
#'   \item{\code{name}}{The filename provided by the web browser. This is
#'   \strong{not} the path to read to get at the actual data that was uploaded
#'   (see
#'   \code{datapath} column).}
#'   \item{\code{size}}{The size of the uploaded data, in
#'   bytes.}
#'   \item{\code{type}}{The MIME type reported by the browser (for example,
#'   \code{text/plain}), or empty string if the browser didn't know.}
#'   \item{\code{datapath}}{The path to a temp file that contains the data that was
#'   uploaded. This file may be deleted if the user performs another upload
#'   operation.}
#' }
#'
#' @family input elements
#'
#' @param inputId Input variable to assign the control's value to.
#' @param label Display label for the control.
#' @param multiple Whether the user should be allowed to select and upload
#'   multiple files at once.
#' @param accept A character vector of MIME types; gives the browser a hint of
#'   what kind of files the server is expecting.
#'
#' @export
fileInput <- function(inputId, label, multiple = FALSE, accept = NULL) {
  inputTag <- tags$input(id = inputId, type = "file")
  if (multiple)
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0)
    inputTag$attribs$accept <- paste(accept, collapse=',')

  tagList(
    label %AND% tags$label(label),
    inputTag,
    tags$div(
      id=paste(inputId, "_progress", sep=""),
      class="progress progress-striped active shiny-file-input-progress",
      tags$div(class="bar"),
      tags$label()
    )
  )
}


#' Checkbox Input Control
#'
#' Create a checkbox that can be used to specify logical values.
#'
#' @param inputId Input variable to assign the control's value to.
#' @param label Display label for the control.
#' @param value Initial value (\code{TRUE} or \code{FALSE}).
#' @return A checkbox control that can be added to a UI definition.
#'
#' @family input elements
#' @seealso \code{\link{checkboxGroupInput}}, \code{\link{updateCheckboxInput}}
#'
#' @examples
#' checkboxInput("outliers", "Show outliers", FALSE)
#' @export
checkboxInput <- function(inputId, label, value = FALSE) {
  inputTag <- tags$input(id = inputId, type="checkbox")
  if (!is.null(value) && value)
    inputTag$attribs$checked <- "checked"
  tags$label(class = "checkbox", `for` = inputId, inputTag, tags$span(label))
}


#' Checkbox Group Input Control
#'
#' Create a group of checkboxes that can be used to toggle multiple choices
#' independently. The server will receive the input as a character vector of the
#' selected values.
#'
#' @param inputId Input variable to assign the control's value to.
#' @param label Display label for the control, or \code{NULL}.
#' @param choices List of values to show checkboxes for. If elements of the list
#'   are named then that name rather than the value is displayed to the user.
#' @param selected The values that should be initially selected, if any.
#' @param inline If \code{TRUE}, render the choices inline (i.e. horizontally)
#' @return A list of HTML elements that can be added to a UI definition.
#'
#' @family input elements
#' @seealso \code{\link{checkboxInput}}, \code{\link{updateCheckboxGroupInput}}
#'
#' @examples
#' checkboxGroupInput("variable", "Variable:",
#'                    c("Cylinders" = "cyl",
#'                      "Transmission" = "am",
#'                      "Gears" = "gear"))
#'
#' @export
checkboxGroupInput <- function(inputId, label, choices, selected = NULL, inline = FALSE) {
  # resolve names
  choices <- choicesWithNames(choices)
  if (!is.null(selected))
    selected <- validateSelected(selected, choices, inputId)

  options <- generateOptions(inputId, choices, selected, inline)

  # return label and select tag
  tags$div(id = inputId,
           class = "control-group shiny-input-checkboxgroup",
           controlLabel(inputId, label),
           options)
}

# Before shiny 0.9, `selected` refers to names/labels of `choices`; now it
# refers to values. Below is a function for backward compatibility.
validateSelected <- function(selected, choices, inputId) {
  # drop names, otherwise toJSON() keeps them too
  selected <- unname(selected)
  # if you are using optgroups, you're using shiny > 0.10.0, and you should
  # already know that `selected` must be a value instead of a label
  if (needOptgroup(choices)) return(selected)

  if (is.list(choices)) choices <- unlist(choices)

  nms <- names(choices)
  # labels and values are identical, no need to validate
  if (identical(nms, unname(choices))) return(selected)
  # when selected labels instead of values
  i <- (selected %in% nms) & !(selected %in% choices)
  if (any(i)) {
    warnFun <- if (all(i)) {
      # replace names with values
      selected <- unname(choices[selected])
      warning
    } else stop  # stop when it is ambiguous (some labels == values)
    warnFun("'selected' must be the values instead of names of 'choices' ",
            "for the input '", inputId, "'")
  }
  selected
}

# generate options for radio buttons and checkbox groups (type = 'checkbox' or
# 'radio')
generateOptions <- function(inputId, choices, selected, inline, type = 'checkbox') {
  # create tags for each of the options
  ids <- paste0(inputId, seq_along(choices))
  # generate a list of <input type=? [checked] />
  mapply(
    ids, choices, names(choices),
    FUN = function(id, value, name) {
      inputTag <- tags$input(
        type = type, name = inputId, id = id, value = value
      )
      if (value %in% selected)
        inputTag$attribs$checked <- "checked"
      tags$label(
        class = paste(type, if (inline) "inline"),
        inputTag, tags$span(name)
      )
    },
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  )
}

#' Create a help text element
#'
#' Create help text which can be added to an input form to provide additional
#' explanation or context.
#'
#' @param ... One or more help text strings (or other inline HTML elements)
#' @return A help text element that can be added to a UI definition.
#'
#' @examples
#' helpText("Note: while the data view will show only",
#'          "the specified number of observations, the",
#'          "summary will be based on the full dataset.")
#' @export
helpText <- function(...) {
  span(class="help-block", ...)
}

controlLabel <- function(controlName, label) {
  label %AND% tags$label(class = "control-label", `for` = controlName, label)
}

# Takes a vector or list, and adds names (same as the value) to any entries
# without names.
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
        val
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

#' Create a select list input control
#'
#' Create a select list that can be used to choose a single or
#' multiple items from a list of values.
#'
#' By default, \code{selectInput()} and \code{selectizeInput()} use the
#' JavaScript library \pkg{selectize.js} (\url{https://github.com/brianreavis/selectize.js})
#' to instead of the basic select input element. To use the standard HTML select
#' input element, use \code{selectInput()} with \code{selectize=FALSE}.
#'
#' @param inputId Input variable to assign the control's value to
#' @param label Display label for the control, or \code{NULL}
#' @param choices List of values to select from. If elements of the list are
#' named then that name rather than the value is displayed to the user.
#' @param selected The initially selected value (or multiple values if
#' \code{multiple = TRUE}). If not specified then defaults to the first value
#' for single-select lists and no values for multiple select lists.
#' @param multiple Is selection of multiple items allowed?
#' @param selectize Whether to use \pkg{selectize.js} or not.
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
                        multiple = FALSE, selectize = TRUE, width = NULL) {
  # resolve names
  choices <- choicesWithNames(choices)

  # default value if it's not specified
  if (is.null(selected)) {
    if (!multiple) selected <- firstChoice(choices)
  } else selected <- validateSelected(selected, choices, inputId)

  # create select tag and add options
  selectTag <- tags$select(id = inputId, selectOptions(choices, selected))
  if (multiple)
    selectTag$attribs$multiple <- "multiple"

  # return label and select tag
  res <- tagList(controlLabel(inputId, label), selectTag)
  if (!selectize) return(res)
  selectizeIt(inputId, res, NULL, width, nonempty = !multiple && !("" %in% choices))
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
        htmlEscape(label),
        selectOptions(choice, selected)
      )

    } else {
      # If single item, just return option string
      sprintf(
        '<option value="%s"%s>%s</option>',
        htmlEscape(choice),
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
  selectizeIt(inputId, selectInput(inputId, ..., selectize = FALSE), options, width)
}

# given a select input and its id, selectize it
selectizeIt <- function(inputId, select, options, width = NULL, nonempty = FALSE) {
  res <- checkAsIs(options)

  selectizeDep <- htmlDependency(
    "selectize", "0.8.5", c(href = "shared/selectize"),
    stylesheet = "css/selectize.bootstrap2.css",
    head = format(tagList(
      HTML('<!--[if lt IE 9]>'),
      tags$script(src = 'shared/selectize/js/es5-shim.min.js'),
      HTML('<![endif]-->'),
      tags$script(src = 'shared/selectize/js/selectize.min.js')
    ))
  )
  attachDependencies(
    tagList(
      select,
      tags$script(
        type = 'application/json',
        `data-for` = inputId, `data-nonempty` = if (nonempty) '',
        `data-eval` = if (length(res$eval)) HTML(toJSON(res$eval)),
        `data-width` = validateCssUnit(width),
        if (length(res$options)) HTML(toJSON(res$options)) else '{}'
      )
    ),
    selectizeDep
  )
}

#' Create radio buttons
#'
#' Create a set of radio buttons used to select an item from a list.
#'
#' @param inputId Input variable to assign the control's value to
#' @param label Display label for the control, or \code{NULL}
#' @param choices List of values to select from (if elements of the list are
#' named then that name rather than the value is displayed to the user)
#' @param selected The initially selected value (if not specified then
#' defaults to the first value)
#' @param inline If \code{TRUE}, render the choices inline (i.e. horizontally)
#' @return A set of radio buttons that can be added to a UI definition.
#'
#' @family input elements
#' @seealso \code{\link{updateRadioButtons}}
#'
#' @examples
#' radioButtons("dist", "Distribution type:",
#'              c("Normal" = "norm",
#'                "Uniform" = "unif",
#'                "Log-normal" = "lnorm",
#'                "Exponential" = "exp"))
#' @export
radioButtons <- function(inputId, label, choices, selected = NULL, inline = FALSE) {
  # resolve names
  choices <- choicesWithNames(choices)

  # default value if it's not specified
  selected <- if (is.null(selected)) choices[[1]] else {
    validateSelected(selected, choices, inputId)
  }
  if (length(selected) > 1) stop("The 'selected' argument must be of length 1")

  options <- generateOptions(inputId, choices, selected, inline, type = 'radio')

  tags$div(id = inputId,
    class = 'control-group shiny-input-radiogroup',
    label %AND% tags$label(class = "control-label", `for` = inputId, label),
    options)
}

#' Create a submit button
#'
#' Create a submit button for an input form. Forms that include a submit
#' button do not automatically update their outputs when inputs change,
#' rather they wait until the user explicitly clicks the submit button.
#'
#' @param text Button caption
#' @param icon Optional \code{\link{icon}} to appear on the button
#' @return A submit button that can be added to a UI definition.
#'
#' @family input elements
#'
#' @examples
#' submitButton("Update View")
#' submitButton("Update View", icon("refresh"))
#' @export
submitButton <- function(text = "Apply Changes", icon = NULL) {
  div(
    tags$button(type="submit", class="btn btn-primary", list(icon, text))
  )
}

#' Action button/link
#'
#' Creates an action button or link whose value is initially zero, and increments by one
#' each time it is pressed.
#'
#' @param inputId Specifies the input slot that will be used to access the
#'   value.
#' @param label The contents of the button or link--usually a text label, but
#'   you could also use any other HTML, like an image.
#' @param icon An optional \code{\link{icon}} to appear on the button.
#' @param ... Named attributes to be applied to the button or link.
#'
#' @family input elements
#' @examples
#' \dontrun{
#' # In server.R
#' output$distPlot <- renderPlot({
#'   # Take a dependency on input$goButton
#'   input$goButton
#'
#'   # Use isolate() to avoid dependency on input$obs
#'   dist <- isolate(rnorm(input$obs))
#'   hist(dist)
#' })
#'
#' # In ui.R
#' actionButton("goButton", "Go!")
#' }
#' @export
actionButton <- function(inputId, label, icon = NULL, ...) {
  tags$button(id=inputId,
              type="button",
              class="btn action-button",
              list(icon, label))
}

#' @rdname actionButton
#' @export
actionLink <- function(inputId, label, icon = NULL, ...) {
  tags$a(id=inputId,
         href="#",
         class="action-button",
         list(icon, label))
}

#' Slider Input Widget
#'
#' Constructs a slider widget to select a numeric value from a range.
#'
#' @param inputId Specifies the \code{input} slot that will be used to access
#'   the value.
#' @param label A descriptive label to be displayed with the widget, or
#'   \code{NULL}.
#' @param min The minimum value (inclusive) that can be selected.
#' @param max The maximum value (inclusive) that can be selected.
#' @param value The initial value of the slider. A numeric vector of length
#'   one will create a regular slider; a numeric vector of length two will
#'   create a double-ended range slider. A warning will be issued if the
#'   value doesn't fit between \code{min} and \code{max}.
#' @param step Specifies the interval between each selectable value on the
#'   slider (\code{NULL} means no restriction).
#' @param round \code{TRUE} to round all values to the nearest integer;
#'   \code{FALSE} if no rounding is desired; or an integer to round to that
#'   number of digits (for example, 1 will round to the nearest 10, and -2 will
#'   round to the nearest .01). Any rounding will be applied after snapping to
#'   the nearest step.
#' @param format Customize format values in slider labels. See
#'   \url{https://code.google.com/p/jquery-numberformatter/} for syntax
#'   details.
#' @param locale The locale to be used when applying \code{format}. See details.
#' @param ticks \code{FALSE} to hide tick marks, \code{TRUE} to show them
#'   according to some simple heuristics.
#' @param animate \code{TRUE} to show simple animation controls with default
#'   settings; \code{FALSE} not to; or a custom settings list, such as those
#'   created using \code{animationOptions}.
#' @inheritParams selectizeInput
#' @family input elements
#' @seealso \code{\link{updateSliderInput}}
#'
#' @details
#'
#' Valid values for \code{locale} are: \tabular{ll}{ Arab Emirates \tab "ae" \cr
#' Australia \tab "au" \cr Austria \tab "at" \cr Brazil \tab "br" \cr Canada
#' \tab "ca" \cr China \tab "cn" \cr Czech \tab "cz" \cr Denmark \tab "dk" \cr
#' Egypt \tab "eg" \cr Finland \tab "fi" \cr France  \tab "fr" \cr Germany \tab
#' "de" \cr Greece \tab "gr" \cr Great Britain \tab "gb" \cr Hong Kong \tab "hk"
#' \cr India \tab "in" \cr Israel \tab "il" \cr Japan \tab "jp" \cr Russia \tab
#' "ru" \cr South Korea \tab "kr" \cr Spain \tab "es" \cr Sweden \tab "se" \cr
#' Switzerland \tab "ch" \cr Taiwan \tab "tw" \cr Thailand \tab "th" \cr United
#' States \tab "us" \cr Vietnam \tab "vn" \cr }
#'
#' @export
sliderInput <- function(inputId, label, min, max, value, step = NULL,
                        round=FALSE, format='#,##0.#####', locale='us',
                        ticks=TRUE, animate=FALSE, width=NULL) {

  if (identical(animate, TRUE))
    animate <- animationOptions()

  if (!is.null(animate) && !identical(animate, FALSE)) {
    if (is.null(animate$playButton))
      animate$playButton <- tags$i(class='icon-play')
    if (is.null(animate$pauseButton))
      animate$pauseButton <- tags$i(class='icon-pause')
  }

  # build slider
  sliderTag <- slider(inputId, min=min, max=max, value=value, step=step,
    round=round, locale=locale, format=format, ticks=ticks, animate=animate,
    width=width)

  if (is.null(label)) {
    sliderTag
  } else {
    tags$div(
      controlLabel(inputId, label),
      sliderTag
    )
  }
}

datePickerDependency <- htmlDependency(
  "bootstrap-datepicker", "1.0.2", c(href = "shared/datepicker"),
  script = "js/bootstrap-datepicker.min.js",
  stylesheet = "css/datepicker.css")

#' Create date input
#'
#' Creates a text input which, when clicked on, brings up a calendar that
#' the user can click on to select dates.
#'
#' The date \code{format} string specifies how the date will be displayed in
#' the browser. It allows the following values:
#'
#' \itemize{
#'   \item \code{yy} Year without century (12)
#'   \item \code{yyyy} Year with century (2012)
#'   \item \code{mm} Month number, with leading zero (01-12)
#'   \item \code{m} Month number, without leading zero (01-12)
#'   \item \code{M} Abbreviated month name
#'   \item \code{MM} Full month name
#'   \item \code{dd} Day of month with leading zero
#'   \item \code{d} Day of month without leading zero
#'   \item \code{D} Abbreviated weekday name
#'   \item \code{DD} Full weekday name
#' }
#'
#' @param inputId Input variable to assign the control's value to.
#' @param label Display label for the control, or \code{NULL}.
#' @param value The starting date. Either a Date object, or a string in
#'   \code{yyyy-mm-dd} format. If NULL (the default), will use the current
#'   date in the client's time zone.
#' @param min The minimum allowed date. Either a Date object, or a string in
#'   \code{yyyy-mm-dd} format.
#' @param max The maximum allowed date. Either a Date object, or a string in
#'   \code{yyyy-mm-dd} format.
#' @param format The format of the date to display in the browser. Defaults to
#'   \code{"yyyy-mm-dd"}.
#' @param startview The date range shown when the input object is first
#'   clicked. Can be "month" (the default), "year", or "decade".
#' @param weekstart Which day is the start of the week. Should be an integer
#'   from 0 (Sunday) to 6 (Saturday).
#' @param language The language used for month and day names. Default is "en".
#'   Other valid values include "bg", "ca", "cs", "da", "de", "el", "es", "fi",
#'   "fr", "he", "hr", "hu", "id", "is", "it", "ja", "kr", "lt", "lv", "ms",
#'   "nb", "nl", "pl", "pt", "pt-BR", "ro", "rs", "rs-latin", "ru", "sk", "sl",
#'   "sv", "sw", "th", "tr", "uk", "zh-CN", and "zh-TW".
#'
#' @family input elements
#' @seealso \code{\link{dateRangeInput}}, \code{\link{updateDateInput}}
#'
#' @examples
#' dateInput("date", "Date:", value = "2012-02-29")
#'
#' # Default value is the date in client's time zone
#' dateInput("date", "Date:")
#'
#' # value is always yyyy-mm-dd, even if the display format is different
#' dateInput("date", "Date:", value = "2012-02-29", format = "mm/dd/yy")
#'
#' # Pass in a Date object
#' dateInput("date", "Date:", value = Sys.Date()-10)
#'
#' # Use different language and different first day of week
#' dateInput("date", "Date:",
#'           language = "de",
#'           weekstart = 1)
#'
#' # Start with decade view instead of default month view
#' dateInput("date", "Date:",
#'           startview = "decade")
#'
#' @export
dateInput <- function(inputId, label, value = NULL, min = NULL, max = NULL,
    format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en") {

  # If value is a date object, convert it to a string with yyyy-mm-dd format
  # Same for min and max
  if (inherits(value, "Date"))  value <- format(value, "%Y-%m-%d")
  if (inherits(min,   "Date"))  min   <- format(min,   "%Y-%m-%d")
  if (inherits(max,   "Date"))  max   <- format(max,   "%Y-%m-%d")

  attachDependencies(
    tags$div(id = inputId,
             class = "shiny-date-input",

      controlLabel(inputId, label),
      tags$input(type = "text",
                 # datepicker class necessary for dropdown to display correctly
                 class = "input-medium datepicker",
                 `data-date-language` = language,
                 `data-date-weekstart` = weekstart,
                 `data-date-format` = format,
                 `data-date-start-view` = startview,
                 `data-min-date` = min,
                 `data-max-date` = max,
                 `data-initial-date` = value
      )
    ),
    datePickerDependency
  )
}


#' Create date range input
#'
#' Creates a pair of text inputs which, when clicked on, bring up calendars that
#' the user can click on to select dates.
#'
#' The date \code{format} string specifies how the date will be displayed in
#' the browser. It allows the following values:
#'
#' \itemize{
#'   \item \code{yy} Year without century (12)
#'   \item \code{yyyy} Year with century (2012)
#'   \item \code{mm} Month number, with leading zero (01-12)
#'   \item \code{m} Month number, without leading zero (01-12)
#'   \item \code{M} Abbreviated month name
#'   \item \code{MM} Full month name
#'   \item \code{dd} Day of month with leading zero
#'   \item \code{d} Day of month without leading zero
#'   \item \code{D} Abbreviated weekday name
#'   \item \code{DD} Full weekday name
#' }
#'
#' @inheritParams dateInput
#' @param start The initial start date. Either a Date object, or a string in
#'   \code{yyyy-mm-dd} format. If NULL (the default), will use the current
#'   date in the client's time zone.
#' @param end The initial end date. Either a Date object, or a string in
#'   \code{yyyy-mm-dd} format. If NULL (the default), will use the current
#'   date in the client's time zone.
#' @param separator String to display between the start and end input boxes.
#'
#' @family input elements
#' @seealso \code{\link{dateInput}}, \code{\link{updateDateRangeInput}}
#'
#' @examples
#' dateRangeInput("daterange", "Date range:",
#'                start = "2001-01-01",
#'                end   = "2010-12-31")
#'
#' # Default start and end is the current date in the client's time zone
#' dateRangeInput("daterange", "Date range:")
#'
#' # start and end are always specified in yyyy-mm-dd, even if the display
#' # format is different
#' dateRangeInput("daterange", "Date range:",
#'                start  = "2001-01-01",
#'                end    = "2010-12-31",
#'                min    = "2001-01-01",
#'                max    = "2012-12-21",
#'                format = "mm/dd/yy",
#'                separator = " - ")
#'
#' # Pass in Date objects
#' dateRangeInput("daterange", "Date range:",
#'                start = Sys.Date()-10,
#'                end = Sys.Date()+10)
#'
#' # Use different language and different first day of week
#' dateRangeInput("daterange", "Date range:",
#'                language = "de",
#'                weekstart = 1)
#'
#' # Start with decade view instead of default month view
#' dateRangeInput("daterange", "Date range:",
#'                startview = "decade")
#'
#' @export
dateRangeInput <- function(inputId, label, start = NULL, end = NULL,
    min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month",
    weekstart = 0, language = "en", separator = " to ") {

  # If start and end are date objects, convert to a string with yyyy-mm-dd format
  # Same for min and max
  if (inherits(start, "Date"))  start <- format(start, "%Y-%m-%d")
  if (inherits(end,   "Date"))  end   <- format(end,   "%Y-%m-%d")
  if (inherits(min,   "Date"))  min   <- format(min,   "%Y-%m-%d")
  if (inherits(max,   "Date"))  max   <- format(max,   "%Y-%m-%d")

  attachDependencies(
    tags$div(id = inputId,
             # input-daterange class is needed for dropdown behavior
             class = "shiny-date-range-input input-daterange",

      controlLabel(inputId, label),
      tags$input(class = "input-small",
                 type = "text",
                 `data-date-language` = language,
                 `data-date-weekstart` = weekstart,
                 `data-date-format` = format,
                 `data-date-start-view` = startview,
                 `data-min-date` = min,
                 `data-max-date` = max,
                 `data-initial-date` = start
                 ),
      HTML(separator),
      tags$input(class = "input-small",
                 type = "text",
                 `data-date-language` = language,
                 `data-date-weekstart` = weekstart,
                 `data-date-format` = format,
                 `data-date-start-view` = startview,
                 `data-min-date` = min,
                 `data-max-date` = max,
                 `data-initial-date` = end
                 )
    ),
    datePickerDependency
  )
}


#' Create a tab panel
#'
#' Create a tab panel that can be included within a \code{\link{tabsetPanel}}.
#'
#' @param title Display title for tab
#' @param ... UI elements to include within the tab
#' @param value The value that should be sent when \code{tabsetPanel} reports
#'   that this tab is selected. If omitted and \code{tabsetPanel} has an
#'   \code{id}, then the title will be used..
#' @param icon Optional icon to appear on the tab. This attribute is only
#' valid when using a \code{tabPanel} within a \code{\link{navbarPage}}.
#' @return A tab that can be passed to \code{\link{tabsetPanel}}
#'
#' @seealso \code{\link{tabsetPanel}}
#'
#' @examples
#' # Show a tabset that includes a plot, summary, and
#' # table view of the generated distribution
#' mainPanel(
#'   tabsetPanel(
#'     tabPanel("Plot", plotOutput("plot")),
#'     tabPanel("Summary", verbatimTextOutput("summary")),
#'     tabPanel("Table", tableOutput("table"))
#'   )
#' )
#' @export
tabPanel <- function(title, ..., value = NULL, icon = NULL) {
  divTag <- div(class="tab-pane",
                title=title,
                `data-value`=value,
                `data-icon-class` = iconClass(icon),
                ...)
}

#' Create a tabset panel
#'
#' Create a tabset that contains \code{\link{tabPanel}} elements. Tabsets are
#' useful for dividing output into multiple independently viewable sections.
#'
#' @param ... \code{\link{tabPanel}} elements to include in the tabset
#' @param id If provided, you can use \code{input$}\emph{\code{id}} in your
#'   server logic to determine which of the current tabs is active. The value
#'   will correspond to the \code{value} argument that is passed to
#'   \code{\link{tabPanel}}.
#' @param selected The \code{value} (or, if none was supplied, the \code{title})
#'   of the tab that should be selected by default. If \code{NULL}, the first
#'   tab will be selected.
#' @param type Use "tabs" for the standard look; Use "pills" for a more plain
#'   look where tabs are selected using a background fill color.
#' @param position The position of the tabs relative to the content. Valid
#'   values are "above", "below", "left", and "right" (defaults to "above").
#'   Note that the \code{position} argument is not valid when \code{type} is
#'   "pill".
#' @return A tabset that can be passed to \code{\link{mainPanel}}
#'
#' @seealso \code{\link{tabPanel}}, \code{\link{updateTabsetPanel}}
#'
#' @examples
#' # Show a tabset that includes a plot, summary, and
#' # table view of the generated distribution
#' mainPanel(
#'   tabsetPanel(
#'     tabPanel("Plot", plotOutput("plot")),
#'     tabPanel("Summary", verbatimTextOutput("summary")),
#'     tabPanel("Table", tableOutput("table"))
#'   )
#' )
#' @export
tabsetPanel <- function(...,
                        id = NULL,
                        selected = NULL,
                        type = c("tabs", "pills"),
                        position = c("above", "below", "left", "right")) {

  # build the tabset
  tabs <- list(...)
  type <- match.arg(type)
  tabset <- buildTabset(tabs, paste0("nav nav-", type), NULL, id, selected)

  # position the nav list and content appropriately
  position <- match.arg(position)
  if (position %in% c("above", "left", "right")) {
    first <- tabset$navList
    second <- tabset$content
  } else if (position %in% c("below")) {
    first <- tabset$content
    second <- tabset$navList
  }

  # create the tab div
  tags$div(class = paste("tabbable tabs-", position, sep=""), first, second)
}

#' Create a navigation list panel
#'
#' Create a navigation list panel that provides a list of links on the left
#' which navigate to a set of tabPanels displayed to the right.
#'
#' @param ... \code{\link{tabPanel}} elements to include in the navlist
#' @param id If provided, you can use \code{input$}\emph{\code{id}} in your
#'   server logic to determine which of the current navlist items is active. The
#'   value will correspond to the \code{value} argument that is passed to
#'   \code{\link{tabPanel}}.
#' @param selected The \code{value} (or, if none was supplied, the \code{title})
#'   of the navigation item that should be selected by default. If \code{NULL},
#'   the first navigation will be selected.
#' @param well \code{TRUE} to place a well (gray rounded rectangle) around the
#'   navigation list.
#' @param fluid \code{TRUE} to use fluid layout; \code{FALSE} to use fixed
#'   layout.
#' @param widths Column withs of the navigation list and tabset content areas
#'   respectively.
#'
#' @details You can include headers within the \code{navlistPanel} by
#' including plain text elements in the list; you can include separators by
#' including "------" (any number of dashes works).
#'
#' @examples
#' shinyUI(fluidPage(
#'
#'   titlePanel("Application Title"),
#'
#'   navlistPanel(
#'     "Header",
#'     tabPanel("First"),
#'     tabPanel("Second"),
#'     "-----",
#'     tabPanel("Third")
#'   )
#' ))
#' @export
navlistPanel <- function(...,
                         id = NULL,
                         selected = NULL,
                         well = TRUE,
                         fluid = TRUE,
                         widths = c(4, 8)) {

  # text filter for defining separtors and headers
  textFilter <- function(text) {
    if (grepl("^\\-+$", text))
      tags$li(class="divider")
    else
      tags$li(class="nav-header", text)
  }

  # build the tabset
  tabs <- list(...)
  tabset <- buildTabset(tabs,
                        "nav nav-list",
                        textFilter,
                        id,
                        selected)

  # create the columns
  columns <- list(
    column(widths[[1]], class=ifelse(well, "well", ""), tabset$navList),
    column(widths[[2]], tabset$content)
  )

  # return the row
  if (fluid)
    fluidRow(columns)
  else
    fixedRow(columns)
}


buildTabset <- function(tabs,
                        ulClass,
                        textFilter = NULL,
                        id = NULL,
                        selected = NULL) {

  # build tab nav list and tab content div

  # add tab input sentinel class if we have an id
  if (!is.null(id))
    ulClass <- paste(ulClass, "shiny-tab-input")

  tabNavList <- tags$ul(class = ulClass, id = id)
  tabContent <- tags$div(class = "tab-content")
  firstTab <- TRUE
  tabsetId <- p_randomInt(1000, 10000)
  tabId <- 1
  for (divTag in tabs) {

    # check for text; pass it to the textFilter or skip it if there is none
    if (is.character(divTag)) {
      if (!is.null(textFilter))
        tabNavList <- tagAppendChild(tabNavList, textFilter(divTag))
      next
    }

    # compute id and assign it to the div
    thisId <- paste("tab", tabsetId, tabId, sep="-")
    divTag$attribs$id <- thisId
    tabId <- tabId + 1

    tabValue <- divTag$attribs$`data-value`
    if (!is.null(tabValue) && is.null(id)) {
      stop("tabsetPanel doesn't have an id assigned, but one of its tabPanels ",
           "has a value. The value won't be sent without an id.")
    }


    # function to append an optional icon to an aTag
    appendIcon <- function(aTag, iconClass) {
      if (!is.null(iconClass)) {
        # for font-awesome we specify fixed-width
        if (grepl("fa-", iconClass, fixed = TRUE))
          iconClass <- paste(iconClass, "fa-fw")
        aTag <- tagAppendChild(aTag, icon(name = NULL, class = iconClass))
      }
      aTag
    }

    # check for a navbarMenu and handle appropriately
    if (inherits(divTag, "shiny.navbarmenu")) {

      # create the a tag
      aTag <- tags$a(href="#",
                     class="dropdown-toggle",
                     `data-toggle`="dropdown")

      # add optional icon
      aTag <- appendIcon(aTag, divTag$iconClass)

      # add the title and caret
      aTag <- tagAppendChild(aTag, divTag$title)
      aTag <- tagAppendChild(aTag, tags$b(class="caret"))

      # build the dropdown list element
      liTag <- tags$li(class = "dropdown", aTag)

      # build the child tabset
      tabset <- buildTabset(divTag$tabs, "dropdown-menu")
      liTag <- tagAppendChild(liTag, tabset$navList)

      # don't add a standard tab content div, rather add the list of tab
      # content divs that are contained within the tabset
      divTag <- NULL
      tabContent <- tagAppendChildren(tabContent,
                                      list = tabset$content$children)
    }
    # else it's a standard navbar item
    else {
      # create the a tag
      aTag <- tags$a(href=paste("#", thisId, sep=""),
                     `data-toggle` = "tab",
                     `data-value` = tabValue)

      # append optional icon
      aTag <- appendIcon(aTag, divTag$attribs$`data-icon-class`)

      # add the title
      aTag <- tagAppendChild(aTag, divTag$attribs$title)

      # create the li tag
      liTag <- tags$li(aTag)
    }

    if (is.null(tabValue)) {
      tabValue <- divTag$attribs$title
    }

    # If appropriate, make this the selected tab (don't ever do initial
    # selection of tabs that are within a navbarMenu)
    if ((ulClass != "dropdown-menu") &&
       ((firstTab && is.null(selected)) ||
        (!is.null(selected) && identical(selected, tabValue)))) {
      liTag$attribs$class <- "active"
      divTag$attribs$class <- "tab-pane active"
      firstTab = FALSE
    }

    divTag$attribs$title <- NULL

    # append the elements to our lists
    tabNavList <- tagAppendChild(tabNavList, liTag)
    tabContent <- tagAppendChild(tabContent, divTag)
  }

  list(navList = tabNavList, content = tabContent)
}


#' Create a text output element
#'
#' Render a reactive output variable as text within an application page. The
#' text will be included within an HTML \code{div} tag by default.
#' @param outputId output variable to read the value from
#' @param container a function to generate an HTML element to contain the text
#' @param inline use an inline (\code{span()}) or block container (\code{div()})
#'   for the output
#' @return A text output element that can be included in a panel
#' @details Text is HTML-escaped prior to rendering. This element is often used
#'   to display \link{renderText} output variables.
#' @examples
#' h3(textOutput("caption"))
#' @export
textOutput <- function(outputId, container = if (inline) span else div, inline = FALSE) {
  container(id = outputId, class = "shiny-text-output")
}

#' Create a verbatim text output element
#'
#' Render a reactive output variable as verbatim text within an
#' application page. The text will be included within an HTML \code{pre} tag.
#' @param outputId output variable to read the value from
#' @return A verbatim text output element that can be included in a panel
#' @details Text is HTML-escaped prior to rendering. This element is often used
#' with the \link{renderPrint} function to preserve fixed-width formatting
#' of printed objects.
#' @examples
#' mainPanel(
#'   h4("Summary"),
#'   verbatimTextOutput("summary"),
#'
#'   h4("Observations"),
#'   tableOutput("view")
#' )
#' @export
verbatimTextOutput <- function(outputId) {
  textOutput(outputId, container = pre)
}

#' Create a image output element
#'
#' Render a \link{renderImage} within an application page.
#' @param outputId output variable to read the image from
#' @param width Image width. Must be a valid CSS unit (like \code{"100\%"},
#'   \code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
#'   string and have \code{"px"} appended.
#' @param height Image height
#' @inheritParams textOutput
#' @return An image output element that can be included in a panel
#' @examples
#' # Show an image
#' mainPanel(
#'   imageOutput("dataImage")
#' )
#' @export
imageOutput <- function(outputId, width = "100%", height="400px", inline=FALSE) {
  style <- paste("width:", validateCssUnit(width), ";",
    "height:", validateCssUnit(height))
  container <- if (inline) span else div
  container(id = outputId, class = "shiny-image-output", style = style)
}

#' Create an plot output element
#'
#' Render a \link{renderPlot} within an application page.
#' @param outputId output variable to read the plot from
#' @param width,height Plot width/height. Must be a valid CSS unit (like
#'   \code{"100\%"}, \code{"400px"}, \code{"auto"}) or a number, which will be
#'   coerced to a string and have \code{"px"} appended. These two arguments are
#'   ignored when \code{inline = TRUE}, in which case the width/height of a plot
#'   must be specified in \code{renderPlot()}.
#' @param clickId If not \code{NULL}, the plot will send coordinates to the
#'   server whenever it is clicked. This information will be accessible on the
#'   \code{input} object using \code{input$}\emph{\code{clickId}}. The value
#'   will be a named list or vector with \code{x} and \code{y} elements
#'   indicating the mouse position in user units.
#' @param hoverId If not \code{NULL}, the plot will send coordinates to the
#'   server whenever the mouse pauses on the plot for more than the number of
#'   milliseconds determined by \code{hoverTimeout}. This information will be
#'   accessible on the \code{input} object using
#'   \code{input$}\emph{\code{clickId}}. The value will be \code{NULL} if the
#'   user is not hovering, and a named list or vector with \code{x} and \code{y}
#'   elements indicating the mouse position in user units.
#' @param hoverDelay The delay for hovering, in milliseconds.
#' @param hoverDelayType The type of algorithm for limiting the number of hover
#'   events. Use \code{"throttle"} to limit the number of hover events to one
#'   every \code{hoverDelay} milliseconds. Use \code{"debounce"} to suspend
#'   events while the cursor is moving, and wait until the cursor has been at
#'   rest for \code{hoverDelay} milliseconds before sending an event.
#' @inheritParams textOutput
#' @note The arguments \code{clickId} and \code{hoverId} only work for R base
#'   graphics (see the \pkg{\link{graphics}} package). They do not work for
#'   \pkg{\link[grid:grid-package]{grid}}-based graphics, such as \pkg{ggplot2},
#'   \pkg{lattice}, and so on.
#' @return A plot output element that can be included in a panel
#' @examples
#' # Show a plot of the generated distribution
#' mainPanel(
#'   plotOutput("distPlot")
#' )
#' @export
plotOutput <- function(outputId, width = "100%", height="400px",
                       clickId = NULL, hoverId = NULL, hoverDelay = 300,
                       hoverDelayType = c("debounce", "throttle"), inline = FALSE) {
  if (is.null(clickId) && is.null(hoverId)) {
    hoverDelay <- NULL
    hoverDelayType <- NULL
  } else {
    hoverDelayType <- match.arg(hoverDelayType)[[1]]
  }

  style <- if (!inline) {
    paste("width:", validateCssUnit(width), ";", "height:", validateCssUnit(height))
  }

  container <- if (inline) span else div
  container(id = outputId, class = "shiny-plot-output", style = style,
      `data-click-id` = clickId,
      `data-hover-id` = hoverId,
      `data-hover-delay` = hoverDelay,
      `data-hover-delay-type` = hoverDelayType)
}

#' Create a table output element
#'
#' Render a \link{renderTable} within an application page.
#' @param outputId output variable to read the table from
#' @return A table output element that can be included in a panel
#' @examples
#' mainPanel(
#'   tableOutput("view")
#' )
#' @export
tableOutput <- function(outputId) {
  div(id = outputId, class="shiny-html-output")
}

dataTableDependency <- list(
  htmlDependency(
    "datatables", "1.10.2", c(href = "shared/datatables"),
    script = "js/jquery.dataTables.min.js"
  ),
  htmlDependency(
    "datatables-bootstrap", "1.10.2", c(href = "shared/datatables"),
    stylesheet = "css/dataTables.bootstrap.css",
    script = "js/dataTables.bootstrap.js"
  )
)

#' @rdname tableOutput
#' @export
dataTableOutput <- function(outputId) {
  attachDependencies(
    div(id = outputId, class="shiny-datatable-output"),
    dataTableDependency
  )
}

#' Create an HTML output element
#'
#' Render a reactive output variable as HTML within an application page. The
#' text will be included within an HTML \code{div} tag, and is presumed to
#' contain HTML content which should not be escaped.
#'
#' \code{uiOutput} is intended to be used with \code{renderUI} on the
#' server side. It is currently just an alias for \code{htmlOutput}.
#'
#' @param outputId output variable to read the value from
#' @inheritParams textOutput
#' @return An HTML output element that can be included in a panel
#' @examples
#' htmlOutput("summary")
#' @export
htmlOutput <- function(outputId, inline = FALSE) {
  container <- if (inline) span else div
  container(id = outputId, class="shiny-html-output")
}

#' @rdname htmlOutput
#' @export
uiOutput <- htmlOutput

#' Create a download button or link
#'
#' Use these functions to create a download button or link; when clicked, it
#' will initiate a browser download. The filename and contents are specified by
#' the corresponding \code{\link{downloadHandler}} defined in the server
#' function.
#'
#' @param outputId The name of the output slot that the \code{downloadHandler}
#'   is assigned to.
#' @param label The label that should appear on the button.
#' @param class Additional CSS classes to apply to the tag, if any.
#'
#' @examples
#' \dontrun{
#' # In server.R:
#' output$downloadData <- downloadHandler(
#'   filename = function() {
#'     paste('data-', Sys.Date(), '.csv', sep='')
#'   },
#'   content = function(con) {
#'     write.csv(data, con)
#'   }
#' )
#'
#' # In ui.R:
#' downloadLink('downloadData', 'Download')
#' }
#'
#' @aliases downloadLink
#' @seealso downloadHandler
#' @export
downloadButton <- function(outputId,
                           label="Download",
                           class=NULL) {
  aTag <- tags$a(id=outputId,
                 class=paste(c('btn shiny-download-link', class), collapse=" "),
                 href='',
                 target='_blank',
                 icon("download"),
                 label)
}

#' @rdname downloadButton
#' @export
downloadLink <- function(outputId, label="Download", class=NULL) {
  tags$a(id=outputId,
         class=paste(c('shiny-download-link', class), collapse=" "),
         href='',
         target='_blank',
         label)
}


#' Create an icon
#'
#' Create an icon for use within a page. Icons can appear on their own,
#' inside of a button, or as an icon for a \code{\link{tabPanel}} within a
#' \code{\link{navbarPage}}.
#'
#' @param name Name of icon. Icons are drawn from the
#'   \href{http://fontawesome.io/icons/}{Font Awesome} library. Note that the
#'   "fa-" prefix should not be used in icon names (i.e. the "fa-calendar" icon
#'   should be referred to as "calendar")
#' @param class Additional classes to customize the style of the icon (see the
#'  \href{http://fontawesome.io/examples/}{usage examples} for
#'   details on supported styles).
#' @param lib Icon library to use ("font-awesome" is only one currently
#'   supported)
#'
#' @return An icon element
#'
#'
#' @examples
#' icon("calendar")            # standard icon
#' icon("calendar", "fa-3x")   # 3x normal size
#'
#' # add an icon to a submit button
#' submitButton("Update View", icon = icon("refresh"))
#'
#' shinyUI(navbarPage("App Title",
#'   tabPanel("Plot", icon = icon("bar-chart-o")),
#'   tabPanel("Summary", icon = icon("list-alt")),
#'   tabPanel("Table", icon = icon("table"))
#' ))
#'
#' @export
icon <- function(name, class = NULL, lib = "font-awesome") {

  # determine stylesheet
  if (!identical(lib, "font-awesome"))
    stop("Unknown font library '", lib, "' specified")

  # build the icon class (allow name to be null so that other functions
  # e.g. builtTabset can pass an explict class value)
  iconClass <- ""
  if (!is.null(name))
    iconClass <- paste0("fa fa-", name)
  if (!is.null(class))
    iconClass <- paste(iconClass, class)

  # return the element and css dependency
  tagList(
    singleton(tags$head(
      tags$link(rel = "stylesheet", type = "text/css",
                href = 'shared/font-awesome/css/font-awesome.min.css')
    )),
    tags$i(class = iconClass)
  )
}

# Helper funtion to extract the class from an icon
iconClass <- function(icon) {
  if (!is.null(icon)) icon[[2]]$attribs$class
}
