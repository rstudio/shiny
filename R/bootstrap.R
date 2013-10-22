#' Create a Twitter Bootstrap page
#' 
#' Create a Shiny UI page that loads the CSS and JavaScript for
#' \href{http://getbootstrap.com}{Twitter Bootstrap}, and has no content in the
#' page body (other than what you provide).
#' 
#' These functions are primarily intended for users who are proficient in
#' HTML/CSS, and know how to lay out pages in Bootstrap. Most users should use
#' template functions like \code{\link{pageWithSidebar}}.
#'
#' \code{basicPage} is the same as \code{bootstrapPage}, with an added
#' \code{<div class="container-fluid">} wrapper to provide a little padding.
#' 
#' @param ... The contents of the document body.
#' @return A UI defintion that can be passed to the \link{shinyUI} function.
#' 
#' @export
bootstrapPage <- function(...) {
  # required head tags for boostrap
  importBootstrap <- function(min = TRUE, responsive = TRUE) {
    
    ext <- function(ext) {
      ifelse(min, paste(".min", ext, sep=""), ext)
    }
    cssExt <- ext(".css")
    jsExt = ext(".js")
    bs <- "shared/bootstrap/"
    
    result <- tags$head(
      tags$link(rel="stylesheet", 
                type="text/css", 
                href="shared/slider/css/jquery.slider.min.css"),
      
      tags$script(src="shared/slider/js/jquery.slider.min.js"),
      
      tags$link(rel="stylesheet", 
                type="text/css", 
                href=paste(bs, "css/bootstrap", cssExt, sep="")),
      
      tags$script(src=paste(bs, "js/bootstrap", jsExt, sep=""))
    )
    
    if (responsive) {
      result <- tagAppendChild(
        result, 
        tags$meta(name="viewport", 
                  content="width=device-width, initial-scale=1.0"))
      result <- tagAppendChild(
        result,
        tags$link(rel="stylesheet", 
                  type="text/css", 
                  href=paste(bs, "css/bootstrap-responsive", cssExt, sep="")))
    }
    
    result
  }
  
  tagList(
    # inject bootstrap requirements into head
    importBootstrap(),
    list(...)
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
pageWithSidebar <- function(headerPanel, sidebarPanel, mainPanel) {
  
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
#' to Twitter Bootstrap's \code{well} CSS class.
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
#' Create a sidebar panel containing input controls that can in turn be 
#' passed to \link{pageWithSidebar}.
#' 
#' @param ... UI elements to include on the sidebar
#' @return A sidebar that can be passed to \link{pageWithSidebar}
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
sidebarPanel <- function(...) {
  div(class="span4",
    tags$form(class="well", 
      ...
    )
  )
}

#' Create a main panel
#' 
#' Create a main panel containing output elements that can in turn be 
#' passed to \link{pageWithSidebar}.
#' 
#' @param ... Ouput elements to include in the main panel
#' @return A main panel that can be passed to \link{pageWithSidebar}
#' 
#' @examples
#' # Show the caption and plot of the requested variable against mpg
#' mainPanel(
#'    h3(textOutput("caption")),
#'    plotOutput("mpgPlot")
#' )
#' @export
mainPanel <- function(...) {
  div(class="span8",
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
    tags$label(label, `for` = inputId),
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
  inputTag <- tags$input(id = inputId, type = "number", value = value)
  if (!is.na(min))
    inputTag$attribs$min = min
  if (!is.na(max))
    inputTag$attribs$max = max
  if (!is.na(step))
    inputTag$attribs$step = step
  
  tagList(
    tags$label(label, `for` = inputId),
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
    tags$label(label),
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
#' @param label Display label for the control.
#' @param choices List of values to show checkboxes for. If elements of the list
#'   are named then that name rather than the value is displayed to the user.
#' @param selected Names of items that should be initially selected, if any.
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
checkboxGroupInput <- function(inputId, label, choices, selected = NULL) {
  # resolve names
  choices <- choicesWithNames(choices)

  # Create tags for each of the options
  ids <- paste0(inputId, seq_along(choices))

  checkboxes <- mapply(ids, choices, names(choices),
    SIMPLIFY = FALSE, USE.NAMES = FALSE,
    FUN = function(id, value, name) {
      inputTag <- tags$input(type = "checkbox",
                             name = inputId,
                             id = id,
                             value = value)

    if (name %in% selected)
      inputTag$attribs$checked <- "checked"

    tags$label(class = "checkbox",
               inputTag,
               tags$span(name))
    }
  )
  
  # return label and select tag
  tags$div(id = inputId,
           class = "control-group shiny-input-checkboxgroup",
           controlLabel(inputId, label),
           checkboxes)
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
  tags$label(class = "control-label", `for` = controlName, label)
}

# Takes a vector or list, and adds names (same as the value) to any entries
# without names.
choicesWithNames <- function(choices) {
  # get choice names
  choiceNames <- names(choices)
  if (is.null(choiceNames))
    choiceNames <- character(length(choices))
  
  # default missing names to choice values
  missingNames <- choiceNames == ""
  choiceNames[missingNames] <- paste(choices)[missingNames]
  names(choices) <- choiceNames
  
  # return choices
  return (choices)
}

#' Create a select list input control
#' 
#' Create a select list that can be used to choose a single or
#' multiple items from a list of values.
#' 
#' @param inputId Input variable to assign the control's value to
#' @param label Display label for the control
#' @param choices List of values to select from. If elements of the list are 
#' named then that name rather than the value is displayed to the user.
#' @param selected Name of initially selected item (or multiple names if
#' \code{multiple = TRUE}). If not specified then defaults to the first item
#' for single-select lists and no items for multiple select lists.
#' @param multiple Is selection of multiple items allowed?
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
selectInput <- function(inputId, 
                        label, 
                        choices, 
                        selected = NULL, 
                        multiple = FALSE) {
  # resolve names
  choices <- choicesWithNames(choices)
  
  # default value if it's not specified
  if (is.null(selected) && !multiple)
    selected <- names(choices)[[1]]
  
  # create select tag and add options
  selectTag <- tags$select(id = inputId)
  if (multiple)
    selectTag$attribs$multiple <- "multiple"

  # Create tags for each of the options
  optionTags <- mapply(choices, names(choices),
    SIMPLIFY = FALSE, USE.NAMES = FALSE,
    FUN = function(choice, name) {
      optionTag <- tags$option(value = choice, name)

      if (name %in% selected)
        optionTag$attribs$selected = "selected"

      optionTag
    }
  )

  selectTag <- tagSetChildren(selectTag, list = optionTags)

  # return label and select tag
  tagList(controlLabel(inputId, label), selectTag)
}

#' Create radio buttons
#' 
#' Create a set of radio buttons used to select an item from a list.
#' 
#' @param inputId Input variable to assign the control's value to
#' @param label Display label for the control
#' @param choices List of values to select from (if elements of the list are 
#' named then that name rather than the value is displayed to the user)
#' @param selected Name of initially selected item (if not specified then
#' defaults to the first item)
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
radioButtons <- function(inputId, label, choices, selected = NULL) {
  # resolve names
  choices <- choicesWithNames(choices)
  
  # default value if it's not specified
  if (is.null(selected))
    selected <- names(choices)[[1]]
  
  # Create tags for each of the options
  ids <- paste0(inputId, seq_along(choices))

  inputTags <- mapply(ids, choices, names(choices),
    SIMPLIFY = FALSE, USE.NAMES = FALSE,
    FUN = function(id, value, name) {
      inputTag <- tags$input(type = "radio",
                             name = inputId,
                             id = id,
                             value = value)

      if (identical(name, selected))
        inputTag$attribs$checked = "checked"

      # Put the label text in a span
      tags$label(class = "radio",
                inputTag,
                tags$span(name)
      )
    }
  )

  tags$div(id = inputId,
           class = 'control-group shiny-input-radiogroup',
           tags$label(class = "control-label", `for` = inputId, label),
           inputTags)
}

#' Create a submit button
#' 
#' Create a submit button for an input form. Forms that include a submit
#' button do not automatically update their outputs when inputs change,
#' rather they wait until the user explicitly clicks the submit button.
#' 
#' @param text Button caption
#' @return A submit button that can be added to a UI definition.
#' 
#' @family input elements
#'
#' @examples
#' submitButton("Update View")
#' @export
submitButton <- function(text = "Apply Changes") {
  div(
    tags$button(type="submit", class="btn btn-primary", text)
  )
}

#' Action button
#'
#' Creates an action button whose value is initially zero, and increments by one
#' each time it is pressed.
#'
#' @param inputId Specifies the input slot that will be used to access the
#'   value.
#' @param label The contents of the button--usually a text label, but you could
#'   also use any other HTML, like an image.
#'
#' @family input elements
#'
#' @export
actionButton <- function(inputId, label) {
  tags$button(id=inputId, type="button", class="btn action-button", label)
}

#' Slider Input Widget
#' 
#' Constructs a slider widget to select a numeric value from a range.
#' 
#' @param inputId Specifies the \code{input} slot that will be used to access 
#'   the value.
#' @param label A descriptive label to be displayed with the widget.
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
#'   \url{http://archive.plugins.jquery.com/project/numberformatter} for syntax 
#'   details.
#' @param locale The locale to be used when applying \code{format}. See details.
#' @param ticks \code{FALSE} to hide tick marks, \code{TRUE} to show them 
#'   according to some simple heuristics.
#' @param animate \code{TRUE} to show simple animation controls with default 
#'   settings; \code{FALSE} not to; or a custom settings list, such as those 
#'   created using \code{\link{animationOptions}}.
#'
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
                        ticks=TRUE, animate=FALSE) {
 
  # validate label
  labelText <- as.character(label)
  if (!is.character(labelText))
    stop("label not specified")
  
  if (identical(animate, TRUE))
    animate <- animationOptions()
  
  if (!is.null(animate) && !identical(animate, FALSE)) {
    if (is.null(animate$playButton))
      animate$playButton <- tags$i(class='icon-play')
    if (is.null(animate$pauseButton))
      animate$pauseButton <- tags$i(class='icon-pause')
  }
  
  # build slider
  tags$div(
    tagList(
      controlLabel(inputId, labelText),
      slider(inputId, min=min, max=max, value=value, step=step, round=round,
             locale=locale, format=format, ticks=ticks,
             animate=animate)
    )
  )
}


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
#' @param label Display label for the control.
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
#'   "nb", "nl", "pl", "pt", "pt", "ro", "rs", "rs-latin", "ru", "sk", "sl",
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

  tagList(
    singleton(tags$head(
      tags$script(src = "shared/datepicker/js/bootstrap-datepicker.min.js"),
      tags$link(rel = "stylesheet", type = "text/css",
                href = 'shared/datepicker/css/datepicker.css')
    )),
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
    )
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

  tagList(
    singleton(tags$head(
      tags$script(src = "shared/datepicker/js/bootstrap-datepicker.min.js"),
      tags$link(rel = "stylesheet", type = "text/css",
                href = 'shared/datepicker/css/datepicker.css')
    )),
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
    )
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
#'   \code{id}, then the title will be used.
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
tabPanel <- function(title, ..., value = NULL) {
  div(class="tab-pane", title=title, `data-value`=value, ...)
}

#' Create a tabset panel
#' 
#' Create a tabset that contains \code{\link{tabPanel}} elements. Tabsets are
#' useful for dividing output into multiple independently viewable sections.
#' 
#' @param ... \code{\link{tabPanel}} elements to include in the tabset
#' @param id If provided, you can use \code{input$}\emph{\code{id}} in your server 
#'   logic to determine which of the current tabs is active. The value will 
#'   correspond to the \code{value} argument that is passed to 
#'   \code{\link{tabPanel}}.
#' @param selected The \code{value} (or, if none was supplied, the \code{title})
#'   of the tab that should be selected by default. If \code{NULL}, the first
#'   tab will be selected.
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
tabsetPanel <- function(..., id = NULL, selected = NULL) {
  
  # build tab-nav and tab-content divs
  tabs <- list(...)
  tabNavList <- tags$ul(class = "nav nav-tabs", id = id)
  tabContent <- tags$div(class = "tab-content")
  firstTab <- TRUE
  tabsetId <- as.integer(stats::runif(1, 1, 10000))
  tabId <- 1
  for (divTag in tabs) {
    # compute id and assign it to the div
    thisId <- paste("tab", tabsetId, tabId, sep="-")
    divTag$attribs$id <- thisId
    tabId <- tabId + 1
    
    tabValue <- divTag$attribs$`data-value`
    if (!is.null(tabValue) && is.null(id)) {
      stop("tabsetPanel doesn't have an id assigned, but one of its tabPanels ",
           "has a value. The value won't be sent without an id.")
    }

    # create the li tag
    liTag <- tags$li(tags$a(href=paste("#", thisId, sep=""),
                            `data-toggle` = "tab", 
                            `data-value` = tabValue,
                            divTag$attribs$title))
    
    if (is.null(tabValue)) {
      tabValue <- divTag$attribs$title
    }

    # If appropriate, make this the selected tab
    if ((firstTab && is.null(selected)) ||
        (!is.null(selected) && identical(selected, tabValue))) {
      liTag$attribs$class <- "active"
      divTag$attribs$class <- "tab-pane active"
      firstTab = FALSE
    }
    
    divTag$attribs$title <- NULL

    # append the elements to our lists
    tabNavList <- tagAppendChild(tabNavList, liTag)
    tabContent <- tagAppendChild(tabContent, divTag)
  }
  
  tabDiv <- tags$div(class = "tabbable", tabNavList, tabContent)
}


#' Create a text output element
#' 
#' Render a reactive output variable as text within an application page. The 
#' text will be included within an HTML \code{div} tag. 
#' @param outputId output variable to read the value from
#' @return A text output element that can be included in a panel
#' @details Text is HTML-escaped prior to rendering. This element is often used 
#' to display \link{renderText} output variables.
#' @examples
#' h3(textOutput("caption"))
#' @export
textOutput <- function(outputId) {
  div(id = outputId, class = "shiny-text-output")
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
  pre(id = outputId, class =  "shiny-text-output")
}

#' Create a image output element
#' 
#' Render a \link{renderImage} within an application page.
#' @param outputId output variable to read the image from
#' @param width Image width. Must be a valid CSS unit (like \code{"100\%"},
#'   \code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
#'   string and have \code{"px"} appended.
#' @param height Image height
#' @return An image output element that can be included in a panel
#' @examples
#' # Show an image
#' mainPanel(
#'   imageOutput("dataImage")
#' )
#' @export
imageOutput <- function(outputId, width = "100%", height="400px") {
  style <- paste("width:", validateCssUnit(width), ";",
    "height:", validateCssUnit(height))
  div(id = outputId, class = "shiny-image-output", style = style)
}

#' Create an plot output element
#' 
#' Render a \link{renderPlot} within an application page.
#' @param outputId output variable to read the plot from
#' @param width Plot width. Must be a valid CSS unit (like \code{"100\%"},
#'   \code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
#'   string and have \code{"px"} appended.
#' @param height Plot height
#' @param clickId If not \code{NULL}, the plot will send coordinates to the
#'   server whenever it is clicked. This information will be accessible on the 
#'   \code{input} object using \code{input$}\emph{\code{clickId}}. The value will be a
#'   named list or vector with \code{x} and \code{y} elements indicating the
#'   mouse position in user units.
#' @param hoverId If not \code{NULL}, the plot will send coordinates to the
#'   server whenever the mouse pauses on the plot for more than the number of
#'   milliseconds determined by \code{hoverTimeout}. This information will be
#    accessible on the \code{input} object using \code{input$}\emph{\code{clickId}}.
#'   The value will be \code{NULL} if the user is not hovering, and a named
#'   list or vector with \code{x} and \code{y} elements indicating the mouse
#'   position in user units.
#' @param hoverDelay The delay for hovering, in milliseconds.
#' @param hoverDelayType The type of algorithm for limiting the number of hover 
#'   events. Use \code{"throttle"} to limit the number of hover events to one
#'   every \code{hoverDelay} milliseconds. Use \code{"debounce"} to suspend
#'   events while the cursor is moving, and wait until the cursor has been at
#'   rest for \code{hoverDelay} milliseconds before sending an event.
#' @return A plot output element that can be included in a panel
#' @examples
#' # Show a plot of the generated distribution
#' mainPanel(
#'   plotOutput("distPlot")
#' )
#' @export
plotOutput <- function(outputId, width = "100%", height="400px",
                       clickId = NULL, hoverId = NULL, hoverDelay = 300,
                       hoverDelayType = c("debounce", "throttle")) {
  if (is.null(clickId) && is.null(hoverId)) {
    hoverDelay <- NULL
    hoverDelayType <- NULL
  } else {
    hoverDelayType <- match.arg(hoverDelayType)[[1]]
  }
  
  style <- paste("width:", validateCssUnit(width), ";",
    "height:", validateCssUnit(height))
  div(id = outputId, class = "shiny-plot-output", style = style,
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

#' @rdname tableOutput
#' @export
dataTableOutput <- function(outputId) {
  tagList(
    singleton(tags$head(
      tags$link(rel = "stylesheet", type = "text/css",
                href = "shared/datatables/css/DT_bootstrap.css"),
      tags$script(src = "shared/datatables/js/jquery.dataTables.min.js"),
      tags$script(src = "shared/datatables/js/DT_bootstrap.js")
    )),
    div(id = outputId, class="shiny-datatable-output")
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
#' @return An HTML output element that can be included in a panel
#' @examples
#' htmlOutput("summary")
#' @export
htmlOutput <- function(outputId) {
  div(id = outputId, class="shiny-html-output")
}

#' @rdname htmlOutput
#' @export
uiOutput <- function(outputId) {
  htmlOutput(outputId)
}

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
downloadButton <- function(outputId, label="Download", class=NULL) {
  tags$a(id=outputId,
         class=paste(c('btn shiny-download-link', class), collapse=" "),
         href='',
         target='_blank',
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

#' Validate proper CSS formatting of a unit
#' 
#' @param x The unit to validate. Will be treated as a number of pixels if a 
#' unit is not specified.
#' @return A properly formatted CSS unit of length, if possible. Otherwise, will
#' throw an error.
#' @examples
#' validateCssUnit("10%")
#' validateCssUnit(400)  #treated as '400px'
#' @export
validateCssUnit <- function(x) {
  if (is.character(x) &&
     !grepl("^(auto|((\\.\\d+)|(\\d+(\\.\\d+)?))(%|in|cm|mm|em|ex|pt|pc|px))$", x)) {
    stop('"', x, '" is not a valid CSS unit (e.g., "100%", "400px", "auto")')
  }
  if (is.numeric(x)) {
    x <- paste(x, "px", sep = "")
  }
  x
}
