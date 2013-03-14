#' Create a Twitter Bootstrap page
#' 
#' Create a Shiny UI page that loads the CSS and JavaScript for
#' \href{http://getbootstrap.com}{Twitter Bootstrap}, and has no content in the
#' page body (other than what you provide).
#' 
#' This function is primarily intended for users who are proficient in HTML/CSS,
#' and know how to lay out pages in Bootstrap. Most users should use template
#' functions like \code{\link{pageWithSidebar}}.
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
#' @examples
#' textInput("caption", "Caption:", "Data Summary")
#' @export
textInput <- function(inputId, label, value = "") {
  tagList(
    tags$label(label),
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
    tags$label(label),
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
#' @seealso \code{\link{checkboxGroupInput}}
#' 
#' @examples
#' checkboxInput("outliers", "Show outliers", FALSE)
#' @export
checkboxInput <- function(inputId, label, value = FALSE) {
  inputTag <- tags$input(id = inputId, type="checkbox")
  if (!is.null(value) && value)
    inputTag$attribs$checked <- "checked"
  tags$label(class = "checkbox", inputTag, label)
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
#' @seealso \code{\link{checkboxInput}}
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
  
  checkboxes <- list()
  for (choiceName in names(choices)) {
    
    checkbox <- tags$input(name = inputId, type="checkbox",
                           value = choices[[choiceName]])
    
    if (choiceName %in% selected)
      checkbox$attribs$checked <- 'checked'
    
    checkboxes[[length(checkboxes)+1]] <- checkbox
    checkboxes[[length(checkboxes)+1]] <- choiceName
    checkboxes[[length(checkboxes)+1]] <- tags$br()
  } 
  
  # return label and select tag
  tags$div(class='control-group',
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
  for (choiceName in names(choices)) {
    optionTag <- tags$option(value = choices[[choiceName]], choiceName)
    if (choiceName %in% selected)
      optionTag$attribs$selected = "selected"
    selectTag <- tagAppendChild(selectTag, optionTag)
  } 
  
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
  
  # build list of radio button tags
  inputTags <- list()
  for (i in 1:length(choices)) {
    id <- paste(inputId, i, sep="")
    name <- names(choices)[[i]]
    value <- choices[[i]]
    inputTag <- tags$input(type = "radio", 
                           name = inputId,
                           id = id,
                           value = value)
    if (identical(name, selected))
      inputTag$attribs$checked = "checked"
    
    labelTag <- tags$label(class = "radio")
    labelTag <- tagAppendChild(labelTag, inputTag)
    labelTag <- tagAppendChild(labelTag, name)
    inputTags[[length(inputTags) + 1]] <- labelTag
  }
  
  tagList(tags$label(class = "control-label", label),
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
#' @examples
#' submitButton("Update View")
#' @export
submitButton <- function(text = "Apply Changes") {
  div(
    tags$button(type="submit", class="btn btn-primary", text)
  )
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
#' @param value The initial value of the slider. A warning will be issued if the
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
  tagList(
    controlLabel(inputId, labelText),  
    slider(inputId, min=min, max=max, value=value, step=step, round=round,
           locale=locale, format=format, ticks=ticks,
           animate=animate)
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
#' @return A tabset that can be passed to \code{\link{mainPanel}}
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
tabsetPanel <- function(..., id = NULL) {
  
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
    
    # set the first tab as active
    if (firstTab) {
      liTag$attribs$class <- "active"
      divTag$attribs$class <- "tab-pane active"
      firstTab = FALSE
    }
    
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
#' @return A plot output element that can be included in a panel
#' @examples
#' # Show a plot of the generated distribution
#' mainPanel(
#'   plotOutput("distPlot")
#' )
#' @export
plotOutput <- function(outputId, width = "100%", height="400px") {
  style <- paste("width:", validateCssUnit(width), ";",
    "height:", validateCssUnit(height))
  div(id = outputId, class = "shiny-plot-output", style = style)
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
