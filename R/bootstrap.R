

#' @export
pageWithSidebar <- function(headerPanel, sidebarPanel, mainPanel) {
  
  # required head tags for boostrap
  importBootstrap <- function(min = TRUE) {
    
    ext <- function(ext) {
      ifelse(min, paste(".min", ext, sep=""), ext)
    }
    cssExt <- ext(".css")
    jsExt = ext(".js")
    bs <- "shared/bootstrap/"
    
    tags$head(
      tags$meta(name="viewport", 
                content="width=device-width, initial-scale=1.0"),
      tags$link(rel="stylesheet", 
                type="text/css", 
                href=paste(bs, "css/bootstrap", cssExt, sep="")),
      
      tags$link(rel="stylesheet", 
                type="text/css", 
                href=paste(bs, "css/bootstrap-responsive", cssExt, sep="")),
      
      tags$script(src=paste(bs, "js/bootstrap", jsExt, sep=""))
    )
  }
  
  list(
    # inject bootstrap requirements into head
    importBootstrap(),
    
    # basic application container divs
    div(class="container-fluid", 
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



#' @export
headerPanel <- function(title) {    
  list(
    tags$head(tags$title(title)),
    div(class="span12", style="padding: 10px 0px;",
      h1(title)
    )
  )
}

#' @export
sidebarPanel <- function(...) {
  div(class="span4",
    tags$form(class="well", 
      ...
    )
  )
}

#' @export
mainPanel <- function(...) {
  div(class="span8",
    ...
  )
}

#' @export
textInput <- function(inputId, label, value = "") {
  list(
    tags$label(label),
    tags$input(id = inputId, type="text", value=value)
  )
}

#' @export
numericInput <- function(inputId, label, value, min = NA, max = NA) {
  
  # build input tag
  inputTag <- tags$input(id = inputId, type = "number", value = value)
  if (!is.na(min))
    inputTag$attribs$min = min
  if (!is.na(max))
    inputTag$attribs$max = max
  
  list(
    tags$label(label),
    inputTag
  )
}

#' @export
checkboxInput <- function(inputId, label, value = FALSE) {
  inputTag <- tags$input(id = inputId, type="checkbox")
  if (value)
    inputTag$attribs$checked <- "checked"
  tags$label(class = "checkbox", inputTag, label)
}

#' @export
helpText <- function(text, ...) {
  text <- c(text, as.character(list(...)))
  text <- paste(text, collapse=" ")
  span(class="help-block", text)
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
  list(controlLabel(inputId, label), selectTag)
}

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
  
  list(tags$label(class = "control-label", label),
       inputTags)
}

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
  
  if (identical(animate, T))
    animate <- animationOptions()
  
  if (!is.null(animate) && !identical(animate, F)) {
    if (is.null(animate$playButton))
      animate$playButton <- tags$i(class='icon-play')
    if (is.null(animate$pauseButton))
      animate$pauseButton <- tags$i(class='icon-pause')
  }
  
  # build slider
  list(
    controlLabel(inputId, labelText),  
    slider(inputId, min=min, max=max, value=value, step=step, round=round,
           locale=locale, format=format, ticks=ticks,
           animate=animate)
  )
}


#' @export
tabPanel <- function(name, ...) {
  div(class="tab-pane", title=name, ...)
}

#' @export
tabsetPanel <- function(...) {
  
  # build tab-nav and tab-content divs
  tabs <- list(...)
  tabNavList <- tags$ul(class = "nav nav-tabs")
  tabContent <- tags$div(class = "tab-content")
  firstTab <- TRUE
  tabsetId <- as.integer(stats::runif(1, 1, 10000))
  tabId <- 1
  for (divTag in tabs) {
    # compute id and assign it to the div
    id <- paste("tab", tabsetId, tabId, sep="-")
    divTag$attribs$id <- id
    tabId <- tabId + 1
    
    # create the li tag
    liTag <- tags$li(tags$a(href=paste("#", id, sep=""),
                            `data-toggle` = "tab", 
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



#' @export
textOutput <- function(outputId) {
  div(id = outputId, class = "shiny-text-output")
}

#' @export
verbatimTextOutput <- function(outputId) {
  pre(id = outputId, class =  "shiny-text-output")
}

#' @export
plotOutput <- function(outputId, width = "100%", height="400px") {
  style <- paste("width:", width, ";", "height:", height)
  div(id = outputId, class="shiny-plot-output", style = style)
}

#' @export
tableOutput <- function(outputId) {
  div(id = outputId, class="shiny-html-output")
}

#' @export
htmlOutput <- function(outputId) {
  div(id = outputId, class="shiny-html-output")
}
