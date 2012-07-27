

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

#' @export
sliderInput <- function(inputId, label, min, max, value, step = NULL,
                        ...,
                        round=FALSE, format='#,##0.#####', locale='us',
                        ticks=TRUE, animate=TRUE, animationInterval=1000) {
 
  # validate label
  labelText <- as.character(label)
  if (!is.character(labelText))
    stop("label not specified")
  
  play <- tags$i(class='icon-play')
  pause <- tags$i(class='icon-pause')
  
  # build slider
  list(
    controlLabel(inputId, labelText),  
    slider(inputId, min=min, max=max, value=value, step=step, round=round,
           locale=locale, format=format, ticks=ticks,
           animate=animate, playButton=play, pauseButton=pause,
           animationInterval=animationInterval)
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
