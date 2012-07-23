

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
headerPanel <- function(...) {
  div(class="span12", style="padding: 10px 0px;",
    ...
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
numericInput <- function(inputId, label, min, max, value = NA) {
  list(
    tags$label(label),
    tags$input(id = inputId, type= "number", min = min, max = max,
               value = ifelse(!is.na(value), value, ""))
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
helpText <- function(text) {
  span(class="help-block", text)
}

controlLabel <- function(controlName, label) {
  tags$label(class = "control-label", `for` = controlName, label)
}

#' @export
selectListInput <- function(inputId, label, choices, value = NULL) {
    
  # get choice names
  choiceNames <- names(choices)
  if (is.null(choiceNames))
    choiceNames <- character(length(choices))
  
  # default missing names to choice values
  missingNames <- choiceNames == ""
  choiceNames[missingNames] <- paste(choices)[missingNames]
  names(choices) <- choiceNames
  
  # default value if it's not specified
  if (is.null(value))
    value <- choiceNames[[1]]
  
  # create select tag and add options
  selectTag <- tags$select(id = inputId)
  for (choiceName in names(choices)) {
    optionTag <- tags$option(value = choiceName, choices[[choiceName]])
    if (identical(choiceName, value))
      optionTag$attribs$selected = "selected"
    selectTag <- tagAppendChild(selectTag, optionTag)
  } 
  
  # return label and select tag
  list(controlLabel(inputId, label), selectTag)
}

#' @export
submitButton <- function(text = "Apply Changes") {
  div(
    tags$button(type="submit", class="btn btn-primary", text)
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
  liveText(outputId)
}

#' @export
verbatimTextOutput <- function(outputId) {
  liveVerbatimText(outputId)
}

#' @export
plotOutput <- function(outputId, width = "100%", height="400px") {
  livePlot(outputId, width, height)
}

#' @export
tableOutput <- function(outputId) {
  liveTable(outputId)
}



