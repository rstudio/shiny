

#' @export
applicationPage <- function(headerPanel, sidebarPanel, mainPanel) {
  
  # required head tags for boostrap
  importBootstrap <- function(min = TRUE) {
    
    ext <- function(ext) {
      ifelse(min, paste(".min", ext, sep=""), ext)
    }
    cssExt <- ext(".css")
    jsExt = ext(".js")
    bs <- "shared/bootstrap/"
    
    withTags({
      head(
        meta(name="viewport", content="width=device-width, initial-scale=1.0"),
        link(rel="stylesheet", 
             type="text/css", 
             href=paste(bs, "css/bootstrap", cssExt, sep="")),
        
        link(rel="stylesheet", 
             type="text/css", 
             href=paste(bs, "css/bootstrap-responsive", cssExt, sep="")),
        
        script(src=paste(bs, "js/bootstrap", jsExt, sep=""))
      )
    })
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
    div(class="well", 
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
    selectTag <- appendChild(selectTag, optionTag)
  } 
  
  # return label and select tag
  list(controlLabel(inputId, label), selectTag)
}


#' @export
tab <- function(name, ...) {
  div(class="tab-pane", id=name, ...)
}

#' @export
tabset <- function(...) {
  
  # build tab-nav and tab-content divs
  tabs <- list(...)
  tabNavList <- tags$ul(class = "nav nav-tabs")
  tabContent <- tags$div(class = "tab-content")
  firstTab <- TRUE
  for (divTag in tabs) {
    id <- divTag$attribs$id
    liTag <- tags$li(tags$a(href=paste("#", id, sep=""),
                            `data-toggle` = "tab", 
                            id))
    
    if (firstTab) {
      liTag$attribs$class <- "active"
      divTag$attribs$class <- "tab-pane active"
      firstTab = FALSE
    }
    
    tabNavList <- appendChild(tabNavList, liTag)
    tabContent <- appendChild(tabContent, divTag)
  }
  
  tabDiv <- tags$div(class = "tabbable", tabNavList, tabContent)
}



#' @export
textOutput <- function(outputId, label = NULL) {
  liveText(outputId, label)
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



