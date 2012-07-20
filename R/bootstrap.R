

requireBootstrap <- function(min = TRUE) {
  
  ext <- function(ext) {
    ifelse(min, paste(".min", ext, sep=""), ext)
  }
  cssExt <- ext(".css")
  jsExt = ext(".js")
  bs <- "shared/bootstrap/"
  
  tags$head(
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
    tags$link(rel="stylesheet", 
              type="text/css", 
              href=paste(bs, "css/bootstrap", cssExt, sep="")),
    
    tags$link(rel="stylesheet", 
              type="text/css", 
              href=paste(bs, "css/bootstrap-responsive", cssExt, sep="")),
    
    tags$script(src=paste(bs, "js/bootstrap", jsExt, sep=""))
  )
}


#' @export
headerPanel <- function(title, ...) {
  tags$div(class="span12", 
    tags$h1(title), 
    ..., 
    tags$br()
  )
}

#' @export
sidebarPanel <- function(...) {
  tags$div(class="span4",
           tags$div(class="well", ...)
  )
}

#' @export
mainPanel <- function(...) {
  tags$div(class="span8",
           tags$div(...)
  )
}

#' @export
tab <- function(label, ...) {
  tags$div(class="tab-pane", id=label, ...)
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
    
    tabNavList <- appendTagChild(tabNavList, liTag)
    tabContent <- appendTagChild(tabContent, divTag)
  }
  
  tabDiv <- tags$div(class = "tabbable", tabNavList, tabContent)
}



#' @export
applicationUI <- function(header, sidebar, main) {
  
  list(
    requireBootstrap(),
    tags$div(class="container-fluid", 
             tags$div(class="row-fluid", header),
             tags$div(class="row-fluid", sidebar, main)
    )
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
    tags$input(id = inputId, 
               type="number",
               min = min,
               max = max,
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
    selectTag <- appendTagChild(selectTag, optionTag)
  } 
  
  # return label and select tag
  list(controlLabel(inputId, label), selectTag)
}



