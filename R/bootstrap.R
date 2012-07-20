

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
textInput <- function(inputId, 
                      caption, 
                      initialValue = "") {
  
  list(
    tags$label(caption),
    tags$input(id = inputId, type="text", value=initialValue)
  )
}


#' @export
checkboxInput <- function(inputId, 
                          caption,
                          initialValue = FALSE) {
  
  inputTag <- tags$input(id = inputId, type="checkbox")
  if (initialValue)
    inputTag$attribs$checked <- "checked"
  
  tags$label(class = "checkbox", inputTag, caption)  
}
  



