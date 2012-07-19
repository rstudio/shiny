

#' @export
textOutput <- function(outputId, 
                       caption = "", 
                       captionOnTop = FALSE) {

  tag <- tags$div()
  if (nzchar(caption)) {
    tag <- appendTagChild(tag, caption)
    if (captionOnTop)
      tag <- appendTagChild(tag, tags$br())
  }
  tag <- appendTagChild(tag, tags$span(id = outputId, class = "live-text"))
}


#' @export
textInput <- function(inputId, 
                      caption = "", 
                      captionOnTop = FALSE,
                      initialValue = "") {
    tag <- tags$p(caption)
    if (captionOnTop)
      tag <- appendTagChild(tag, tags$br())
    tag <- appendTagChild(tag, tags$input(name = inputId, 
                                          type = 'text', 
                                          value = initialValue))   
}

#' @export
checkboxInput <- function(inputId, 
                          caption,
                          initialValue = FALSE) {
  tag <- tags$p()
  inputTag <- tags$input(type="checkbox", name=inputId)
  if (initialValue)
    inputTag$attribs$checked <- "checked"
  tag <- appendTagChild(tag, inputTag)
  
  tag <- appendTagChild(tag, caption)
}



renderPage <- function(ui, connection) {
  
  # provide a filter so we can intercept head tag requests
  context <- new.env()
  context$head <- character()
  context$filter <- function(tag) {
    if (identical(tag$name, "head")) {
      textConn <- textConnection(NULL, "w") 
      textConnWriter <- function(text) cat(text, file = textConn)
      writeTagChildren(tag$children, textConnWriter, 1, context)
      context$head <- append(context$head, textConnectionValue(textConn))
      close(textConn)
      return (FALSE)
    }
    else {
      return (TRUE)
    }
  }
  
  # write ui HTML to a character vector
  textConn <- textConnection(NULL, "w") 
  writeTag(ui, function(text) cat(text, file = textConn), 0, context)
  uiHTML <- textConnectionValue(textConn)
  close(textConn)
 
  # write preamble
  writeLines(c('<!DOCTYPE html>',
               '<html>',
               '<head>',
               '   <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>',
               '   <script src="shared/jquery.js" type="text/javascript"></script>',
               '   <script src="shared/shiny.js" type="text/javascript"></script>',
               '   <link rel="stylesheet" type="text/css" href="shared/shiny.css"/>',
               context$head[!duplicated(context$head)],
               '</head>',
               '<body>', 
               recursive=TRUE),
             con = connection)
  
  # write UI html to connection
  writeLines(uiHTML, con = connection)
  
  # write end document
  writeLines(c('</body>',
               '</html>'),
             con = connection)
}

#' @export
clientPage <- function(..., path='/') {
  
  ui <- tags$div(class="shiny-ui", ...)
  
  function(ws, header) {
    if (header$RESOURCE != path)
      return(NULL)
    
    textConn <- textConnection(NULL, "w") 
    on.exit(close(textConn))
    
    renderPage(ui, textConn)
    html <- paste(textConnectionValue(textConn), collapse='\n')
    return(http_response(ws, 200, content=html))
  }
}

