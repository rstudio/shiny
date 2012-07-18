
tag <- function(name, varArgs) {
  
  # create basic tag data structure
  tag <- list()
  class(tag) <- "shiny.tag"
  tag$name <- name
  tag$attribs <- list()
  tag$children <- list()
  
  # process varArgs
  varArgsNames <- names(varArgs)
  if (is.null(varArgsNames))
    varArgsNames <- character(length=length(varArgs))
    
  for (i in 1:length(varArgsNames)) {
    # save name and value
    name <- varArgsNames[[i]]
    value <- varArgs[[i]]
    
    # process attribs
    if (nzchar(name))
      tag$attribs[[name]] <- value
    
    # process child tags
    else if (inherits(value, "shiny.tag")) {
      tag$children[[length(tag$children)+1]] <- value
    }
    
    # process lists of children
    else if (is.list(value)) {
      for(child in value) {
        if (inherits(child, "shiny.tag")) 
          tag$children[[length(tag$children)+1]] <- child
        else
          tag$children[[length(tag$children)+1]] <- as.character(child)
      }
    }
    
    # everything else treated as text
    else {
      tag$children[[length(tag$children)+1]] <- as.character(value)
    }
  }
    
  # return the tag
  return (tag)
}

h1 <- function(...) {
  tag("h1", list(...))
}

h2 <- function(...) {
  tag("h2", list(...))
}

p <- function(...) {
  tag("p", list(...))
}

div <- function(...) {
  tag("div", list(...))
}

img <- function(...) {
  tag("img", list(...))
}

head <- function(...) {
  tag("head", list(...))
}

script <- function(...) {
  tag("script", list(...))
}

style <- function(...) {
  tag("style", list(...))
}

shinyPlot <- function(outputId) {
  list(head(script(src="foobar.js"),
            style(src="foobar.css")),
       img(id = outputId, class ="live-plot"))
}

header <- function(...) {
  div(class="shiny-header", ...)
}

inputs <- function(...) {
  div(class="shiny-inputs", ...)
}

outputs <- function(...) {
  div(class="shiny-outputs", ...)
}

defineUI <- function(...) {
  div(class="shiny-ui", ...)
}

writeTag <- function(context, tag, textWriter, indent=0) {
    
  # function used to write children
  writeChildren <- function(children, childTextWriter, indent) {
    for (child in children) {
      if (inherits(child, "shiny.tag")) {
        writeTag(context, child, childTextWriter, indent)
      }
      else {
        indentText <- paste(rep(" ", indent*3), collapse="")
        childTextWriter(paste(indentText, child, "\n", sep=""))
      }
    }
  }
  
  # special case for head tags, their children get written into
  # a chracter vector which is later rendered into the head
  if (identical(tag$name, "head")) {
    textConn <- textConnection(NULL, "w") 
    textConnWriter <- function(text) cat(text, file = textConn)
    writeChildren(tag$children, textConnWriter, 1)
    context$head <- append(context$head, textConnectionValue(textConn))
    close(textConn)
    return (NULL)
  }
  
  # compute indent text
  indentText <- paste(rep(" ", indent*3), collapse="")
  
  # write tag name
  textWriter(paste(indentText, "<", tag$name, sep=""))
  
  # write attributes
  for (attrib in names(tag$attribs))
    textWriter(paste(" ", attrib,"=\"", tag$attribs[[attrib]], "\"", sep=""))
  
  # write any children
  if (length(tag$children) > 0) {
    
    # special case for a single child text node (skip newlines and indentation)
    if ((length(tag$children) == 1) && is.character(tag$children[[1]]) ) {
      textWriter(paste(">", tag$children[1], "</", tag$name, ">\n", sep=""))
    }
    else {
      textWriter(">\n")
      writeChildren(tag$children, textWriter, indent+1)
      textWriter(paste(indentText, "</", tag$name, ">\n", sep=""))
    }
  }
  else {
    textWriter("/>\n")
  }
}

renderPage <- function(ui, connection) {
  # setup context
  context <- new.env()
  context$head <- character()
  
  # write ui HTML to a character vector
  textConn <- textConnection(NULL, "w") 
  writeTag(context, ui, function(text) cat(text, file = textConn))
  uiHTML <- textConnectionValue(textConn)
  close(textConn)
 
  # write preamble
  writeLines(c('<html>',
               '<head>',
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


ui <- defineUI(
  header(
    h1("My first application"),
    p("This is a really exciting application")
  ),
  inputs(
    p("Here are the inputs")
  ),
  outputs(
    p("Check out my shiny plot:"),
    shinyPlot("plot1"),
    p("Check out my other shiny plot:"),
    shinyPlot("plot2")
  )
)


#renderPage(ui, stdout())










