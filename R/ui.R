

# attribs, text, children

# explicit named args (my attributes)
# special named args (class, style, attribs)
# un-named character string (content)
# children (which you must explicitly render)
# you return a function 

indentText <- function(indent) {
  paste(rep(" ", indent*3), collapse="")
}

writeLine <- function(text, indent, writer) {
  # compute indent text
  indentText <- indentText(indent)
  
  # write text
  writer(paste(indentText, text, "\n", sep=""))
}

writeTag <- function(tag, indent, writer) {
  
  # compute indent text
  indentText <- indentText(indent)
  
  # write tag name
  writer(paste(indentText, "<", tag$name, sep=""))
   
  # write attributes
  for (attrib in names(tag$attribs))
    writer(paste(" ", attrib,"=\"", tag$attribs[[attrib]], "\"", sep=""))
  
  # write any children
  if (length(tag$children) > 0) {
      
    # special case for a single child text note (skip newlines and indentation)
    if ((length(tag$children) == 1) && is.character(tag$children[[1]]) ) {
      writer(paste(">", tag$children[1], "</", tag$name, ">\n", sep=""))
    }
    else {
      writer(">\n")
      
      for (child in tag$children) {
        if (inherits(child, "shiny.tag"))
          child$renderer(child, indent+1, writer)
        else
          writeLine(child, indent+1, writer)
      }
      writer(paste(indentText, "</", tag$name, ">\n", sep=""))
    }
  }
  else {
    writer("/>\n")
  }
}


createTag <- function(name, renderer, varArgs) {
  
  # create basic tag data structure
  tag <- list()
  class(tag) <- "shiny.tag"
  tag$name <- name
  tag$attribs <- list()
  tag$children <- list()
  tag$renderer <- renderer
  
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
  createTag("h1", writeTag, list(...))
}

h2 <- function(...) {
  createTag("h2", writeTag, list(...))
}

p <- function(...) {
  createTag("p", writeTag, list(...))
}

div <- function(...) {
  createTag("div", writeTag, list(...))
}

img <- function(...) {
  createTag("img", writeTag, list(...))
}

shinyPlot <- function(id) {
  img(id = id, class ="live-plot")
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

defineUI <- function(header, inputs, outputs) {
  div(class="shiny-ui", header, inputs, outputs)
}

renderUI <- function(ui, writer) {
  ui$renderer(ui, 0, writer)
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
    shinyPlot("plot1"))
  )


#renderUI(ui, cat)



