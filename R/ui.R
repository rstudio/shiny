#' @export
appendChild <- function(tag, child) {
  tag$children[[length(tag$children)+1]] <- child
  tag
}

#' @export
tag <- function(`_tag_name`, ...) {
  
  # create basic tag data structure
  tag <- list()
  class(tag) <- "shiny.tag"
  tag$name <- `_tag_name`
  tag$attribs <- list()
  tag$children <- list()
  
  # process varArgs
  varArgs <- list(...)
  varArgsNames <- names(varArgs)
  if (is.null(varArgsNames))
    varArgsNames <- character(length=length(varArgs))
    
  if (length(varArgsNames) > 0) {
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
            tag <- appendChild(tag, child)
          else
            tag <- appendChild(tag, as.character(child))
        } 
      }
      
      # everything else treated as text
      else {
        tag <- appendChild(tag, as.character(value))
      }
    }
  }
    
  # return the tag
  return (tag)
}

#' @export
h1 <- function(...) {
  tag("h1", ...)
}

#' @export
h2 <- function(...) {
  tag("h2", ...)
}

#' @export
p <- function(...) {
  tag("p", ...)
}

#' @export
div <- function(...) {
  tag("div", ...)
}

#' @export
img <- function(...) {
  tag("img", ...)
}

#' @export
withHeadTags <- function(tag, ...) {
  list(tag, tag("head", ...))
}

#' @export
script <- function(...) {
  tag("script", ...)
}

#' @export
style <- function(...) {
  tag("style", ...)
}

#' @export
input <- function(...) {
  tag("input", ...)
}

#' @export
br <- function(...) {
  tag("br", ...)
}

htmlEscape <- local({
  .htmlSpecials <- list(
    `&` = '&amp;',
    `<` = '&lt;',
    `>` = '&gt;'
  )
  .htmlSpecialsPattern <- paste(names(.htmlSpecials), collapse='|')
  .htmlSpecialsAttrib <- c(
    .htmlSpecials,
    `'` = '&#39;',
    `"` = '&quot;',
    `\r` = '&#13;',
    `\n` = '&#10;'
  )
  .htmlSpecialsPatternAttrib <- paste(names(.htmlSpecialsAttrib), collapse='|')

  function(text, attribute=T) {
    pattern <- if(attribute)
      .htmlSpecialsPatternAttrib 
    else
      .htmlSpecialsPattern

    # Short circuit in the common case that there's nothing to escape
    if (!grep(pattern, text))
      return(text)
    
    specials <- if(attribute)
      .htmlSpecialsAttrib
    else
      .htmlSpecials
    
    for (chr in names(specials)) {
      text <- gsub(chr, specials[[chr]], text, fixed=T)
    }
    
    return(text)
  }
})

#' @export
shinyPlot <- function(outputId) {
  withHeadTags(script(src="foobar.js"),
               style(src="foobar.css"),
               tag = div(id = outputId, class ="live-plot"))
}

#' @export
shinyText <- function(outputId) {
  div(id = outputId, class = "live-text")
}




#' @export
textInput <- function(inputId, label, value = "", labelOnTop = FALSE) {
  tag <- p(label)
  if (labelOnTop)
    tag <- appendChild(tag, br())
  tag <- appendChild(tag, input(name = inputId, type = 'text', value = value))
}



#' @export
header <- function(...) {
  div(class="shiny-header", ...)
}

#' @export
inputs <- function(...) {
  div(class="shiny-inputs", ...)
}

#' @export
outputs <- function(...) {
  div(class="shiny-outputs", ...)
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
  for (attrib in names(tag$attribs)) {
    attribValue <- tag$attribs[[attrib]]
    if (!is.na(attribValue))
      textWriter(paste(" ", attrib,"=\"", attribValue, "\"", sep=""))
    else
      textWriter(paste(" ", attrib, sep=""))
  }
  
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
    # only self-close void elements 
    # (see: http://dev.w3.org/html5/spec/single-page.html#void-elements)
    if (tag$name %in% c("area", "base", "br", "col", "command", "embed", "hr", 
                        "img", "input", "keygen", "link", "meta", "param",
                        "source", "track", "wbr")) {
      textWriter("/>\n")
    }
    else {
      textWriter(paste("></", tag$name, ">\n", sep=""))
    }
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
  
  ui <- div(class="shiny-ui", ...)
  
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

