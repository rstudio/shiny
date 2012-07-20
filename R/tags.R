

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

isTag <- function(x) {
  inherits(x, "shiny.tag")
}

#' @export
appendTagChild <- function(tag, child) {
  tag$children[[length(tag$children)+1]] <- child
  tag
}

# create a tag 
#' @export
createTag <- function(`_tag_name`, varArgs) {
  
  # create basic tag data structure
  tag <- list()
  class(tag) <- "shiny.tag"
  tag$name <- `_tag_name`
  tag$attribs <- list()
  tag$children <- list()
  
  # process varArgs
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
      else if (isTag(value)) {
        tag$children[[length(tag$children)+1]] <- value
      }
      
      # recursively process lists of children
      else if (is.list(value)) {
        
        appendChildren <- function(tag, children) {
          for(child in children) {
            if (isTag(child))
              tag <- appendTagChild(tag, child)
            else if (is.list(child))
              tag <- appendChildren(tag, child)
            else
              tag <- appendTagChild(tag, as.character(child))
          }
          return (tag)
        }
        
        tag <- appendChildren(tag, value)
      }
      
      # everything else treated as text
      else {
        tag <- appendTagChild(tag, as.character(value))
      }
    }
  }
  
  # return the tag
  return (tag)
}


#' @export
writeTagChildren <- function(children, textWriter, indent, context) {
  for (child in children) {
    if (isTag(child)) {
      writeTag(child, textWriter, indent, context)
    }
    else {
      indentText <- paste(rep(" ", indent*3), collapse="")
      textWriter(paste(indentText, child, "\n", sep=""))
    }
  }
}

#' @export
writeTag <- function(tag, textWriter, indent=0, context = NULL) {
  
  # first call optional filter -- exit function if it returns false
  if (!is.null(context) && !is.null(context$filter) && !context$filter(tag))
    return (NULL)
  
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
      writeTagChildren(tag$children, textWriter, indent+1, context)
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


# environment used to store all available tags
#' @export
tags <- new.env()
tags$a <- function(...) createTag("a", list(...))
tags$abbr <- function(...) createTag("abbr", list(...))
tags$address <- function(...) createTag("address", list(...))
tags$area <- function(...) createTag("area", list(...))
tags$article <- function(...) createTag("article", list(...))
tags$aside <- function(...) createTag("aside", list(...))
tags$audio <- function(...) createTag("audio", list(...))
tags$b <- function(...) createTag("b", list(...))
tags$base <- function(...) createTag("base", list(...))
tags$bdi <- function(...) createTag("bdi", list(...))
tags$bdo <- function(...) createTag("bdo", list(...))
tags$blockquote <- function(...) createTag("blockquote", list(...))
tags$body <- function(...) createTag("body", list(...))
tags$br <- function(...) createTag("br", list(...))
tags$button <- function(...) createTag("button", list(...))
tags$canvas <- function(...) createTag("canvas", list(...))
tags$caption <- function(...) createTag("caption", list(...))
tags$cite <- function(...) createTag("cite", list(...))
tags$code <- function(...) createTag("code", list(...))
tags$col <- function(...) createTag("col", list(...))
tags$colgroup <- function(...) createTag("colgroup", list(...))
tags$command <- function(...) createTag("command", list(...))
tags$data <- function(...) createTag("data", list(...))
tags$datalist <- function(...) createTag("datalist", list(...))
tags$dd <- function(...) createTag("dd", list(...))
tags$del <- function(...) createTag("del", list(...))
tags$details <- function(...) createTag("details", list(...))
tags$dfn <- function(...) createTag("dfn", list(...))
tags$div <- function(...) createTag("div", list(...))
tags$dl <- function(...) createTag("dl", list(...))
tags$dt <- function(...) createTag("dt", list(...))
tags$em <- function(...) createTag("em", list(...))
tags$embed <- function(...) createTag("embed", list(...))
tags$eventsource <- function(...) createTag("eventsource", list(...))
tags$fieldset <- function(...) createTag("fieldset", list(...))
tags$figcaption <- function(...) createTag("figcaption", list(...))
tags$figure <- function(...) createTag("figure", list(...))
tags$footer <- function(...) createTag("footer", list(...))
tags$form <- function(...) createTag("form", list(...))
tags$h1 <- function(...) createTag("h1", list(...))
tags$h2 <- function(...) createTag("h2", list(...))
tags$h3 <- function(...) createTag("h3", list(...))
tags$h4 <- function(...) createTag("h4", list(...))
tags$h5 <- function(...) createTag("h5", list(...))
tags$h6 <- function(...) createTag("h6", list(...))
tags$head <- function(...) createTag("head", list(...))
tags$header <- function(...) createTag("header", list(...))
tags$hgroup <- function(...) createTag("hgroup", list(...))
tags$hr <- function(...) createTag("hr", list(...))
tags$html <- function(...) createTag("html", list(...))
tags$i <- function(...) createTag("i", list(...))
tags$iframe <- function(...) createTag("iframe", list(...))
tags$img <- function(...) createTag("img", list(...))
tags$input <- function(...) createTag("input", list(...))
tags$ins <- function(...) createTag("ins", list(...))
tags$kbd <- function(...) createTag("kbd", list(...))
tags$keygen <- function(...) createTag("keygen", list(...))
tags$label <- function(...) createTag("label", list(...))
tags$legend <- function(...) createTag("legend", list(...))
tags$li <- function(...) createTag("li", list(...))
tags$link <- function(...) createTag("link", list(...))
tags$mark <- function(...) createTag("mark", list(...))
tags$map <- function(...) createTag("map", list(...))
tags$menu <- function(...) createTag("menu", list(...))
tags$meta <- function(...) createTag("meta", list(...))
tags$meter <- function(...) createTag("meter", list(...))
tags$nav <- function(...) createTag("nav", list(...))
tags$noscript <- function(...) createTag("noscript", list(...))
tags$object <- function(...) createTag("object", list(...))
tags$ol <- function(...) createTag("ol", list(...))
tags$optgroup <- function(...) createTag("optgroup", list(...))
tags$option <- function(...) createTag("option", list(...))
tags$output <- function(...) createTag("output", list(...))
tags$p <- function(...) createTag("p", list(...))
tags$param <- function(...) createTag("param", list(...))
tags$pre <- function(...) createTag("pre", list(...))
tags$progress <- function(...) createTag("progress", list(...))
tags$q <- function(...) createTag("q", list(...))
tags$ruby <- function(...) createTag("ruby", list(...))
tags$rp <- function(...) createTag("rp", list(...))
tags$rt <- function(...) createTag("rt", list(...))
tags$s <- function(...) createTag("s", list(...))
tags$samp <- function(...) createTag("samp", list(...))
tags$script <- function(...) createTag("script", list(...))
tags$section <- function(...) createTag("section", list(...))
tags$select <- function(...) createTag("select", list(...))
tags$small <- function(...) createTag("small", list(...))
tags$source <- function(...) createTag("source", list(...))
tags$span <- function(...) createTag("span", list(...))
tags$strong <- function(...) createTag("strong", list(...))
tags$style <- function(...) createTag("style", list(...))
tags$sub <- function(...) createTag("sub", list(...))
tags$summary <- function(...) createTag("summary", list(...))
tags$sup <- function(...) createTag("sup", list(...))
tags$table <- function(...) createTag("table", list(...))
tags$tbody <- function(...) createTag("tbody", list(...))
tags$td <- function(...) createTag("td", list(...))
tags$textarea <- function(...) createTag("textarea", list(...))
tags$tfoot <- function(...) createTag("tfoot", list(...))
tags$th <- function(...) createTag("th", list(...))
tags$thead <- function(...) createTag("thead", list(...))
tags$time <- function(...) createTag("time", list(...))
tags$title <- function(...) createTag("title", list(...))
tags$tr <- function(...) createTag("tr", list(...))
tags$track <- function(...) createTag("track", list(...))
tags$u <- function(...) createTag("u", list(...))
tags$ul <- function(...) createTag("ul", list(...))
tags$var <- function(...) createTag("var", list(...))
tags$video <- function(...) createTag("video", list(...))
tags$wbr <- function(...) createTag("wbr", list(...))







