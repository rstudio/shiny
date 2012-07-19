

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
createTag <- function(`_tag_name`, ...) {
  
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
      else if (isTag(value)) {
        tag$children[[length(tag$children)+1]] <- value
      }
      
      # process lists of children
      else if (is.list(value)) {
        for(child in value) {
          if (isTag(value))
            tag <- appendTagChild(tag, child)
          else
            tag <- appendTagChild(tag, as.character(child))
        } 
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
tags$a <- function(...) createTag("a", ...)
tags$abbr <- function(...) createTag("abbr", ...)
tags$address <- function(...) createTag("address", ...)
tags$area <- function(...) createTag("area", ...)
tags$article <- function(...) createTag("article", ...)
tags$aside <- function(...) createTag("aside", ...)
tags$audio <- function(...) createTag("audio", ...)
tags$b <- function(...) createTag("b", ...)
tags$base <- function(...) createTag("base", ...)
tags$bdi <- function(...) createTag("bdi", ...)
tags$bdo <- function(...) createTag("bdo", ...)
tags$blockquote <- function(...) createTag("blockquote", ...)
tags$body <- function(...) createTag("body", ...)
tags$br <- function(...) createTag("br", ...)
tags$button <- function(...) createTag("button", ...)
tags$canvas <- function(...) createTag("canvas", ...)
tags$caption <- function(...) createTag("caption", ...)
tags$cite <- function(...) createTag("cite", ...)
tags$code <- function(...) createTag("code", ...)
tags$col <- function(...) createTag("col", ...)
tags$colgroup <- function(...) createTag("colgroup", ...)
tags$command <- function(...) createTag("command", ...)
tags$data <- function(...) createTag("data", ...)
tags$datalist <- function(...) createTag("datalist", ...)
tags$dd <- function(...) createTag("dd", ...)
tags$del <- function(...) createTag("del", ...)
tags$details <- function(...) createTag("details", ...)
tags$dfn <- function(...) createTag("dfn", ...)
tags$div <- function(...) createTag("div", ...)
tags$dl <- function(...) createTag("dl", ...)
tags$dt <- function(...) createTag("dt", ...)
tags$em <- function(...) createTag("em", ...)
tags$embed <- function(...) createTag("embed", ...)
tags$eventsource <- function(...) createTag("eventsource", ...)
tags$fieldset <- function(...) createTag("fieldset", ...)
tags$figcaption <- function(...) createTag("figcaption", ...)
tags$figure <- function(...) createTag("figure", ...)
tags$footer <- function(...) createTag("footer", ...)
tags$form <- function(...) createTag("form", ...)
tags$h1 <- function(...) createTag("h1", ...)
tags$h2 <- function(...) createTag("h2", ...)
tags$h3 <- function(...) createTag("h3", ...)
tags$h4 <- function(...) createTag("h4", ...)
tags$h5 <- function(...) createTag("h5", ...)
tags$h6 <- function(...) createTag("h6", ...)
tags$head <- function(...) createTag("head", ...)
tags$header <- function(...) createTag("header", ...)
tags$hgroup <- function(...) createTag("hgroup", ...)
tags$hr <- function(...) createTag("hr", ...)
tags$html <- function(...) createTag("html", ...)
tags$i <- function(...) createTag("i", ...)
tags$iframe <- function(...) createTag("iframe", ...)
tags$img <- function(...) createTag("img", ...)
tags$input <- function(...) createTag("input", ...)
tags$ins <- function(...) createTag("ins", ...)
tags$kbd <- function(...) createTag("kbd", ...)
tags$keygen <- function(...) createTag("keygen", ...)
tags$label <- function(...) createTag("label", ...)
tags$legend <- function(...) createTag("legend", ...)
tags$li <- function(...) createTag("li", ...)
tags$link <- function(...) createTag("link", ...)
tags$mark <- function(...) createTag("mark", ...)
tags$map <- function(...) createTag("map", ...)
tags$menu <- function(...) createTag("menu", ...)
tags$meta <- function(...) createTag("meta", ...)
tags$meter <- function(...) createTag("meter", ...)
tags$nav <- function(...) createTag("nav", ...)
tags$noscript <- function(...) createTag("noscript", ...)
tags$object <- function(...) createTag("object", ...)
tags$ol <- function(...) createTag("ol", ...)
tags$optgroup <- function(...) createTag("optgroup", ...)
tags$option <- function(...) createTag("option", ...)
tags$output <- function(...) createTag("output", ...)
tags$p <- function(...) createTag("p", ...)
tags$param <- function(...) createTag("param", ...)
tags$pre <- function(...) createTag("pre", ...)
tags$progress <- function(...) createTag("progress", ...)
tags$q <- function(...) createTag("q", ...)
tags$ruby <- function(...) createTag("ruby", ...)
tags$rp <- function(...) createTag("rp", ...)
tags$rt <- function(...) createTag("rt", ...)
tags$s <- function(...) createTag("s", ...)
tags$samp <- function(...) createTag("samp", ...)
tags$script <- function(...) createTag("script", ...)
tags$section <- function(...) createTag("section", ...)
tags$select <- function(...) createTag("select", ...)
tags$small <- function(...) createTag("small", ...)
tags$source <- function(...) createTag("source", ...)
tags$span <- function(...) createTag("span", ...)
tags$strong <- function(...) createTag("strong", ...)
tags$style <- function(...) createTag("style", ...)
tags$sub <- function(...) createTag("sub", ...)
tags$summary <- function(...) createTag("summary", ...)
tags$sup <- function(...) createTag("sup", ...)
tags$table <- function(...) createTag("table", ...)
tags$tbody <- function(...) createTag("tbody", ...)
tags$td <- function(...) createTag("td", ...)
tags$textarea <- function(...) createTag("textarea", ...)
tags$tfoot <- function(...) createTag("tfoot", ...)
tags$th <- function(...) createTag("th", ...)
tags$thead <- function(...) createTag("thead", ...)
tags$time <- function(...) createTag("time", ...)
tags$title <- function(...) createTag("title", ...)
tags$tr <- function(...) createTag("tr", ...)
tags$track <- function(...) createTag("track", ...)
tags$u <- function(...) createTag("u", ...)
tags$ul <- function(...) createTag("ul", ...)
tags$var <- function(...) createTag("var", ...)
tags$video <- function(...) createTag("video", ...)
tags$wbr <- function(...) createTag("wbr", ...)



#' @export
withTags <- function(expr) {
  eval(substitute(expr), envir = tags, enclos = parent.frame())
}






