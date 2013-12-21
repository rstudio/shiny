

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
  
  function(text, attribute=TRUE) {
    pattern <- if(attribute)
      .htmlSpecialsPatternAttrib 
    else
      .htmlSpecialsPattern
    
    # Short circuit in the common case that there's nothing to escape
    if (!grepl(pattern, text))
      return(text)
    
    specials <- if(attribute)
      .htmlSpecialsAttrib
    else
      .htmlSpecials
    
    for (chr in names(specials)) {
      text <- gsub(chr, specials[[chr]], text, fixed=TRUE)
    }
    
    return(text)
  }
})

isTag <- function(x) {
  inherits(x, "shiny.tag")
}

#' @S3method print shiny.tag
print.shiny.tag <- function(x, ...) {
  print(as.character(x), ...)
  invisible(x)
}

#' @S3method format shiny.tag
format.shiny.tag <- function(x, ...) {
  as.character(renderTags(x)$html)
}

#' @S3method as.character shiny.tag
as.character.shiny.tag <- function(x, ...) {
  renderTags(x)$html
}

#' @S3method print shiny.tag.list
print.shiny.tag.list <- print.shiny.tag

#' @S3method format shiny.tag.list
format.shiny.tag.list <- format.shiny.tag

#' @S3method as.character shiny.tag.list
as.character.shiny.tag.list <- as.character.shiny.tag

#' @S3method print html
print.html <- function(x, ...) {
  cat(x, "\n")
  invisible(x)
}

#' @S3method format html
format.html <- function(x, ...) {
  as.character(x)
}

normalizeText <- function(text) {
  if (!is.null(attr(text, "html")))
    text
  else
    htmlEscape(text, attribute=FALSE)
  
}

#' @export
tagList <- function(...) {
  lst <- list(...)
  class(lst) <- c("shiny.tag.list", "list")
  return(lst)
}

#' @export
tagAppendChild <- function(tag, child) {
  tag$children[[length(tag$children)+1]] <- child
  tag
}

#' @export
tagAppendChildren <- function(tag, ..., list = NULL) {
  tag$children <- c(tag$children, c(list(...), list))
  tag
}

#' @export
tagSetChildren <- function(tag, ..., list = NULL) {
  tag$children <- c(list(...), list)
  tag
}

#' @export
tag <- function(`_tag_name`, varArgs) {
  # Get arg names; if not a named list, use vector of empty strings
  varArgsNames <- names(varArgs)
  if (is.null(varArgsNames))
    varArgsNames <- character(length=length(varArgs))

  # Named arguments become attribs, dropping NULL values
  named_idx <- nzchar(varArgsNames)
  attribs <- dropNulls(varArgs[named_idx])
  
  # Unnamed arguments are flattened and added as children.
  # Use unname() to remove the names attribute from the list, which would
  # consist of empty strings anyway.
  children <- flattenTags(unname(varArgs[!named_idx]))

  # Return tag data structure
  structure(
    list(name = `_tag_name`,
         attribs = attribs,
         children = children),
    class = "shiny.tag"
  )
}

tagWrite <- function(tag, textWriter, indent=0, context = NULL, eol = "\n") {
  
  # optionally process a list of tags
  if (!isTag(tag) && is.list(tag)) {
    sapply(tag, function(t) tagWrite(t, textWriter, indent, context))
    return (NULL)
  }
  
  # first call optional filter -- exit function if it returns false
  if (!is.null(context) && !is.null(context$filter) && !context$filter(tag))
    return (NULL)
  
  # compute indent text
  indentText <- paste(rep(" ", indent*2), collapse="")
  
  # Check if it's just text (may either be plain-text or HTML)
  if (is.character(tag)) {
    textWriter(paste(indentText, normalizeText(tag), eol, sep=""))
    return (NULL)
  }
  
  # write tag name
  textWriter(paste(indentText, "<", tag$name, sep=""))
  
  # concatenate attributes
  attribs <- tag$attribs
  attribs <- lapply(split(attribs, names(attribs)), paste, collapse = " ")

  # write attributes
  for (attrib in names(attribs)) {
    attribValue <- attribs[[attrib]]
    if (!is.na(attribValue)) {
      if (is.logical(attribValue))
        attribValue <- tolower(attribValue)
      text <- htmlEscape(attribValue, attribute=TRUE) 
      textWriter(paste(" ", attrib,"=\"", text, "\"", sep=""))
    }
    else {
      textWriter(paste(" ", attrib, sep=""))
    }
  }
  
  # write any children
  if (length(tag$children) > 0) {
    textWriter(">")
    
    # special case for a single child text node (skip newlines and indentation)
    if ((length(tag$children) == 1) && is.character(tag$children[[1]]) ) {
      tagWrite(tag$children[[1]], textWriter, 0, context, "")
      textWriter(paste("</", tag$name, ">", eol, sep=""))
    }
    else {
      textWriter("\n")
      for (child in tag$children)
        tagWrite(child, textWriter, indent+1, context)
      textWriter(paste(indentText, "</", tag$name, ">", eol, sep=""))
    }
  }
  else {
    # only self-close void elements 
    # (see: http://dev.w3.org/html5/spec/single-page.html#void-elements)
    if (tag$name %in% c("area", "base", "br", "col", "command", "embed", "hr", 
                        "img", "input", "keygen", "link", "meta", "param",
                        "source", "track", "wbr")) {
      textWriter(paste("/>", eol, sep=""))
    }
    else {
      textWriter(paste("></", tag$name, ">", eol, sep=""))
    }
  }
}

renderTags <- function(ui, singletons = character(0)) {
  # provide a filter so we can intercept head tag requests
  context <- new.env()
  context$head <- character()
  context$singletons <- singletons
  context$filter <- function(content) {
    if (inherits(content, 'shiny.singleton')) {
      sig <- digest(content, algo='sha1')
      if (sig %in% context$singletons)
        return(FALSE)
      context$singletons <- c(sig, context$singletons)
    }
    
    if (isTag(content) && identical(content$name, "head")) {
      textConn <- textConnection(NULL, "w") 
      textConnWriter <- function(text) cat(text, file = textConn)
      tagWrite(content$children, textConnWriter, 1, context)
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
  tagWrite(ui, function(text) cat(text, file = textConn), 0, context)
  uiHTML <- paste(textConnectionValue(textConn), collapse = "\n")
  close(textConn)
  
  return(list(head = HTML(paste(context$head, collapse = "\n")),
              singletons = context$singletons,
              html = HTML(uiHTML)))
}

# environment used to store all available tags
#' @export
tags <- list(
  a = function(...) tag("a", list(...)),
  abbr = function(...) tag("abbr", list(...)),
  address = function(...) tag("address", list(...)),
  area = function(...) tag("area", list(...)),
  article = function(...) tag("article", list(...)),
  aside = function(...) tag("aside", list(...)),
  audio = function(...) tag("audio", list(...)),
  b = function(...) tag("b", list(...)),
  base = function(...) tag("base", list(...)),
  bdi = function(...) tag("bdi", list(...)),
  bdo = function(...) tag("bdo", list(...)),
  blockquote = function(...) tag("blockquote", list(...)),
  body = function(...) tag("body", list(...)),
  br = function(...) tag("br", list(...)),
  button = function(...) tag("button", list(...)),
  canvas = function(...) tag("canvas", list(...)),
  caption = function(...) tag("caption", list(...)),
  cite = function(...) tag("cite", list(...)),
  code = function(...) tag("code", list(...)),
  col = function(...) tag("col", list(...)),
  colgroup = function(...) tag("colgroup", list(...)),
  command = function(...) tag("command", list(...)),
  data = function(...) tag("data", list(...)),
  datalist = function(...) tag("datalist", list(...)),
  dd = function(...) tag("dd", list(...)),
  del = function(...) tag("del", list(...)),
  details = function(...) tag("details", list(...)),
  dfn = function(...) tag("dfn", list(...)),
  div = function(...) tag("div", list(...)),
  dl = function(...) tag("dl", list(...)),
  dt = function(...) tag("dt", list(...)),
  em = function(...) tag("em", list(...)),
  embed = function(...) tag("embed", list(...)),
  eventsource = function(...) tag("eventsource", list(...)),
  fieldset = function(...) tag("fieldset", list(...)),
  figcaption = function(...) tag("figcaption", list(...)),
  figure = function(...) tag("figure", list(...)),
  footer = function(...) tag("footer", list(...)),
  form = function(...) tag("form", list(...)),
  h1 = function(...) tag("h1", list(...)),
  h2 = function(...) tag("h2", list(...)),
  h3 = function(...) tag("h3", list(...)),
  h4 = function(...) tag("h4", list(...)),
  h5 = function(...) tag("h5", list(...)),
  h6 = function(...) tag("h6", list(...)),
  head = function(...) tag("head", list(...)),
  header = function(...) tag("header", list(...)),
  hgroup = function(...) tag("hgroup", list(...)),
  hr = function(...) tag("hr", list(...)),
  html = function(...) tag("html", list(...)),
  i = function(...) tag("i", list(...)),
  iframe = function(...) tag("iframe", list(...)),
  img = function(...) tag("img", list(...)),
  input = function(...) tag("input", list(...)),
  ins = function(...) tag("ins", list(...)),
  kbd = function(...) tag("kbd", list(...)),
  keygen = function(...) tag("keygen", list(...)),
  label = function(...) tag("label", list(...)),
  legend = function(...) tag("legend", list(...)),
  li = function(...) tag("li", list(...)),
  link = function(...) tag("link", list(...)),
  mark = function(...) tag("mark", list(...)),
  map = function(...) tag("map", list(...)),
  menu = function(...) tag("menu", list(...)),
  meta = function(...) tag("meta", list(...)),
  meter = function(...) tag("meter", list(...)),
  nav = function(...) tag("nav", list(...)),
  noscript = function(...) tag("noscript", list(...)),
  object = function(...) tag("object", list(...)),
  ol = function(...) tag("ol", list(...)),
  optgroup = function(...) tag("optgroup", list(...)),
  option = function(...) tag("option", list(...)),
  output = function(...) tag("output", list(...)),
  p = function(...) tag("p", list(...)),
  param = function(...) tag("param", list(...)),
  pre = function(...) tag("pre", list(...)),
  progress = function(...) tag("progress", list(...)),
  q = function(...) tag("q", list(...)),
  ruby = function(...) tag("ruby", list(...)),
  rp = function(...) tag("rp", list(...)),
  rt = function(...) tag("rt", list(...)),
  s = function(...) tag("s", list(...)),
  samp = function(...) tag("samp", list(...)),
  script = function(...) tag("script", list(...)),
  section = function(...) tag("section", list(...)),
  select = function(...) tag("select", list(...)),
  small = function(...) tag("small", list(...)),
  source = function(...) tag("source", list(...)),
  span = function(...) tag("span", list(...)),
  strong = function(...) tag("strong", list(...)),
  style = function(...) tag("style", list(...)),
  sub = function(...) tag("sub", list(...)),
  summary = function(...) tag("summary", list(...)),
  sup = function(...) tag("sup", list(...)),
  table = function(...) tag("table", list(...)),
  tbody = function(...) tag("tbody", list(...)),
  td = function(...) tag("td", list(...)),
  textarea = function(...) tag("textarea", list(...)),
  tfoot = function(...) tag("tfoot", list(...)),
  th = function(...) tag("th", list(...)),
  thead = function(...) tag("thead", list(...)),
  time = function(...) tag("time", list(...)),
  title = function(...) tag("title", list(...)),
  tr = function(...) tag("tr", list(...)),
  track = function(...) tag("track", list(...)),
  u = function(...) tag("u", list(...)),
  ul = function(...) tag("ul", list(...)),
  var = function(...) tag("var", list(...)),
  video = function(...) tag("video", list(...)),
  wbr = function(...) tag("wbr", list(...))
)

#' Mark Characters as HTML
#' 
#' Marks the given text as HTML, which means the \link{tag} functions will know
#' not to perform HTML escaping on it.
#' 
#' @param text The text value to mark with HTML
#' @param ... Any additional values to be converted to character and
#'   concatenated together
#' @return The same value, but marked as HTML.
#' 
#' @examples
#' el <- div(HTML("I like <u>turtles</u>"))
#' cat(as.character(el))
#'   
#' @export
HTML <- function(text, ...) {
  htmlText <- c(text, as.character(list(...)))
  htmlText <- paste(htmlText, collapse=" ")
  attr(htmlText, "html") <- TRUE
  class(htmlText) <- c("html", "character")
  htmlText
}

#' Evaluate an expression using the \code{tags}
#'
#' This function makes it simpler to write HTML-generating code. Instead of
#' needing to specify \code{tags} each time a tag function is used, as in
#' \code{tags$div()} and \code{tags$p()}, code inside \code{withTags} is
#' evaluated with \code{tags} searched first, so you can simply use
#' \code{div()} and \code{p()}.
#'
#' If your code uses an object which happens to have the same name as an
#' HTML tag function, such as \code{source()} or \code{summary()}, it will call
#' the tag function. To call the intended (non-tags function), specify the
#' namespace, as in \code{base::source()} or \code{base::summary()}.
#'
#' @param code A set of tags.
#'
#' @examples
#' # Using tags$ each time
#' tags$div(class = "myclass",
#'   tags$h3("header"),
#'   tags$p("text")
#' )
#'
#' # Equivalent to above, but using withTags
#' withTags(
#'   div(class = "myclass",
#'     h3("header"),
#'     p("text")
#'   )
#' )
#'
#'
#' @export
withTags <- function(code) {
  eval(substitute(code), envir = as.list(tags), enclos = parent.frame())
}


# Given a list of tags, lists, and other items, return a flat list, where the
# items from the inner, nested lists are pulled to the top level, recursively.
flattenTags <- function(x) {
  if (isTag(x)) {
    # For tags, wrap them into a list (which will be unwrapped by caller)
    list(x)
  } else if (is.list(x)) {
    if (length(x) == 0) {
      # Empty lists are simply returned
      x
    } else {
      # For items that are lists (but not tags), recurse
      unlist(lapply(x, flattenTags), recursive = FALSE)
    }

  } else if (is.character(x)){
    # This will preserve attributes if x is a character with attribute,
    # like what HTML() produces
    list(x)

  } else {
    # For other items, coerce to character and wrap them into a list (which
    # will be unwrapped by caller). Note that this will strip attributes.
    list(as.character(x))
  }
}
