

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

# indent can be numeric to indicate an initial indent level,
# or FALSE to suppress
#' @S3method format shiny.tag
format.shiny.tag <- function(x, ..., singletons = character(0), indent = 0) {
  as.character(renderTags(x, singletons = singletons, indent = indent)$html)
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

#' @rdname tag
#' @export
tagList <- function(...) {
  lst <- list(...)
  class(lst) <- c("shiny.tag.list", "list")
  return(lst)
}

#' @rdname tag
#' @export
tagAppendAttributes <- function(tag, ...) {
  tag$attribs <- c(tag$attribs, list(...))
  tag
}

#' @rdname tag
#' @export
tagAppendChild <- function(tag, child) {
  tag$children[[length(tag$children)+1]] <- child
  tag
}

#' @rdname tag
#' @export
tagAppendChildren <- function(tag, ..., list = NULL) {
  tag$children <- c(tag$children, c(list(...), list))
  tag
}

#' @rdname tag
#' @export
tagSetChildren <- function(tag, ..., list = NULL) {
  tag$children <- c(list(...), list)
  tag
}

#' HTML Tag Object
#'
#' \code{tag()} creates an HTML tag definition. Note that all of the valid HTML5
#' tags are already defined in the \code{\link{tags}} environment so these
#' functions should only be used to generate additional tags.
#' \code{tagAppendChild()} and \code{tagList()} are for supporting package
#' authors who wish to create their own sets of tags; see the contents of
#' bootstrap.R for examples.
#' @param _tag_name HTML tag name
#' @param varArgs List of attributes and children of the element. Named list
#'   items become attributes, and unnamed list items become children. Valid
#'   children are tags, single-character character vectors (which become text
#'   nodes), and raw HTML (see \code{\link{HTML}}). You can also pass lists that
#'   contain tags, text nodes, and HTML.
#' @param tag A tag to append child elements to.
#' @param child A child element to append to a parent tag.
#' @param ...  Unnamed items that comprise this list of tags.
#' @param list An optional list of elements. Can be used with or instead of the
#'   \code{...} items.
#' @return An HTML tag object that can be rendered as HTML using
#'   \code{\link{as.character}()}.
#' @export
#' @examples
#' tagList(tags$h1("Title"),
#'         tags$h2("Header text"),
#'         tags$p("Text here"))
#'
#' # Can also convert a regular list to a tagList (internal data structure isn't
#' # exactly the same, but when rendered to HTML, the output is the same).
#' x <- list(tags$h1("Title"),
#'           tags$h2("Header text"),
#'           tags$p("Text here"))
#' tagList(x)
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
  children <- unname(varArgs[!named_idx])

  # Return tag data structure
  structure(
    list(name = `_tag_name`,
         attribs = attribs,
         children = children),
    class = "shiny.tag"
  )
}

tagWrite <- function(tag, textWriter, indent=0, eol = "\n") {

  if (length(tag) == 0)
    return (NULL)

  # optionally process a list of tags
  if (!isTag(tag) && is.list(tag)) {
    tag <- dropNullsOrEmpty(flattenTags(tag))
    lapply(tag, tagWrite, textWriter, indent)
    return (NULL)
  }

  nextIndent <- if (is.numeric(indent)) indent + 1 else indent
  indent <- if (is.numeric(indent)) indent else 0

  # compute indent text
  indentText <- paste(rep(" ", indent*2), collapse="")

  # Check if it's just text (may either be plain-text or HTML)
  if (is.character(tag)) {
    textWriter(paste(indentText, normalizeText(tag), eol, sep=""))
    return (NULL)
  }

  # write tag name
  textWriter(paste(indentText, "<", tag$name, sep=""))

  # Convert all attribs to chars explicitly; prevents us from messing up factors
  attribs <- lapply(tag$attribs, as.character)
  # concatenate attributes
  # split() is very slow, so avoid it if possible
  if (anyDuplicated(names(attribs)))
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
  children <- dropNullsOrEmpty(flattenTags(tag$children))
  if (length(children) > 0) {
    textWriter(">")

    # special case for a single child text node (skip newlines and indentation)
    if ((length(children) == 1) && is.character(children[[1]]) ) {
      textWriter(paste(normalizeText(children[[1]]), "</", tag$name, ">", eol,
        sep=""))
    }
    else {
      textWriter("\n")
      for (child in children)
        tagWrite(child, textWriter, nextIndent)
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

doRenderTags <- function(ui, indent = 0) {
  # Render the body--the bodyHtml variable will be created
  conn <- file(open="w+")
  connWriter <- function(text) writeChar(text, conn, eos = NULL)
  htmlResult <- tryCatch({
    tagWrite(ui, connWriter, indent)
    flush(conn)
    readLines(conn)
  },
    finally = close(conn)
  )
  return(HTML(paste(htmlResult, collapse = "\n")))
}

renderTags <- function(ui, singletons = character(0), indent = 0) {
  # Do singleton and head processing before rendering
  singletonInfo <- takeSingletons(ui, singletons)
  headInfo <- takeHeads(singletonInfo$ui)

  headIndent <- if (is.numeric(indent)) indent + 1 else indent
  headHtml <- doRenderTags(headInfo$head, indent = headIndent)
  bodyHtml <- doRenderTags(headInfo$ui, indent = indent)

  return(list(head = headHtml,
              singletons = singletonInfo$singletons,
              html = bodyHtml))
}

# Walk a tree of tag objects, rewriting objects according to func.
# preorder=TRUE means preorder tree traversal, that is, an object
# should be rewritten before its children.
rewriteTags <- function(ui, func, preorder) {
  if (preorder)
    ui <- func(ui)

  if (isTag(ui)) {
    ui$children[] <- lapply(ui$children, rewriteTags, func, preorder)
  } else if (is.list(ui)) {
    ui[] <- lapply(ui, rewriteTags, func, preorder)
  }

  if (!preorder)
    ui <- func(ui)

  return(ui)
}

# Preprocess a tag object by changing any singleton X into
# <!--SHINY.SINGLETON[sig]-->X'<!--/SHINY.SINGLETON[sig]-->
# where sig is the sha1 of X, and X' is X minus the singleton
# attribute.
#
# In the case of nested singletons, outer singletons are processed
# before inner singletons (otherwise the processing of inner
# singletons would cause the sha1 of the outer singletons to be
# different).
surroundSingletons <- local({
  surroundSingleton <- function(uiObj) {
    if (inherits(uiObj, "shiny.singleton")) {
      sig <- digest(uiObj, "sha1")
      class(uiObj) <- class(uiObj)[class(uiObj) != "shiny.singleton"]
      return(tagList(
        HTML(sprintf("<!--SHINY.SINGLETON[%s]-->", sig)),
        uiObj,
        HTML(sprintf("<!--/SHINY.SINGLETON[%s]-->", sig))
      ))
    } else {
      uiObj
    }
  }

  function(ui) {
    rewriteTags(ui, surroundSingleton, TRUE)
  }
})

# Given a tag object, apply singleton logic (allow singleton objects
# to appear no more than once per signature) and return the processed
# HTML objects and also the list of known singletons.
takeSingletons <- function(ui, singletons=character(0), desingleton=TRUE) {
  result <- rewriteTags(ui, function(uiObj) {
    if (inherits(uiObj, "shiny.singleton")) {
      sig <- digest(uiObj, "sha1")
      if (sig %in% singletons)
        return(NULL)
      singletons <<- append(singletons, sig)
      if (desingleton)
        class(uiObj) <- class(uiObj)[class(uiObj) != "shiny.singleton"]
      return(uiObj)
    } else {
      return(uiObj)
    }
  }, TRUE)

  return(list(ui=result, singletons=singletons))
}

# Given a tag object, extract out any children of tags$head
# and return them separate from the body.
takeHeads <- function(ui) {
  headItems <- list()
  result <- rewriteTags(ui, function(uiObj) {
    if (isTag(uiObj) && tolower(uiObj$name) == "head") {
      headItems <<- append(headItems, uiObj$children)
      return(NULL)
    }
    return(uiObj)
  }, FALSE)

  return(list(ui=result, head=headItems))
}

#' HTML Builder Functions
#'
#' Simple functions for constructing HTML documents.
#'
#' The \code{tags} environment contains convenience functions for all valid
#' HTML5 tags. To generate tags that are not part of the HTML5 specification,
#' you can use the \code{\link{tag}()} function.
#'
#' Dedicated functions are available for the most common HTML tags that do not
#' conflict with common R functions.
#'
#' The result from these functions is a tag object, which can be converted using
#' \code{\link{as.character}()}.
#'
#' @name builder
#' @param ... Attributes and children of the element. Named arguments become
#'   attributes, and positional arguments become children. Valid children are
#'   tags, single-character character vectors (which become text nodes), and raw
#'   HTML (see \code{\link{HTML}}). You can also pass lists that contain tags,
#'   text nodes, and HTML.
#' @export tags
#' @examples
#' doc <- tags$html(
#'   tags$head(
#'     tags$title('My first page')
#'   ),
#'   tags$body(
#'     h1('My first heading'),
#'     p('My first paragraph, with some ',
#'       strong('bold'),
#'       ' text.'),
#'     div(id='myDiv', class='simpleDiv',
#'         'Here is a div with some attributes.')
#'   )
#' )
#' cat(as.character(doc))
NULL

#' @rdname builder
#' @format NULL
#' @docType NULL
#' @keywords NULL
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
