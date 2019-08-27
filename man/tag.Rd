\name{tag}
\alias{tag}
\alias{tagList}
\alias{tagAppendAttributes}
\alias{tagHasAttribute}
\alias{tagGetAttribute}
\alias{tagAppendChild}
\alias{tagAppendChildren}
\alias{tagSetChildren}
\title{HTML Tag Object}
\usage{
tagList(...)

tagAppendAttributes(tag, ...)

tagHasAttribute(tag, attr)

tagGetAttribute(tag, attr)

tagAppendChild(tag, child)

tagAppendChildren(tag, ..., list = NULL)

tagSetChildren(tag, ..., list = NULL)

tag(`_tag_name`, varArgs, .noWS = NULL)
}
\arguments{
\item{...}{Unnamed items that comprise this list of tags.}

\item{tag}{A tag to append child elements to.}

\item{attr}{The name of an attribute.}

\item{child}{A child element to append to a parent tag.}

\item{list}{An optional list of elements. Can be used with or instead of the
\code{...} items.}

\item{_tag_name}{HTML tag name}

\item{varArgs}{List of attributes and children of the element. Named list
items become attributes, and unnamed list items become children. Valid
children are tags, single-character character vectors (which become text
nodes), and raw HTML (see \code{\link{HTML}}). You can also pass lists that
contain tags, text nodes, and HTML.}

\item{.noWS}{Character vector used to omit some of the whitespace that would
normally be written around this tag. Valid options include \code{before},
\code{after}, \code{outside}, \code{after-begin}, and \code{before-end}.
Any number of these options can be specified.}
}
\value{
An HTML tag object that can be rendered as HTML using
  \code{\link{as.character}()}.
}
\description{
\code{tag()} creates an HTML tag definition. Note that all of the valid HTML5
tags are already defined in the \code{\link{tags}} environment so these
functions should only be used to generate additional tags.
\code{tagAppendChild()} and \code{tagList()} are for supporting package
authors who wish to create their own sets of tags; see the contents of
bootstrap.R for examples.
}
\examples{
tagList(tags$h1("Title"),
        tags$h2("Header text"),
        tags$p("Text here"))

# Can also convert a regular list to a tagList (internal data structure isn't
# exactly the same, but when rendered to HTML, the output is the same).
x <- list(tags$h1("Title"),
          tags$h2("Header text"),
          tags$p("Text here"))
tagList(x)

# suppress the whitespace between tags
oneline <- tag("span",
  tag("strong", "Super strong", .noWS="outside")
)
cat(as.character(oneline))
}
