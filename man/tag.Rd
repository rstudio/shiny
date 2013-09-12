\name{tag}
\alias{tag}
\alias{tagAppendChild}
\alias{tagAppendChildren}
\alias{tagSetChildren}
\alias{tagList}
\title{
HTML Tag Object
}
\description{
\code{tag} creates an HTML tag definition. Note that all of the valid HTML5 tags
are already defined in the \link{tags} environment so these functions should
only be used to generate additional tags. \code{tagAppendChild} and
\code{tagList} are for supporting package authors who wish to create their own
sets of tags; see the contents of bootstrap.R for examples.

\code{tag(_tag_name, varArgs)}

\code{tagAppendChild(tag, child)}

\code{tagAppendChildren(tag, child1, child2)}

\code{tagAppendChildren(tag, list = list(child1, child2))}

\code{tagSetChildren(tag, child1, child2)}

\code{tagSetChildren(tag, list = list(child1, child2))}

\code{tagList(...)}
}

\arguments{
  \item{_tag_name}{
    HTML tag name
}
  \item{varArgs}{
    List of attributes and children of the element. Named list items
    become attributes, and unnamed list items become children. Valid
    children are tags, single-character character vectors (which become
    text nodes), and raw HTML (see \code{\link{HTML}}). You can also
    pass lists that contain tags, text nodes, and HTML.
}
  \item{tag}{
    A tag to append child elements to.
}
  \item{child}{
    A child element to append to a parent tag.
}
  \item{...}{
    Unnamed items that comprise this list of tags.
}
  \item{list}{
    An optional list of elements. Can be used with or instead of the \code{...}
    items.
}
}

\value{
  An HTML tag object that can be rendered as HTML using \link{as.character}.
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
}
