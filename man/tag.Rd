\name{tag}
\alias{tag}
\alias{tagAppendChild}
\title{
HTML Tag Object
}
\description{
Create an HTML tag definition. Note that all of the valid HTML5 tags are already defined in the \link{tags} environment so these functions should only be used to generate additional tags.

\code{tag(_tag_name, varArgs)}

\code{tagAppendChild(tag, child)}
}

\arguments{
  \item{_tag_name}{
    HTML tag name
}
  \item{varArgs}{
    List of attributes and children of the element. Named list items
    become attributes, and other items become children. Valid
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
}

\value{
  An HTML tag object that can be rendered as HTML using \link{as.character}.
}
