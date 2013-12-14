\name{icon}
\alias{icon}
\title{Create an icon}
\usage{
  icon(name, class = NULL, lib = "font-awesome")
}
\arguments{
  \item{name}{Name of icon. Icons are drawn from the
  \href{http://fontawesome.io/icons/}{Font Awesome}
  library. Note that the "fa-" prefix should not be used in
  icon names (i.e. the "fa-calendar" icon should be
  referred to as "calendar")}

  \item{class}{Additional classes to customize the style of
  the icon (see the
  \href{http://fontawesome.io/examples/}{usage examples}
  for details on supported styles).}

  \item{lib}{Icon library to use ("font-awesome" is only
  one currently supported)}
}
\value{
  An icon element
}
\description{
  Create an icon for use within a page. Icons can appear on
  their own or as a part of a button or navbar menu.
}
\examples{
icon("calendar")            # standard icon
icon("calendar", "fa-3x")   # 3x normal size

# add an icon to a submit button
submitButton("Update View", icon = icon("refresh"))
}

