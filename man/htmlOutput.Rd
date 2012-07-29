\name{htmlOutput}
\alias{htmlOutput}
\title{Create an HTML output element}
\usage{
  htmlOutput(outputId)
}
\arguments{
  \item{outputId}{output variable to read the value from}
}
\value{
  An HTML output element that can be included in a panel
}
\description{
  Render a reactive output variable as HTML within an
  application page. The text will be included within an
  HTML \code{div} tag, and is presumed to contain HTML
  content which should not be escaped.
}
\examples{
htmlOutput("summary")
}

