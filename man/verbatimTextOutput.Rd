\name{verbatimTextOutput}
\alias{verbatimTextOutput}
\title{Create a verbatim text output element}
\usage{
  verbatimTextOutput(outputId)
}
\arguments{
  \item{outputId}{output variable to read the value from}
}
\value{
  A verbatim text output element that can be included in a
  panel
}
\description{
  Render a reactive output variable as verbatim text within
  an application page. The text will be included within an
  HTML \code{pre} tag.
}
\details{
  Text is HTML-escaped prior to rendering. This element is
  often used with the \link{renderPrint} function to
  preserve fixed-width formatting of printed objects.
}
\examples{
mainPanel(
  h4("Summary"),
  verbatimTextOutput("summary"),

  h4("Observations"),
  tableOutput("view")
)
}

