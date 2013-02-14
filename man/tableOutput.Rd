\name{tableOutput}
\alias{tableOutput}
\title{Create a table output element}
\usage{
  tableOutput(outputId)
}
\arguments{
  \item{outputId}{output variable to read the table from}
}
\value{
  A table output element that can be included in a panel
}
\description{
  Render a \link{renderTable} within an application page.
}
\examples{
mainPanel(
  tableOutput("view")
)
}

