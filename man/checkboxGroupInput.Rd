\name{checkboxGroupInput}
\alias{checkboxGroupInput}
\title{Checkbox Group Input Control}
\usage{
  checkboxGroupInput(inputId, label, choices,
    selected = NULL)
}
\arguments{
  \item{inputId}{Input variable to assign the control's
  value to.}

  \item{label}{Display label for the control.}

  \item{choices}{List of values to show checkboxes for. If
  elements of the list are named then that name rather than
  the value is displayed to the user.}

  \item{selected}{Names of items that should be initially
  selected, if any.}
}
\value{
  A list of HTML elements that can be added to a UI
  definition.
}
\description{
  Create a group of checkboxes that can be used to toggle
  multiple choices independently. The server will receive
  the input as a character vector of the selected values.
}
\examples{
checkboxGroupInput("variable", "Variable:",
                   c("Cylinders" = "cyl",
                     "Transmission" = "am",
                     "Gears" = "gear"))
}
\seealso{
  \code{\link{checkboxInput}}
}

