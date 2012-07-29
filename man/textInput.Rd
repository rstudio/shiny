\name{textInput}
\alias{textInput}
\title{Create a text input control}
\usage{
  textInput(inputId, label, value = "")
}
\arguments{
  \item{inputId}{Input variable to assign the control's
  value to}

  \item{label}{Display label for the control}

  \item{value}{Initial value}
}
\value{
  A text input control that can be added to a UI
  definition.
}
\description{
  Create an input control for entry of unstructured text
  values
}
\examples{
textInput("caption", "Caption:", "Data Summary")
}

