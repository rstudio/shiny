\name{radioButtons}
\alias{radioButtons}
\title{Create radio buttons}
\usage{
  radioButtons(inputId, label, choices, selected = NULL)
}
\arguments{
  \item{inputId}{Input variable to assign the control's
  value to}

  \item{label}{Display label for the control}

  \item{choices}{List of values to select from (if elements
  of the list are named then that name rather than the
  value is displayed to the user)}

  \item{selected}{Name of initially selected item (if not
  specified then defaults to the first item)}
}
\value{
  A set of radio buttons that can be added to a UI
  definition.
}
\description{
  Create a set of radio buttons used to select an item from
  a list.
}
\examples{
radioButtons("dist", "Distribution type:",
             c("Normal" = "norm",
               "Uniform" = "unif",
               "Log-normal" = "lnorm",
               "Exponential" = "exp"))
}

