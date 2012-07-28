\name{sliderInput}
\alias{sliderInput}
\title{Slider Input Widget}
\usage{
  sliderInput(inputId, label, min, max, value, step = NULL,
    round = FALSE, format = "#,##0.#####", locale = "us",
    ticks = TRUE, animate = FALSE)
}
\arguments{
  \item{inputId}{Specifies the \code{input} slot that will
  be used to access the value.}

  \item{label}{A descriptive label to be displayed with the
  widget.}

  \item{min}{The minimum value (inclusive) that can be
  selected.}

  \item{max}{The maximum value (inclusive) that can be
  selected.}

  \item{value}{The initial value of the slider. A warning
  will be issued if the value doesn't fit between
  \code{min} and \code{max}.}

  \item{step}{Specifies the interval between each
  selectable value on the slider (\code{NULL} means no
  restriction).}

  \item{round}{\code{TRUE} to round all values to the
  nearest integer; \code{FALSE} if no rounding is desired;
  or an integer to round to that number of digits (for
  example, 1 will round to the nearest 10, and -2 will
  round to the nearest .01). Any rounding will be applied
  after snapping to the nearest step.}

  \item{format}{Customize format values in slider labels.
  See
  \url{http://archive.plugins.jquery.com/project/numberformatter}
  for syntax details.}

  \item{locale}{The locale to be used when applying
  \code{format}. See details.}

  \item{ticks}{\code{FALSE} to hide tick marks, \code{TRUE}
  to show them according to some simple heuristics.}

  \item{animate}{\code{TRUE} to show simple animation
  controls with default settings; \code{FALSE} not to; or a
  custom settings list, such as those created using
  \code{\link{animationOptions}}.}
}
\description{
  Constructs a slider widget to select a numeric value from
  a range.
}
\details{
  Valid values for \code{locale} are: \tabular{ll}{ Arab
  Emirates \tab "ae" \cr Australia \tab "au" \cr Austria
  \tab "at" \cr Brazil \tab "br" \cr Canada \tab "ca" \cr
  China \tab "cn" \cr Czech \tab "cz" \cr Denmark \tab "dk"
  \cr Egypt \tab "eg" \cr Finland \tab "fi" \cr France \tab
  "fr" \cr Germany \tab "de" \cr Greece \tab "gr" \cr Great
  Britain \tab "gb" \cr Hong Kong \tab "hk" \cr India \tab
  "in" \cr Israel \tab "il" \cr Japan \tab "jp" \cr Russia
  \tab "ru" \cr South Korea \tab "kr" \cr Spain \tab "es"
  \cr Sweden \tab "se" \cr Switzerland \tab "ch" \cr Taiwan
  \tab "tw" \cr Thailand \tab "th" \cr United States \tab
  "us" \cr Vietnam \tab "vn" \cr }
}

