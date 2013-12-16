# Shiny

[![Build Status](https://travis-ci.org/rstudio/shiny.png)](https://travis-ci.org/rstudio/shiny)

Shiny is a new package from RStudio that makes it incredibly easy to build interactive web applications with R.

For an introduction and examples, visit the [Shiny homepage](http://www.rstudio.com/shiny/).

## Features

* Build useful web applications with only a few lines of code&mdash;no JavaScript required.
* Shiny applications are automatically "live" in the same way that spreadsheets are live. Outputs change instantly as users modify inputs, without requiring a reload of the browser.
* Shiny user interfaces can be built entirely using R, or can be written directly in HTML, CSS, and JavaScript for more flexibility.
* Works in any R environment (Console R, Rgui for Windows or Mac, ESS, StatET, RStudio, etc.)
* Attractive default UI theme based on [Bootstrap](http://getbootstrap.com/2.3.2/).
* A highly customizable slider widget with built-in support for animation.
* Pre-built output widgets for displaying plots, tables, and printed output of R objects.
* Fast bidirectional communication between the web browser and R using the [httpuv](https://github.com/rstudio/httpuv) package.
* Uses a [reactive](http://en.wikipedia.org/wiki/Reactive_programming) programming model that eliminates messy event handling code, so you can focus on the code that really matters.
* Develop and redistribute your own Shiny widgets that other developers can easily drop into their own applications (coming soon!).

## Installation

To install the stable version from CRAN, simply run the following from an R console:

```r
install.packages("shiny")
```

To install the latest development builds directly from GitHub, run this instead:

```r
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("shiny", "rstudio")
```

## Getting Started

To learn more we highly recommend you check out the [Shiny Tutorial](http://rstudio.github.com/shiny/tutorial). The tutorial explains the framework in-depth, walks you through building a simple application, and includes extensive annotated examples.

We hope you enjoy using Shiny. If you have general questions about using Shiny, please use the Shiny [mailing list](https://groups.google.com/forum/#!forum/shiny-discuss). For bug reports, please use the [issue tracker](https://github.com/rstudio/shiny/issues).

## License

The shiny package is licensed under the GPLv3. See these files in the inst directory for additional details:

- COPYING - shiny package license (GPLv3)
- NOTICE  - Copyright notices for additional included software
