# shiny <img src="man/figures/logo.png" align="right" width=120 height=139 alt="" />

<!-- badges: start -->
[![CRAN](https://www.r-pkg.org/badges/version/shiny)](https://CRAN.R-project.org/package=shiny)
[![R build status](https://github.com/rstudio/shiny/workflows/R-CMD-check/badge.svg)](https://github.com/rstudio/shiny/actions)
[![RStudio community](https://img.shields.io/badge/community-shiny-blue?style=social&logo=rstudio&logoColor=75AADB)](https://community.rstudio.com/new-topic?category=shiny&tags=shiny)

<!-- badges: end -->

Shiny is a new package from RStudio that makes it incredibly easy to build interactive web applications with R.

For an introduction and examples, visit the [Shiny Dev Center](http://shiny.rstudio.com/).

If you have general questions about using Shiny, please use the [RStudio Community website](https://community.rstudio.com). For bug reports, please use the [issue tracker](https://github.com/rstudio/shiny/issues).

## Features

* Build useful web applications with only a few lines of code&mdash;no JavaScript required.
* Shiny applications are automatically "live" in the same way that spreadsheets are live. Outputs change instantly as users modify inputs, without requiring a reload of the browser.
* Shiny user interfaces can be built entirely using R, or can be written directly in HTML, CSS, and JavaScript for more flexibility.
* Works in any R environment (Console R, Rgui for Windows or Mac, ESS, StatET, RStudio, etc.).
* Attractive default UI theme based on [Bootstrap](http://getbootstrap.com/).
* A highly customizable slider widget with built-in support for animation.
* Prebuilt output widgets for displaying plots, tables, and printed output of R objects.
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
if (!require("remotes"))
  install.packages("remotes")
remotes::install_github("rstudio/shiny")
```

## Getting Started

To learn more we highly recommend you check out the [Shiny Tutorial](http://shiny.rstudio.com/tutorial/). The tutorial explains the framework in-depth, walks you through building a simple application, and includes extensive annotated examples.

## Bootstrap 3 migration

Shiny versions 0.10.2.2 and below used the Bootstrap 2 web framework. After 0.10.2.2, Shiny switched to Bootstrap 3. For most users, the upgrade should be seamless. However, if you have have customized your HTML-generating code to use features specific to Bootstrap 2, you may need to update your code to work with Bootstrap 3.

If you do not wish to update your code at this time, you can use the [shinybootstrap2](https://github.com/rstudio/shinybootstrap2) package for backward compatibility.

If you prefer to install an older version of Shiny, you can do it using the devtools package:

```R
devtools::install_version("shiny", version = "0.10.2.2")
```

## Development notes

The Javascript code in Shiny is minified using tools that run on Node.js. See the tools/ directory for more information.

## Guidelines for contributing

We welcome contributions to the **shiny** package. Please see our [CONTRIBUTING.md](https://github.com/rstudio/shiny/blob/master/.github/CONTRIBUTING.md) file for detailed guidelines of how to contribute.

## License

The shiny package as a whole is licensed under the GPLv3. See the [LICENSE](LICENSE) file for more details.
