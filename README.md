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
* Attractive default UI theme based on [Bootstrap](http://getbootstrap.com/) as well as easy customization of colors and fonts via [bootstraplib](https://rstudio.github.io/bootstraplib/) package.
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
if (!require("remotes")) {
 install.packages("remotes")
}
remotes::install_github("rstudio/shiny")
```

## Getting Started

To learn more we highly recommend going through:

* The [Shiny Tutorial](https://shiny.rstudio.com/tutorial/) which explains the framework in-depth, walks you through building a simple application, and includes extensive annotated examples.

* The [Shiny Gallery](https://shiny.rstudio.com/gallery/) to see a variety of examples that demonstrate the functionality and power of the framework.

* [Shiny Articles](https://shiny.rstudio.com/articles/) to dive further into more advanced topics.

## A Note on Shiny UI

Some Shiny UI functions implicitly require [Bootstrap](https://getbootstrap.com/) (e.g., `fluidPage()`, `column()`, etc). These functions, import Bootstrap 3 by default, but are also compatible with **bootstraplib**'s patched version of Bootstrap 4+. See [**bootstraplib**'s website](https://rstudio.github.io/bootstraplib) for more on using different versions of Bootstrap, using Bootswatch themes, as well as [customizing Bootstrap's default styles](https://rstudio.github.io/bootstraplib/articles/recipes.html) from R (no CSS required!).

Note also that the Shiny framework doesn't necessarily force the use of Bootstrap (or any other HTML/CSS framework for that matter), but avoiding Bootstrap will likely require writing your own HTML/CSS (in such case, [`htmlTemplate()` can be useful](https://shiny.rstudio.com/articles/templates.html)).

## Development notes

The Javascript code in Shiny is minified using tools that run on Node.js. See the `tools/` directory for more information.

## Guidelines for contributing

We welcome contributions to the **shiny** package. Please see our [CONTRIBUTING.md](https://github.com/rstudio/shiny/blob/master/.github/CONTRIBUTING.md) file for detailed guidelines of how to contribute.

## License

The shiny package as a whole is licensed under the GPLv3. See the [LICENSE](LICENSE) file for more details.
