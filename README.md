# shiny <img src="man/figures/logo.png" align="right" width=120 height=139 alt="" />

<!-- badges: start -->
[![CRAN](https://www.r-pkg.org/badges/version/shiny)](https://CRAN.R-project.org/package=shiny)
[![R build status](https://github.com/rstudio/shiny/workflows/R-CMD-check/badge.svg)](https://github.com/rstudio/shiny/actions)
[![RStudio community](https://img.shields.io/badge/community-shiny-blue?style=social&logo=rstudio&logoColor=75AADB)](https://community.rstudio.com/new-topic?category=shiny&tags=shiny)

<!-- badges: end -->

Easily build rich and productive interactive web apps in R &mdash; no HTML/CSS/JavaScript required.

## Features

* An intuitive and extensible [reactive](https://en.wikipedia.org/wiki/Reactive_programming) programming model which makes it easy to transform existing R code into a "live app" where outputs automatically react to new user input.
  * This programming model naturally leads to more efficient and simpler code then the alternative (i.e., event-based programming) because, when Shiny receives new user input, it intelligently finds the minimal amount of work necessary to update output(s).
* A prebuilt set of highly sophisticated, customizable, and easy-to-use widgets (e.g., plots, tables, sliders, dropdowns, date pickers, and more).
* An attractive default look based on [Bootstrap](https://getbootstrap.com/) which can also be easily customized with the [bslib](https://github.com/rstudio/bslib) package or avoided entirely with more direct R bindings to HTML/CSS/JavaScript.
* Seamless integration with [R Markdown](https://beta.rstudioconnect.com/content/2671/Combining-Shiny-R-Markdown.html), making it easy to embed numerous applications natively within a larger dynamic document.
* Tools for improving and monitoring performance, including native support for [async programming](https://blog.rstudio.com/2018/06/26/shiny-1-1-0/), [caching](https://talks.cpsievert.me/20201117), [load testing](https://rstudio.github.io/shinyloadtest/), and [more](https://support.rstudio.com/hc/en-us/articles/231874748-Scaling-and-Performance-Tuning-in-RStudio-Connect).
* [A framework](https://shiny.rstudio.com/articles/modules.html) for reducing code duplication and complexity.
* An ability to [bookmark application state](https://shiny.rstudio.com/articles/bookmarking-state.html) and/or [generate code to reproduce its output](https://github.com/rstudio/shinymeta).
* A rich ecosystem of extension packages for more [custom widgets](http://www.htmlwidgets.org/), [input validation](https://github.com/rstudio/shinyvalidate), [unit testing](https://github.com/rstudio/shinytest), and more.

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

There are currently two great places to get started with learning fundamental Shiny concepts, both the [Shiny Tutorial](https://shiny.rstudio.com/tutorial/) and the [Mastering Shiny](https://mastering-shiny.org/) book. The latter is more up-to-date with modern Shiny features, whereas the former takes a deeper, more visual, dive into fundamental concepts. 

For inspiration from other Shiny users, you may also want to consider checking out the [Shiny User Gallery](https://shiny.rstudio.com/gallery/).

## Contributing

We welcome contributions to the **shiny** package. Please see our [CONTRIBUTING.md](https://github.com/rstudio/shiny/blob/master/.github/CONTRIBUTING.md) file for detailed guidelines of how to contribute.

## License

The shiny package as a whole is licensed under the GPLv3. See the [LICENSE](LICENSE) file for more details.
