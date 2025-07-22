# shiny <img src="man/figures/logo.png" align="right" width=120 height=139 alt="" />

<!-- badges: start -->
[![CRAN](https://www.r-pkg.org/badges/version/shiny)](https://CRAN.R-project.org/package=shiny)
[![R build status](https://github.com/rstudio/shiny/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rstudio/shiny/actions)
[![RStudio community](https://img.shields.io/badge/community-shiny-blue?style=social&logo=rstudio&logoColor=75AADB)](https://forum.posit.co/new-topic?category=shiny&tags=shiny)

<!-- badges: end -->

Easily build rich and productive interactive web apps in R &mdash; no HTML/CSS/JavaScript required.

## Features

* An intuitive and extensible [reactive programming](https://en.wikipedia.org/wiki/Reactive_programming) model which makes it easy to transform existing R code into a "live app" where outputs automatically react to new user input.
  * Compared to event-based programming, reactivity allows Shiny to do the minimum amount of work when input(s) change, and allows humans to more easily reason about complex [MVC logic](https://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93controller).
* A prebuilt set of highly sophisticated, customizable, and easy-to-use widgets (e.g., plots, tables, sliders, dropdowns, date pickers, and more).
* An attractive default look based on [Bootstrap](https://getbootstrap.com/) which can also be easily customized with the [bslib](https://github.com/rstudio/bslib) package or avoided entirely with more direct R bindings to HTML/CSS/JavaScript.
* Seamless integration with [R Markdown](https://shiny.rstudio.com/articles/interactive-docs.html), making it easy to embed numerous applications natively within a larger dynamic document.
* Tools for improving and monitoring performance, including native support for [async programming](https://posit.co/blog/shiny-1-1-0/), [caching](https://talks.cpsievert.me/20201117), [load testing](https://rstudio.github.io/shinyloadtest/), and more.
* [Modules](https://shiny.rstudio.com/articles/modules.html): a framework for reducing code duplication and complexity.
* An ability to [bookmark application state](https://shiny.rstudio.com/articles/bookmarking-state.html) and/or [generate code to reproduce output(s)](https://github.com/rstudio/shinymeta).
* A rich ecosystem of extension packages for more [custom widgets](http://www.htmlwidgets.org/), [input validation](https://github.com/rstudio/shinyvalidate), [unit testing](https://github.com/rstudio/shinytest), and more.

## Installation

To install the stable version from CRAN:

```r
install.packages("shiny")
```

## Getting Started

Once installed, load the library and run an example:

```r
library(shiny)
# Launches an app, with the app's source code included
runExample("06_tabsets")
# Lists more prepackaged examples
runExample()
```

For more examples and inspiration, check out the [Shiny User Gallery](https://shiny.rstudio.com/gallery/).

For help with learning fundamental Shiny programming concepts, check out the [Mastering Shiny](https://mastering-shiny.org/) book and the [Shiny Tutorial](https://shiny.rstudio.com/tutorial/). The former is currently more up-to-date with modern Shiny features, whereas the latter takes a deeper, more visual, dive into fundamental concepts.

## Join the conversation

If you want to chat about Shiny, meet other developers, or help us decide what to work on next, [join us on Discord](https://discord.com/invite/yMGCamUMnS).

## Getting Help

To ask a question about Shiny, please use the [RStudio Community website](https://forum.posit.co/new-topic?category=shiny&tags=shiny).

For bug reports, please use the [issue tracker](https://github.com/rstudio/shiny/issues) and also keep in mind that by [writing a good bug report](https://github.com/rstudio/shiny/wiki/Writing-Good-Bug-Reports), you're more likely to get help with your problem.

## Contributing

We welcome contributions to the **shiny** package. Please see our [CONTRIBUTING.md](https://github.com/rstudio/shiny/blob/main/.github/CONTRIBUTING.md) file for detailed guidelines of how to contribute.

## License

The shiny package as a whole is licensed under the GPLv3. See the [LICENSE](LICENSE) file for more details.

## R version support

Shiny is supported on the latest release version of R, as well as the previous four minor release versions of R. For example, if the latest release R version is 4.3, then that version is supported, as well as 4.2, 4.1, 4.0, 3.6.
