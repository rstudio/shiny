# Shiny 

Shiny is a new package from RStudio that makes it incredibly easy to build interactive web applications with R. Building applications with Shiny has many compelling benefits, including:

* [Reactive](http://en.wikipedia.org/wiki/Reactive_programming) data-binding between inputs and outputs that makes it possible to build applications with only a few lines of code.
* Shiny user-interfaces are "live" in the same way that spreadsheets are live. Outputs change instantly as users modify inputs without explicit programming or requiring a reload of the browser.
* Applications can be built entirely using R, or can optionally make use of custom HTML, CSS, and JavaScript.
* A highly customizable slider control that has built-in support for animation.
* Pre-built output widgets for displaying plots, tables, and printed output of R objects.
* Fast bi-directional communication between the web browser and R using the [websockets](http://illposed.net/websockets.html) package.

## Installation


Shiny is currently available only directly from this GitHub repository (it will be on CRAN in a few months). The purpose of the current release is to gather feedback so we can improve and evolve Shiny's capabilities. As a result there may be breaking changes as we make improvements--these will all be clearly documented so that it's as straightforward as possible to migrate code that may have broken.

You can download the Shiny source code using one of these methods:

* [Download tar.gz](https://github.com/rstudio/shiny/tarball/master) 
* [Download zip](https://github.com/rstudio/shiny/zipball/master)
* `git clone git@github.com:rstudio/shiny.git` **[Recommended]**

Once you've extracted the Shiny source code to a directory you can install it using the [devtools](http://cran.r-project.org/web/packages/devtools/index.html) package as follows:

```r
library(devtools); install("<shiny-dir>");
```

Note that if you are on Windows installing Shiny from source also requires that have you previously installed the appropriate [Rtools](http://cran.r-project.org/bin/windows/Rtools/) binaries for your version of R.

## Getting Started

To learn more we highly recommend you check out the [Shiny Tutorial](http://rstudio.github.com/shiny/tutorial). The tutorial walks you through building a simple application and demostrates nearly all of the Shiny features.

We hope you enjoy using Shiny. As you learn more and work with the package please [let us know](https://github.com/rstudio/shiny/issues) what problems you encounter and how you'd like to see Shiny evolve.

## License

The shiny package is licensed under the GPLv3. See these files in the inst directory for additional details:

- COPYING - shiny package license (GPLv3)
- NOTICE  - Copyright notices for additional included software
