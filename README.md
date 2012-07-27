# Shiny 

Shiny is a new package from RStudio that makes it incredibly easy to build interactive web applications with R. Building applications with Shiny has many compelling benefits, including:

* [Reactive](http://en.wikipedia.org/wiki/Reactive_programming) data-binding between inputs and outputs that makes it possible to build applications with only a few lines of code.
* Shiny user-interfaces are "live" in the same way that spreadsheets are live. Outputs change instantly as users modify inputs without explicit programming or requiring a reload of the browser.
* Applications can be built entirely using R, or can optionally make use of custom HTML, CSS, and JavaScript.
* A highly customizable slider control that has built-in support for animation.
* Pre-built output widgets for displaying plots, tables, and printed output of R objects.
* Fast bi-directional communication between the web browser and R using the [websockets](http://illposed.net/websockets.html) package.

## Installation


Shiny is currently available only directly from this GitHub repository (it will be on CRAN in a few months). You can download the Shiny source code using one of these methods:

* [Download tar.gz](https://github.com/rstudio/shiny/tarball/master) 
* [Download zip](https://github.com/rstudio/shiny/zipball/master)
* `git clone https://github.com/rstudio/shiny.git` **[Recommended]**

Once you've extracted the Shiny source code to a directory you can install it as follows:

```r
install.packages(c("websockets", "RJSONIO", "xtable"))
system("R CMD INSTALL <shiny-dir>")
```

To verify that Shiny is installed correctly you can run an example application:

```r
library(shiny)
runExample("01_hello")
```

If everything is installed and working correctly a browser will open and navigate to the running application. 

## Tutorial

To learn more about Shiny we highly recommend you check out the [Shiny Tutorial](http://rstudio.github.com/shiny/tutorial). The tutorial walks you through building a simple application and demostrates nearly all of the Shiny features.

## License

The shiny package is licensed under the GPLv3. See these files in the inst directory for additional details:

- COPYING - shiny package license (GPLv3)
- NOTICE  - Copyright notices for additional included software
