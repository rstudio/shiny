# Shiny 

The R package **shiny** makes it incredibly easy to build interactive web applications with R. Shiny features automatic "reactive" data binding between user inputs and outputs as well as pre-built widgets for displaying tables, plots, and dynamic text. Shiny applications can be built entirely with R or can optionally make use of custom HTML, CSS, and JavaScript.


## Installation

You can install shiny directly from github using the `devtools` package. In order to install shiny you'll also need to be configured to build R packages from source. To install devtools and verify that you can build packages from source:

```r
install.packages("devtools")
library(devtools)
has_devel()
```
If the `has_devel` function produces an error then you'll need to install some additional components required to build packages from source:

- *Windows* -- Install the appropriate [Rtools](http://cran.r-project.org/bin/windows/Rtools/) binaries for your version of R.
- *MacOS X* -- Install the [Command Line Tools for Xcode](https://developer.apple.com/downloads/).
- *Debian/Ubuntu* -- `sudo apt-get install r-base-dev`

Next, you need to install a modified version of the `websockets` package from github:

```r
install_github("R-websockets", "jcheng5")
```

Finally, you can install the `shiny` package from github:

```r
install_github("shiny", "rstudio")
```

To verify that Shiny is installed correctly you can run an example application:

```r
library(shiny)
runExample("01_hello")
```

If everything is installed and working correctly a browser will open and navigate to the running application. 

## Tutorial

See the tutorial here: 

## Feedback 

Please use the github issue tracker

## License

The shiny package is licensed under the GPLv3. See these files in the inst
directory for additional details:

- COPYING - shiny package license (GPLv3)
- NOTICE  - Copyright notices for additional included software
