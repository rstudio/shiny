# Assume the working dir is the root dir of the shiny source package, and the
# shiny-examples repo (https://github.com/rstudio/shiny-examples) is under the
# same dir as the shiny package
library(shiny)
apps <- list.files('tests/test-encoding/', '^[0-9]{2}-', full.names = TRUE)
apps <- c(apps, '../shiny-examples/022-unicode-chinese/')
locales <- c('Chinese', 'English')

# Run these apps in the RStudio preview window one by one in different locales.
# Note 022-unicode-chinese may not work well in the English locale (explained at
# https://github.com/rstudio/shiny/pull/968)
for (app in apps) {
  for (locale in locales) {
    print(Sys.setlocale(locale = locale))
    tryCatch(shiny::runApp(app), interrupt = function(e) {})
  }
}
