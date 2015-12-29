# Assume the working dir is the root dir of the shiny source package, and the
# shiny-examples repo (https://github.com/rstudio/shiny-examples) is under the
# same dir as the shiny package
library(shiny)


# Get the current locale settings in a named vector. According to
# ?Sys.setlocale, only some categories are set when setting LC_ALL. Since these
# tests set LC_ALL, we only need to keep these categories to restore later. Also,
# trying to restore some categories with Sys.setlocale will result in an error;
# these categories should be OK.
getLocale <- function() {
  categories <- c("LC_COLLATE", "LC_CTYPE", "LC_MONETARY",  "LC_TIME")

  locale <- strsplit(Sys.getlocale(), ";")[[1]]
  names(locale) <- sub("=.*", "", locale)
  locale <- sub(".*=", "", locale)
  locale <- locale[names(locale) %in% categories]
  locale
}

restoreLocale <- function(locale) {
  mapply(Sys.setlocale, names(locale), locale)
}

withLocale <- function(locale, expr) {
  oldLocale <- getLocale()
  on.exit(restoreLocale(oldLocale))
  Sys.setlocale(locale = locale)

  force(expr)
}


if (.Platform$OS.type == 'windows') {
  locales <- c('Chinese', 'English')
} else {
  # On non-Windows, only test with the default locale
  locales <- getLocale()["LC_CTYPE"]
}

apps <- list.files('tests/test-encoding/', '^[0-9]{2}-', full.names = TRUE)
apps <- c(apps, '../shiny-examples/022-unicode-chinese/')

# Run these apps in the RStudio preview window one by one in different locales.
# Note 022-unicode-chinese may not work well in the English locale (explained at
# https://github.com/rstudio/shiny/pull/968)
for (app in apps) {
  for (locale in locales) {
    withLocale(locale, {
      cat("Running app: ", app, "\n")
      cat("Locale: ", Sys.setlocale(locale = locale))
      tryCatch(shiny::runApp(app), interrupt = function(e) {})
    })
  }
}
