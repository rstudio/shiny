# source("tools/updateHtmltoolsMan.R")
# Will update all man files that are re-exported from htmltools
# Will save all aliases to `./R/htmltools.R` and document to enforce all re-exports

library(magrittr)

local({

  htmltools_github_man_location <- "https://raw.githubusercontent.com/rstudio/htmltools/master/man/"

  local_man_folder <- rprojroot::find_package_root_file("man")
  local_htmltools_r_file <- rprojroot::find_package_root_file("R/htmltools.R")

  alias_list <- list()

  update_htmltools_man_file <- function(man_file, ignore = NULL) {
    lines <- paste0(htmltools_github_man_location, man_file) %>%
      readLines() %>%
      { .[-(1:2)] } # remove first two roxygen2 comments

    man_file_path <- file.path(local_man_folder, man_file)
    lines %>%
      paste0(collapse = "\n") %>%
      writeLines(man_file_path)
    message("Updated: ", man_file_path)

    alias_list[[man_file]] <<-
      lines[grepl("\\alias{", lines, fixed = TRUE)] %>%
      sub("\\alias{", "", ., fixed = TRUE) %>%
      sub("}$", "", .) %>%
      setdiff(ignore)
  }

  update_htmltools_man_file("builder.Rd", "builder")
  update_htmltools_man_file("tag.Rd")
  update_htmltools_man_file("HTML.Rd")
  update_htmltools_man_file("include.Rd", "include")
  update_htmltools_man_file("singleton.Rd")
  update_htmltools_man_file("validateCssUnit.Rd")
  update_htmltools_man_file("htmlTemplate.Rd")
  update_htmltools_man_file("suppressDependencies.Rd")
  update_htmltools_man_file("withTags.Rd")


  alias_list %>%
    vapply(paste0, collapse = " ", character(1), USE.NAMES = FALSE) %>%
    paste0("#' @export ", .) %>%
    paste0(collapse = "\n") %>%
    paste0("#' @import htmltools\n", ., "\nNULL") %>%
    writeLines(local_htmltools_r_file)
  message("Updated: ", local_htmltools_r_file)

  devtools::document()
})
