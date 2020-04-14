#' @include globals.R
NULL

# Given the name of a license, return the appropriate link HTML for the
# license, which may just be the name of the license if the name is
# unrecognized.
#
# Recognizes the 'standard' set of licenses used for R packages
# (see http://cran.r-project.org/doc/manuals/R-exts.html)
licenseLink <- function(licenseName) {
  licenses <- list(
    "GPL-2" = "https://gnu.org/licenses/gpl-2.0.txt",
    "GPL-3" = "https://gnu.org/licenses/gpl-3.0.txt",
    "LGPL-3" = "https://www.gnu.org/licenses/lgpl-3.0.txt",
    "LGPL-2" = "http://www.gnu.org/licenses/old-licenses/lgpl-2.0.txt",
    "LGPL-2.1" = "http://www.gnu.org/licenses/lgpl-2.1.txt",
    "AGPL-3" = "http://www.gnu.org/licenses/agpl-3.0.txt",
    "Artistic-2.0" = "http://www.r-project.org/Licenses/Artistic-2.0",
    "BSD_2_clause" = "http://www.r-project.org/Licenses/BSD_2_clause",
    "BSD_3_clause" = "http://www.r-project.org/Licenses/BSD_3_clause",
    "MIT" = "http://www.r-project.org/Licenses/MIT",
    "CC-BY-SA-4.0" = "https://www.r-project.org/Licenses/CC-BY-SA-4.0")
  if (exists(licenseName, where = licenses)) {
    tags$a(href=licenses[[licenseName]], licenseName)
  } else {
    licenseName
  }
}

# Returns tags containing showcase directives intended for the <HEAD> of the
# document.
showcaseHead <- function() {

  deps  <- list(
    htmlDependency("jqueryui", "1.12.1", c(href="shared/jqueryui"),
      script = "jquery-ui.min.js"),
    htmlDependency("showdown", "0.3.1", c(href="shared/showdown/compressed"),
      script = "showdown.js"),
    htmlDependency("highlight.js", "6.2", c(href="shared/highlight"),
      script = "highlight.pack.js")
  )

  mdfile <- file.path.ci(getwd(), 'Readme.md')
  html <- with(tags, tagList(
    script(src="shared/shiny-showcase.js"),
    link(rel="stylesheet", type="text/css",
         href="shared/highlight/rstudio.css"),
    link(rel="stylesheet", type="text/css",
         href="shared/shiny-showcase.css"),
    if (file.exists(mdfile))
      script(type="text/markdown", id="showcase-markdown-content",
        paste(readUTF8(mdfile), collapse="\n"))
    else ""
  ))

  return(attachDependencies(html, deps))
}

# Returns tags containing the application metadata (title and author) in
# showcase mode.
appMetadata <- function(desc) {
  cols <- colnames(desc)
  if ("Title" %in% cols)
    with(tags, h4(class="text-muted shiny-showcase-apptitle", desc[1,"Title"],
      if ("Author" %in% cols) small(
        br(), "by",
        if ("AuthorUrl" %in% cols)
          a(href=desc[1,"AuthorUrl"], class="shiny-showcase-appauthor",
            desc[1,"Author"])
        else
          desc[1,"Author"],
        if ("AuthorEmail" %in% cols)
          a(href=paste("mailto:", desc[1,"AuthorEmail"], sep = ''),
            class="shiny-showcase-appauthoreemail",
            desc[1,"AuthorEmail"])
        else "")
      else ""))
  else ""
}

navTabsHelper <- function(files, prefix = "") {
  lapply(files, function(file) {
    with(tags,
      li(class=if (tolower(file) %in% c("app.r", "server.r")) "active" else "",
         a(href=paste("#", gsub(".", "_", file, fixed=TRUE), "_code", sep=""),
           "data-toggle"="tab", paste0(prefix, file)))
    )
  })
}

navTabsDropdown <- function(files) {
  if (length(files) > 0) {
    with(tags,
      li(role="presentation", class="dropdown",
        a(class="dropdown-toggle", `data-toggle`="dropdown", href="#",
          role="button", `aria-haspopup`="true", `aria-expanded`="false",
          "www", span(class="caret")
        ),
        ul(class="dropdown-menu", navTabsHelper(files))
      )
    )
  }
}

tabContentHelper <- function(files, path, language) {
  lapply(files, function(file) {
      tags$div(class=paste("tab-pane",
                      if (tolower(file) %in% c("app.r", "server.r")) " active"
                      else "",
                      sep=""),
          id=paste(gsub(".", "_", file, fixed=TRUE),
                   "_code", sep=""),
          tags$pre(class="shiny-code",
              # we need to prevent the indentation of <code> ... </code>
              HTML(format(tags$code(
                class=paste0("language-", language),
                paste(readUTF8(file.path.ci(path, file)), collapse="\n")
              ), indent = FALSE))))
  })
}

# Returns tags containing the application's code in Bootstrap-style tabs in
# showcase mode.
showcaseCodeTabs <- function(codeLicense) {
  rFiles <- list.files(pattern = "\\.[rR]$")
  wwwFiles <- list()
  if (isTRUE(.globals$IncludeWWW)) {
    path <- file.path(getwd(), "www")
    wwwFiles$jsFiles <- list.files(path, pattern = "\\.js$")
    wwwFiles$cssFiles <- list.files(path, pattern = "\\.css$")
    wwwFiles$htmlFiles <- list.files(path, pattern = "\\.html$")
  }
  with(tags, div(id="showcase-code-tabs",
    a(id="showcase-code-position-toggle",
      class="btn btn-default btn-sm",
      onclick="toggleCodePosition()",
      icon("level-up"),
      "show with app"),
    ul(class="nav nav-tabs",
       navTabsHelper(rFiles),
       navTabsDropdown(unlist(wwwFiles))
    ),
    div(class="tab-content", id="showcase-code-content",
        tabContentHelper(rFiles, path = getwd(), language = "r"),
        tabContentHelper(wwwFiles$jsFiles,
                         path = paste0(getwd(), "/www"),
                         language = "javascript"),
        tabContentHelper(wwwFiles$cssFiles,
                         path = paste0(getwd(), "/www"),
                         language = "css"),
        tabContentHelper(wwwFiles$htmlFiles,
                         path = paste0(getwd(), "/www"),
                         language = "xml")
    ),
    codeLicense))
}

# Returns tags containing the showcase application information (readme and
# code).
showcaseAppInfo <- function() {
  descfile <- file.path.ci(getwd(), "DESCRIPTION")
  hasDesc <- file.exists(descfile)
  readmemd <- file.path.ci(getwd(), "Readme.md")
  hasReadme <- file.exists(readmemd)
  if (hasDesc) {
    con <- textConnection(readUTF8(descfile))
    on.exit(close(con), add = TRUE)
    desc <- read.dcf(con)
  }
  with(tags,
    div(class="container-fluid shiny-code-container well",
        id="showcase-well",
        div(class="row",
          if (hasDesc || hasReadme) {
            div(id="showcase-app-metadata", class="col-sm-4",
                if (hasDesc) appMetadata(desc) else "",
                if (hasReadme) div(id="readme-md"))
          } else "",
          div(id="showcase-code-inline",
              class=if (hasReadme || hasDesc) "col-sm-8" else "col-sm-10 col-sm-offset-1",
              showcaseCodeTabs(
                if (hasDesc && "License" %in% colnames(desc)) {
                  small(class="showcase-code-license text-muted",
                        "Code license: ",
                        licenseLink(desc[1,"License"]))
                } else "")))))
}


# Returns the body of the showcase document, given the HTML it should wrap.
showcaseBody <- function(htmlBody) {
  with(tags, tagList(
    table(id="showcase-app-code",
          tr(td(id="showcase-app-container",
                class="showcase-app-container-expanded",
                htmlBody,
             td(id="showcase-sxs-code",
                class="showcase-sxs-code-collapsed")))),
    showcaseAppInfo()))
}

# Sets the defaults for showcase mode (for app boot).
setShowcaseDefault <- function(showcaseDefault) {
  .globals$showcaseDefault <- showcaseDefault
  .globals$showcaseOverride <- as.logical(showcaseDefault)
}


# Given a UI tag/tagList, wrap it in appropriate tags for showcase mode.
showcaseUI <- function(ui) {
  # If top-level tag is a body, replace its children with children wrapped in
    # showcase stuff.
  if (inherits(ui, "shiny.tag") && ui$name == "body") {
    ui$children <- showcaseUI(ui$children)
    return(ui)
  }

  tagList(
    tags$head(showcaseHead()),
    showcaseBody(ui)
  )
}

