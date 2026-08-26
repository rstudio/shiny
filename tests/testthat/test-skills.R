# Guard tests for the shiny-for-r agent skill (inst/skills/shiny-for-r/).
#
# These tests protect four contracts:
#  - SKILL.md frontmatter stays valid for {btw} skill discovery at runtime.
#  - Every references/*.md linked from SKILL.md exists, and every file in
#    references/ is linked (no orphans, no dead links).
#  - Every R code chunk across the skill's markdown files parses as R.
#  - The shiny exports each reference file documents still exist, so a
#    renamed/removed export fails this test instead of silently rotting docs.
#
# skill_dir() resolves under both devtools::load_all() (inst/skills/...) and
# an installed package (skills/...), skipping cleanly if neither is found.

skill_dir <- function() {
  dir <- system.file("skills", "shiny-for-r", package = "shiny")
  skip_if(dir == "", "skill directory not found")
  dir
}

read_frontmatter <- function(path) {
  # {yaml} is a Suggests, so it is absent from the depends-only check.
  skip_if_not_installed("yaml")
  lines <- readLines(path)
  bounds <- which(lines == "---")
  yaml::yaml.load(paste(lines[(bounds[1] + 1):(bounds[2] - 1)], collapse = "\n"))
}

test_that("SKILL.md frontmatter is valid for btw discovery", {
  fm <- read_frontmatter(file.path(skill_dir(), "SKILL.md"))
  expect_identical(fm$name, "shiny-for-r")
  expect_match(fm$name, "^[a-z0-9][a-z0-9-]*$")
  expect_lte(nchar(fm$name), 64)
  expect_true(is.character(fm$description) && nzchar(fm$description))
  expect_lte(nchar(fm$description), 1024)
  allowed <- c("name", "description", "license", "compatibility", "metadata", "allowed-tools")
  expect_in(names(fm), allowed)
})

test_that("SKILL.md links and reference files match one-to-one", {
  skill <- readLines(file.path(skill_dir(), "SKILL.md"))
  # Keep the character class wide enough to match any file that could land in
  # references/; a narrower one would report a new file as an orphan rather
  # than as a link, which reads like the wrong failure.
  linked <- unique(unlist(regmatches(skill, gregexpr("references/[a-zA-Z0-9._-]+\\.md", skill))))
  actual <- file.path("references", list.files(file.path(skill_dir(), "references")))
  expect_setequal(linked, actual)
})

test_that("every R chunk in the skill parses", {
  files <- list.files(skill_dir(), pattern = "\\.md$", recursive = TRUE, full.names = TRUE)
  for (f in files) {
    lines <- readLines(f)
    starts <- grep("^```r\\s*$", lines)
    ends <- grep("^```\\s*$", lines)
    for (s in starts) {
      e <- ends[ends > s][1]
      # An empty chunk (``` immediately after ```r) has no body; `(s + 1):(e -
      # 1)` would count backwards and grab the fences themselves.
      code <- if (e > s + 1) paste(lines[(s + 1):(e - 1)], collapse = "\n") else ""
      # `message =` on expect_no_error() filters which conditions count as a
      # failure, so it cannot carry a "which chunk?" label; catch the error
      # and put the location in `info` instead.
      err <- tryCatch(parse(text = code), error = identity)
      failed <- inherits(err, "error")
      expect_false(
        failed,
        info = sprintf(
          "%s line %d: %s",
          basename(f), s, if (failed) conditionMessage(err) else ""
        )
      )
    }
  }
})

test_that("shiny APIs documented in references still exist", {
  # Explicit sync list: reference file -> exported shiny functions it documents.
  # When renaming an export, update the reference file AND this list.
  #
  # Every file in references/ appears here, so a new reference is not silently
  # unguarded. Files that mostly document other packages (bslib in the two
  # dashboard-* files, promises in async.md) still list the shiny exports they
  # lean on.
  apis <- list(
    reactivity = c("reactive", "observe", "observeEvent", "eventReactive", "reactiveVal", "reactiveValues", "bindEvent", "req", "isolate", "invalidateLater", "reactivePoll", "reactiveFileReader", "bindCache"),
    modules = c("moduleServer", "NS"),
    `session-lifecycle` = c("onSessionEnded", "onStop", "onUnhandledError", "onFlush", "onFlushed"),
    `extended-tasks` = c("ExtendedTask"),
    async = c("observe", "observeEvent", "renderText", "textOutput"),
    layouts = c("fluidPage", "sidebarLayout", "sidebarPanel", "mainPanel", "fluidRow", "column", "fillPage", "titlePanel", "wellPanel"),
    navigation = c("tabsetPanel", "tabPanel", "navbarPage", "navbarMenu", "navlistPanel", "tabPanelBody", "updateTabsetPanel", "updateNavbarPage", "updateNavlistPanel", "showTab", "hideTab"),
    `dashboard-components` = c("plotOutput", "renderPlot", "selectInput", "sliderInput", "checkboxInput"),
    `dashboard-design` = c("reactive", "selectInput"),
    `dynamic-ui` = c("renderUI", "uiOutput", "insertUI", "removeUI", "conditionalPanel", "updateSelectInput", "updateCheckboxGroupInput", "updateSliderInput", "updateTextInput", "freezeReactiveValue"),
    `theming-assets` = c("includeCSS", "includeScript", "addResourcePath"),
    plots = c("renderPlot", "plotOutput", "nearPoints", "brushedPoints", "clickOpts", "hoverOpts", "brushOpts", "renderCachedPlot", "renderImage", "imageOutput"),
    tables = c("renderTable", "tableOutput", "renderDataTable", "dataTableOutput", "debounce", "throttle", "eventReactive", "bindEvent"),
    files = c("fileInput", "downloadHandler", "downloadButton", "downloadLink", "outputOptions"),
    feedback = c("showNotification", "removeNotification", "modalDialog", "modalButton", "showModal", "removeModal", "withProgress", "incProgress", "validate", "need"),
    bookmarking = c("enableBookmarking", "bookmarkButton", "onBookmark", "onBookmarked", "onRestore", "onRestored", "setBookmarkExclude", "updateQueryString", "reactiveValuesToList"),
    `custom-components` = c("registerInputHandler"),
    testing = c("testServer", "exportTestValues"),
    debugging = c("req", "validate", "need", "reactlogShow"),
    opentelemetry = c("withOtelCollect", "localOtelCollect"),
    ecosystem = c("reactlogShow")
  )
  # A reference with no entry above would pass this test by doing nothing, so
  # check the coverage itself.
  refs <- sub("\\.md$", "", list.files(file.path(skill_dir(), "references")))
  expect_setequal(names(apis), refs)

  exports <- getNamespaceExports("shiny")
  for (ref in names(apis)) {
    missing <- setdiff(apis[[ref]], exports)
    if (length(missing)) {
      fail(sprintf("references/%s.md documents non-existent exports: %s", ref, toString(missing)))
    } else {
      succeed()
    }
  }
})
