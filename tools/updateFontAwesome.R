#!/usr/bin/env Rscript

# Note: This script contains some commented-out code which was originally from
# https://github.com/rstudio/shiny/pull/2221, but was never merged. We haven't
# implemented full support for the stuff in that PR, so the unused parts are
# commented out.

# This script can be run from the command line or sourced from an R session.
library(rprojroot)
library(jsonlite)

# =============================================================================
# Download and unzip to temp dir
# =============================================================================
version <- "5.13.0"

zip_file <- file.path(tempdir(), paste0("font-awesome-", version, ".zip"))

url <- paste0(
  "https://github.com/FortAwesome/Font-Awesome/releases/download/",
  version, "/fontawesome-free-", version, "-web.zip"
)
download.file(url, zip_file)

unzip(zip_file, exdir = tempdir())
source_dir <- file.path(tempdir(), paste0("fontawesome-free-", version, "-web"))

# =============================================================================
# Remove old stuff
# =============================================================================
dest_dir <- find_package_root_file("inst", "www", "shared", "fontawesome")
unlink(dest_dir, recursive = TRUE)

# =============================================================================
# Copy the files
# =============================================================================
copy_files <- function(srcdir, destdir, filenames) {
  # Create needed directories
  dest_subdirs <- file.path(destdir, unique(dirname(filenames)))
  for (dest_subdir in dest_subdirs) {
    dir.create(dest_subdir, recursive = TRUE)
  }

  res <- file.copy(
    from = paste0(srcdir,  "/", filenames),
    to   = paste0(destdir, "/", filenames)
  )

  if (!all(res)) {
    message("Problem copying ", sum(!res), " files: \n  ",
      paste0(filenames[!res], collapse = "\n  ")
    )
  }
}

filenames <- c(
  "css/all.css",
  "css/all.min.css",
  "css/v4-shims.css",
  "css/v4-shims.min.css",
  file.path("webfonts", dir(file.path(source_dir, "webfonts")))
)

copy_files(source_dir, dest_dir, filenames)

# # Save the icon metadata in data-raw/fontawesome/
# file.copy(
#   file.path(source_dir, "metadata", "icons.json"),
#   find_package_root_file("data-raw", "fontawesome")
# )

# =============================================================================
# Update the fa_icons data frame in R/sysdata.rda
# =============================================================================

# In Font-Awesome 5, there are four different CSS prefix classes: "fab", "fas",
# "far", and "fal" (which is only in the Pro version, so we won't use it here).
# These are for "brands", "solid", "regular", and "light", respectively. Each
# icon supports various combinations of these prefixes.
#
# The CSS prefix class for Font-Awesome 4 is "fa". Some of the icon names from
# V4 do not exist in V5, like "bank". Some of them do exist in V5, but the name
# now refers to a very different icon. This is true of "calendar". The
# v4-shims.css file will make "fa fa-calendar" (this is V4 syntax) display one
# icon (which is very similar to the V4 version), and "fas fa-calendar" (V5
# syntax) display a different icon.

# Find the icons that existed in version 4. These icons will have the CSS class
# "fa".
v4_shim_css <- readLines(find_package_root_file("inst", "www", "shared", "fontawesome", "css", "v4-shims.css"))
v4_icons <- v4_shim_css[grepl("^\\.fa", v4_shim_css)]
# Remove prefix ".fa.fa-" and suffix " {" or ":before {".
v4_icons <- sub("^\\.fa\\.fa-(.*?)(:before)? \\{$", "\\1", v4_icons)
v4_icons <- unique(v4_icons)

v4_icon_df <- data.frame(
  name = v4_icons,
  fa = TRUE,
  stringsAsFactors = FALSE
)

# Get info on icons in version 5.
metadata <- fromJSON(find_package_root_file("data-raw", "fontawesome", "icons.json"))
styles <- lapply(metadata, `[[`, "styles")

icon_df <- data.frame(
  name = names(metadata),
  fab  = vapply(styles, function(style) "brands"  %in% style, TRUE),
  fas  = vapply(styles, function(style) "solid"   %in% style, TRUE),
  far  = vapply(styles, function(style) "regular" %in% style, TRUE),
  stringsAsFactors = FALSE
)

fa_icons <- merge(v4_icon_df, icon_df, all = TRUE)

# Replace NA's with FALSE
fa_icons$fa [is.na(fa_icons$fa)]  <- FALSE
fa_icons$fab[is.na(fa_icons$fab)] <- FALSE
fa_icons$fas[is.na(fa_icons$fas)] <- FALSE
fa_icons$far[is.na(fa_icons$far)] <- FALSE

# The default behavior is to try to provide the v5 icon (for a given name) if
# available, and if not, then provide the v4 icon.
fa_icons$default <-
  ifelse(fa_icons$fab, "fab",
    ifelse(fa_icons$fas, "fas",
      ifelse(fa_icons$far, "far",
        ifelse(fa_icons$fa, "fa",
          NA_character_
        )
      )
    )
  )

# If the user specifies v4, then only use the v4 icon.
fa_icons$default_v4 <- ifelse(fa_icons$fa, "fa", NA_character_)

# If the user specifies v5, then only use the v5 icon.
fa_icons$default_v5 <-
  ifelse(fa_icons$fab, "fab",
    ifelse(fa_icons$fas, "fas",
      ifelse(fa_icons$far, "far",
        NA_character_
      )
    )
  )


# fa_version <- version

# Save to text for nice diffing
# message("Writing fa_icons and fa_version to data-raw")
# write.csv(
#   fa_icons,
#   file = find_package_root_file("data-raw", "fa_icons.csv"),
#   row.names = FALSE
# )
# writeLines(fa_version, find_package_root_file("data-raw", "fa_version.txt"))

# message("Writing fa_icons and fa_version to R/sysdata.rda")
# # Note: if in the future we add more objects to sysdata.rda, we'll have to do
# # this differently.
# save("fa_icons", "fa_version", file = find_package_root_file("R", "sysdata.rda"))

# Generate R/font-awesome.R, which contains a list of Brand icons.
font_awesome_r <- find_package_root_file("R", "font-awesome.R")
cat('font_awesome_brands <- c(\n  "', file = font_awesome_r)
cat(
  paste(fa_icons$name[fa_icons$fab], collapse = '",\n  "'),
  file = font_awesome_r,
  append = TRUE
)
cat('"\n)', file = font_awesome_r, append = TRUE)
