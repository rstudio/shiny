jq_cdn_download <- function(version) {
  srcs <- c(".min.js", ".min.map", ".js")
  download.file(
    file.path("https://code.jquery.com", paste0("jquery-", version, srcs)),
    file.path("inst", "www", "shared",  paste0("jquery", srcs))
  )
}

jq_cdn_download("3.5.1")

download.file(
  "https://raw.githubusercontent.com/jquery/jquery/master/AUTHORS.txt",
  "inst/www/shared/jquery-AUTHORS.txt"
)

