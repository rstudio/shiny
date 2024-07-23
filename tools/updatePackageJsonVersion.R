# Updates the package.json `version` to match the DESCRIPTION `Version`
pkg <- jsonlite::read_json("package.json")
version <- as.list(read.dcf("DESCRIPTION")[1,])$Version
pkg$version <- gsub("^(\\d+).(\\d+).(\\d+).(.+)$", "\\1.\\2.\\3-alpha.\\4", version)
pkg$files <- as.list(pkg$files)
jsonlite::write_json(pkg, path = "package.json", pretty = TRUE, auto_unbox = TRUE)
