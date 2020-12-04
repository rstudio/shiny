
# revdepcheck::revdep_reset()

# maximize output width
options(width = as.numeric(system2("tput", "cols", stdout = TRUE)) - 10)
revdepcheck::revdep_check(pkgload::pkg_path("."), num_workers = detectCores() - 2, bioc = TRUE, timeout = as.difftime(30, units = "mins"))

# save cran comments to a file
cat(file = file.path(pkgload::pkg_path("."), "revdep", "revdep-cran-comments.md"), paste0(collapse = "\n", capture.output({revdepcheck::revdep_report_cran()})))
