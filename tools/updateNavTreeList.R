library(sass)
home <- rprojroot::find_package_root_file("inst/www/shared/navtree")
# TODO: write a unit test
withr::with_dir(
  home, {
    sass_partial(
      sass_file("navtree.scss"),
      bslib::bs_theme(),
      output = "navtree.css",
      cache = FALSE,
      write_attachments = FALSE
    )
  }
)

