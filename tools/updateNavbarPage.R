library(sass)
home <- rprojroot::find_package_root_file("inst/www/shared/navbarPage")
# TODO: write a unit test
withr::with_dir(
  home, {
    sass_partial(
      sass_file("navbarPage.scss"),
      bslib::bs_theme(version = 3),
      output = "navbarPage.min.css",
      options = sass::sass_options(output_style = "compressed"),
      cache = FALSE,
      write_attachments = FALSE
    )
  }
)
