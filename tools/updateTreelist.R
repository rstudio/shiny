library(sass)
home <- rprojroot::find_package_root_file("inst/www/shared/treelist")

withr::with_dir(
  home, {
    sass_partial(
      sass_file("treelist.scss"),
      bslib::bs_theme(),
      output = "treelist.css",
      cache = FALSE,
      write_attachments = FALSE
    )
  }
)

