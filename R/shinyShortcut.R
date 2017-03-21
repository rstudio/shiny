#' Create Shiny App Shortcut
#'
#' Creates an executable file that, when ran, will open the co-located shiny
#' app using the user's default internet browser.
#'
#' @param shinyDirectory The top-level directory containing shiny code and
#' materials. Also where the final shortcut file will be saved.
#' @param OS The operating system that R is being ran on.
#'
#' @details Calling \code{shinyShortcut} will write an executable file
#' that when called will run the shiny app in the default browser.
#' This executable is stored in a created folder named  .shiny_run'
#' whilst a shortcut that will silently run the executable
#' is saved in the home directory as 'shinyShortcut' (this will
#' be a .vbs script on windows or a .desktop file on unix systems).
#'
#' @examples
#' shinyShortcut()
#' @export
shinyShortcut <- function(shinyDirectory = getwd(), OS = .Platform$OS.type) {

  # if relevant files exist delete them first
  unlink(".shiny_run", recursive = TRUE, force = TRUE)
  unlink("shiny_run.desktop", recursive = FALSE, force = TRUE)
  unlink("shinyShortcut.vbs", recursive = FALSE, force = TRUE)
  unlink("shinyShortcut.cmd", recursive = FALSE, force = TRUE)

  dir.create(paste(shinyDirectory, ".shiny_run", sep = "/"))

  if (OS == "windows") {

    # write batch file to .shiny_run
    rscriptForwardDash <-
      paste(R.home(), "bin/Rscript.exe", sep = "/")
    rscript <- gsub("/", "\\\\", rscriptForwardDash)

    shinyCommand <- paste0(
      "shiny::runApp('", shinyDirectory, "',",
      " launch.browser = TRUE)"
    )

    batchCode <- paste0("\"", rscript, "\"", " -e ",
                        "\"", shinyCommand, "\"")

    batchReference <- paste0(shinyDirectory,
                             "/.shiny_run",
                             "/shinyShortcut.cmd")

    write(batchCode, batchReference)

    # write vbs file to home directory
    batchReferenceBackDash <-
      gsub("/", "\\\\", batchReference)

    vbsCode <- paste0(
      "Set objShell = WScript.CreateObject(\"WScript.Shell\")",
      "\n",
      "objShell.Run(\"",
      batchReferenceBackDash,
      "\"), 0, True"
    )

    vbsReference <- paste0(shinyDirectory,
                           "/shinyShortcut.vbs")

    write(vbsCode, vbsReference)

  } else if (OS == "unix"){

    # write bash file to .shiny_run
    rscript <-
      paste("#!", R.home(), "bin/Rscript", sep = "/")

    shinyCommand <- paste0(
      "shiny::runApp('", shinyDirectory, "',",
      " launch.browser = TRUE)"
    )

    bashCode <- paste0(rscript, "\n\n",
                       shinyCommand)

    bashReference <- paste0(shinyDirectory,
                            "/.shiny_run",
                            "/shinyShortcut.r")

    write(bashCode, bashReference)

    # make executable
    system(paste0("chmod +x ", bashReference))

    # add shortuct in home directory
    shortcut_code <- paste0(
      "[Desktop Entry]\n",
      "Name=shinyShortcut\n",
      "Comment=Run Shiny App\n",
      "Exec=", bashReference, "\n",
      #"Icon=/home/user/youricon.gif\n",
      "Terminal=false\n",
      "Type=Application")

    shortcut_reference <- paste0(shinyDirectory,
                                 "/shinyShortcut.desktop")

    write(shortcut_code, shortcut_reference)

    # make executable
    system(paste0("chmod +x ", shortcut_reference))

  } else stop("OS must be one of \"windows\" or \"unix\"")
}
