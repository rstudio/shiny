#' Run a Shiny App from a Package
#'
#' \code{shiny_demo} is a user-friendly interface to running Shiny applications
#' from R packages. It is designed to work in a similar fashion as the
#' \code{\link{demo}} function. \code{shiny_demo} gives a list of available
#' Shiny apps within laoded packages. Setting the \code{includeAllInstalled}
#' parameter to \code{TRUE} will also search all installed packages but note
#' it will take longer to execute.
#'
#' For package developers, simply put Shiny apps in the
#' \code{inst/shiny} directory of the package. This function will find any
#' apps located there once installed.
#'
#' @param topic the topic/app which should be run.
#' @param package the package which contains the app to run. If \code{NULL} the
#'   first app with the given topic name will be run.
#' @param lib.loc a character vector of directory names of R libraries, or NULL.
#'   The default value of NULL corresponds to all libraries currently known. If
#'   the default is used, the loaded packages are searched before the libraries.
#' @param verbose a logical. If TRUE, additional diagnostics are printed.
#' @param includeAllInstalled a logical. If TRUE and topic not specified, all
#'   Shiny apps from all installed packages will be listed.
#' @return a data frame with two columns, \code{package} and \code{app}, listing
#'   all the available Shiny apps.
#' @seealso demo
#' @export
#' @author Jason Bryer (jason@@bryer.org)
shiny_demo <- function(topic,
					   package = NULL,
					   lib.loc = NULL,
					   verbose = getOption("verbose"),
					   includeAllInstalled = FALSE) {
	paths <- find.package(package, lib.loc, verbose = verbose)
	if(includeAllInstalled & missing(topic)) {
		installed <- installed.packages()[,'Package']
		paths <- find.package(installed, lib.loc, verbose = verbose)
	}

	pkgs <- basename(paths)

	# List available shiny demos
	shiny.apps <- data.frame()
	shiny.paths <- file.path(paths, "shiny")
	for(i in seq_along(shiny.paths)) {
		apps <- list.dirs(shiny.paths[i], recursive=FALSE, full.names=FALSE)
		if(length(apps) > 0) {
			shiny.apps <- rbind(shiny.apps, data.frame(
				package = rep(pkgs[i], length(apps)),
				app = apps,
				stringsAsFactors=FALSE
			))
		}
	}

	if(missing(topic)) {
		if(nrow(shiny.apps) > 0) {
			message(shiny.apps, row.names=FALSE)
		} else {
			message('No Shiny apps found in loaded packages.')
		}
	} else { #Run the shiny app
		if(is.null(package)) { # find the package containing the topic
			pos <- which(shiny.apps$app == topic)
			if(length(pos) == 0) {
				stop(paste0(topic, ' app not found in a loaded package.'))
			} else if(length(pos) > 1) {
				warning(paste0(topic, ' named app found in more than one package. ',
							   'Running app from ', pkgs[pos[1]], ' package.'))
			}
			package <- pkgs[pos[1]]
		}
		message(paste0('Running ', topic, ' app from the ', package, ' package'))
		app.path <- file.path(path.package(package), 'shiny', topic)
		runApp(app.path)
	}

	invisible(shiny.apps)
}
