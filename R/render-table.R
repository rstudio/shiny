#' Table Output
#'
#' Creates a reactive table that is suitable for assigning to an \code{output}
#' slot.
#'
#' The corresponding HTML output tag should be \code{div} and have the CSS class
#' name \code{shiny-html-output}.
#'
#' @param expr An expression that returns an R object that can be used with
#'   \code{\link[xtable]{xtable}}.
#' @param ... Arguments to be passed through to \code{\link[xtable]{xtable}} and
#'   \code{\link[xtable]{print.xtable}}.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @param func A function that returns an R object that can be used with
#'   \code{\link[xtable]{xtable}} (deprecated; use \code{expr} instead).
#'
#' @export
renderTable <- function(expr, ..., env=parent.frame(), quoted=FALSE, func=NULL) {
  if (!is.null(func)) {
    shinyDeprecated(msg="renderTable: argument 'func' is deprecated. Please use 'expr' instead.")
  } else {
    installExprFunction(expr, "func", env, quoted)
  }

  markRenderFunction(tableOutput, function() {
    classNames <- getOption('shiny.table.class') %OR% 'data table table-bordered table-condensed'
    data <- func()

    if (is.null(data) || identical(data, data.frame()))
      return("")

    # Separate the ... args to pass to xtable() vs print.xtable()
    dots <- list(...)
    xtable_argnames <- setdiff(names(formals(xtable)), c("x", "..."))
    xtable_args <- dots[intersect(names(dots), xtable_argnames)]
    non_xtable_args <- dots[setdiff(names(dots), xtable_argnames)]

    # Call xtable with its args
    xtable_res <- do.call(xtable, c(list(data), xtable_args))

    # Set up print args
    print_args <- list(
      xtable_res,
      type = 'html',
      html.table.attributes = paste('class="', htmlEscape(classNames, TRUE),
                                    '"', sep='')
    )
    print_args <- c(print_args, non_xtable_args)

    return(paste(
      utils::capture.output(
        do.call(print, print_args)
      ),
      collapse="\n"
    ))
  })
}
