#' Table Output (Bootstrap)
#'
#' Creates a reactive table using the Bootstrap design that is suitable for assigning to
#' an \code{output} slot.
#'
#' The corresponding HTML output tag should be \code{div} and have the CSS class
#' name \code{shiny-html-output}.
#'
#' @param expr An expression that returns an R object that can be used with
#'   \code{\link[xtable]{xtable}}.
#' @param ... Arguments to be passed through to \code{\link[xtable]{xtable}} and
#'   \code{\link[xtable]{print.xtable}}.
#' @param format An optional string with the Bootstrap table format to apply to the
#'   table (options: striped, bordered, hover, condensed).
#'@param  width An optional string with the width of the table, as a percentage of
#'   the total width of the page.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @param func A function that returns an R object that can be used with
#'   \code{\link[xtable]{xtable}} (deprecated; use \code{expr} instead).
#'
#' @export
renderBootstrapTable <- function(expr, ..., format=NULL, width=NULL, env=parent.frame(), quoted=FALSE, func=NULL) {
  if (!is.null(func)) {
    shinyDeprecated(msg="renderTable: argument 'func' is deprecated. Please use 'expr' instead.")
  } else {
    installExprFunction(expr, "func", env, quoted)
  }

  markRenderFunction(tableOutput, function() {
    classNames <- 'table'
    if ( !is.null(format) ) classNames <- paste0( classNames, " table-", format )
    data <- func()

    if (is.null(data) || identical(data, data.frame()))
      return("")

    # Separate the ... args to pass to xtable() vs print.xtable()
    dots <- list(...)
    xtable_argnames <- setdiff(names(formals(xtable)), c("x", "..."))
    xtable_args <- dots[intersect(names(dots), xtable_argnames)]
    non_xtable_args <- dots[setdiff(names(dots), xtable_argnames)]

    # Call xtable with its args
    #cols <- paste0( rep( "r", ncol( data )), collapse = "" )    ## used to align all cells to the right, not yet decided if we should keep this or not
    xtable_res <- do.call(xtable, c(list(data), xtable_args))
                                   # align = paste0("l", cols)))

    # Check if user specified width, else set it to auto
    if (!is.null(width)) {
      width <- paste0('style="width:', noquote(validateCssUnit(width)),';"')
    } else {
      width <- paste0('style="width:auto;"')
    }

    # Set up print args
    print_args <- list(
      xtable_res,
      type = 'html',
      html.table.attributes = paste0('class="', htmlEscape(classNames, TRUE), '" ',
                                     width))

    print_args <- c(print_args, non_xtable_args)

    # By default, don't include row numbers
    if( !"include.rownames" %in% names(list(... ))) {
      print_args <- c(print_args, include.rownames = FALSE)
    }

    return(paste(
      utils::capture.output(
        do.call(print, print_args)
      ),
      collapse="\n"
    ))
  })
}
