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
#'   table (options: basic, striped, bordered, hover, condensed).
#'@param  width An optional string with the width of the table, as a percentage of
#'   the total width of the page.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @param func A function that returns an R object that can be used with
#'   \code{\link[xtable]{xtable}} (deprecated; use \code{expr} instead).
#'
#' @export
renderBootstrapTable <- function(expr, ..., format="basic", width="auto",
                                 rownames=FALSE, colnames=TRUE, align=NULL,
                                 digits=NULL, na="NA",
                                 env=parent.frame(), quoted=FALSE, func=NULL) {
  if (!is.null(func)) {
    shinyDeprecated(msg="renderTable: argument 'func' is deprecated. Please use 'expr' instead.")
  } else {
    installExprFunction(expr, "func", env, quoted)
  }

  if ( is.function(format) )
    formatWrapper <- reactive({ format() })
  else
    formatWrapper <- function() { format }

  if ( is.function(width) )
    widthWrapper <- reactive({ width() })
  else
    widthWrapper <- function() { width }

  if ( is.function(rownames) )
    rownamesWrapper <- reactive({ rownames() })
  else
    rownamesWrapper <- function() { rownames }

  if ( is.function(colnames) )
    colnamesWrapper <- reactive({ colnames() })
  else
    colnamesWrapper <- function() { colnames }

  if ( is.function(align) )
    alignWrapper <- reactive({ align() })
  else
    alignWrapper <- function() { align }

  if ( is.function(digits) )
    digitsWrapper <- reactive({ digits() })
  else
    digitsWrapper <- function() { digits }

  if ( is.function(na) )
    naWrapper <- reactive({ na() })
  else
    naWrapper <- function() { na }


  markRenderFunction(tableOutput, function() {

    format <- formatWrapper()
    width <- widthWrapper()
    rownames <- rownamesWrapper()
    colnames <- colnamesWrapper()
    align <- alignWrapper()
    digits <- digitsWrapper()
    na <- naWrapper()

    classNames <- "table anotherclass" ### CHANGE THIS CHAGE THIS CHANGE THIS
    classNames <- paste0( classNames, " table-", format )
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

    # Set up print args
    print_args <- list(
      xtable_res,
      type = 'html',
      include.rownames = rownames,
      include.colnames = colnames,
      html.table.attributes = paste0('class="', htmlEscape(classNames, TRUE), '"
                                     style="width:', noquote(validateCssUnit(width)),';"'))

    print_args <- c(print_args, non_xtable_args)

    return(paste(
      utils::capture.output(
        do.call(print, print_args)
      ),
      collapse="\n"
    ))
  })
}
