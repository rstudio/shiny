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
renderTable <- function(expr, format="basic", width="auto",
                        rownames=FALSE, colnames=TRUE, align=NULL,
                        digits=NULL, na="NA", ...,
                        env=parent.frame(), quoted=FALSE, func=NULL) {
  if (!is.null(func)) {
    shinyDeprecated(msg="renderTable: argument 'func' is deprecated. Please use 'expr' instead.")
  } else {
    installExprFunction(expr, "func", env, quoted)
  }

  # Create wrappers for most arguments so that functions can also be passed
  # in, rather than only literals (useful for shiny apps)
  formatWrapper <- createWrapper( format )
  widthWrapper <- createWrapper( width )
  rownamesWrapper <- createWrapper( rownames )
  colnamesWrapper <- createWrapper( colnames )
  alignWrapper <- createWrapper( align )
  digitsWrapper <- createWrapper( digits )
  naWrapper <- createWrapper( na )

  # Main render function
  markRenderFunction(tableOutput, function() {
    format <- formatWrapper()
    width <- widthWrapper()
    rownames <- rownamesWrapper()
    colnames <- colnamesWrapper()
    align <- alignWrapper()
    digits <- digitsWrapper()
    na <- naWrapper()

    # For css styling
    classNames <- "table shiny-table"
    classNames <- paste0( classNames, " table-", format )
    data <- func()

    # Return empty string if no data is provided
    if (is.null(data) || identical(data, data.frame()))
      return("")

    # Separate the ... args to pass to xtable() vs print.xtable()
    dots <- list(...)
    xtable_argnames <- setdiff(names(formals(xtable)), c("x", "..."))
    xtable_args <- dots[intersect(names(dots), xtable_argnames)]
    non_xtable_args <- dots[setdiff(names(dots), xtable_argnames)]

    # Figure out column alignment
    ## Case 1: if align=NULL, check if rownames are numbers. If not, make
    ## sure to left align them (xtable right aligns them by default, which
    ## looks weird when the rownames are strings).
    if ( is.null(align) ){
      n <- rownames( data )
      if ( !( suppressWarnings( is.na( all( n == as.character( as.numeric(n) )))))){
        xtable_args <- c( xtable_args, align = NULL )
      }
      else {
        cols <- "l"
        for ( i in 1:ncol(data) ){
          cls <- class( data[,i] )
          if ( cls=="numeric" || cls=="integer" ) cols <- paste0( cols, "r" )
          else cols <- paste0( cols, "l" )
        }
        xtable_args <- c( xtable_args, align = cols )
      }
    }
    ## Case 2: if align!=NULL, check if it is only one character or a vector
    ## and process it accordingly.
    else {
      num_cols <- ifelse( rownames, nchar(align), nchar(align) + 1 )
      valid <- !grepl( "[^lcr]", align )

      if ( num_cols == ncol(data) + 1 && valid ){
        if ( !rownames ) align <- paste0( "r", align )
        xtable_args <- c( xtable_args, align = align )
      }
      else if ( nchar(align) == 1 && valid ){
        cols <- paste0( rep( align, ncol(data) + 1 ), collapse = "" )
        xtable_args <- c( xtable_args, align = cols )
      }
      else {
        stop("`align` must contain only the characters `l`, `c` and/or `r` and have
             length either equal to 1 or to the total number of columns")
      }
      }

    # Call xtable with its args
    xtable_res <- do.call( xtable, c(list(data), xtable_args, digits = digits ))

    # Set up print args
    print_args <- list(
      xtable_res,
      type = 'html',
      include.rownames = rownames,
      include.colnames = colnames,
      NA.string = na,
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



# Create a wrapper for an argument passed to renderTable
createWrapper <- function( arg ){
  if ( is.function( arg )) wrapper <- reactive( {arg()} )
  else wrapper <- function() { arg }
  return( wrapper )
}
