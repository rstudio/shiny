#' Table Output
#'
#' Creates a reactive table that is suitable for assigning to an \code{output}
#' slot.
#'
#' The corresponding HTML output tag should be \code{div} and have the CSS
#' class name \code{shiny-html-output}.
#'
#' @param expr An expression that returns an R object that can be used with
#'   \code{\link[xtable]{xtable}}.
#' @param striped,condensed,hover,bordered Logicals: if \code{TRUE}, apply
#'   the corresponding Bootstrap table format to the output table.
#' @param width Table width. Must be a valid CSS unit (like "100%", 400px",
#'   "auto") or a number, which will be coerced to a string and
#'    have "px" appended.
#' @param align A string that specifies the column alignment. If equal to
#'   \code{'l'}, \code{'c'} or \code{'r'}, then all columns will be,
#'   respectively, left-, center- or right-aligned. Otherwise, \code{align}
#'   must have the same number of characters as the resulting table (if
#'   \code{rownames = TRUE}, this will be equal to \code{ncol()+1}), with
#'   the \emph{i}-th character specifying the alignment for the
#'   \emph{i}-th column (again, only \code{'l'}, \code{'c'} or
#'   \code{'r'} are permitted). If \code{NULL}, then all numeric/integer
#'   columns (including the row names, if they are numbers) will be
#'   right-aligned and everything else will be left-aligned.
#' @param rownames,colnames Logicals: include rownames? include colnames
#'   (column headers)?
#' @param digits An integer specifying the number of decimal places for
#'   the numeric columns (this will not apply to columns with an integer
#'   class). If \code{digits} is set to a negative value, then the numeric
#'   columns will be displayed in scientific format with a precision of
#'   \code{abs(digits)} digits.
#' @param na The string to use in the table cells whose values are missing
#'   (i.e. they either evaluate to \code{NA} or \code{NaN}).
#' @param ... Arguments to be passed through to \code{\link[xtable]{xtable}}
#'   and \code{\link[xtable]{print.xtable}}.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})?
#'   This is useful if you want to save an expression in a variable.
#' @param func A function that returns an R object that can be used with
#'   \code{\link[xtable]{xtable}} (deprecated; use \code{expr} instead).
#'
#' @export
renderTable <- function(expr, striped=FALSE, condensed=TRUE,
                        hover=FALSE, bordered=FALSE,
                        width="auto", align=NULL,
                        rownames=FALSE, colnames=TRUE,
                        digits=NULL, na="NA", ...,
                        env=parent.frame(), quoted=FALSE, func=NULL) {
  if (!is.null(func)) {
    shinyDeprecated(msg="renderTable: argument 'func' is deprecated. Please use 'expr' instead.")
  } else {
    installExprFunction(expr, "func", env, quoted)
  }

  # A small helper function to create a wrapper for an argument that was
  # passed to renderTable()
  createWrapper <- function(arg) {
    if (is.function(arg)) wrapper <- arg
    else wrapper <- function() arg
    return(wrapper)
  }

  # Create wrappers for most arguments so that functions can also be passed
  # in, rather than only literals (useful for shiny apps)
  stripedWrapper <- createWrapper(striped)
  condensedWrapper <- createWrapper(condensed)
  hoverWrapper <- createWrapper(hover)
  borderedWrapper <- createWrapper(bordered)
  widthWrapper <- createWrapper(width)
  alignWrapper <- createWrapper(align)
  rownamesWrapper <- createWrapper(rownames)
  colnamesWrapper <- createWrapper(colnames)
  digitsWrapper <- createWrapper(digits)
  naWrapper <- createWrapper(na)

  # Main render function
  markRenderFunction(tableOutput, function() {
    striped <- stripedWrapper()
    condensed <- condensedWrapper()
    hover <- hoverWrapper()
    bordered <- borderedWrapper()
    format <- c(striped=striped, condensed=condensed,
                hover=hover, bordered=bordered)
    width <- widthWrapper()
    align <- alignWrapper()
    rownames <- rownamesWrapper()
    colnames <- colnamesWrapper()
    digits <- digitsWrapper()
    na <- naWrapper()

    # For css styling
    classNames <- paste0("table shiny-table",
                         paste0(" table-", names(format)[format], collapse="" ))

    data <- as.data.frame(func())

    # Return NULL if no data is provided
    if (is.null(data) || identical(data, data.frame())) return(NULL)

    # Separate the ... args to pass to xtable() vs print.xtable()
    dots <- list(...)
    xtable_argnames <- setdiff(names(formals(xtable)), c("x", "..."))
    xtable_args <- dots[intersect(names(dots), xtable_argnames)]
    non_xtable_args <- dots[setdiff(names(dots), xtable_argnames)]

    # A small helper function to determine if the row.names can be coerced
    # to numeric or if they are legitimate strings
    isNumber <- function(n) {
      grepl("^\\d$+", n)
    }

    # Figure out column alignment
    ## Case 1: if align=NULL, check if row.names are numbers. If not, make
    ## sure to left-align them (xtable right-aligns them by default, which
    ## looks weird when the row.names are strings).
    if (is.null(align)) {
      cols <- if (isNumber(row.names(data))) "r" else "l"
      for (i in seq_len(ncol(data))) {
        cls <- class(data[,i])
        if (cls == "numeric" || cls == "integer") cols <- paste0(cols, "r")
        else cols <- paste0(cols, "l")
      }
    } else {
      ## Case 2: if align!=NULL, check if it is only one character or a vector
      ## and process it accordingly.
      num_cols <- if (rownames) nchar(align) else nchar(align)+1
      valid <- !grepl("[^lcr]", align)
      if (num_cols == ncol(data)+1 && valid) {
        cols <- if (rownames) align else paste0("r", align)
      } else if (nchar(align) == 1 && valid) {
        cols <- paste0(rep(align, ncol(data)+1), collapse="")
      } else {
        stop("`align` must contain only the characters `l`, `c` and/or `r` and have
             length either equal to 1 or to the total number of columns")
      }
    }

    # Call xtable with its (updated) args
    xtable_args <- c(xtable_args, align=cols, digits=digits)
    xtable_res <- do.call(xtable, c(list(data), xtable_args))

    # Set up print args
    print_args <- list(
      xtable_res,
      type = 'html',
      include.rownames = rownames,
      include.colnames = colnames,
      NA.string = na,
      html.table.attributes = paste0("class='", htmlEscape(classNames, TRUE), "' ",
                                     "style='width:", validateCssUnit(width), ";'"))

    print_args <- c(print_args, non_xtable_args)

    # Capture the raw html table returned by print.xtable(), and store it in
    # a variable for further processing
    tab <- paste(
      utils::capture.output(
        do.call(print, print_args)
      ),
      collapse="\n"
    )

    # All further processing concerns the table headers, so we don't need to run
    # any of this if colnames=FALSE
    if (colnames) {
      # Make sure that the final html table has a proper header (not included
      # in the print.xtable() default)
      tab <- sub("<tr>", "<thead> <tr>", tab)
      tab <- sub("</tr>", "</tr> </thead> <tbody>", tab)
      tab <- sub("</table>$", "</tbody> </table>", tab)

      # Update the `cols` string (which stores the alignment of each column) so
      # that it only includes the alignment for the table variables (and not
      # for the row.names)
      cols <- if (rownames) cols else substr(cols, 2, nchar(cols))

      # Create a vector whose i-th entry corresponds to the i-th table variable
      # alignment (substituting "l" by "left", "c" by "center" and "r" by "right")
      cols <- strsplit(cols, "")[[1]]
      cols[cols == "l"] <- "left"
      cols[cols == "r"] <- "right"
      cols[cols == "c"] <- "center"

      # Align each header accordingly (this guarantees that each header and its
      # corresponding column have the same alignment)
      for (i in seq_len(length(cols))) {
        tab <- sub("<th>", paste0("<th style='text-align: ", cols[i], ";'>"), tab)
      }
    }
    return(tab)
  })
}
