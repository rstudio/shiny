#' Panel with absolute positioning
#'
#' Creates a panel whose contents are absolutely positioned.
#'
#' The \code{absolutePanel} function creates a \code{<div>} tag whose CSS
#' position is set to \code{absolute} (or fixed if \code{fixed = TRUE}). The way
#' absolute positioning works in HTML is that absolute coordinates are specified
#' relative to its nearest parent element whose position is not set to
#' \code{static} (which is the default), and if no such parent is found, then
#' relative to the page borders. If you're not sure what that means, just keep
#' in mind that you may get strange results if you use \code{absolutePanel} from
#' inside of certain types of panels.
#'
#' The \code{fixedPanel} function is the same as \code{absolutePanel} with
#' \code{fixed = TRUE}.
#'
#' The position (\code{top}, \code{left}, \code{right}, \code{bottom}) and size
#' (\code{width}, \code{height}) parameters are all optional, but you should
#' specify exactly two of \code{top}, \code{bottom}, and \code{height} and
#' exactly two of \code{left}, \code{right}, and \code{width} for predictable
#' results.
#'
#' Like most other distance parameters in Shiny, the position and size
#' parameters take a number (interpreted as pixels) or a valid CSS size string,
#' such as \code{"100px"} (100 pixels) or \code{"25\%"}.
#'
#' For arcane HTML reasons, to have the panel fill the page or parent you should
#' specify \code{0} for \code{top}, \code{left}, \code{right}, and \code{bottom}
#' rather than the more obvious \code{width = "100\%"} and \code{height =
#' "100\%"}.
#'
#' @param ... Attributes (named arguments) or children (unnamed arguments) that
#'   should be included in the panel.
#'
#' @param top Distance between the top of the panel, and the top of the page or
#'   parent container.
#' @param left Distance between the left side of the panel, and the left of the
#'   page or parent container.
#' @param right Distance between the right side of the panel, and the right of
#'   the page or parent container.
#' @param bottom Distance between the bottom of the panel, and the bottom of the
#'   page or parent container.
#' @param width Width of the panel.
#' @param height Height of the panel.
#' @param draggable If \code{TRUE}, allows the user to move the panel by
#'   clicking and dragging.
#' @param fixed Positions the panel relative to the browser window and prevents
#'   it from being scrolled with the rest of the page.
#' @param cursor The type of cursor that should appear when the user mouses over
#'   the panel. Use \code{"move"} for a north-east-south-west icon,
#'   \code{"default"} for the usual cursor arrow, or \code{"inherit"} for the
#'   usual cursor behavior (including changing to an I-beam when the cursor is
#'   over text). The default is \code{"auto"}, which is equivalent to
#'   \code{ifelse(draggable, "move", "inherit")}.
#' @return An HTML element or list of elements.
#'
#' @export
absolutePanel <- function(...,
                          top = NULL, left = NULL, right = NULL, bottom = NULL,
                          width = NULL, height = NULL,
                          draggable = FALSE, fixed = FALSE,
                          cursor = c('auto', 'move', 'default', 'inherit')) {
  cssProps <- list(
    top = top,
    left = left,
    right = right,
    bottom = bottom,
    width = width,
    height = height
  )
  cssProps <- cssProps[!sapply(cssProps, is.null)]
  cssProps <- sapply(cssProps, validateCssUnit)
  cssProps[['position']] <- ifelse(fixed, 'fixed', 'absolute')
  cssProps[['cursor']] <- match.arg(cursor)
  if (identical(cssProps[['cursor']], 'auto'))
    cssProps[['cursor']] <- ifelse(draggable, 'move', 'inherit')

  style <- paste(paste(names(cssProps), cssProps, sep = ':', collapse = ';'), ';', sep='')
  divTag <- tags$div(style=style, ...)
  if (isTRUE(draggable)) {
    divTag <- tagAppendAttributes(divTag, class='draggable')
    return(tagList(
      # IMPORTANT NOTE: If you update jqueryui, make sure you DON'T include the datepicker,
      # as it collides with our bootstrap datepicker!
      singleton(tags$head(tags$script(src='shared/jqueryui/jquery-ui.min.js'))),
      divTag,
      tags$script('$(".draggable").draggable();')
    ))
  } else {
    return(divTag)
  }
}

#' @rdname absolutePanel
#' @export
fixedPanel <- function(...,
                       top = NULL, left = NULL, right = NULL, bottom = NULL,
                       width = NULL, height = NULL,
                       draggable = FALSE,
                       cursor = c('auto', 'move', 'default', 'inherit')) {
  absolutePanel(..., top=top, left=left, right=right, bottom=bottom,
                width=width, height=height, draggable=draggable, cursor=match.arg(cursor),
                fixed=TRUE)
}
