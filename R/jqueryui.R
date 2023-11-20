#' Panel with absolute positioning
#'
#' Creates a panel whose contents are absolutely positioned.
#'
#' The `absolutePanel` function creates a `<div>` tag whose CSS
#' position is set to `absolute` (or fixed if `fixed = TRUE`). The way
#' absolute positioning works in HTML is that absolute coordinates are specified
#' relative to its nearest parent element whose position is not set to
#' `static` (which is the default), and if no such parent is found, then
#' relative to the page borders. If you're not sure what that means, just keep
#' in mind that you may get strange results if you use `absolutePanel` from
#' inside of certain types of panels.
#'
#' The `fixedPanel` function is the same as `absolutePanel` with
#' `fixed = TRUE`.
#'
#' The position (`top`, `left`, `right`, `bottom`) and size
#' (`width`, `height`) parameters are all optional, but you should
#' specify exactly two of `top`, `bottom`, and `height` and
#' exactly two of `left`, `right`, and `width` for predictable
#' results.
#'
#' Like most other distance parameters in Shiny, the position and size
#' parameters take a number (interpreted as pixels) or a valid CSS size string,
#' such as `"100px"` (100 pixels) or `"25%"`.
#'
#' For arcane HTML reasons, to have the panel fill the page or parent you should
#' specify `0` for `top`, `left`, `right`, and `bottom`
#' rather than the more obvious `width = "100%"` and `height =
#' "100%"`.
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
#' @param draggable If `TRUE`, allows the user to move the panel by
#'   clicking and dragging.
#' @param fixed Positions the panel relative to the browser window and prevents
#'   it from being scrolled with the rest of the page.
#' @param cursor The type of cursor that should appear when the user mouses over
#'   the panel. Use `"move"` for a north-east-south-west icon,
#'   `"default"` for the usual cursor arrow, or `"inherit"` for the
#'   usual cursor behavior (including changing to an I-beam when the cursor is
#'   over text). The default is `"auto"`, which is equivalent to
#'   `ifelse(draggable, "move", "inherit")`.
#' @return An HTML element or list of elements.
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

  if (identical(draggable, FALSE)) {
    return(divTag)
  }

  # Add Shiny inputs and htmlwidgets to 'non-draggable' elements
  # Cf. https://api.jqueryui.com/draggable/#option-cancel
  dragOpts <- '{cancel: ".shiny-input-container,.html-widget,input,textarea,button,select,option"}'
  dragJS <- sprintf('$(".draggable").draggable(%s);', dragOpts)
  tagList(
    tagAppendAttributes(divTag, class='draggable'),
    jqueryuiDependency(),
    tags$script(HTML(dragJS))
  )
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


jqueryuiDependency <- function() {
  htmlDependency(
    "jqueryui",
    version_jqueryui,
    src = "www/shared/jqueryui",
    package = "shiny",
    script = "jquery-ui.min.js"
  )
}
