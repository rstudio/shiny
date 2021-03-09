#' @export
navtreePanel <- function(..., id = NULL,
                          selected = NULL,
                          fluid = TRUE,
                         # Also allow for string to determine padding in a flex layout?
                          widths = c(3, 9)) {
  # TODO: how to incorporate this into a sidebar layout?

  if (!is.null(id))
    selected <- restoreInput(id = id, default = selected)

  tabset <- buildTreePanel(..., ulClass = "nav nav-navtree", id = id, selected = selected)

  row <- if (fluid) fluidRow else fixedRow

  row(
    class = "navtree-container",
    column(widths[[1]], tabset$navList),
    column(widths[[2]], tabset$content),
    bslib::bs_dependency_defer(navtreeCssDependency)
  )
}

# Algorithm inspired by buildTabset() but we need different HTML/CSS,
# and menus are rendered as collapse toggles instead of Bootstrap dropdowns
buildTreePanel <- function(..., ulClass, id = NULL, selected = NULL, foundSelected = FALSE, depth = 0) {

  tabs <- list2(...)
  res <- findAndMarkSelectedTab(tabs, selected, foundSelected)
  tabs <- res$tabs
  foundSelected <- res$foundSelected

  # add input class if we have an id
  if (!is.null(id)) ulClass <- paste(ulClass, "shiny-tab-input")

  if (anyNamed(tabs)) {
    nms <- names(tabs)
    nms <- nms[nzchar(nms)]
    stop("Tabs should all be unnamed arguments, but some are named: ",
         paste(nms, collapse = ", "))
  }

  tabsetId <- p_randomInt(1000, 10000)
  tabs <- lapply(
    seq_len(length(tabs)), buildTreeItem,
    tabsetId = tabsetId,
    foundSelected = foundSelected,
    tabs = tabs, depth = depth
  )

  list(
    navList = tags$ul(
      class = ulClass, id = id,
      `data-tabsetid` = tabsetId,
      !!!lapply(tabs, "[[", "liTag")
    ),
    content = div(
      class = "tab-content",
      `data-tabsetid` = tabsetId,
      !!!lapply(tabs, "[[", "divTag")
    )
  )
}

buildTreeItem <- function(index, tabsetId, foundSelected, tabs = NULL, divTag = NULL, depth = 0) {
  divTag <- divTag %||% tabs[[index]]

  subMenuPadding <- if (depth > 0) css(padding_left = paste0(depth * 1.25, "rem"))

  if (isNavbarMenu(divTag)) {
    icon <- getIcon(iconClass = divTag$iconClass)
    if (!is.null(icon)) {
      warning("Configurable icons are not yet supported in navtreePanel().")
    }
    tabset <- buildTreePanel(
      !!!divTag$tabs, ulClass = "nav nav-navtree",
      foundSelected = foundSelected, depth = depth + 1
    )
    # Sort of like .dropdown in the tabsetPanel() case,
    # but utilizes collapsing (which is recursive) instead of dropdown
    active <- containsSelectedTab(divTag$tabs)
    menuId <- paste0("collapse-", p_randomInt(1000, 10000))
    liTag <- tags$li(
      tags$a(
        class = if (!active) "collapsed",
        "data-toggle" = "collapse",
        "data-value" = divTag$menuName,
        "data-target" = paste0("#", menuId),
        role = "button",
        style = subMenuPadding,
        getIcon(iconClass = divTag$iconClass),
        divTag$title
      ),
      div(
        class = "collapse",
        class = if (active) "show in",
        id = menuId,
        tabset$navList
      )
    )
    return(list(liTag = liTag, divTag = tabset$content$children))
  }

  if (isTabPanel(divTag)) {
    # Borrow from the usual nav logic so we get the right
    # li.active vs li > a.active (BS4) markup
    navItem <- buildNavItem(divTag, tabsetId, index)
    liTag <- navItem$liTag
    return(
      list(
        divTag = navItem$divTag,
        liTag = tagFunction(function() {
          # Incoming liTag should be a tagFunction()
          liTag <- if (inherits(liTag, "shiny.tag.function")) liTag() else liTag

          # Add padding for sub menu anchors
          liTag$children[[1]] <- tagAppendAttributes(
            liTag$children[[1]], style = subMenuPadding
          )

          liTag
        })
      )
    )
  }

  abort("navtreePanel() items must be tabPanel()s and/or tabPanelMenu()s")
}


navtreeCssDependency <- function(theme) {
  name <- "navtreePanel"
  version <- packageVersion("shiny")
  if (!is_bs_theme(theme)) {
    # TODO: Should we allow navtreePanel() to be statically rendered?
    # Can/should we move away from href="shared/*"?
    htmlDependency(
      name = name, version = version,
      src = c(href = "shared/navtree", file = "www/shared/navtree"),
      package = "shiny",
      stylesheet = "navtree.css",
      script = "navtree.js"
    )
  } else {
    navtree <- system.file(package = "shiny", "www/shared/navtree")
    bslib::bs_dependency(
      sass::sass_file(file.path(navtree, "navtree.scss")),
      theme = theme,
      name = name,
      version = version,
      cache_key_extra = version,
      .dep_args = list(script = file.path(navtree, "navtree.js"))
    )
  }
}
