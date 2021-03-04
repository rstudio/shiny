#' @export
treelistPanel <- function(..., id = NULL,
                          selected = NULL,
                          fluid = TRUE,
                          widths = c(4, 8)) {
  if (!is.null(id))
    selected <- restoreInput(id = id, default = selected)

  # build the tabset
  # TODO: would allowing strings make sense?
  tabset <- buildTreePanel(..., ulClass = "treelist-nav", id = id, selected = selected)

  row <- if (fluid) fluidRow else fixedRow

  row(
    class = "treelist-container",
    column(widths[[1]], tabset$navList),
    column(widths[[2]], tabset$content),
    bslib::bs_dependency_defer(treelistCssDependency)
  )
}

treelistCssDependency <- function(theme) {
  name <- "treelistPanel"
  version <- packageVersion("shiny")
  if (!is_bs_theme(theme)) {
    htmlDependency(
      name = name, version = version,
      src = c(href = "treelist"),
      stylesheet = "treelist.css"
    )
  } else {
    scss <- system.file(package = "shiny", "www/shared/treelist/treelist.scss")
    bslib::bs_dependency(
      sass::sass_file(scss),
      theme = theme,
      name = name,
      version = version,
      cache_key_extra = version
    )
  }
}

# Algorithm inspired by buildTabset() but we need different HTML/CSS,
# and menus are rendered as collapse toggles instead of Bootstrap dropdowns
buildTreePanel <- function(..., ulClass, id = NULL, selected = NULL, foundSelected = FALSE) {

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
    tabs = tabs
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

buildTreeItem <- function(index, tabsetId, foundSelected, tabs = NULL, divTag = NULL) {
  divTag <- divTag %||% tabs[[index]]

  if (isTabPanelMenu(divTag)) {
    # TODO: allow the collapse icon to be configured?
    icon <- getIcon(iconClass = divTag$iconClass)
    if (!is.null(icon)) {
      warning("Configurable icons are not yet supported in treelistPanel().")
    }
    tabset <- buildTreePanel(
      !!!divTag$tabs, ulClass = "treelist-nav", foundSelected = foundSelected
    )
    # Sort of like .dropdown in the tabsetPanel() case,
    # but utilizes collapsing (which is recursive) instead of dropdown
    active <- containsSelectedTab(divTag$tabs)
    menuId <- paste0("collapse-", p_randomInt(1000, 10000))
    liTag <- tags$li(
      tags$button(
        class = "btn",
        "data-toggle" = "collapse",
        "data-value" = divTag$menuName,
        "data-target" = paste0("#", menuId),
        divTag$title
      ),
      div(
        class = "collapse",
        class = if (active) "show",
        id = menuId,
        tabset$navList
      )
    )

    # list of tab content divs from the child tabset
    divTag <- tabset$content$children
    return(list(liTag = liTag, divTag = divTag))
  }

  if (isTabPanel(divTag)) {
    x <- buildNavItem(divTag, tabsetId, index)
    liTag <- x$liTag
    # We don't want BS4's nav-item/nav-link classes
    x$liTag <- tagFunction(function() {
      liTag <- if (inherits(liTag, "shiny.tag.function")) liTag() else liTag
      if (!tagHasClass(liTag, "nav-item")) return(liTag)
      liTag$attribs$class <- NULL
      liTag$children[[1]]$attribs$class <- NULL
      liTag
    })

    return(list(liTag = x$liTag, divTag = x$divTag))
  }

  abort("treelistPanel() items must be tabPanel()s and/or tabPanelMenu()s")
}
