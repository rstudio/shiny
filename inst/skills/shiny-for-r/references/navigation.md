# Navigation in Shiny for R

## Overview

Group switchable content into a tab container instead of faking tabs with
`actionButton()`/radio buttons plus `conditionalPanel()`. A real tab
container gives you the tab strip, active styling, keyboard/ARIA behavior,
and a server-readable selection for free; hand-rolled tab-switching logic
duplicates all of that and drifts out of sync as panels are added. This
reference covers shiny's original navigation functions (`tabsetPanel()`,
`navbarPage()`) and the bslib equivalents (`navset_*()`, `page_navbar()`)
that supersede them. Reach for the bslib functions in new code: they are
still fully supported, but the shiny ones no longer gain features and do not
participate in bslib's fill system.

## Superseded shiny: tabsetPanel and tabPanel

`tabsetPanel()` holds `tabPanel()` children and renders a tab strip that
shows one panel at a time. Give it an `id` to read the active panel's
`value` (its `title` if none was set) as `input$<id>`.

```r
# Partial snippet: inside a fluidPage() ui
tabsetPanel(
  id = "tabs",
  tabPanel("Plot", plotOutput("plot")),
  tabPanel("Summary", verbatimTextOutput("summary"))
)
```

## Programmatic tab switching from a button

`updateTabsetPanel()` changes the active tab from the server, targeting the
panel's `value`. `navbarPage()`/`navlistPanel()` have matching
`updateNavbarPage()`/`updateNavlistPanel()` functions with the same signature.

```r
library(shiny)

ui <- fluidPage(
  actionButton("go", "Go to Summary"),
  tabsetPanel(
    id = "tabs",
    tabPanel("Plot", plotOutput("plot")),
    tabPanel("Summary", verbatimTextOutput("summary"))
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlot(plot(1:10))
  output$summary <- renderPrint("Summary content")

  observeEvent(input$go, {
    updateTabsetPanel(session, "tabs", selected = "Summary")
  })
}

shinyApp(ui, server)
```

## Hidden tabs driven entirely by the server

Set `type = "hidden"` on `tabsetPanel()` to hide the tab strip and drive the
active panel from other controls, such as a wizard's Next button. Use
`tabPanelBody()`, not `tabPanel()`, for the panels in this mode:

```r
# Partial snippet: ui excerpt plus matching server code
tabsetPanel(
  id = "wizard",
  type = "hidden",
  tabPanelBody("step1", "Step 1", actionButton("next1", "Next")),
  tabPanelBody("step2", "Step 2", actionButton("submit", "Submit"))
)

# Server
observeEvent(input$next1, {
  updateTabsetPanel(session, "wizard", selected = "step2")
})
```

## Show, hide, and remove panels

`showTab()`/`hideTab()` toggle a panel's visibility in the strip without
removing it; both take the container's `inputId` and the target panel's
`value`.

```r
# Partial snippet: inside an observeEvent() or observe()
observeEvent(input$toggle_admin, {
  if (isTRUE(input$toggle_admin)) {
    showTab("tabs", target = "Admin", select = TRUE)
  } else {
    hideTab("tabs", target = "Admin")
  }
})
```

## Multi-page apps: navbarPage and navlistPanel

`navbarPage()` builds a full page whose top-level `tabPanel()`s become
navbar pages, with `navbarMenu()` for dropdowns. `navlistPanel()` renders a
vertical list of panels beside the content instead of a top strip.

```r
# Partial snippet: a full navbarPage() ui
navbarPage(
  "My App",
  id = "nav",
  tabPanel("Home", "Welcome"),
  tabPanel("Analysis", plotOutput("plot")),
  navbarMenu("More", tabPanel("About", "About page"))
)
```

## bslib: navset containers

bslib replaces `tabsetPanel()` with `navset_*()` containers holding
`nav_panel()` children, chosen by visual style: `navset_tab()` (classic
bordered tabs), `navset_pill()` (rounded buttons), `navset_underline()`
(underlined links), or `navset_card_tab()` (tabs wrapped in
a card header — don't additionally wrap panel content in `card()`).

```r
library(shiny)
library(bslib)

ui <- page_fillable(
  navset_card_tab(
    title = "Analysis",
    id = "tabs",
    nav_panel("Plot", plotOutput("plot")),
    nav_panel("Summary", verbatimTextOutput("summary"))
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlot(plot(1:10))
  output$summary <- renderPrint(paste("Active:", input$tabs))
}

shinyApp(ui, server)
```

`page_navbar()` is the bslib equivalent of `navbarPage()`, and `navset_bar()`
embeds the same navbar style inside a larger page instead of as the whole
page. `nav_menu()` replaces `navbarMenu()`, and `nav_select()` is bslib's
equivalent of `updateTabsetPanel()`.

```r
# Partial snippet: a full page_navbar() ui
page_navbar(
  title = "My App",
  id = "nav",
  nav_panel("Home", "Welcome"),
  nav_panel("Analysis", plotOutput("plot")),
  nav_menu("More", nav_panel("About", "About page"))
)
```

## Quick reference

| Function | Purpose |
|---|---|
| `tabsetPanel(..., id = , type = )` | Superseded tab container; `type = "hidden"` for server-driven tabs |
| `tabPanel(title, ...)` | One panel inside `tabsetPanel()`/`navbarPage()` |
| `navbarPage(title, ...)` | Superseded full-page navbar app; prefer `page_navbar()` |
| `navlistPanel(...)` | Superseded vertical list navigation; prefer `navset_pill_list()` |
| `updateTabsetPanel()` / `updateNavbarPage()` / `updateNavlistPanel()` | Switch the active panel from the server |
| `showTab()` / `hideTab()` | Show or hide a panel without removing it |
| `nav_panel(title, ...)` | bslib panel inside a `navset_*()` or `page_navbar()` |
| `navset_card_tab()` / `navset_bar()` | bslib tab container / navbar-style container |
| `nav_menu(title, ...)` | Dropdown of nav items |
| `nav_select(id, selected)` | bslib equivalent of `updateTabsetPanel()` |

## Common mistakes

- **Faking tabs with `conditionalPanel()` + radio buttons.** Use
  `tabsetPanel()`/`navset_*()` with an `id`; you get styling, ARIA, and the
  selection input for free without hand-tracking state.
- **Reading the active tab and finding no input.** The container needs an
  `id=`; without it there is no `input$<id>`.
- **Passing a `title`, not a `value`, to `updateTabsetPanel()`/`showTab()`.**
  These target the panel's `value` (defaults to `title` if unset); if you set
  a distinct `value=`, use it, not the displayed label.
- **Wrapping `nav_panel()` content in `card()` inside `navset_card_*()`.**
  The navset already provides the card container — this produces a
  card-within-a-card.
- **Building a navbar-style container as a whole page with `navset_bar()`.**
  Use `page_navbar()` for a top-level navbar; `navset_bar()` is for a navbar
  embedded inside another page.
