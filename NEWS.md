shiny 0.13.2.9005
=================

## Breaking changes

* Progress indicators can now either use the new notification API, using
  `style="notification"`, or be displayed with the previous styling, using
  `style="old"`. You can also call `shinyOptions(progress.style="old")`
  in the server function to make all progress indicators use the old styling.
  (#1160, #1327, #1329)

* Closed #1161: Deprecated the `position` argument to `tabsetPanel()` since
  Bootstrap 3 stopped supporting this feature.

* The long-deprecated ability to pass a `func` argument to many of the
  `render` functions has been removed.

## New features

* Added the ability to bookmark and restore application state.

* Added a new notification API. From R, there are new functions
  `showNotification` and `hideNotification`. From JavaScript, there is a new
  `Shiny.notification` object that controls notifications. (#1141)

* Added the ability for the client browser to reconnect to a new session on
  the server, by setting `session$allowReconnect(TRUE)`. This requires a
  version of Shiny Server that supports reconnections. (#1074)

* Added modal dialogs. (#1157)

* Added insertUI and removeUI functions to be able to add and remove chunks
  of UI, standalone, and all independent of one another.

* Improved `renderTable()` function to make the tables look prettier and also
  provide the user with a lot more parameters to customize their tables with.

* Added support for the `pool` package (use Shiny's timer/scheduler)

## Minor new features and improvements

* Added `cancelOutput` argument to `req()`. This causes the currently
  executing reactive to cancel its execution, and leave its previous state
  alone (as opposed to clearing the output).

* `Display: Showcase` now displays the .js, .html and .css files in the `www`
  directory by default. In order to use showcase mode and not display these,
  include a new line in your Description file: `IncludeWWW: False`. (#1185)

* Added an error sanitization option: `options(shiny.sanitize.errors = TRUE)`.
  By default, this option is `FALSE`. When `TRUE`, normal errors will be
  sanitized, displaying only a generic error message. This changes the look
  of an app when errors are printed (but the console log remains the same).
  (#1123, #1156)

* Added the option of passing arguments to an `xxxOutput()` function through
  the corresponding `renderXXX()` function via an `outputArgs` parameter to the
  latter. This is only valid for snippets of Shiny code in an interactive
  `runtime: shiny` Rmd document (never for full apps, even if embedded in an
  Rmd).

* Added `updateActionButton()` function, so the user can change an
  `actionButton`'s (or `actionLink`'s) label and/or icon. It also checks that
  the icon argument (for both creation and updating of a button) is valid and
  throws a warning otherwise. (#1134)

* Added code diagnostics: if there is an error parsing ui.R, server.R, app.R,
  or global.R, Shiny will search the code for missing commas, extra commas,
  and unmatched braces, parens, and brackets, and will print out messages
  pointing out those problems. (#1126)

* Added support for horizontal dividers in `navbarMenu`. (#888)

* navbarMenu now has dividers and dropdown headers (#888)

* Added `placeholder` option to `passwordInput`. (#1152)

* Added `session$resetBrush(brushId)` (R) and `Shiny.resetBrush(brushId)` (JS)
  to programatically clear brushes from `imageOutput`/`plotOutput`. (#1197)

* Added textAreaInput. (Thanks to @nuno-agostinho. #1183, #1300)

* Added `session$sendBinaryMessage(type, message)` method for sending custom
  binary data to the client. See `?session`. (Thanks, @daef! #1316, #1320)

* Almost all code examples now have a runnable example with `shinyApp()`, so
  that users can run the examples and see them in action. (#1137, #1158)

* When resized, plots are drawn with `replayPlot()`, instead of re-executing
  all plotting code. This results in faster plot rendering. (#1112)

* Exported the `isTruthy()` function.

* Reactive log now shows elapsed time for reactives and observers.

* Nodes in the reactlog visualization are now sticky if the user drags them.

## Bug fixes

* Fixed #1350: Highlighting of reactives didn't work in showcase mode. (#1351)

* Fixed #1331: `renderPlot()` now correctly records and replays plots when
  `execOnResize=FALSE`. (#1337)

* `updateDateInput()` and `updateDateRangeInput()` can now clear the date
  input fields. (#1299, #896, #1315)

* Fixed #561: DataTables previously might pop up a warning when the data was
  updated extremely frequently.

* Fixed #776: In some browsers, plots sometimes flickered when updated.

* Fixed #543, #855: When `navbarPage()` had a `navbarMenu()` as the first
  item, it did not automatically select an item.

* Fixed #970: `navbarPage()` previously did not have an option to set the
  selected tab.

* Fixed #1253: Memory could leak when an observer was destroyed without first
  being invalidated. (#1254)

* Fixed #931: Nested observers could leak memory. (#1256)

* Fixed #1144: `updateRadioButton()` and `updateCheckboxGroupInput()` broke
  controls when used in modules (thanks, @sipemu!). (#1285)

* Fixed #1093: `updateRadioButtons()` and `updateCheckboxGroupInput()` didn't
  work if `choices` was numeric vector.

* Fixed #1122: `downloadHandler()` popped up empty browser window if the file
  wasn't present. It now gives a 404 error code.

* Fixed #1284: Reactive system was being flushed too often (usually this just
  means a more-expensive no-op than necessary).

* Fixed #1179: `updateDateInput()` sometimes didn't work with malformed dates.

* Fixed #1257: `updateSelectInput()` didn't work correctly in IE 11 and Edge.
  (#1277)

* Fixed #803: Malformed date input values crashed app. (#1298)

* Fixed #971: `runApp()` would give confusing error if `port` was not numeric.

* Shiny now avoids using ports that Chrome deems unsafe. (#1222)

* Added workaround for quartz graphics device resolution bug, where resolution
  is hard-coded to 72 ppi.

## Library updates

* Updated to ion.RangeSlider 2.1.2.

* Updated to Font Awesome 4.6.3.

* Updated to Bootstrap 3.3.7.

* Updated to jQuery 1.12.4.


shiny 0.13.2
============

* Updated documentation for `htmlTemplate`.


shiny 0.13.1
============

* `flexCol` did not work on RStudio for Windows or Linux.

* Fixed RStudio debugger integration.

* BREAKING CHANGE: The long-deprecated ability to pass functions (rather than
  expressions) to reactive() and observe() has finally been removed.


shiny 0.13.0
============

* Fixed #962: plot interactions did not work with the development version of
  ggplot2 (after ggplot2 1.0.1).

* Fixed #902: the `drag_drop` plugin of the selectize input did not work.

* Fixed #933: `updateSliderInput()` does not work when only the label is
  updated.

* Multiple imageOutput/plotOutput calls can now share the same brush id. Shiny
  will ensure that performing a brush operation will clear any other brush with
  the same id.

* Added `placeholder` option to `textInput`.

* Improved support for Unicode characters on Windows (#968).

* Fixed bug in `selectInput` and `selectizeInput` where values with double
  quotes were not properly escaped.

* `runApp()` can now take a path to any .R file that yields a `shinyApp` object;
  previously, the path had to be a directory that contained an app.R file (or
  server.R if using separately defined server and UI). Similarly, introduced
  `shinyAppFile()` function which creates a `shinyApp` object for an .R file
  path, just as `shinyAppDir()` does for a directory path.

* Introduced Shiny Modules, which are designed to 1) simplify the reuse of
  Shiny UI/server logic and 2) make authoring and maintaining complex Shiny
  apps much easier. See the article linked from `?callModule`.

* `invalidateLater` and `reactiveTimer` no longer require an explicit `session`
  argument; the default value uses the current session.

* Added `session$reload()` method, the equivalent of hitting the browser's
  Reload button.

* Added `shiny.autoreload` option, which will automatically cause browsers to
  reload whenever Shiny app files change on disk. This is intended to shorten
  the feedback cycle when tweaking UI code.

* Errors are now printed with stack traces! This should make it tremendously
  easier to track down the causes of errors in Shiny. Try it by calling
  `stop("message")` from within an output, reactive, or observer. Shiny itself
  adds a lot of noise to the call stack, so by default, it attempts to hide all
  but the relevant levels of the call stack. You can turn off this behavior by
  setting `options(shiny.fullstacktrace=TRUE)` before or during app startup.

* Fixed #1018: the selected value of a selectize input is guaranteed to be
  selected in server-side mode.

* Added `req` function, which provides a simple way to prevent a reactive,
  observer, or output from executing until all required inputs and values are
  available. (Similar functionality has been available for a while using
  validate/need, but req provides a much simpler and more direct interface.)

* Improve stability with Shiny Server when many subapps are used, by deferring
  the loading of subapp iframes until a connection has first been established
  with the server.

* Upgrade to Font Awesome 4.5.0.

* Upgraded to Bootstrap 3.3.5.

* Upgraded to jQuery 1.12.4

* Switched to an almost-complete build of jQuery UI with the exception of the
  datepicker extension, which conflicts with Shiny's date picker.

* Added `fillPage` function, an alternative to `fluidPage`, `fixedPage`, etc.
  that is designed for apps that fill the entire available page width/height.

* Added `fillRow` and `fillCol` functions, for laying out proportional grids in
  `fillPage`. For modern browsers only.

* Added `runGadget`, `paneViewer`, `dialogViewer`, and `browserViewer`
  functions to support Shiny Gadgets. More detailed docs about gadgets coming
  soon.

* Added support for the new htmltools 0.3 feature `htmlTemplate`. It's now
  possible to use regular HTML markup to design your UI, but still use R
  expressions to define inputs, outputs, and HTML widgets.


shiny 0.12.2
============

* GitHub changed URLs for gists from .tar.gz to .zip, so `runGist` was updated
  to work with the new URLs.

* Callbacks from the session object are now guaranteed to execute in the order
  in which registration occurred.

* Minor bugs in sliderInput's animation behavior have been fixed. (#852)

* Updated to ion.rangeSlider to 2.0.12.

* Added `shiny.minified` option, which controls whether the minified version
  of shiny.js is used. Setting it to FALSe can be useful for debugging. (#826,
  #850)

* Fixed an issue for outputting plots from ggplot objects which also have an
  additional class whose print method takes precedence over `print.ggplot`.
  (#840, 841)

* Added `width` option to Shiny's input functions. (#589, #834)

* Added two alias functions of `updateTabsetPanel()` to update the selected tab:
  `updateNavbarPage()` and `updateNavlistPanel()`. (#881)

* All non-base functions are now explicitly namespaced, to pass checks in
  R-devel.

* Shiny now correctly handles HTTP HEAD requests. (#876)


shiny 0.12.1
============

* Fixed an issue where unbindAll() causes subsequent bindAll() to be ignored for
  previously bound outputs. (#856)

* Undeprecate `dataTableOutput` and `renderDataTable`, which had been deprecated
  in favor of the new DT package. The DT package is a bit too new and has a
  slightly different API, we were too hasty in deprecating the existing Shiny
  functions.


shiny 0.12.0
============

* Switched from RJSONIO to jsonlite. This improves consistency and speed when
  converting between R data structures and JSON. One notable change is that
  POSIXt objects are now serialized to JSON in UTC8601 format (like
  "2015-03-20T20:00:00Z"), instead of as seconds from the epoch).

* In addition to the existing support for clicking and hovering on plots
  created by base graphics, added support for double-clicking and brushing.
  (#769)

* Added support for clicking, hovering, double-clicking, and brushing for
  plots created by ggplot2, including support for facets. (#802)

* Added `nearPoints` and `brushedPoints` functions for easily selecting rows of
  data that are clicked/hovered, or brushed. (#802)

* Added `shiny.port` option. If this is option is set, `runApp()` will listen on
  this port by default. (#756)

* `runUrl`, `runGist`, and `runGitHub` now can save downloaded applications,
  with the `destdir` argument. (#688)

* Restored ability to set labels for `selectInput`. (#741)

* Travis continuous integration now uses Travis's native R support.

* Fixed encoding issue when the server receives data from the client browser.
  (#742)

* The `session` object now has class `ShinySession`, making it easier to test
  whether an object is indeed a session object. (#720, #746)

* Fix JavaScript error when an output appears in nested uiOutputs. (Thanks,
  Gregory Zhang. #749)

* Eliminate delay on receiving new value when `updateSliderInput(value=...)` is
  called.

* Updated to DataTables (Javascript library) 1.10.5.

* Fixed downloading of files that have no filename extension. (#575, #753)

* Fixed bug where nested UI outputs broke outputs. (#749, #750)

* Removed unneeded HTML ID attributes for `checkboxGroupInputs` and
  `radioButtons`. (#684)

* Fixed bug where checkboxes were still active even after `Shiny.unbindAll()`
  was called. (#206)

* The server side selectize input will load the first 1000 options by default
  before users start to type and search in the box. (#823)

* renderDataTable() and dataTableOutput() have been deprecated in shiny and will
  be removed in future versions of shiny. Please use the DT package instead:
  http://rstudio.github.io/DT/ (#807)


shiny 0.11.1
============

* Major client-side performance improvements for pages that have many
  conditionalPanels, tabPanels, and plotOutputs. (#693, #717, #723)

* `tabPanel`s now use the `title` for `value` by default. This fixes a bug
  in which an icon in the title caused problems with a conditionalPanel's test
  condition. (#725, #728)

* `selectInput` now has a `size` argument to control the height of the input
  box. (#729)

* `navbarPage` no longer includes a first row of extra whitespace when
  `header=NULL`. (#722)

* `selectInput`s now use Bootstrap styling when `selectize=FALSE`. (#724)

* Better vertical spacing of label for checkbox groups and radio buttons.

* `selectInput` correctly uses width for both selectize and non-selectize
  inputs. (#702)

* The wrapper tag generated by `htmlOutput` and `uiOutput` can now be any type
  of HTML tag, instead of just span and div. Also, custom classes are now
  allowed on the tag. (#704)

* Slider problems in IE 11 and Chrome on touchscreen-equipped Windows computers
  have been fixed. (#700)

* Sliders now work correctly with draggable panels. (#711)

* Fixed arguments in `fixedPanel`. (#709)

* downloadHandler content callback functions are now invoked with a temp file
  name that has the same extension as the final filename that will be used by
  the download. This is to deal with the fact that some file writing functions
  in R will auto-append the extension for their file type (pdf, zip).


shiny 0.11
==========

* Changed sliders from jquery-slider to ion.rangeSlider. These sliders have
  an improved appearance, support updating more properties from the server,
  and can be controlled with keyboard input.

* Switched from Bootstrap 2 to Bootstrap 3. For most users, this will work
  seamlessly, but some users may need to use the shinybootstrap2 package for
  backward compatibility.

* The UI of a Shiny app can now have a body tag. This is useful for CSS
  templates that use classes on the body tag.

* `actionButton` and `actionLink` now pass their `...` arguments to the
  underlying tag function. (#607)

* Added `observeEvent` and `eventReactive` functions for clearer, more concise
  handling of `actionButton`, plot clicks, and other naturally-imperative
  inputs.

* Errors that happen in reactives no longer prevent any remaining pending
  observers from executing. It is also now possible for users to control how
  errors are handled, with the 'shiny.observer.error' global option. (#603,
  #604)

* Added an `escape` argument to `renderDataTable()` to escape the HTML entities
  in the data table for security reasons. This might break tables from previous
  versions of shiny that use raw HTML in the table content, and the old behavior
  can be brought back by `escape = FALSE` if you are aware of the security
  implications. (#627)

* Changed the URI encoding/decoding functions internally to use `encodeURI()`,
  `encodeURIComponent()`, and `decodeURIComponent()` from the httpuv package
  instead of `utils::URLencode()` and `utils::URLdecode()`. (#630)

* Shiny's web assets are now minified.

* The default reactive domain is now available in event handler functions. (#669)

* Password input fields can now be used, with `passwordInput()`. (#672)


shiny 0.10.2.2
==============

* Remove use of `rstudio::viewer` in a code example, for R CMD check.


shiny 0.10.2.1
==============

* Changed some examples to use \donttest instead of \dontrun.


shiny 0.10.2
============

* The minimal version of R required for the shiny package is 3.0.0 now.

* Shiny apps can now consist of a single file, app.R, instead of ui.R and
  server.R.

* Upgraded DataTables from 1.9.4 to 1.10.2. This might be a breaking change if
  you have customized the DataTables options in your apps. (More info:
  https://github.com/rstudio/shiny/pull/558)

* File uploading via `fileInput()` works for Internet Explorer 8 and 9 now. Note
  IE8/9 do not support multiple files from a single file input. If you need to
  upload multiple files, you have to use one file input for each file.

* Switched away from reference classes to R6.

* Reactive log performance has been greatly improved.

* Added `Progress` and `withProgress`, to display the progress of computation
  on the client browser.

* Fixed #557: updateSelectizeInput(choices, server = TRUE) did not work when
  `choices` is a character vector.

* Searching in DataTables is case-insensitive and the search strings are not
  treated as regular expressions by default now. If you want case-sensitive
  searching or regular expressions, you can use the configuration options
  `search$caseInsensitive` and `search$regex`, e.g. `renderDataTable(...,
  options = list(search = list(caseInsensitve = FALSE, regex = TRUE)))`.

* Added support for `htmltools::htmlDependency`'s new `attachment` parameter to
  `renderUI`/`uiOutput`.

* Exported `createWebDependency`. It takes an `htmltools::htmlDependency` object
  and makes it available over Shiny's built-in web server.

* Custom output bindings can now render `htmltools::htmlDependency` objects at
  runtime using `Shiny.renderDependencies()`.

* Fixes to rounding behavior of sliderInput. (#301, #502)

* Updated selectize.js to version 0.11.2. (#596)

* Added `position` parameter to `navbarPage`.


shiny 0.10.1
============

* Added Unicode support for Windows. Shiny apps running on Windows must use the
  UTF-8 encoding for ui.R and server.R (also the optional global.R) if they
  contain non-ASCII characters. See this article for details and examples:
  http://shiny.rstudio.com/gallery/unicode-characters.html (#516)

* `runGitHub()` also allows the 'username/repo' syntax now, which is equivalent
  to `runGitHub('repo', 'username')`. (#427)

* `navbarPage()` now accepts a `windowTitle` parameter to set the web browser
  page title to something other than the title displayed in the navbar.

* Added an `inline` argument to `textOutput()`, `imageOutput()`, `plotOutput()`,
  and `htmlOutput()`. When `inline = TRUE`, these outputs will be put in
  `span()` instead of the default `div()`. This occurs automatically when these
  outputs are created via the inline expressions (e.g. `r renderText(expr)`) in
  R Markdown documents. See an R Markdown example at
  http://shiny.rstudio.com/gallery/inline-output.html (#512)

* Added support for option groups in the select/selectize inputs. When the
  `choices` argument for `selectInput()`/`selectizeInput()` is a list of
  sub-lists and any sub-list is of length greater than 1, the HTML tag
  `<optgroup>` will be used. See an example at
  http://shiny.rstudio.com/gallery/option-groups-for-selectize-input.html (#542)


shiny 0.10.0
============

* BREAKING CHANGE: By default, observers now terminate themselves if they were
  created during a session and that session ends. See ?domains for more details.

* Shiny can now be used in R Markdown v2 documents, to create "Shiny Docs":
  reports and presentations that combine narrative, statically computed output,
  and fully dynamic inputs and outputs. For more info, including examples, see
  http://rmarkdown.rstudio.com/authoring_shiny.html.

* The `session` object that can be passed into a server function (e.g.
  shinyServer(function(input, output, session) {...})) is now documented: see
  `?session`.

* Most inputs can now accept `NULL` label values to omit the label altogether.

* New `actionLink` input control; like `actionButton`, but with the appearance
  of a normal link.

* `renderPlot` now calls `print` on its result if it's visible (i.e. no more
  explicit `print()` required for ggplot2).

* Introduced Shiny app objects (see `?shinyApp`). These essentially replace the
  little-advertised ability for `runApp` to take a `list(ui=..., server=...)`
  as the first argument instead of a directory (though that ability remains for
  backward compatibility). Unlike those lists, Shiny app objects are tagged with
  class `shiny.appobj` so they can be run simply by printing them.

* Added `maskReactiveContext` function. It blocks the current reactive context,
  to evaluate expressions that shouldn't use reactive sources directly. (This
  should not be commonly needed.)

* Added `flowLayout`, `splitLayout`, and `inputPanel` functions for putting UI
  elements side by side. `flowLayout` lays out its children in a left-to-right,
  top-to-bottom arrangement. `splitLayout` evenly divides its horizontal space
  among its children (or unevenly divides if `cellWidths` argument is provided).
  `inputPanel` is like `flowPanel`, but with a light grey background, and is
  intended to be used to encapsulate small input controls wherever vertical
  space is at a premium.

* Added `serverInfo` to obtain info about the Shiny Server if the app is served
  through it.

* Added an `inline` argument (TRUE/FALSE) in `checkboxGroupInput()` and
  `radioButtons()` to allow the horizontal layout (inline = TRUE) of checkboxes
  or radio buttons. (Thanks, @saurfang, #481)

* `sliderInput` and `selectizeInput`/`selectInput` now use a standard horizontal
  size instead of filling up all available horizontal space. Pass `width="100%"`
  explicitly for the old behavior.

* Added the `updateSelectizeInput()` function to make it possible to process
  searching on the server side (i.e. using R), which can be much faster than the
  client side processing (i.e. using HTML and JavaScript). See the article at
  http://shiny.rstudio.com/articles/selectize.html for a detailed introduction.

* Fixed a bug of renderDataTable() when the data object only has 1 row and 1
  column. (Thanks, ZJ Dai, #429)

* `renderPrint` gained a new argument 'width' to control the width of the text
  output, e.g. renderPrint({mtcars}, width = 40).

* Fixed #220: the zip file for a directory created by some programs may not have
  the directory name as its first entry, in which case runUrl() can fail. (#220)

* `runGitHub()` can also take a value of the form "username/repo" in its first
  argument, e.g. both runGitHub("shiny_example", "rstudio") and
  runGitHub("rstudio/shiny_example") are valid ways to run the GitHub repo.


shiny 0.9.1
===========

* Fixed warning 'Error in Context$new : could not find function "loadMethod"'
  that was happening to dependent packages on "R CMD check".


shiny 0.9.0
===========

* BREAKING CHANGE: Added a `host` parameter to runApp() and runExample(),
  which defaults to the shiny.host option if it is non-NULL, or "127.0.0.1"
  otherwise. This means that by default, Shiny applications can only be
  accessed on the same machine from which they are served. To allow other
  clients to connect, as in previous versions of Shiny, use "0.0.0.0"
  (or the IP address of one of your network interfaces, if you care to be
  explicit about it).

* Added a new function `selectizeInput()` to use the JavaScript library
  selectize.js (https://github.com/brianreavis/selectize.js), which extends
  the basic select input in many aspects.

* The `selectInput()` function also gained a new argument `selectize = TRUE`
  to makes use of selectize.js by default. If you want to revert back to the
  original select input, you have to call selectInput(..., selectize = FALSE).

* Added Showcase mode, which displays the R code for an app right in the app
  itself. You can invoke Showcase mode by passing `display.mode="showcase"`
  to the `runApp()` function. Or, if an app is designed to run in Showcase
  mode by default, add a DESCRIPTION file in the app dir with Title, Author,
  and License fields; with "Type: Shiny"; and with "DisplayMode: Showcase".

* Upgraded to Bootstrap 2.3.2 and jQuery 1.11.0.

* Make `tags$head()` and `singleton()` behave correctly when used with
  `renderUI()` and `uiOutput()`. Previously, "hoisting content to the head"
  and "only rendering items a single time" were features that worked only
  when the page was initially loading, not in dynamic rendering.

* Files are now sourced with the `keep.source` option, to help with debugging
  and profiling.

* Support user-defined input parsers for data coming in from JavaScript using
  the parseShinyInput method.

* Fixed the bug #299: renderDataTable() can deal with 0-row data frames now.
  (reported by Harlan Harris)

* Added `navbarPage()` and `navbarMenu()` functions to create applications
  with multiple top level panels.

* Added `navlistPanel()` function to create layouts with a a bootstrap
  navlist on the left and tabPanels on the right

* Added `type` parameter to `tabsetPanel()` to enable the use of pill
  style tabs in addition to the standard ones.

* Added `position` paramter to `tabsetPanel()` to enable positioning of tabs
  above, below, left, or right of tab content.

* Added `fluidPage()` and `fixedPage()` functions as well as related row and
  column layout functions for creating arbitrary bootstrap grid layouts.

* Added `hr()` builder function for creating horizontal rules.

* Automatically concatenate duplicate attributes in tag definitions

* Added `responsive` parameter to page building functions for opting-out of
  bootstrap responsive css.

* Added `theme` parameter to page building functions for specifying alternate
  bootstrap css styles.

* Added `icon()` function for embedding icons from the
  [font awesome](http://fontawesome.io/) icon library

* Added `makeReactiveBinding` function to turn a "regular" variable into a
  reactive one (i.e. reading the variable makes the current reactive context
  dependent on it, and setting the variable is a source of reactivity).

* Added a function `withMathJax()` to include the MathJax library in an app.

* The argument `selected` in checkboxGroupInput(), selectInput(), and
  radioButtons() refers to the value(s) instead of the name(s) of the
  argument `choices` now. For example, the value of the `selected` argument
  in selectInput(..., choices = c('Label 1' = 'x1', 'Label 2' = 'x2'),
  selected = 'Label 2') must be updated to 'x2', although names/labels will
  be automatically converted to values internally for backward
  compatibility. The same change applies to updateCheckboxGroupInput(),
  updateSelectInput(), and updateRadioButtons() as well. (#340)

* Now it is possible to only update the value of a checkbox group, select input,
  or radio buttons using the `selected` argument without providing the
  `choices` argument in updateCheckboxGroupInput(), updateSelectInput(), and
  updateRadioButtons(), respectively. (#340)

* Added `absolutePanel` and `fixedPanel` functions for creating absolute-
  and fixed-position panels. They can be easily made user-draggable by
  specifying `draggable = TRUE`.

* For the `options` argument of the function `renderDataTable()`, we can
  pass literal JavaScript code to the DataTables library via `I()`. This
  makes it possible to use any JavaScript object in the options, e.g. a
  JavaScript function (which is not supported in JSON). See
  `?renderDataTable` for details and examples.

* DataTables also works under IE8 now.

* Fixed a bug in DataTables pagination when searching is turned on, which
  caused failures for matrices as well as empty rows when displaying data
  frames using renderDataTable().

* The `options` argument in `renderDataTable()` can also take a function
  that returns a list. This makes it possible to use reactive values in the
  options. (#392)

* `renderDataTable()` respects more DataTables options now: (1) either
  bPaginate = FALSE or iDisplayLength = -1 will disable pagination (i.e. all
  rows are returned from the data); besides, this means we can also use -1
  in the length menu, e.g. aLengthMenu = list(c(10, 30, -1), list(10, 30,
  'All')); (2) we can disable searching for individual columns through the
  bSearchable option, e.g. aoColumns = list(list(bSearchable = FALSE),
  list(bSearchable = TRUE),...) (the search box for the first column is
  hidden); (3) we can turn off searching entirely (for both global searching
  and individual columns) using the option bFilter = FALSE.

* Added an argument `callback` in `renderDataTable()` so that a custom
  JavaScript function can be applied to the DataTable object. This makes it
  much easier to use DataTables plug-ins.

* For numeric columns in a DataTable, the search boxes support lower and
  upper bounds now: a search query of the form "lower,upper" (without
  quotes) indicates the limits [lower, upper]. For a column X, this means
  the rows corresponding to X >= lower & X <= upper are returned. If we omit
  either the lower limit or the upper limit, only the other limit will be
  used, e.g. ",upper" means X <= upper.

* `updateNumericInput(value)` tries to preserve numeric precision by avoiding
  scientific notation when possible, e.g. 102145 is no longer rounded to
  1.0214e+05 = 102140. (Thanks, Martin Loos. #401)

* `sliderInput()` no longer treats a label wrapped in HTML() as plain text,
  e.g. the label in sliderInput(..., label = HTML('<em>A Label</em>')) will
  not be escaped any more. (#119)

* Fixed #306: the trailing slash in a path could fail `addResourcePath()`
  under Windows. (Thanks, ZJ Dai)

* Dots are now legal characters for inputId/outputId. (Thanks, Kevin
  Lindquist. #358)


shiny 0.8.0
===========

* Debug hooks are registered on all user-provided functions and (reactive)
  expressions (e.g., in renderPlot()), which makes it possible to set
  breakpoints in these functions using the latest version of the RStudio
  IDE, and the RStudio visual debugging tools can be used to debug Shiny
  apps. Internally, the registration is done via installExprFunction(),
  which is a new function introduced in this version to replace
  exprToFunction() so that the registration can be automatically done.

* Added a new function renderDataTable() to display tables using the
  JavaScript library DataTables. It includes basic features like pagination,
  searching (global search or search by individual columns), sorting (by
  single or multiple columns). All these features are implemented on the R
  side; for example, we can use R regular expressions for searching.
  Besides, it also uses the Bootstrap CSS style. See the full
  documentation and examples in the tutorial:
  http://rstudio.github.io/shiny/tutorial/#datatables

* Added a new option `shiny.error` which can take a function as an error
  handler. It is called when an error occurs in an app (in user-provided
  code), e.g., after we set options(shiny.error = recover), we can enter a
  specified environment in the call stack to debug our code after an error
  occurs.

* The argument `launch.browser` in runApp() can also be a function,
  which takes the URL of the shiny app as its input value.

* runApp() uses a random port between 3000 and 8000 instead of 8100 now. It
  will try up to 20 ports in case certain ports are not available.

* Fixed a bug for conditional panels: the value `input.id` in the condition
  was not correctly retrieved when the input widget had a type, such as
  numericInput(). (reported by Jason Bryer)

* Fixed two bugs in plotOutput(); clickId and hoverId did not give correct
  coordinates in Firefox, or when the axis limits of the plot were changed.
  (reported by Chris Warth and Greg D)

* The minimal required version for the httpuv package was increased to 1.2
  (on CRAN now).


shiny 0.7.0
===========

* Stopped sending websocket subprotocol. This fixes a compatibility issue with
  Google Chrome 30.

* The `input` and `output` objects are now also accessible via `session$input`
  and `session$output`.

* Added click and hover events for static plots; see `?plotOutput` for details.

* Added optional logging of the execution states of a reactive program, and
  tools for visualizing the log data. To use, start a new R session and call
  `options(shiny.reactlog=TRUE)`. Then launch a Shiny app and interact with it.
  Press Ctrl+F3 (or for Mac, Cmd+F3) in the browser to launch an interactive
  visualization of the reactivity that has occurred. See `?showReactLog` for
  more information.

* Added `includeScript()` and `includeCSS()` functions.

* Reactive expressions now have class="reactive" attribute. Also added
  `is.reactive()` and `is.reactivevalues()` functions.

* New `stopApp()` function, which stops an app and returns a value to the caller
  of `runApp()`.

* Added the `shiny.usecairo` option, which can be used to tell Shiny not to use
  Cairo for PNG output even when it is installed. (Defaults to `TRUE`.)

* Speed increases for `selectInput()` and `radioButtons()`, and their
  corresponding updater functions, for when they have many options.

* Added `tagSetChildren()` and `tagAppendChildren()` functions.

* The HTTP request object that created the websocket is now accessible from the
  `session` object, as `session$request`. This is a Rook-like request
  environment that can be used to access HTTP headers, among other things.
  (Note: When running in a Shiny Server environment, the request will reflect
  the proxy HTTP request that was made from the Shiny Server process to the R
  process, not the request that was made from the web browser to Shiny Server.)

* Fix `getComputedStyle` issue, for IE8 browser compatibility (#196). Note:
  Shiny Server is still required for IE8/9 compatibility.

* Add shiny.sharedSecret option, to require the HTTP header Shiny-Shared-Secret
  to be set to the given value.


shiny 0.6.0
===========

* `tabsetPanel()` can be directed to start with a specific tab selected.

* Fix bug where multiple file uploads with 3 or more files result in incorrect
  data.

* Add `withTags()` function.

* Add dateInput and dateRangeInput.

* `shinyServer()` now takes an optional `session` argument, which is used for
  communication with the session object.

* Add functions to update values of existing inputs on a page, instead of
  replacing them entirely.

* Allow listening on domain sockets.

* Added `actionButton()` to Shiny.

* The server can now send custom JSON messages to the client. On the client
  side, functions can be registered to handle these messages.

* Callbacks can be registered to be called at the end of a client session.

* Add ability to set priority of observers and outputs. Each priority level
  gets its own queue.

* Fix bug where the presence of a submit button would prevent sending of
  metadata until the button was clicked.

* `reactiveTimer()` and `invalidateLater()` by default no longer invalidate
  reactive objects after the client session has closed.

* Shiny apps can be run without a server.r and ui.r file.


shiny 0.5.0
===========

* Switch from websockets package for handling websocket connections to httpuv.

* New method for detecting hidden output objects. Instead of checking that
  height and width are 0, it checks that the object or any ancestor in the DOM
  has style display:none.

* Add `clientData` reactive values object, which carries information about the
  client. This includes the hidden status of output objects, height/width plot
  output objects, and the URL of the browser.

* Add `parseQueryString()` function.

* Add `renderImage()` function for sending arbitrary image files to the client,
  and its counterpart, `imageOutput()`.

* Add support for high-resolution (Retina) displays.

* Fix bug #55, where `renderTable()` would throw error with an empty data frame.


shiny 0.4.1
===========

* Fix bug where width and height weren't passed along properly from
  `reactivePlot` to `renderPlot`.

* Fix bug where infinite recursion would happen when `reactivePlot` was passed
  a function for width or height.


shiny 0.4.0
===========

* Added suspend/resume capability to observers.

* Output objects are automatically suspended when they are hidden on the user's
  web browser.

* `runGist()` accepts GitHub's new URL format, which includes the username.

* `reactive()` and `observe()` now take expressions instead of functions.

* `reactiveText()`, `reactivePlot()`, and so on, have been renamed to
  `renderText()`, `renderPlot()`, etc.  They also now take expressions instead
  of functions.

* Fixed a bug where empty values in a numericInput were sent to the R process
  as 0. They are now sent as NA.


shiny 0.3.1
===========

* Fix issue #91: bug where downloading files did not work.

* Add [[<- operator for shinyoutput object, making it possible to assign values
  with `output[['plot1']] <- ...`.

* Reactive functions now preserve the visible/invisible state of their returned
  values.


shiny 0.3.0
===========

* Reactive functions are now evaluated lazily.

* Add `reactiveValues()`.

* Using `as.list()` to convert a reactivevalues object (like `input`) to a list
  is deprecated. The new function `reactiveValuesToList()` should be used
  instead.

* Add `isolate()`. This function is used for accessing reactive functions,
  without them invalidating their parent contexts.

* Fix issue #58: bug where reactive functions are not re-run when all items in
  a checkboxGroup are unchecked.

* Fix issue #71, where `reactiveTable()` would return blank if the first
  element of a data frame was NA.

* In `plotOutput`, better validation for CSS units when specifying width and
  height.

* `reactivePrint()` no longer displays invisible output.

* `reactiveText()` no longer displays printed output, only the return value
  from a function.

* The `runGitHub()` and `runUrl()` functions have been added, for running
  Shiny apps from GitHub repositories and zip/tar files at remote URLs.

* Fix issue #64, where pressing Enter in a textbox would cause a form to
  submit.

shiny 0.2.4
===========

* `runGist` has been updated to use the new download URLs from
  https://gist.github.com.

* Shiny now uses `CairoPNG()` for output, when the Cairo package is available.
  This provides better-looking output on Linux and Windows.

shiny 0.2.3
===========

* Ignore request variables for routing purposes

shiny 0.2.2
===========

* Fix CRAN warning (assigning to global environment)


shiny 0.2.1
===========

* [BREAKING] Modify API of `downloadHandler`: The `content` function now takes
  a file path, not writable connection, as an argument. This makes it much
  easier to work with APIs that only write to file paths, not connections.


shiny 0.2.0
===========

* Fix subtle name resolution bug--the usual symptom being S4 methods not being
  invoked correctly when called from inside of ui.R or server.R


shiny 0.1.14
===========

* Fix slider animator, which broke in 0.1.10


shiny 0.1.13
===========

* Fix temp file leak in reactivePlot


shiny 0.1.12
===========

* Fix problems with runGist on Windows

* Add feature for on-the-fly file downloads (e.g. CSV data, PDFs)

* Add CSS hooks for app-wide busy indicators


shiny 0.1.11
===========

* Fix input binding with IE8 on Shiny Server

* Fix issue #41: reactiveTable should allow print options too

* Allow dynamic sizing of reactivePlot (i.e. using a function instead of a fixed
  value)


shiny 0.1.10
===========

* Support more MIME types when serving out of www

* Fix issue #35: Allow modification of untar args

* headerPanel can take an explicit window title parameter

* checkboxInput uses correct attribute `checked` instead of `selected`

* Fix plot rendering with IE8 on Shiny Server


shiny 0.1.9
===========


* Much less flicker when updating plots

* More customizable error display

* Add `includeText`, `includeHTML`, and `includeMarkdown` functions for putting
  text, HTML, and Markdown content from external files in the application's UI.


shiny 0.1.8
===========

* Add `runGist` function for conveniently running a Shiny app that is published
  on gist.github.com.

* Fix issue #27: Warnings cause reactive functions to stop executing.

* The server.R and ui.R filenames are now case insensitive.

* Add `wellPanel` function for creating inset areas on the page.

* Add `bootstrapPage` function for creating new Bootstrap based
  layouts from scratch.


shiny 0.1.7
===========

* Fix issue #26: Shiny.OutputBindings not correctly exported.

* Add `repeatable` function for making easily repeatable versions of random
  number generating functions.

* Transcode JSON into UTF-8 (prevents non-ASCII reactivePrint values from
  causing errors on Windows).


shiny 0.1.6
===========

* Import package dependencies, instead of attaching them (with the exception of
  websockets, which doesn't currently work unless attached).

* conditionalPanel was animated, now it is not.

* bindAll was not correctly sending initial values to the server; fixed.


shiny 0.1.5
===========

* BREAKING CHANGE: JS APIs Shiny.bindInput and Shiny.bindOutput removed and
  replaced with Shiny.bindAll; Shiny.unbindInput and Shiny.unbindOutput removed
  and replaced with Shiny.unbindAll.

* Add file upload support (currently only works with Chrome and Firefox). Use
  a normal HTML file input, or call the `fileInput` UI function.

* Shiny.unbindOutputs did not work, now it does.

* Generally improved robustness of dynamic input/output bindings.

* Add conditionalPanel UI function to allow showing/hiding UI based on a JS
  expression; for example, whether an input is a particular value. Also works in
  raw HTML (add the `data-display-if` attribute to the element that should be
  shown/hidden).

* htmlOutput (CSS class `shiny-html-output`) can contain inputs and outputs.


shiny 0.1.4
===========

* Allow Bootstrap tabsets to act as reactive inputs; their value indicates which
  tab is active

* Upgrade to Bootstrap 2.1

* Add `checkboxGroupInput` control, which presents a list of checkboxes and
  returns a vector of the selected values

* Add `addResourcePath`, intended for reusable component authors to access CSS,
  JavaScript, image files, etc. from their package directories

* Add Shiny.bindInputs(scope), .unbindInputs(scope), .bindOutputs(scope), and
  .unbindOutputs(scope) JS API calls to allow dynamic binding/unbinding of HTML
  elements


shiny 0.1.3
===========

* Introduce Shiny.inputBindings.register JS API and InputBinding class, for
  creating custom input controls

* Add `step` parameter to numericInput

* Read names of input using `names(input)`

* Access snapshot of input as a list using `as.list(input)`

* Fix issue #10: Plots in tabsets not rendered


shiny 0.1.2
===========

* Initial private beta release!
