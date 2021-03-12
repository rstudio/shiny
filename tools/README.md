This directory contains build tools for Shiny.

For TypeScript / JavaScript build tools, see the `./srcts` folder.

Updating web libraries
======================

## Updating and patching `bootstrap-datepicker`

### Updating

[bootstrap-datepicker](https://github.com/uxsolutions/bootstrap-datepicker) can be updated with the script `updateBootstrapDatepicker.R`.

After updating, our patches to `bootstrap-datepicker` must be applied using the script `applyDatepickerPatches.R`

After updating and applying patches, `yarn grunt` should be run per the instructions above in order to generate a minified JavaScript file.

### Making a new patch

To create a new patch:

1. Make any necessary changes to files in `inst/www/shared/datepicker`
1. **Do not commit your changes.**
1. Instead, create a patch with a command like `git diff > tools/datepicker-patches/012-a-description.patch`. Patches are applied in alphabetic order (per `list.files`), so you should name your patch based on the last one in `tools/datepicker-patches` so that it's applied last.
1. Revert `bootstrap-datepicker` to its unpatched state by running `updateBootstrapDatepicker.R`
1. Apply all patches, including the one you just made, by running `applyDatepickerPatches.R`
1. Run `yarn grunt`
1. Test your changes
1. `git add` the new `.patch` and any resulting changes


## Updating and patching ion.rangeSlider

### Updating

[ion.rangeSlider](https://github.com/IonDen/ion.rangeSlider) can be updated with the script `updateIonRangeSlider.R`. That script downloads a specific version of ion.rangeSlider and applies our patches in tools/ion.rangeSlider-patches.

After updating and applying patches, `yarn grunt` should be run per the instructions above in order to generate a minified JavaScript file.

### Making a new patch

To create a new patch:

1. Make any necessary changes to files in `inst/www/shared/ion.rangeSlider`
1. **Do not commit your changes.**
1. Instead, create a patch with a command like `git diff > tools/ion.rangeSlider-patches/0004-a-description.patch`. Patches are applied in alphabetic order (per `list.files`), so you should name your patch based on the last one in `tools/ion.rangeSlider-patches` so that it's applied last.
1. Run `updateIonRangeSlider.R` to download the library and apply patches.
1. Run `yarn grunt`
1. Test your changes
1. `git add` the new `.patch` and any resulting changes


## Updating Font-Awesome

* Edit `updateFontAwesome.R` to use the new version, and then run it. This will download and copy the files to the relevant locations.
* Update the "font-awesome" htmlDependency in `R/bootstrap.R` to reflect the new version.
* Update the documentation for the `icon()` function in `R/bootstrap.R` to reflect the new version.
* Run `devtools::document()`.
* Commit the changes.

## Updating jQuery

* * Edit `updatejQuery.R` to use the new version, and then run it. This will download and copy the files to the relevant locations.
* Update the "jquery" htmlDependency in `R/shinyui.R` to reflect the new version.
* Update the documentation for the `shiny.jquery.version` option in `R/shiny-options.R` to reflect the new version.
* Run `devtools::document()`.
* Commit the changes.


## Updating Bootstrap-Accessibility-Plugin

* [bootstrap-accessibility-plugin](https://github.com/paypal/bootstrap-accessibility-plugin) can be updated with the script `updateBootstrapAccessibilityPlugin.R`.
* Edit `updateBootstrapAccessibilityPlugin.R` to use the new version, and then run it. This will download and copy the files to the relevant locations.
* Update the documentation for the `bootstrapLib()` function in `R/bootstrap.R` to reflect the new version.
* Run `devtools::document()`.
* Commit the changes.


## Updating and patching selectize

### Updating

[selectize](https://github.com/selectize/selectize.js) and [its accessibility plugin](https://github.com/SLMNBJ/selectize-plugin-a11y) can be updated with the script `updateSelectize.R`. That script downloads a specific version of selectize and selectize-plugin-a11y, and applies our patches in tools/selectize-patches.

After updating and applying patches, `yarn grunt` should be run per the instructions above in order to generate a minified JavaScript file.

### Making a new patch

To create a new patch:

1. Make any necessary changes to files in `inst/www/shared/selectize`
1. **Do not commit your changes.**
1. Instead, create a patch with a command like `git diff > tools/selectize-patches/000-assign-unique-id-per-option.patch`. Patches are applied in alphabetic order (per `list.files`), so you should name your patch based on the last one in `tools/selectize-patches` so that it's applied last.
1. Run `updateSelectize.R` to download the library and apply patches.
1. Run `yarn grunt`
1. Test your changes
1. `git add` the new `.patch` and any resulting changes

## Updating Shiny's [S]CSS

1. Make any desired changes to source files in `inst/www/shared/shiny_scss`
1. Run `Rscript updateShinyCSS.R` to generate a built `shiny.min.css` file
1. Commit any changes
