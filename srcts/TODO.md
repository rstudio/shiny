# Approach
* Move everything into a single ts file. This will allow for the functions to find themselves
  * Except the utils.js already converted
* After merging this PR, each _file_ will be pulled out as possible into smaller files


# Rules
* Put Imports at top
* Put Exports at bottom
* Any `window.***` calls are done in `./external` folder only.
  * Add exported values / function if necessary.
  * This helps keep each file self contained. Trying not to have random inputs from anywhere

# TODO

* es6 shiny.js
  * √ Pass in version before compilation
    (* Can be run many times safely)
  * √ Move all shiny files in order to main.ts
  * √ validate polyfills are working by finding them in the code
  * √ delete 'old' folder
* es6 other shiny files (ex: showcasemode)
* Document package.json scripts
* Produce minified shiny js


# Later TODO

* Remove parcel from package.json and only use esbuild
* break up `./utils` into many files
* remove any `: any` types
* fix all `// eslint-disable-next-line no-prototype-builtins` lines
* Convert FileProcessor to a true class definition
* Minify showcase mode and other shiny js files
* delete 'shiny-es5' files
