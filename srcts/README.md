# JavaScript build tools

## First-time setup
Shiny's JavaScript build tools use Node.js, along with [yarn](https://yarnpkg.com/) to manage the JavaScript packages.

Installation of Node.js differs across platforms, see [the official Node.js website](https://nodejs.org/) for instructions on downloading and installing. We presume that you have Node.js installed on your machine before continuing.

Install yarn using the [official instructions](https://yarnpkg.com/en/docs/install).

You can test that Node.js and yarn are installed properly by running the following commands:

```bash
node --version
yarn --version
```

Once both are installed, run the following in this directory (`srcts/`) to install the packages :

```bash
yarn install
```

### Yarn v2

This repo uses Yarn v2. If you have the latest yarn installed from brew (v1.x), Yarn will pick up on the fact that this repo is a Yarn v2 (`berry`) repo.

For compatibility with `esbuild`, the `./node_modules` is still maintained.

## Updating Node

To avoid finding and determining which is the best `node` version to use, use a package called `n` to help facilitate installing the latest stable version of node.

```bash
# Only need to install `n` once
yarn global add n

# Update to the latest stable node version
n stable

# View installed versions
n ls
```

## Adding packages
If in the future you want to upgrade or add a package, run:

```bash
yarn add --dev [packagename]
```

This will automatically add the package to the dependencies in package.json, and it will also update the yarn.lock to reflect that change. If someone other than yourself does this, simply run `yarn` to update your local packages to match the new package.json.

## Upgrading packages
Periodically, it's good to upgrade the packages to a recent version. There's two ways of doing this, depending on your intention:

1. Use `yarn upgrade` to upgrade all dependencies to their latest version based on the version range specified in the package.json file (the yarn.lock file will be recreated as well. Yarn packages use [semantic versioning](https://yarnpkg.com/en/docs/dependency-versions), i.e. each version is writen with a maximum of 3 dot-separated numbers such that: `major.minor.patch`. For example in the version `3.1.4`, 3 is the major version number, 1 is the minor version number and 4 is the patch version number. Here are the most used operators (these appear before the version number):

  - `~` is for upgrades that keep the minor version the same (assuming that was specified);

  - `^` is for upgrades that keep the major version the same (more or less -- more specifically, it allow changes that do not modify the first non-zero digit in the version, either the 3 in 3.1.4 or the 4 in 0.4.2.). This is the default operator added to the package.json when you run `yarn add [package-name]`.

2. Use `yarn upgrade [package]` to upgrade a single named package to the version specified by the latest tag (potentially upgrading the package across major versions).

For more information about upgrading or installing new packages, see the [yarn workflow documentation](https://yarnpkg.com/en/docs/yarn-workflow).

## Bundling the TypeScript

[esbuild](https://esbuild.github.io/) is a build tool that (for Shiny's purposes) compiles the TypeScript into a single JavaScript file.

## Using `esbuild`

To run all default build tasks, simply go into the `./srcts` directory and run:

```bash
yarn build
```

<!-- Sometimes grunt gets confused about whether the output files are up to date, and won't overwrite them even if the input files have changed. If this happens, run:

```bash
yarn clean
``` -->

It's also useful to have `esbuild` watch for updated files and immediately re-build `shiny.js` as necessary during development. This is done with:

```bash
yarn watch
```

Be sure to use `shiny::devmode()` to enable "un-minified" JavaScript files as `yarn watch` only creates `shiny.js` and not `shiny.min.js`.

Both JavaScript files will produce a sourcemap (`**.js.map`) that the browser will understand.  This will help you debug Shiny's JavaScript code within the browser and point back to the original TypeScript files.

<!-- #### Auto build and browser refresh

An alternative to `yarn watch` is to use `entr` to trigger `grunt` when sources change. `entr` can be installed with `brew install entr` on a Mac, or on Linux using your distribution's package manager. Using this technique, it's possible to both automatically rebuild sources and reload Chrome at the same time:

*macOS*:

```bash
find ../srcts/ | entr bash -c './node_modules/grunt/bin/grunt && osascript -e "tell application \"Google Chrome\" to reload active tab of window 1"'
```

*Linux*:

For this to work you must first install `xdotool` using your distribution's package manager.

```bash
find ../srcts/ | entr bash -c './node_modules/grunt/bin/grunt && xdotool search --onlyvisible --class Chrome windowfocus key ctrl+r'
``` -->

## Development in VSCode

VSCode does not like to develop TypeScript in a subfolder. To leverage full VSCode capabilities, it is recommended to open the `./srcts` folder as the root folder of a project. This will enable VSCode to readily find all of the compilation and linting configuration files.

# Updating web libraries
## `@types/jquery`

As of v3.5.5, `@types/jquery` produces a globally available constant of `$` and `jQuery`. This is problematic as TypeScript is there to enforce that all variables are accounted for. Declaring that these two variables exist globally removes the requirement to import `$` (or `jQuery`). This is bad for Shiny as the `$` would not be enforced to the "within package" `$`.

To overcome this, a patch is used to remove the two variable declarations. Yarn v2 has a [`patch` protocol](https://yarnpkg.com/features/protocols#patch) that it understands. A local patch is applied to the publically available `@types/jquery` package upon installation.

If in the future the variables are not declared anymore, the patch may be removed and `@types/jquery` can be imported directly.

## `babel-polyfill`

To update the version of `babel-polyfill`:

* Check if there is a newer version available by running `yarn outdated babel-polyfill`. (If there's no output, then you have the latest version.)
* Run `yarn add --dev babel-polyfill --exact`.
* Edit R/shinyui.R. The `renderPage` function has an `htmlDependency` for
  `babel-polyfill`. Update this to the new version number.
