# Contributing to Shiny

We welcome contributions to the **shiny** package. To submit a contribution:

1. [Fork](https://github.com/rstudio/shiny/fork) the repository and make your changes.

2. Submit a [pull request](https://help.github.com/articles/using-pull-requests).

3. Ensure that you have signed the contributor license agreement. It will appear as a "Check"
   on your PR and a comment from "CLAassistant" will also appear explaining whether you have
   yet to sign. After you sign, you can click the "Recheck" link in that comment and the check
   will flip to reflect that you've signed.

We generally do not merge pull requests that update included web libraries (such as Bootstrap or jQuery) because it is difficult for us to verify that the update is done correctly; we prefer to update these libraries ourselves.

## How to make changes

Before you submit a pull request, please do the following:

* Add an entry to `NEWS.md` concisely describing what you changed.

* If appropriate, add unit tests in the tests/ directory.

* If you made any changes to the JavaScript files in the `srcjs/` directory, make sure you build the output JavaScript files. See `tools/README.md` file for information on using the build system.

* Run Build->Check Package in the RStudio IDE, or `devtools::check()`, to make sure your change did not add any messages, warnings, or errors.

Doing these things will make it easier for the Shiny development team to evaluate your pull request. Even so, we may still decide to modify your code or even not merge it at all. Factors that may prevent us from merging the pull request include:

* breaking backward compatibility
* adding a feature that we do not consider relevant for Shiny
* is hard to understand
* is hard to maintain in the future
* is computationally expensive
* is not intuitive for people to use

We will try to be responsive and provide feedback in case we decide not to merge your pull request.


## Filing issues

If you find a bug in Shiny, you can also [file an issue](https://github.com/rstudio/shiny/issues/new). Please provide as much relevant information as you can, and include a minimal reproducible example if possible.

## Development environment setup

As a web technology, contributing to Shiny involves setting up a Javscript environment,
as well as the corresponding R or Python environment.

### Setup: JavaScript / TypeScript

You will need to have [node.js](https://nodejs.org/en/download) installed.np

We will be using [node version manager (nvm)](https://github.com/nvm-sh/nvm) to install node.js and [node package manager (npm)]() to install packages.

See [this workflow](https://github.com/rstudio/shiny-workflows/blob/main/.github/workflows/routine.yaml#L18) that lists the version of node.js that shiny is using in CI/CD.

#### yarn

```bash
# Download and install Yarn:
corepack enable yarn

# Verify Yarn version:
yarn -v
```


#### Install dependencies

```bash
yarn install --immutable
```

#### Shiny TypeScript definitions

```bash
npm install https://github.com/rstudio/shiny\#v1.11.1 --save-dev
# or
yarn add shiny@https://github.com/rstudio/shiny\#v1.11.1 --dev
```


#### `core-js`

To update the version of `core-js`:

* Check if there is a newer version available by running `yarn outdated core-js`. (If there's no output, then you have the latest version.)
* Run `yarn add --dev core-js --exact`.


#### Updating node


To avoid finding and determining which is the best `node` version to use, use a package called `n` to help facilitate installing the latest stable version of node.

> `npx` is a command that allows you to run a package and command even if the package is not installed. It is distributed with the latest releases of node.

```bash
# Update to the latest stable node version
npx n stable

# View installed versions
npx n ls
```

### Adding packages
If in the future you want to upgrade or add a package, run:

```bash
yarn add --dev [packagename]
```

This will automatically add the package to the dependencies in `package.json`, and it will also update the `yarn.lock` to reflect that change. If someone other than yourself does this, simply run `yarn` to update your local packages to match the new `package.json`.

### Upgrading packages
Periodically, it's good to upgrade the packages to a recent version. There's two ways of doing this, depending on your intention:

1. Use `yarn up` to upgrade all dependencies to their latest version based on the version range specified in the package.json file (the `yarn.lock` file will be recreated as well. Yarn packages use [semantic versioning](https://yarnpkg.com/en/docs/dependency-versions), i.e. each version is writen with a maximum of 3 dot-separated numbers such that: `major.minor.patch`. For example in the version `3.1.4`, 3 is the major version number, 1 is the minor version number and 4 is the patch version number. Here are the most used operators (these appear before the version number):

  - `~` is for upgrades that keep the minor version the same (assuming that was specified);

  - `^` is for upgrades that keep the major version the same (more or less -- more specifically, it allow changes that do not modify the first non-zero digit in the version, either the 3 in 3.1.4 or the 4 in 0.4.2.). This is the default operator added to the package.json when you run `yarn add [package-name]`.

2. Use `yarn up [package]` to upgrade a single named package to the version specified by the latest tag (potentially upgrading the package across major versions).

3. To see all outdated packages, run `yarn outdated`
