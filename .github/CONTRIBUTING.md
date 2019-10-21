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

* Add an entry to NEWS.md concisely describing what you changed.

* If appropriate, add unit tests in the tests/ directory.

* If you made any changes to the JavaScript files in the srcjs/ directory, make sure you build the output JavaScript files. See tools/README.md file for information on using the build system.

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
