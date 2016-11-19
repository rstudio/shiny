
Shiny is an open source project and we occasionally accept pull requests from outside contributors. These range from simple typo fixes to more complicated multi-file PRs. *The bar for us to merge a PR gets increasingly higher the more code you are changing or adding.*

We'll try to be as responsive as possible in reviewing and accepting pull requests. We appreciate your contributions!

### Trivial changes
For very trivial PRs (like typo fixes), you don't need to do anything else except submit the PR with a one-line explanation of what you are fixing. If you never forked and submitted a pull request to a repository before, you may benefit from reading [this guide](https://gist.github.com/Chaser324/ce0505fbed06b947d962). In very broad strokes, you need to [fork](https://github.com/rstudio/shiny/fork) the repository, make your changes and then submit a [pull request](https://help.github.com/articles/about-pull-requests/).

*Note*: We tend not to accept library update PRs (for example, a PR that updates Shiny to the latest version of Bootstrap or font-awesome). Since these can sometimes break previous examples and apps, we prefer to always do the updating ourselves.

### Non-trivial changes
For pretty much everything else, before you submit your PR, you need to:

- Check your code by running `devtools::check()` to make sure you did not add any messages, warnings or errors. This also checks to make sure that you've added all relevant documentation, that the DESCRIPTION and NAMESPACE files are up-to-date, etc.

- Test your code if needed, preferably through unit tests (which go in the "tests" folder).

- If you made any changes to the JS files in the "srcjs" directory, make sure you run `grunt` (from the terminal, navigate to the "tools" directory and enter the command `grunt`).

- Add a [NEWS.md](https://github.com/rstudio/shiny/blob/master/NEWS.md) entry (in the appropriate section) concisely describing what you changed.

- Be responsive to the Shiny team's feedback on your PR.

If you do these things, it makes our lives a lot easier and lets us judge the merit of your PR without worrying about all the boilerplate associated with changes to existing code. However, we may still decide to modify your code somewhat or even not merge it at all. There are a lot of factors at play here, including whether or not your change:

- breaks backward compatibility;
- adds a feature that we do not consider relevant for Shiny;
- is computationally expensive;
- is hard to maintain in the future;
- is intuitive for people to use and incorporate in their apps.

We will always try to be responsive and provide feedback in case we decide not to merge your PR. 

### RStudio Contributor Agreement
If we do decide to merge your non-trivial PR, you have one more step to complete, which is to **sign the [individual](http://www.rstudio.com/wp-content/uploads/2014/06/RStudioIndividualContributorAgreement.pdf) or [corporate](http://www.rstudio.com/wp-content/uploads/2014/06/RStudioCorporateContributorAgreement.pdf) contributor agreement** as appropriate. You only have to do this once (the first time you contribute), so if you've contributed in the past, we should still have your agreement on file.

This is necessary to let us make changes to the license in the future (for example, to change to a BSD license if GPL becomes a problem, or a later GPL version). If you're OK with that, then sign the form, scan it and email it to jj@rstudio.com.

### Filing issues is also a valuable contribution!
If you notice a bug or you think of a new feature that you'd like to see in Shiny, you can also [file an issue](https://github.com/rstudio/shiny/issues/new). This is ideal when you don't know how you'd solve the problem or even if you have an idea of how to do so, but you prefer to leave the coding up to us. Issues are actually an extremely helpful contribution, so don't hesitate to do so.

When you file an issue, please give as much information as you can (for example, if you think you know what is causing the bug, you could link to the appropriate file and line). Also, please provide a minimal reproducible example if appropriate (pretty much every time you're reporting a bug). This will allow us to quickly understand what's the problem and how we can fix it.
