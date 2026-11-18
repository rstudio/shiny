# After CRAN accepts

Do these in order once CRAN has accepted the release.

1. Mark the release PR ready and squash merge it. The subject should be
   `release: vX.Y.Z (#N)`. Earlier release tags point at the squash commit on
   `main`.
2. Tag and create the GitHub release from `main`:
   ```r
   usethis::use_github_release()
   ```
   The release name is `shiny X.Y.Z` and the body is the NEWS section.
3. `usethis::use_dev_version(push = TRUE)` from `main`, then sync
   `package.json` and run `npm run build` as for the version bump.
4. Publish the blog post. The issue's "Add link to blog post in pkgdown news
   menu" item does not apply: shiny has no pkgdown site of its own
   (`tools/documentation/pkgdown.yml` only defines the reference index that
   shiny-dev-center uses). Mark it skipped.
5. Update [rstudio/shiny-dev-center](https://github.com/rstudio/shiny-dev-center)
   reference docs. Its `Build R reference` workflow builds docs for any shiny
   tag that is missing and opens a PR from `bot/r-reference-<version>`
   (titled like `Update R Shiny reference docs (vX.Y.Z)`). It runs on pushes
   to its `main`, so after tagging, start it by hand:
   ```bash
   gh workflow run build-r-reference.yml -R rstudio/shiny-dev-center
   ```
   Then review and merge the bot PR; merging deploys the site.
6. Update shinylive. See [Shinylive](#shinylive) below. This is not on the
   usethis release issue.
7. Share on social media, and post the blog, Bluesky and LinkedIn links in
   `#open-source-comms` on Slack. The user does these.

## Shinylive

Shinylive does not install shiny from CRAN. It uses a webR build of the
[r-wasm/shiny](https://github.com/r-wasm/shiny) fork, so a CRAN release does
not reach shinylive until both steps below are done.

### 1. Release `vX.Y.Z.8000` on r-wasm/shiny

The fork carries a few commits on top of each shiny tag. Compare the last fork
release with its shiny tag to see them:

```bash
gh release list -R r-wasm/shiny --limit 3
gh api 'repos/r-wasm/shiny/compare/rstudio:shiny:vA.B.C...r-wasm:shiny:vA.B.C.8000' \
  -q '.commits[] | "\(.sha[0:8]) \(.commit.message|split("\n")[0])"'
```

As of `v1.13.0.8000`, they:

- set `Version: X.Y.Z.8000` and add a `Config/Needs/wasm:` list of shiny's
  dependencies to `DESCRIPTION`
- use less call stack in `hybrid_chain()` in `R/utils.R` (webR has a small
  stack)
- add `.github/workflows/release-file-system-image.yml`, which builds the webR
  library image when a GitHub release is **published** (republish to rebuild)

To make the new release:

1. Branch from the new shiny tag `vX.Y.Z` and reapply the fork commits.
2. Compare shiny's `Imports` with the `Config/Needs/wasm` list and add any new
   dependencies. Every dependency must exist in webR's package repo:
   ```bash
   curl -s https://repo.r-wasm.org/bin/emscripten/contrib/4.6/PACKAGES \
     | grep -A1 '^Package: <pkg>$'
   ```
   If one is missing, it has to be built for webR, or the fork has to drop it.
3. Tag `vX.Y.Z.8000` and publish a GitHub release named `Shiny X.Y.Z.8000`
   ("Shiny X.Y.Z, modified for use with webR."). Wait for the workflow to
   attach `library.data.gz` and `library.js.metadata`.

Pushing to r-wasm/shiny needs access to the r-wasm org. If the user does not
have it, open an issue there instead.

### 2. Bump shiny in posit-dev/shinylive

In [posit-dev/shinylive](https://github.com/posit-dev/shinylive), set
`R_SHINY_VERSION = X.Y.Z.8000` in `Makefile`. `make webr` downloads the two
files from the r-wasm/shiny release into `build/shinylive/webr/`. Open a PR,
then release shinylive. py-shinylive and r-shinylive pick up the new shinylive
assets in their own releases.
