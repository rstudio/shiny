---
name: r-shiny-release
description: >
  Walk through a Shiny for R (rstudio/shiny) CRAN release step by step. Use when
  the user says "release", "start a release", "release shiny", "rc branch", or
  wants to work through the usethis release checklist issue, or asks to polish
  NEWS.md for a release. Covers the release issue, rc branch and PR, NEWS,
  checks, revdeps, cran-comments, version bump, CRAN submission, GitHub
  release, and post-release updates such as the shiny-dev-center reference
  docs.
---

# Shiny for R release

Work through the release one step at a time. The usethis release issue is the
checklist; this skill adds the shiny-specific details for each item.

## Ground rules

- Do one checklist item at a time. After each item, report the result and wait
  for the user before starting the next one.
- If the user says "skip", mark the item as skipped on the issue (see
  [Ticking boxes](#ticking-boxes)) and move on.
- **Never run `devtools::submit_cran()`, tag a release, or merge the release PR
  unless the user asks for that step directly.** Ask, then wait.
- Long jobs (`devtools::check()`, `revdepcheck::cloud_check()`, CI) run in the
  background. Keep their logs and status files in `.context/` (gitignored).
- Commit each step on the rc branch with a conventional commit message, for
  example `docs(news): Polish NEWS for v1.15.0` or
  `chore(revdep): Update revdep results for v1.15.0`. Only the PR title uses
  the `release:` type.
- Pull before pushing. The `routine` CI job can push commits to the PR branch
  (see `memory-bank/commitConventions.md`).

## Getting started

1. Ask for the version. Look at the last tag (`git describe --tags --abbrev=0`)
   and the `# shiny (development version)` section of `NEWS.md` and suggest
   minor (new features) or patch (fixes only).
2. Create the release issue:
   ```r
   usethis::use_release_issue(version = "X.Y.Z")
   ```
   This **creates the GitHub issue right away**; it does not just open a
   prefilled page. Without `version`, it asks in a menu, which fails when not
   interactive.
3. Create the branch `rc-vX.Y.Z` (rename the current branch if the user asks).
4. Open a draft PR. A PR needs at least one commit, so open it with the first
   step's commit (the revdep results work well).
   - Title: `release: vX.Y.Z`
   - Body: link to the release issue and
     `https://github.com/rstudio/shiny/compare/vA.B.C...rc-vX.Y.Z`

### Ticking boxes

Update the release issue as each item is done:

```bash
gh issue view <N> -R rstudio/shiny --json body -q .body > .context/issue_body.md
sed -i '' 's/^\* \[ \] `urlchecker::url_check()`/* [x] `urlchecker::url_check()`/' \
  .context/issue_body.md
gh issue edit <N> -R rstudio/shiny --body-file .context/issue_body.md
```

For a skipped item, strike it through and say so:
`* [x] ~Advance deprecations~ (skipped for this release)`.

## Prepare for release

Follow [references/prepare-release.md](references/prepare-release.md). It
covers each item in the issue's "Prepare for release" list, plus the version
bump, which is done before `devtools::check_win_devel()`.

## Submit to CRAN

Stop here and wait for the user to ask. Then:

- `devtools::submit_cran()` uploads the package. It cannot be undone.
- CRAN emails the maintainer in `DESCRIPTION` to confirm. Only they can approve.

## After CRAN accepts

Follow [references/after-cran.md](references/after-cran.md): merge the release
PR, tag and create the GitHub release, start the next dev version, publish the
blog post, update shiny-dev-center, and get the new version into shinylive.
