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

## Prepare for release

### `git pull`

Rebase the rc branch on `origin/main`.

### Check current CRAN check results

```bash
curl -s https://cran.r-project.org/web/checks/check_results_shiny.html \
  | sed 's/<[^>]*>/ /g' | grep -E '\S' | head -60
```

For any failure, find out whether `main` already fixes it
(`git log vA.B.C..origin/main -- <file>`). Note it for `cran-comments.md`.

### Advance deprecations

List the calls with `grep -rn 'shinyDeprecated(' R/`. Ask the user whether to
advance any. Often skipped.

### Polish NEWS

Follow [references/update-news.md](references/update-news.md). Show the diff
and let the user comment before committing.

### `urlchecker::url_check()`

Fix moved URLs in the roxygen source in `R/`, not in `man/`, then run
`devtools::document()` and check again.

### `devtools::check(remote = TRUE, manual = TRUE)`

Run it in the background:

```bash
mkdir -p .context/check
Rscript -e 'res <- devtools::check(remote = TRUE, manual = TRUE,
  check_dir = ".context/check", error_on = "never"); print(res)' \
  > .context/check/check.log 2>&1
```

Expected note: "CRAN incoming feasibility". It lists the maintainer, the
`.9000` version (gone after the version bump), and the ROR link
`https://ror.org/03wc8by49` with status 429. ror.org rate-limits the check; the
ID is valid (`curl -s https://api.ror.org/v2/organizations/03wc8by49`).

### Bump the version (before win-builder)

Do this before `check_win_devel()` so win-builder does not flag the `.9000`
version. The issue lists `use_version()` under "Submit to CRAN"; tick it there.

```bash
Rscript -e 'usethis::use_version("minor")'   # or "patch"
Rscript tools/updatePackageJsonVersion.R      # package.json version
npm run build                                 # version banner in inst/www/shared/
```

- `use_version()` updates `DESCRIPTION` and the NEWS heading but does not
  commit when not interactive. It refuses to run with uncommitted changes, so
  set them aside first (copy to `.context/`, restore after). Do not use a bare
  `git stash`.
- `npm run build` should only change the `/*! shiny X.Y.Z ...` banner lines.
  Check the diff before committing.
- Commit: `chore: Increment version number to X.Y.Z`.

### `devtools::check_win_devel()`

Results go to the maintainer email in `DESCRIPTION` by default. To send them to
the person running the release:

```r
devtools::check_win_devel(email = "<their email>")
```

Changing the email adds "New maintainer" to the incoming note. That only
applies to this upload; the real submission uses the `DESCRIPTION` email.
Results are at the `https://win-builder.r-project.org/<id>/` link in the
email; read `00check.log` there.

### `revdepcheck::cloud_check()`

Takes about 1.5 hours for ~1700 packages. Build the tarball into `.context/`
and watch the job in the background:

```bash
mkdir -p .context/revdep
Rscript -e '
tb <- pkgbuild::build(".", dest_path = ".context/revdep", quiet = TRUE)
job <- revdepcheck::cloud_check(tarball = tb)
writeLines(job, ".context/revdep/job_id.txt")'
```

```r
# .context/revdep/watch.R
job <- readLines(".context/revdep/job_id.txt")
revdepcheck::cloud_status(job, update_interval = 60)
revdepcheck::cloud_fetch_results(job)
revdepcheck::cloud_report(job)
```

- Needs `RSTUDIO_CLOUD_REVDEP_KEY` set.
- `cloud_report()` always writes `README.md`, `cran.md`, `problems.md` and
  `failures.md` into `revdep/`, even with `file =`. Commit `revdep/README.md`
  and `revdep/cran.md`; `failures.md` is gitignored. Raw results go to
  `revdep/cloud.noindex/` (gitignored).
- For each package that failed to check, check whether it also fails with the
  CRAN version of shiny (the "In both" part of `failures.md`).

### Update `cran-comments.md`

Sections:

1. `## Comments`: what this release fixes (for example, the CRAN check
   failures from above) and a short summary. Signed by the maintainer in
   `DESCRIPTION`. Drop old dated notes from earlier releases.
2. `` ## `R CMD check` results: `` — `0 errors | 0 warnings | 1 note` and the
   note text, with one line explaining the ROR 429.
3. `## revdepcheck results`: from `revdep/cran.md`. In "Failed to check", list
   bare package names (no `(NA)`, no padding).

### `git push`

Push the branch and watch CI. Check the run on the PR's **current head** SHA
(`gh run list --branch rc-vX.Y.Z --json headSha,conclusion`). If a `routine`
run failed at "Git Push (PR)" with `[rejected] ... (fetch first)`, a newer push
won the race; it is fine when the newer run passes and adds no commits.

### Draft blog post

Posts live in `posit-dev/open-source-website`. Use the `release-post` skill or
link the PR the user gives you.

### Update static imports: `staticimports::import()`

staticimports is often not installed. Install it into a temporary library:

```bash
mkdir -p .context/rlib
Rscript -e 'pak::pak("wch/staticimports", lib = ".context/rlib", ask = FALSE)'
R_LIBS=.context/rlib Rscript -e 'staticimports::import()'
git diff -- R/staticimports.R
```

Usually no diff.

## Submit to CRAN

Stop here and wait for the user to ask. Then:

- `devtools::submit_cran()` uploads the package. It cannot be undone.
- CRAN emails the maintainer in `DESCRIPTION` to confirm. Only they can approve.

## After CRAN accepts

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
6. Share on social media, and post the blog, Bluesky and LinkedIn links in
   `#open-source-comms` on Slack. The user does these.

## Ticking boxes

Update the release issue as each item is done:

```bash
gh issue view <N> -R rstudio/shiny --json body -q .body > .context/issue_body.md
sed -i '' 's/^\* \[ \] `urlchecker::url_check()`/* [x] `urlchecker::url_check()`/' \
  .context/issue_body.md
gh issue edit <N> -R rstudio/shiny --body-file .context/issue_body.md
```

For a skipped item, strike it through and say so:
`* [x] ~Advance deprecations~ (skipped for this release)`.
