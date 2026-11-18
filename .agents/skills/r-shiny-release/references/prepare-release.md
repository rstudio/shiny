# Prepare for release

Details for each item in the "Prepare for release" part of the release issue,
in order. Do one item at a time.

## `git pull`

Rebase the rc branch on `origin/main`.

## Check current CRAN check results

```bash
curl -s https://cran.r-project.org/web/checks/check_results_shiny.html \
  | sed 's/<[^>]*>/ /g' | grep -E '\S' | head -60
```

For any failure, find out whether `main` already fixes it
(`git log vA.B.C..origin/main -- <file>`). Note it for `cran-comments.md`.

## Advance deprecations

List the calls with `grep -rn 'shinyDeprecated(' R/`. Ask the user whether to
advance any. Often skipped.

## Polish NEWS

Follow [update-news.md](update-news.md). Show the diff
and let the user comment before committing.

## `urlchecker::url_check()`

Fix moved URLs in the roxygen source in `R/`, not in `man/`, then run
`devtools::document()` and check again.

## `devtools::check(remote = TRUE, manual = TRUE)`

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

## Bump the version (before win-builder)

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

## `devtools::check_win_devel()`

Results go to the maintainer email in `DESCRIPTION` by default. To send them to
the person running the release:

```r
devtools::check_win_devel(email = "<their email>")
```

Changing the email adds "New maintainer" to the incoming note. That only
applies to this upload; the real submission uses the `DESCRIPTION` email.
Results are at the `https://win-builder.r-project.org/<id>/` link in the
email; read `00check.log` there.

## `revdepcheck::cloud_check()`

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

## Update `cran-comments.md`

Sections:

1. `## Comments`: what this release fixes (for example, the CRAN check
   failures from above) and a short summary. Signed by the maintainer in
   `DESCRIPTION`. Drop old dated notes from earlier releases.
2. `` ## `R CMD check` results: `` — `0 errors | 0 warnings | 1 note` and the
   note text, with one line explaining the ROR 429.
3. `## revdepcheck results`: from `revdep/cran.md`. In "Failed to check", list
   bare package names (no `(NA)`, no padding).

## `git push`

Push the branch and watch CI. Check the run on the PR's **current head** SHA
(`gh run list --branch rc-vX.Y.Z --json headSha,conclusion`). If a `routine`
run failed at "Git Push (PR)" with `[rejected] ... (fetch first)`, a newer push
won the race; it is fine when the newer run passes and adds no commits.

## Draft blog post

Posts live in `posit-dev/open-source-website`. Use the `release-post` skill or
link the PR the user gives you.

## Update static imports: `staticimports::import()`

staticimports is often not installed. Install it into a temporary library:

```bash
mkdir -p .context/rlib
Rscript -e 'pak::pak("wch/staticimports", lib = ".context/rlib", ask = FALSE)'
R_LIBS=.context/rlib Rscript -e 'staticimports::import()'
git diff -- R/staticimports.R
```

Usually no diff.
