---
title: "Git Commit and PR Conventions"
description: "Commit and PR title style used on main, squash-merge behavior, NEWS.md expectations, and what the shared routine CI workflow auto-commits to PR branches. Read before committing, opening, or merging a PR."
---

# Git Commit and PR Conventions

## Commit and PR title format

Most PRs use a **conventional-commit-style** title. This is a strong habit,
not an enforced check: no workflow lints titles.

```
<type>[(<scope>)][!]: <description> (#<PR>)
```

Conventions seen on `main`:

- Types: `fix`, `feat`, `docs`, `chore`, `refactor`, `ci`, `build`, `test`.
  Use `!` for a breaking change (`feat!(submitButton): ...`).
- Scopes name the affected area: `otel`, `test`, `check`, `js`, `scss`,
  `assets`, `license` (e.g. `fix(otel): ...`, `build(js): ...`).
- The description's case is mixed (`fix: add ...` vs `fix: Provide ...`).
  Imperative or present tense, no trailing period.
- Some older titles are plain sentences ("Update jQuery to 3.7.1",
  "Fix: ...", "Docs: ..."). New PRs should use lowercase conventional types.
- The PR number is appended by GitHub on merge: `... (#4425)`. If the PR
  fixes an issue, the issue number can go in the title too, which produces
  two numbers on `main`: `fix: dateInput() datesdisabled ignored
  non-default format (#4281) (#4434)`.

Release and version commits follow fixed wording:
`v1.13.0 release candidate (#4360)`, `Start v1.14.0 release candidate (#4393)`,
`Increment version number to 1.14.0.9000 (#4402)`.

## Merging

- PRs are **squash merged**; merge commits on `main` are rare. The repo allows squash, merge, and
  rebase merges, with `squash_merge_commit_title: COMMIT_OR_PR_TITLE`. So
  the PR title becomes the commit subject when the PR has more than one
  commit; a single-commit PR keeps its commit message. Make the PR title
  final before merging.
- The squash body is GitHub's default bullet list of the branch's commit
  subjects, including any `(GitHub Actions)` commits from
  `routine`. Branch commit messages end up on `main`, so keep them
  meaningful.
- CI runs `.github/workflows/R-CMD-check.yaml` on pushes to `main` and
  `rc-**`, on all PRs, and weekly. It calls three shared workflows from
  `rstudio/shiny-workflows@v1`: `website`, `routine`, and `R-CMD-check`.

## NEWS.md

User-facing changes need a bullet under `# shiny (development version)` in
`NEWS.md`, ending with the PR number, e.g. `(#4427)`. See
`memory-bank/documentationStyle.md` for bullet style. You won't know the PR
number until the PR is open, so add it in a follow-up commit. No CI check
enforces NEWS entries.

## The `routine` workflow

`rstudio/shiny-workflows/.github/workflows/routine.yaml` regenerates
derived files and, **on same-repo PRs, commits and pushes them back to the
PR branch**. Commits it can create (each only if something changed):

| Step | Commit message |
|---|---|
| `usethis::use_tidy_description()` | `` `usethis::use_tidy_description()` (GitHub Actions) `` |
| `devtools::document()` (`DESCRIPTION`, `NAMESPACE`, `man/`) | `` `devtools::document()` (GitHub Actions) `` |
| README build (only if `README.Rmd` exists; shiny has none) | `` `devtools::build_readme()` (GitHub Actions) `` |
| `oxipng` lossless PNG optimization (`man/figures/*.png`, `vignettes/*.png`) | `` `oxipng`; Optimize PNGs (GitHub Actions) `` |
| `npm ci` + `npm run build` (stages `inst/`, `srcts/`, `package-lock.json`) | `` `npm run build` (GitHub Actions) `` |

It then runs this repo's `.github/shiny-workflows/routine.sh`:

1. `tools/documentation/checkDocsCurrent.sh`: fails if roxygen docs,
   reexports, or the pkgdown index are stale. This step does not commit.
2. `tools/updatePackageJsonVersion.R`: if `package.json` changed, reruns
   `npm run build` and commits `Sync package version (GitHub Actions)`.

After pushing, it runs lint (if a `.lintr` exists), `npm test`, and a
`{staticimports}` freshness warning.

To refresh generated files without code changes, push an empty commit
(e.g. "chore: trigger routine workflow").

## Trailers

- Do not add `Co-Authored-By: Claude ...` trailers. Older commits on `main`
  contain them. Don't copy that from history.

## Pitfalls

- **Pull before pushing again.** `routine` pushes commits to your PR branch.
  If you push without pulling first, the push is rejected or you end up
  with a confusing merge.
- **Fork PRs and pushes to `main` cannot be auto-fixed.** In both cases
  `routine` fails with "Verify no new commits" instead of pushing. Run
  `devtools::document()` and `npm run build` locally and commit the
  results.
- **`checkDocsCurrent.sh` is not auto-fixed.** A stale pkgdown index or
  reexport list fails `routine` even on same-repo PRs. Fix it by hand (see
  `memory-bank/documentationStyle.md`).
- **Version bumps on `rc-v*` branches are not automatic.** `routine` has
  `urlchecker::url_update()` and `usethis::use_version()` steps for `rc-v*`
  branches, but they only run on push events, where the "Verify no new
  commits" step fails instead of pushing. Set `Version:` and the `NEWS.md`
  heading by hand (and run `urlchecker::url_update()` yourself).
