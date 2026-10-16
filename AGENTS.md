# AGENTS.md

This file provides guidance to coding agents when working with code in this repository.

## Overview

shiny is the R package for building reactive web applications. The server side is R (`R/`); the browser client is TypeScript (`srcts/`) bundled into `inst/www/shared/`. Many UI components and themes come from the bslib and htmltools packages.

## Development Commands

```bash
# R
Rscript -e 'devtools::load_all()'                  # Load for interactive work
Rscript -e 'devtools::document()'                  # Regenerate man/, NAMESPACE, Collate
Rscript -e 'devtools::test()'                      # All testthat tests
Rscript -e 'devtools::test(filter = "reactivity")' # One file: tests/testthat/test-reactivity.R
Rscript -e 'devtools::check()'                     # Full R CMD check

# TypeScript (run `npm ci` once first)
npm run build      # Lint, type check, TS tests, and bundle into inst/www/shared/
npm run watch      # Rebuild shiny.js on change
npm run checks     # Lint, tsc, TS tests, type coverage, circular imports
```

## Memory Bank

The `memory-bank/` directory holds contributor documentation with YAML
frontmatter (`title`, `description`). **Before exploring the codebase**, invoke
the `memory-bank` skill to find and read the relevant docs. Update or add docs
when you learn something future contributors will need (see the skill for
conventions).

Contributor skills live in `.agents/skills/`; `.claude/skills` is a symlink to
it. Only allowlisted skills are tracked, so when adding or removing one, update
the `!/.agents/skills/<name>/` lines in `.gitignore` to match.

## Bundled Agent Skill

The package ships an Agent Skill for app authors under
`inst/skills/shiny-for-r/`. Treat it like user-facing documentation: when a
change touches a public API it documents, update the skill in the same PR.

## Common Pitfalls

- **New R files**: `DESCRIPTION` has an explicit `Collate:` field. Run `devtools::document()` after adding a file so it gets collated.
- **Generated files**: Don't hand-edit `man/`, `NAMESPACE`, or `inst/www/shared/shiny*.js`; edit the roxygen comments or `srcts/` and regenerate.
- **NEWS.md**: User-facing changes need a bullet under `# shiny (development version)`.
