---
title: "Bundled Agent Skill (shiny-for-r)"
description: "Audience, layout, frontmatter contract, how to add a topic, and the guard tests for the Agent Skill shipped under inst/skills/shiny-for-r/. Read before editing the skill or changing a public API it documents."
---

# Bundled Agent Skill (shiny-for-r)

shiny ships one [Agent Skill](https://agentskills.io) at
`inst/skills/shiny-for-r/` (added in #4427). `SKILL.md` is an index: a short
intro plus grouped tables, each row pointing at a `references/<topic>.md`
file with the real instructions.

## Audience

The skill is **package data for app authors' coding agents**, not
contributor documentation.

- Installed, `inst/skills/` becomes `skills/` in the package library, i.e.
  `system.file("skills", "shiny-for-r", package = "shiny")`.
- [btw](https://posit-dev.github.io/btw/) discovers it at runtime: btw scans
  `system.file("skills", package = pkg)` for every **attached** package
  (`.packages()`), so `library(shiny)` is enough. Each subdirectory with a
  `SKILL.md` is a skill.
- `btw::btw_skill_install_package("shiny")` copies it into a project for
  Claude Code and other skill-aware tools (see `README.md` "Agent Skill").

Contributor guidance (internals, workflows) belongs in `memory-bank/`, which
is `.Rbuildignore`d and never ships. If a topic serves both audiences, the
skill documents the public API and the memory-bank doc documents internals.

## Layout

```
inst/skills/
  shiny-for-r/
    SKILL.md          # frontmatter + grouped index tables
    references/       # one file per topic, e.g. reactivity.md
```

- One skill directory only. Adding a topic **does not** mean a new skill
  directory.
- Index groups in `SKILL.md`: Foundations, Async, Dashboard building,
  Navigation & dynamic UI, Outputs, Feedback & state, Theming, assets &
  extending, Testing & observability.
- Each row is `| Topic | Use when | Reference |`, with the reference written
  as a backticked relative path (`` `references/<topic>.md` ``). Keep
  references one level deep; no files pointing at further files.
- There are no `scripts/` or `assets/` directories. Keep each reference
  file focused on one topic.

## Frontmatter

Current `SKILL.md` frontmatter keys: `name`, `description` (double-quoted),
`license: MIT`, and `metadata` (`author`, `version`).

Contract (enforced by `tests/testthat/test-skills.R`):

- `name` is exactly `shiny-for-r`, matches `^[a-z0-9][a-z0-9-]*$`, and is at
  most 64 characters. It must match the directory name (the test hard-codes
  the string rather than reading the directory).
- `description` is a non-empty string of at most 1024 characters.
- Only these top-level keys are allowed: `name`, `description`, `license`,
  `compatibility`, `metadata`, `allowed-tools`.

The description is the only text an agent sees before deciding to load the
skill. It is one umbrella statement: what it covers (reactivity, modules,
bslib layouts, testing, ...), then "Use when ...", including the wrong
approaches it should catch ("custom HTML tables, fake tabs, polling loops,
...."). Routing to a specific topic is the job of the index rows, not the
description.

## Adding or editing a topic

1. Write `inst/skills/shiny-for-r/references/<topic>.md`. Existing files use
   an `# <Title>` heading, an `## Overview` that states the principle and the
   workaround it replaces, task-oriented sections with short runnable
   examples against the **public API**, and common-mistake notes.
2. Add one row to the right table in `SKILL.md` with a "Use when" trigger.
3. Add a `<topic> = c(...)` entry to the `apis` list in
   `tests/testthat/test-skills.R` naming the shiny exports the file relies
   on.
4. Fence R code as ```` ```r ```` so the parse test sees it.
5. Add or update a `NEWS.md` bullet if the change is user-visible.

Prefer extending an existing reference over creating a new one. Topics not
covered on purpose (the `SKILL.md` intro says so): plain inputs, deployment,
and performance tuning.

## Guard tests (`tests/testthat/test-skills.R`)

`skill_dir()` resolves through `system.file()`, so it works under both
`devtools::load_all()` (`inst/skills/...`) and an installed package
(`skills/...`), and skips if neither is found. The tests are not
`skip_on_cran()`, so they also run in `R CMD check`.

| Test | What it checks |
|---|---|
| frontmatter valid for btw discovery | The frontmatter contract above. Parsed with `yaml::yaml.load()`; skips if {yaml} (a Suggests) is missing. |
| links and reference files match one-to-one | Every `references/*.md` path mentioned in `SKILL.md` exists, and every file in `references/` is mentioned (no orphans, no dead links). |
| every R chunk parses | Every ```` ```r ```` chunk in every `.md` under the skill goes through `parse()`. Failures report file and line. |
| documented APIs still exist | The `apis` list (reference name -> shiny exports) must cover exactly the files in `references/`, and every listed name must be in `getNamespaceExports("shiny")`. |

Run them with `Rscript -e 'devtools::test(filter = "skills")'`.

## Build and install

- Nothing in `.Rbuildignore` matches `inst/skills`, so the skill ships in the
  source tarball.
- `.Rinstignore` only excludes `^tools$` and `^Rmd$`, which are paths
  relative to `inst/`, so the skill is installed.
- Keep it that way. A broad new `.Rbuildignore` or `.Rinstignore` pattern
  (for example an unanchored `skills` or `\.md$`) would silently drop the
  skill. The guard tests would then *skip*, not fail.

## Maintenance

- Treat the skill as user-facing documentation. When a PR renames, removes,
  or changes the behavior of a public API the skill documents, update the
  reference file **and** its `apis` entry in the same PR. Run
  `grep -rn '<name>' inst/skills/` as part of the change.
- Document behavior as shipped. Never document an API before it is merged.
- Version caveats for other packages (bslib, promises, ...) are easy to get
  wrong. #4427's review fixed one (`toolbar()` shipped in bslib 0.11.0, not
  0.12.0). Check upstream NEWS before stating a version.

## Pitfalls

- **The API sync list is manual.** The fourth test checks only the names
  listed in `apis`, not every function a reference mentions. A new function
  in a reference, or anything in `SKILL.md` itself, is unguarded unless you
  add it. The test also ignores non-shiny functions (bslib, promises,
  reactlog).
- **Only exact ```` ```r ```` fences are parsed.** The test matches
  `^```r\s*$` and closes on `^```\s*$`. ```` ```R ````, ```` ```{r} ````,
  indented fences inside lists, and ```` ```js ````/```` ```yaml ```` blocks
  are not checked. A ```` ```r ```` chunk with no closing fence makes the
  test error, not fail cleanly.
- **Partial snippets must still parse.** A snippet that is only the inside
  of a `server` function is fine; a deliberately incomplete line
  (`ui <- fluidPage(`) is not.
- **Quote the description.** It contains `: `, which is invalid in an
  unquoted YAML scalar. btw and other installers use real YAML parsers.
- **Link matching is text-based.** Any `references/<name>.md` string in
  `SKILL.md`, even in prose, counts as a link. Mentioning a file that does
  not exist fails the one-to-one test.
