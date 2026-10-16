---
name: memory-bank
description: |
  Discover, search, and maintain the project's memory-bank documentation.
  Use before architectural exploration or when finding, reading, updating,
  or creating contributor documentation for the shiny R package.
---

<!-- Adapted from https://github.com/posit-dev/assistant/blob/main/.agents/skills/memory-bank/SKILL.md -->

# Memory Bank

The `memory-bank/` directory at the repo root holds contributor-facing documentation for the shiny R package: architecture, conventions, and step-by-step recipes. Each file covers one subsystem or workflow and starts with YAML frontmatter containing `title` and `description`. It is excluded from the built package via `.Rbuildignore`.

This is distinct from `inst/skills/`, which ships with the package and documents shiny's *public* API for app authors. See `memory-bank/agentSkills.md` for that boundary.

## Finding and reading documents

Loading this skill is preparation, not completion of documentation discovery. Before exploring code, identify the relevant memory-bank docs and read their applicable sections.

List the catalog (titles and descriptions) with:

```bash
grep -A1 -m1 '^title:' memory-bank/*.md
```

- Read small, relevant documents directly. For larger ones, list their headings first (`grep -n '^#' memory-bank/<file>.md`) and read only the sections you need.
- If titles and descriptions don't point at anything, search contents with task terms, related symbols, and synonyms (`grep -rni <term> memory-bank/`) before concluding no guidance exists.
- Follow source-code references to verify current behavior; docs can lag the code.

## Updating documents

Update a memory-bank file when:

- You discover an architectural pattern or pitfall worth recording
- You make a significant change to something a doc describes
- The user asks to **update the memory bank**

Keep the frontmatter `title` and `description` in sync with the content.

## Creating new documents

1. Use **camelCase** naming (e.g., `newFeatureSystem.md`)
2. Add YAML frontmatter with `title` and `description`; quote values that contain `: `
3. If the doc should be read for a common task, add a pointer to it in `AGENTS.md`
