# Polish NEWS

Polish the latest release's bullets in `NEWS.md` according to the
[tidyverse style guide](https://github.com/tidyverse/style/blob/main/news.qmd).
Do the steps in order, then show the diff and let the user comment before
committing.

## 1. Sort

Sort the bullets by the first function name in each one.

## 2. Proofread

- Each bullet is a complete sentence and ends with a full stop.
- Put the function name as close to the start as possible:
  `` `testServer()` now ... ``, not "Fixed #123: ...".
- Wrap functions, arguments, and file names in backticks. Function names
  include parentheses. Leave out "the argument" and "the function".
- Avoid generic verbs like "enables" or "provides".
- Use present tense, and describe what happens now. Drop "Before, ..."
  sentences about the old behavior.
- Keep bullets short. Drop internal details a user does not need, such as how
  the fix works inside the client.
- Issue and PR numbers go in parentheses before the final full stop:
  `... (#4485).`
- Credit outside contributors as `(@user, #1234)`.
- Wrap lines at 80 characters.

Include a summary of the changes you made.

## 3. Group into sections

Use these level 2 headings, in sentence case, and leave out empty ones:

- `## Lifecycle changes`: deprecated, defunct, and experimental changes.
- `## New features`
- `## Bug fixes and minor improvements`

In each section, bullets with no function come first, then the rest sorted by
first function.

Keep one bullet per PR. Do not merge several PRs into one bullet, even when
they change the same function.
