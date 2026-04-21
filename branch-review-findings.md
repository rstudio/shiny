# Branch Review Findings

Review date: 2026-04-21
Branch: `resize-observer-v2`
Reviewer: Codex

## Status

- [ ] Finding 1: Restore the guarantee that output clientdata is flushed before the next input batch is sent
- [x] Finding 2: Prevent post-unbind observer callbacks from emitting `.clientdata_output_null_*` inputs
- [ ] Simplification: Move output-info observer setup / flush / disposal behind a dedicated module API

## Finding 1

Severity: High
Status: Open

Summary:
The new observer path no longer guarantees that output clientdata is flushed before the next input batch is sent.

Details:
The central `sendOutputInfoFns` debouncer is still wired into `InputBatchSender.lastChanceCallback`, but the per-element `ResizeObserver` and `IntersectionObserver` handlers in `srcts/src/shiny/index.ts` debounce for 100ms and then call `doSendHiddenState()` / `doSendSize()` directly, bypassing that flush path.

Risk:
A tab/show/layout change followed immediately by a click, brush, or other input can reach the server with stale `.clientdata_output_*` width, height, or hidden values.

Relevant files:
- `srcts/src/shiny/index.ts`
- `srcts/src/shiny/sendOutputInfo.ts`

Notes:
- Old behavior routed these updates through debouncers that were explicitly flushed before send.

## Finding 2

Severity: Medium
Status: Fixed on branch

Summary:
Queued observer callbacks can survive unbind and emit bogus `.clientdata_output_null_*` inputs.

Details:
`getIdFromEl()` now returns `null` when `shiny-output-binding` has already been removed, but the callbacks created in `ensureObservers()` have no cancellation mechanism. `unbindOutputs()` disconnects the observers, but it does not cancel already-scheduled debounced callbacks; when one fires after unbind, `doSendHiddenState()` / `doSendSize()` / `doSendTheme()` will build keys from `id === null`.

Risk:
Dynamic UI replacement or removal can send invalid clientdata inputs after the output has already been unbound.

Relevant files:
- `srcts/src/shiny/index.ts`
- `srcts/src/shiny/bind.ts`
- `srcts/src/time/debounce.ts`

Resolution:
Added explicit cancellation support to the shared `debounce()` helper, stored the debounced observer callbacks alongside each observer, and canceled those callbacks during `unbindOutputs()` before removing the output binding data.

## Simplification

Status: Open

Summary:
Most of the new output-info state machine still lives inline in `initialize()`.

Opportunity:
Pull observer setup, flush behavior, and disposal behind one module-owned API so that lifecycle handling is centralized and easier to reason about.

Relevant files:
- `srcts/src/shiny/index.ts`
- `srcts/src/shiny/sendOutputInfo.ts`

## Verification

- `npm run build_types` passed during review
- Browser-level regression coverage for this new output-info pipeline was not run
