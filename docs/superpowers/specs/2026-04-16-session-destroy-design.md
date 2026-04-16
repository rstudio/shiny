# session$destroy() — Clean Up Dangling Reactivity

**Date:** 2026-04-16
**Port of:** [posit-dev/py-shiny#2209](https://github.com/posit-dev/py-shiny/pull/2209)

## Problem

When dynamic UI is removed via `removeUI()`, the server-side reactive objects (observers, reactive expressions, reactive values, inputs, outputs) continue to persist and execute. This "dangling reactivity" wastes resources and can cause errors. There is no mechanism to destroy a module scope's reactive state.

## Solution

Add `session$destroy()` and `session$onDestroy(callback)` to both `ShinySession` and session proxies. Reactive primitives auto-register weak destroy callbacks during initialization, so calling `session$destroy()` on a module scope automatically cleans up all associated reactive state, including descendant module scopes.

## API

### `session$onDestroy(callback)`

Registers a callback to be invoked when `session$destroy()` is called. Returns an unsubscribe function.

- On `ShinySession`: stores under the `""` (root) namespace key
- On session proxies: stores under the proxy's namespace string on the root session's `destroyCallbacksByNs` map

### `session$destroy()`

Destroys the session scope and all descendant scopes.

- Finds all namespace keys in `destroyCallbacksByNs` that match the scope's prefix
- Sorts deepest-namespace-first (children before parents)
- Invokes each namespace's `Callbacks` with `onError` handler (continue-on-error)
- Cleans up inputs, outputs, message handlers, dynamic routes, bookmark callbacks for matching namespaces
- Removes the namespace keys from the map

On `ShinySession`, `destroy()` is called automatically in `wsClosed()` after `closedCallbacks$invoke()`.

## Data Structures

### Root session (`ShinySession`, `R/shiny.R`)

New private field:

- `destroyCallbacksByNs` — a `Map` keyed by namespace string, where each value is a `Callbacks` object

New private helper:

- `getOrCreateDestroyCallbacks(ns)` — lazily creates and returns the `Callbacks` for a given namespace key

New public methods:

- `onDestroy(callback)` — registers under `""` key, returns unsubscribe function
- `destroy()` — invokes all destroy callbacks (deepest-first), cleans up all resources

### Session proxies (`makeScope()`, `R/shiny.R`)

New override entries in `createSessionProxy()`:

- `onDestroy(callback)` — registers under the proxy's namespace string on the root session
- `destroy()` — prefix-matches namespaces, invokes callbacks deepest-first, cleans up namespace-scoped resources

## Reactive Primitive Auto-Registration

### Observer (`R/reactives.R`)

Already has `.domain`, `destroy()`, and `autoDestroy` via `onEnded` (strong ref).

Changes:
- During `initialize`, if `.domain` is non-NULL, also register a **weak** destroy callback via `.domain$onDestroy(weakCallback(self, self$destroy))`. Store the unsubscribe handle as `.autoDestroyOnDestroyHandle`.
- `destroy()`: additionally call the unsubscribe handle to deregister from `onDestroy`.

Dual registration: `onEnded` = strong ref (guaranteed at session end), `onDestroy` = weak ref (allows GC, fires on scope destroy).

### Observable / `reactive()` (`R/reactives.R`)

Already stores `.domain` but has no `destroy()` method.

Changes:
- Add `._destroyed` flag (default `FALSE`)
- Add `destroy()` method: sets `._destroyed`, invalidates `.dependents`, clears `.value`/`.error`, invalidates `.ctx` if present
- During `initialize`, if `.domain` is non-NULL, register via `.domain$onDestroy(weakCallback(self, self$destroy))`. Store unsubscribe handle as `._destroyHandle`.
- Guard `getValue()`: if `._destroyed`, raise `shiny.destroyed.error`

### ReactiveVal (`R/reactives.R`)

Currently does NOT store `.domain`.

Changes:
- During `initialize`, capture `domain <- getDefaultReactiveDomain()` (transient, not stored as a field)
- Add `._destroyed` flag (default `FALSE`)
- Add `destroy()` method: sets `._destroyed`, invalidates `dependents`, clears `value`
- Register via `domain$onDestroy(weakCallback(self, self$destroy))` if domain is non-NULL during init. Store unsubscribe handle as private `._destroyHandle`.
- Guard `get()` and `set()`: if `._destroyed`, raise `shiny.destroyed.error`

### Weak callback helper

Utility function `weakCallback(key, callback)` where `key` is the R6 object (used as the weak reference key) and `callback` is the function to invoke (e.g., `self$destroy`):
- Creates a weak reference via `rlang::new_weakref(key)` where `key` is the reactive object itself
- Returns a wrapper function that checks `rlang::wref_key()` before invoking `callback`
- If key is NULL (object was GC'd), silently no-ops; the callback is not invoked
- The wrapper holds a strong reference to `callback`, but since the weak ref key is the R6 object, the callback becomes effectively unreachable once the object is GC'd

This follows the existing pattern in `Dependents$register()` (`R/reactives.R:28`) which already uses `rlang::new_weakref`.

## Input/Output/Resource Cleanup

When `destroy()` fires for a namespace `ns`:

### Inputs

Add a `_destroy(nsPrefix)` method to `ReactiveValues`:
- Find all keys in `.values` starting with `nsPrefix`
- For each: invalidate the key's `Dependents`, remove from `.values`, `.nameOrder`, `.dependents`
- Invalidate `.namesDeps` (since `names()` changed)
- Covers both regular inputs and `.clientData` keys

### Outputs

On the root session:
- Find all output names in `private$.outputs` starting with the namespace prefix
- Call `destroy()` on each matching output Observer
- Remove from `private$.outputs`, `private$.outputOptions`
- Remove from `private$invalidatedOutputValues`, `private$invalidatedOutputErrors`

### Message handlers

Remove custom input message handlers (registered via `session$sendInputMessage`) and custom message handlers (registered via `session$sendCustomMessage` / `addCustomMessageHandler`) matching the destroyed namespace prefix. The root session stores these in `private$inputMessageHandlers` and similar structures.

### Dynamic routes

Clean up `registerDataObj` entries (`self$files`, `self$downloads`) matching the namespace prefix.

### Bookmark callbacks

The scope's bookmark/restore/restored callbacks registered on the root session should be deregistered. Capture the unsubscribe functions returned by `Callbacks$register()` during `makeScope()` and invoke them during destroy.

## Error Handling

### DestroyedReactiveError

A custom condition class `"shiny.destroyed.error"` inheriting from `"error"`. Raised when accessing a destroyed reactive. This is a loud error — it propagates through output observers as a visible error (red message in UI), making it clear to the developer they're accessing a destroyed reactive.

Message format: `"Can't read from reactive; its module session has been destroyed"`

### Destroy callback errors

Destroy callbacks invoke with `onError` handler so one failing callback doesn't prevent others from running. Errors are printed but execution continues.

### Idempotency

All `destroy()` methods check their destroyed flag first and no-op if already destroyed. Safe to call multiple times.

### Ordering

Destroy callbacks fire deepest-namespace-first (e.g., `"mymod-inner-"` before `"mymod-"`), matching py-shiny's behavior so child modules clean up before parents.

## Files to Modify

| File | Changes |
|------|---------|
| `R/shiny.R` | `ShinySession`: add `destroyCallbacksByNs`, `getOrCreateDestroyCallbacks()`, `onDestroy()`, `destroy()`. `makeScope()`: add `onDestroy`/`destroy` overrides, capture bookmark unsubscribe handles. `wsClosed()`: call `destroy()` after `closedCallbacks`. |
| `R/reactives.R` | `ReactiveVal`: add `._destroyed`, `destroy()`, guards, auto-register. `Observable`: add `._destroyed`, `destroy()`, guards, auto-register. `Observer`: add weak `onDestroy` registration alongside existing `onEnded`. `ReactiveValues`: add `_destroy(nsPrefix)` method. |
| `R/modules.R` | No structural changes — session_proxy stays as plain environment. |
| `R/mock-session.R` | `MockShinySession`: add `onDestroy()` and `destroy()` methods. |
| `R/utils.R` | Add `weakCallback()` utility function. |

## Testing Strategy

### Unit tests (testthat)

- Destroy callbacks: register, call `destroy()`, verify correct order (deepest-first), verify no double-fire
- Weak callback GC: create reactive, force GC, call `destroy()`, verify GC'd callback silently skipped
- ReactiveVal destroy: create, destroy, verify `get()`/`set()` raise `shiny.destroyed.error`
- Observable destroy: create, destroy, verify access raises error, verify dependents invalidated
- Observer destroy: create, destroy, verify suspended and doesn't re-execute
- Input cleanup: set namespaced inputs, destroy namespace, verify keys removed and dependents invalidated
- Output cleanup: define namespaced outputs, destroy, verify observers destroyed and removed
- Nested modules: destroy parent, verify child namespaces also destroyed
- Idempotency: call `destroy()` twice, no error

### Integration test (Shiny app test)

App with dynamic module UI — insert module, interact, remove UI + call `session$destroy()`, verify server-side reactivity cleaned up and re-inserting works fresh. Similar to py-shiny's `tests/playwright/shiny/bugs/2207-dangling-reactivity/`.

### MockShinySession

Add `onDestroy()` and `destroy()` methods so unit tests of modules can exercise the destroy path.

## Future Considerations

- An exported function to explicitly destroy individual reactive objects (e.g., `destroyReactive(rv)`) may be added later
- `removeUI()` could optionally call `session$destroy()` automatically, but that's a separate decision
