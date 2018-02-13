# Promises TODO

## Documentation

- [x] Motivation -- why should I care about async? Why shouldn't I (what are the limitations)?
- [x] High level technical overview
- [ ] Cookbook-style examples
- [ ] Top-down porting of a sync app to async

## Core API
- [x] Should as.promise() convert regular values to promises? Or throw?
  - [x] If as.promise() doesn't convert regular values to promises, add promise_resolved(value) and promise_rejected(err) functions?

## later
- [ ] Add support for multiple event loops
- [x] Add timeout to run_now

## Error handling/debugging
- [ ] ..stacktraceon../..stacktraceoff.. and stack traces in general
- [x] long stack traces
  - [x] require opt-in
- [ ] options(shiny.error) should work in promise handlers
- [x] Detect when reactives are used across process boundaries, and error

## Render functions
- [x] Non-async render functions should have their code all execute on the current tick. Otherwise order of execution will be surprising if they have side effects and explicit priorities.
- [x] Promise domains should maybe have an onExecute, for the "sync" part that kicks off async operations to also have wrapping behavior (like capturing output). Right now, I have to start off renderPrint with promise(~resolve(TRUE)) and then execute the user code in a then(), just to get the promise behavior. Same will be true when we tackle error handling (stack trace capture).
- [x] invisible() doesn't seem to be working correctly with renderPrint. .visible doesn't survive promise chaining, e.g. promise(~resolve(promise(~resolve(invisible("Hi"))))) %>% then(function(x, .visible) { cat(.visible) }) will print TRUE, not FALSE.
- [x] renderDataTable should support async
- [x] Support downloadHandler
  - [ ] Support async filename?
  - [x] Should prevent session from continuing until download completes (ref count)

## Flush lifecycle
- [x] While async operations are running in a session, hold off on any further processing of inputs and scheduled task items until all operations are complete.
- [x] Hold all outputs/errors until async operations are complete.
- [ ] Allow both sync and async outputs to be displayed before all outputs are done. (opt-in)

## Testing
- [x] App that tests that all built-in render functions support async
- [x] Apps that test flush lifecycle, including onFlushed(once = FALSE)
- [x] Apps that test invisible() behavior for renderPrint, both sync and async
- [x] Apps that ensure all render functions execute synchronous code before tick is over
- [x] App that tests async downloadHandler
- [x] App that verifies inputs/timers don't fire for a session while it has async operations pending
- [x] App that verifies req(FALSE), req(FALSE, cancelOutput = TRUE), validate/need, etc. all work in async

## External packages
- [x] DT
- [x] htmlwidgets: Don't require async-aware version of Shiny if not using async
- [x] Plotly

## Bugs
- [x] req(FALSE, cancelOutput = TRUE) shows grey (even without async)
