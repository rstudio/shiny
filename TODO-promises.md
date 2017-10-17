# Promises TODO

## Core API
- [ ] Should as.promise() convert regular values to promises? Or throw?

## later
- [ ] Add support for multiple event loops

## Error handling/debugging
- [ ] ..stacktraceon../..stacktraceoff.. and stack traces in general
- [ ] long stack traces
- [ ] options(shiny.error) should work in promise handlers

## Render functions
- [x] Non-async render functions should have their code all execute on the current tick. Otherwise order of execution will be surprising if they have side effects and explicit priorities.
- [x] Promise domains should maybe have an onExecute, for the "sync" part that kicks off async operations to also have wrapping behavior (like capturing output). Right now, I have to start off renderPrint with promise(~resolve(TRUE)) and then execute the user code in a then(), just to get the promise behavior. Same will be true when we tackle error handling (stack trace capture).
- [x] invisible() doesn't seem to be working correctly with renderPrint. .visible doesn't survive promise chaining, e.g. promise(~resolve(promise(~resolve(invisible("Hi"))))) %>% then(function(x, .visible) { cat(.visible) }) will print TRUE, not FALSE.
- [x] renderDataTable should support async
- [x] App that tests that all built-in render functions support async

## Flush lifecycle
- [x] While async operations are running in a session, hold off on any further processing of inputs and scheduled task items until all operations are complete.
- [x] Hold all outputs/errors until async operations are complete.
- [ ] Allow both sync and async outputs to be displayed before all outputs are done. (opt-in)

## Testing
- [ ] Apps that test flush lifecycle, including onFlushed(once = FALSE)
- [ ] Apps that test invisible() behavior for renderPrint, both sync and async
- [ ] Apps that ensure all render functions execute synchronous code before tick is over
