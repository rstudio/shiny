# Promises TODO

- [x] How to handle invisible/withVisible? This is needed for dealing with ggplot2 in renderPlot.
- [x] renderPlot is broken. plotFunc is synchronous but calls func() which is potentially asynchronous.
- [x] Brushing not working; the mapping is null for ggplot.
- [ ] ..stacktraceon../..stacktraceoff.. and stack traces in general
- [ ] long stack traces
- [ ] Non-async render functions should have their code all execute on the current tick. Otherwise order of execution will be surprising if they have side effects and explicit priorities.
- [x] Respect execOnResize
- [x] Accidentally did then(cars) instead of then(~cars), which caused an *unhandled* promise exception
- [ ] promises::resolved(logic()) should use the current reactive domain to wrap the call to logic()
- [ ] invisible() doesn't seem to be working correctly with renderPrint. .visible doesn't survive promise chaining, e.g. promise(~resolve(promise(~resolve(invisible("Hi"))))) %>% then(function(x, .visible) { cat(.visible) }) will print TRUE, not FALSE.
- [ ] Do we allow the Shiny session to proceed as normal while an async operation is running, or do we hold off on any further processing of inputs until all operations are complete? (Possible exception for cancelling) In either case I don't know if we can stop invalidateLater? Well, maybe we can.
- [ ] What does the client see after flushReact finishes but there's still an async operation running? Hold all outputs/errors?
