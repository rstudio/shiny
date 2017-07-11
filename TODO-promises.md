# Promises TODO

- [x] How to handle invisible/withVisible? This is needed for dealing with ggplot2 in renderPlot.
- [x] renderPlot is broken. plotFunc is synchronous but calls func() which is potentially asynchronous.
- [x] Brushing not working; the mapping is null for ggplot.
- [ ] ..stacktraceon../..stacktraceoff.. and stack traces in general
- [ ] Non-async render functions should have their code all execute on the current tick. Otherwise order of execution will be surprising if they have side effects and explicit priorities.
- [x] Respect execOnResize
- [ ] Accidentally did then(cars) instead of then(~cars), which caused an *unhandled* promise exception
