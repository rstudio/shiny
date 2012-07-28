## Reactivity In Depth

If you're new to reactive programming (or new to this particular style of it), you might be wondering how the "automatic" dependency tracking actually works. Shiny's reactive programming core is inspired by the [Meteor](http://meteor.com) JavaScript web framework, and both [their docs](http://docs.meteor.com/#reactivity) and [this Stack Overflow answer](http://stackoverflow.com/questions/10260015/how-does-meteors-reactivity-work-behind-the-scenes/10293760#10293760) essentially describe the Shiny implementation as well. The core reactive context logic can be found in Shiny's `react.R` source file.

Reactive values have an important restriction: you can only read them from inside a reactive function (or from inside an **observer** function, keep reading). More precisely, at the time that a reactive value is being read, a function somewhere on the call stack needs to be reactive. (Reactive values can, on the other hand, be *written* without being in a reactive function.) Reactive values are implemented as the `Values` reference class in `reactives.R`.

Reactive functions have the same restriction: you can only call them from inside another reactive function (or from inside an observer). They also have a few other interesting properties:

* **Reactive functions can't have function parameters**; however, like all R functions, they can access values in the environment around them (i.e. they are closures)
* **Reactive functions are lazily evaluated:** when a reactive function's dependencies change, it doesn't re-execute itself automatically, but rather just notifies its dependents
* **Reactive functions cache their results**, so as long as their dependencies don't change, they will only execute once no matter how often they are called

Reactive functions are implemented as the `Observable` reference class in `reactives.R`.

An **observer** function is similar to a reactive function in that they have no parameters, can read reactive values and execute reactive functions, and are notified when those reactive dependencies change. However:

* **Observer functions are eagerly evaluated**; when their dependencies change they will re-execute
* **Observer functions cannot be directly called**, but they are called automatically on creation and whenever their dependencies change
* **Observer functions cannot return results**, so they are only useful for their <a href="http://en.wikipedia.org/wiki/Side_effect_(computer_science)">side effects</a> (in contrast to reactive functions, which are generally only useful for their results)

In a Shiny app, you might never need to create an observer explicitly, but every time you set something on `output`, an observer is being created for you.

Observer functions are implemented as the `Observer` reference class in `reactives.R`.

You can think of a working reactive system as a graph of dependencies through which change flows; it starts at reactive values, flows through reactive functions, and stops at observers. Change is *pulled* through the graph by the observers (rather than *pushed* through the graph by the values); so any parts of the graph that do not terminate in an observer will not execute.