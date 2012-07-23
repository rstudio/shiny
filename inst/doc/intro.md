<style type="text/css">
p {font-size: 140%}
</style>

# Guide to Coding in Shiny

The Shiny web framework is fundamentally about making it easy to wire up *input values* from a web page, making them easily available to you in R, and have the results of your R code be written as *output values* back out to the web page.

    input values => R code => output values

Since Shiny web apps are interactive, the input values can change at any time, and the output values need to be updated immediately to reflect those changes.

Shiny comes with a **reactive programming** library that you will use to structure your application logic. By using this library, changing input values will naturally cause the right parts of your R code to be reexecuted, which will in turn cause any changed outputs to be updated.

## Reactive Programming: The Basics

Reactive programming is a coding style that starts with **reactive values**--values that change over time, or in response to the user--and builds on top of them with **reactive functions**--functions that access reactive values and execute other reactive functions.

What's interesting about reactive functions is that whenever they execute, they automatically keep track of what reactive values they read and what reactive functions they called. If those "dependencies" become out of date, then they know that their own return value has also become out of date. Because of this dependency tracking, changing a reactive value will automatically instruct all reactive functions that directly or indirectly depended on that value to re-execute.

The most common way you'll encounter reactive values in Shiny is using the `input` object. The `input` object, which is automatically in scope when your Shiny application's R code runs, lets you access the web page's user input fields using a list-like syntax. For example, if there was a text field named `foo` in your HTML, you would access it in your R code with `input$foo`. Code-wise, it looks like you're grabbing a value from a list or data frame, but you're actually reading a reactive value.

That brings us to the first constraint of the reactive framework: **reactive values can only be read from inside a reactive function.** It doesn't need to be the currently executing function that's reactive; it can be the function that's calling the current function, or the function that's calling that one, and so on. But if none of the functions on the current call stack are reactive, then attempting to read a reactive value will give you an error telling you so.

To create a reactive function, just take a regular function, and wrap it in a call to `reactive`:

```r
reactive(function() {
  print(input$foo)
})
```

This creates a reactive function that depends on `input$foo`. Whenever the value of `input$foo` changes, the function will execute and print the value.

```r
a.to.b <- reactive(function() {
  input$a:input$b
})
```



To explain how reactive programming works, let's step through a series of examples.

## Example 1: Hello World

Our simplest example app will present the user with a textbox; anything that is typed into the textbox will be echoed back to the user in uppercase.

index.html (abridged):
```html
<p>
  Input:<br/>
  <input name="val" type="text" value="Hello World!"/>
</p>

<p>
  You said:<br/>
  <div id="valUpper" class="live-text"/>
</p>
```

app.R:
```r
output$valUpper <- reactive(function() {
  toupper(input$val)
})
```

Lauch Example App 1 and try it out. [TODO: Directions for how to launch example app 1] You can see that as you type into the textbox, the output immediately updates.

Take a look at app.R. You probably figured out that `input$val` is how we access the value of the `val` field in index.html, and assigning to `output$echo` is how we define what goes into the `valUpper` div.

The only thing that needs explanation here is `reactive`. You pass it a function, and it returns to you a **reactive** version of that same function.

Now it's time to explain what *reactive* actually means.

* A **reactive value** is a value that may change in the future.
* A **reactive function** is a function that accesses reactive values and/or executes other reactive functions. (It can also access regular, non-reactive values and execute regular, non-reactive functions; otherwise, it'd be hard to get them to do anything useful!)

The unique thing about reactive values and functions is that they track their own dependencies. That is, when you execute a reactive function, it's actually doing two things:

1. Evaluating the function body and returning a value (just like any other R function)
2. Keeping track of what other reactive functions are being called and what reactive values are being accessed

When a reactive value changes, any reactive functions that previously accessed this value (and thus "depended" on it) are notified that they are out of date and need to re-execute. Those reactive functions, in turn, will notify any reactive functions that depended on them, and so on.

In this case, our function calls `input$val`, so that counts as one of the inputs. Whenever the user makes a change to `val`, this function notices the change and re-executes itself. And since the `output` data structure is designed to work with reactive functions, it'll notice when the re-execution happens and send the result back to the web page.

