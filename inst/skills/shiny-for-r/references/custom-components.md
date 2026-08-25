# Custom JavaScript components in Shiny for R

## Overview

Shiny ships a large set of inputs and outputs; most apps never need more than
those plus `uiOutput()`/`renderUI()`. Reach for a custom component only when
no built-in combination covers what you need — a third-party JS widget or a
bespoke interaction. The anti-pattern this reference prevents is a
`tags$script()` blob that reaches into `#id` elements, polls the DOM, or
opens a side channel by hand instead of registering with Shiny's client-side
APIs (`Shiny.InputBinding`,
`Shiny.OutputBinding`, `Shiny.addCustomMessageHandler`). Those give
serialization, initial values, reconnects, and automatic re-binding of
elements inserted after page load for free; hand-rolled `<script>` tags get
none of it and break once the element is re-rendered.

## Ship the JS/CSS: htmltools::htmlDependency()

Put your JS/CSS files in a package's `inst/` (or app's `www/`) folder and
wrap them in one `htmltools::htmlDependency()` call, attached to a tag or
returned from the UI. This injects the assets once, in order, and lets Shiny
deduplicate them — never drop a bare `<script src>` into the UI, or ordering
and dedup break. See `?htmltools::htmlDependency` for the full argument list.

## Custom input: a minimal complete binding

The simplest custom input calls `Shiny.setInputValue("id", value)`, read as
`input$id` server-side — but that only handles one instance and skips
reconnect restoration. For a reusable element type, register a
`Shiny.InputBinding` subclass so Shiny finds every matching element and
restores its value on reconnect.

Bindings do not namespace ids for you: `getId(el)` reads the element's
`data-input-id` or `id` attribute as-is. Inside a module the *UI author*
must namespace the id, exactly as with a built-in input — pass
`ns("counter")` rather than `"counter"`.

```r
# Partial snippet: UI side of the "counter" input, paired with the JS below
library(shiny)

# Callers namespace the id themselves: counterInput(ns("counter"), "Clicks")
counterInput <- function(id, label) {
  tagList(
    tags$button(
      id = id,
      class = "my-counter",
      type = "button",
      "data-value" = "0",
      label
    )
  )
}
```

```js
// www/counter.js
class CounterBinding extends Shiny.InputBinding {
  find(scope) {
    return $(scope).find(".my-counter");
  }
  getValue(el) {
    return Number(el.dataset.value || 0);
  }
  subscribe(el, callback) {
    // Namespace the handler so unsubscribe() can remove exactly this one.
    $(el).on("click.counterBinding", () => {
      el.dataset.value = Number(el.dataset.value || 0) + 1;
      callback(true); // true = deferred send; false = send immediately
    });
  }
  unsubscribe(el) {
    $(el).off(".counterBinding");
  }
}
Shiny.inputBindings.register(new CounterBinding(), "myapp.counter");
```

The server reads it like any input: `input$counter` (or
`session$ns("counter")`'s value inside a module). `find()` runs per-scope, so
elements injected later by `renderUI()` or `insertUI()` are bound
automatically; `unsubscribe()` undoes `subscribe()`.

Outputs mirror this with `Shiny.OutputBinding`: `find()` locates placeholders,
and `renderValue(el, data)` receives whatever `render*()` returned,
registered via `Shiny.outputBindings.register(new MyOutputBinding(),
"myapp.myoutput")`.

## Converting input values: registerInputHandler()

If the client sends a value JSON can't represent faithfully, register a
handler that runs after jsonlite deserializes it, before it lands in
`input`. Call it once, from a package's `.onLoad()`:

```r
# Partial snippet: call once, e.g. from .onLoad()
registerInputHandler("myapp.roundedint", function(x, shinysession, name) {
  if (is.null(x)) return(NA)
  round(x)
})
```

The binding's `getType(el)` must return the matching type string so Shiny
applies the right handler; see `?registerInputHandler` for the argument
contract and built-in types (`shiny.matrix`, `shiny.number`, `shiny.date`).
Registering a type that already exists is an error — pass `force = TRUE` to
overwrite, which is what you want when reloading a package during
development.

## Server-to-client messages: sendCustomMessage()

For a server-initiated push not tied to an output — flashing a banner,
resetting a widget — use `session$sendCustomMessage(type, message)` with
`Shiny.addCustomMessageHandler(type, fn)` in JS. This app inlines the handler
in `tags$script()` as a self-contained demo; a real component ships that JS
through `htmlDependency()` instead, as shown above.

```r
library(shiny)

ui <- fluidPage(
  tags$script(HTML(
    "Shiny.addCustomMessageHandler('flash', function(msg) {
       document.getElementById('banner').textContent = msg.text;
     });"
  )),
  actionButton("go", "Flash the banner"),
  tags$div(id = "banner", "(waiting)")
)

server <- function(input, output, session) {
  observeEvent(input$go, {
    session$sendCustomMessage("flash", list(text = "Hello from the server!"))
  })
}

shinyApp(ui, server)
```

`message` must be jsonlite-encodable, and `type` must match the handler name
exactly. The related, narrower `session$sendInputMessage(inputId, message)`
updates one bound input via its binding's `receiveMessage(el, data)`;
`update*Input()` functions use it internally, so prefer those unless writing
a binding yourself.

## Quick reference

| Function | Purpose |
|---|---|
| `htmltools::htmlDependency()` | Ship a component's JS/CSS as one deduplicated bundle |
| `registerInputHandler(type, fun)` | Convert a client value before `input` sees it |
| `session$sendCustomMessage(type, message)` | Push a one-off message to the client |
| `session$sendInputMessage(inputId, message)` | Update one bound input via `receiveMessage()` |
| `Shiny.inputBindings.register(binding, name)` | Register a reusable JS input binding |
| `Shiny.outputBindings.register(binding, name)` | Register a reusable JS output binding |
| `Shiny.addCustomMessageHandler(type, fn)` | Receive messages from `sendCustomMessage()` |

## Common mistakes

- **Bare `<script src>`/CDN tag instead of `htmlDependency()`.** Assets load
  out of order or duplicate, and break inside modules.
- **`tags$script()` blobs grabbing `#id` elements directly.** Breaks after
  `renderUI()` re-renders the element, or once it moves inside a module —
  register a binding instead.
- **Assuming a binding namespaces ids inside a module.** It reads the id
  off the element; the UI function's caller must pass `ns("...")`.
- **Output id/class doesn't match the binding's `find()`.** `renderValue()`
  never fires; keep the class equal to the selector used.
- **`type` mismatch between `sendCustomMessage()` and
  `addCustomMessageHandler()`.** The message is silently dropped.
- **A custom binding just to convert a type.** `registerInputHandler()`
  alone is enough if the only issue is the wrong R type.
- **Forgetting `unsubscribe()`.** Leaves stale listeners attached when Shiny
  unbinds and rebinds the element.
- **"Unsubscribing" by replacing the element** (`el.replaceWith(
  el.cloneNode(true))`). That swaps the node out from under Shiny, which
  still holds a reference to the original; detach the listener instead, via
  a namespaced handler or an `AbortController` signal.
