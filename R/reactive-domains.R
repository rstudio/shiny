#' @include globals.R
NULL

#
# Over the last few months we've seen a number of cases where it'd be helpful
# for objects that are instantiated within a Shiny app to know what Shiny
# session they are "owned" by. I put "owned" in quotes because there isn't a
# built-in notion of object ownership in Shiny today, any more than there is a
# notion of one object owning another in R.
#
# But it's intuitive to everyone, I think, that the outputs for a session are
# owned by that session, and any logic that is executed as part of the output
# is done on behalf of that session. And it seems like in the vast majority of
# cases, observers that are created inside a shinyServer function (i.e. one per
# session) are also intuitively owned by the session that's starting up.
#
# This notion of ownership is important/helpful for a few scenarios that have
# come up in recent months:
#
# 1. The showcase mode that Jonathan implemented recently highlights
# observers/reactives as they execute. In order for sessions to only receive
# highlights for their own code execution, we need to know which sessions own
# which observers. 2. We've seen a number of apps crash out when observers
# outlive their sessions and then try to do things with their sessions (the
# most common error message was something like "Can't write to a closed
# websocket", but we now silently ignore writes to closed websockets). It'd be
# convenient for the default behavior of observers to be that they don't
# outlive their parent sessions. 3. The reactive log visualizer currently
# visualizes all reactivity in the process; it would be great if by default it
# only visualized the current session. 4. When an observer has an error, it
# would be great to be able to send the error to the session so it can do its
# own handling (such as sending the error info to the client so the user can be
# notified). 5. Shiny Server Pro wants to show the admin how much time is being
# spent servicing each session.
#
# So what are the rules for establishing ownership?
#
# 1. Define the "current domain" as a global variable whose value will own any
# newly created observer (by default). A domain is a reference class or
# environment that contains the functions `onEnded(callback)`, `isEnded()`, and
# `reactlog(logEntry)`.
#
## ------------------------------------------------------------------------
createMockDomain <- function() {
  callbacks <- Callbacks$new()
  ended <- FALSE
  domain <- new.env(parent = emptyenv())
  domain$onEnded <- function(callback) {
    return(callbacks$register(callback))
  }
  domain$isEnded <- function() {
    ended
  }
  domain$reactlog <- function(logEntry) NULL
  domain$end <- function() {
    if (!ended) {
      ended <<- TRUE
      callbacks$invoke()
    }
    invisible()
  }
  domain$incrementBusyCount <- function() NULL
  domain$decrementBusyCount <- function() NULL
  return(domain)
}

#
# 2. The initial value of "current domain" is null.
#
## ------------------------------------------------------------------------
.globals$domain <- NULL

#
# 3. Objects that can be owned include observers, reactive expressions,
# invalidateLater instances, reactiveTimer instances. Whenever one of these is
# created, by default its owner will be the current domain.
#
## ------------------------------------------------------------------------

#' @name domains
#' @rdname domains
#' @export
getDefaultReactiveDomain <- function() {
  .globals$domain
}

#
# 4. While a session is being created and the shinyServer function is executed,
# the current domain is set to the new session. When the shinyServer function
# is done executing, the previous value of the current domain is restored. This
# is made foolproof using a `withReactiveDomain` function.
#
## ------------------------------------------------------------------------

#' @rdname domains
#' @export
withReactiveDomain <- function(domain, expr) {
  promises::with_promise_domain(createVarPromiseDomain(.globals, "domain", domain), expr)
}

#
# 5. While an observer or reactive expression is executing, the current domain
# is set to the owner of the observer. When the observer completes, the
# previous value of the current domain is restored.
#
# 6. Note that once created, an observer/reactive expression belongs to the
# same domain forever, regardless of how many times it is invalidated and
# re-executed, and regardless of what caused the invalidation to happen.
#
# 7. When a session ends, any observers that it owns are suspended, any
# invalidateLater/reactiveTimers are stopped.
#
## ------------------------------------------------------------------------

#' @rdname domains
#' @export
onReactiveDomainEnded <- function(domain, callback, failIfNull = FALSE) {
  if (is.null(domain)) {
    if (isTRUE(failIfNull))
      stop("onReactiveDomainEnded called with null domain and failIfNull=TRUE")
    else
      return()
  }
  domain$onEnded(callback)
}

#
# 8. If an uncaught error occurs while executing an observer, the session gets
# a chance to handle it. I suppose the default behavior would be to send the
# message to the client if possible, and then perhaps end the session (or not,
# I could argue either way).
#
# The basic idea here is inspired by Node.js domains, which you can think of as
# a way to track execution contexts across callback- or listener-oriented
# asynchronous code. They use it to unify error handling code across a graph of
# related objects. Our domains will be to unify both lifetime and error
# handling across a graph of related reactive primitives.
#
# (You could imagine that as a client update is being processed, the session
# associated with that client would become the current domain. IIRC this is how
# showcase mode is implemented today. I don't think this would cover any cases
# not covered by rule 5 above, and the absence of rule 5 would leave cases that
# this rule would not cover.)
#
# Pitfalls/open issues:
#
# 1. Our current approach has the issue of observers staying alive longer than
# they ought to. This proposal introduces the opposite risk: that
# observers/invalidateLater/reactiveTimer instances, having implicitly been
# assigned a parent, are suspended/disposed earlier than they ought to have
# been. I find this especially worrisome for invalidateLater/reactiveTimer,
# which will often be called in a reactive expression, and thus execute under
# unpredictable circumstances. Perhaps those should continue to accept an
# explicit "session=" parameter that the user is warned about if they don't
# provide a value.
#
# 2. Are there situations where it is ambiguous what the right thing to do is,
# and we should warn/error to ask the user to provide a domain explicitly?
#
## ------------------------------------------------------------------------

#' Reactive domains
#'
#' Reactive domains are a mechanism for establishing ownership over reactive
#' primitives (like reactive expressions and observers), even if the set of
#' reactive primitives is dynamically created. This is useful for lifetime
#' management (i.e. destroying observers when the Shiny session that created
#' them ends) and error handling.
#'
#' At any given time, there can be either a single "default" reactive domain
#' object, or none (i.e. the reactive domain object is `NULL`). You can
#' access the current default reactive domain by calling
#' `getDefaultReactiveDomain`.
#'
#' Unless you specify otherwise, newly created observers and reactive
#' expressions will be assigned to the current default domain (if any). You can
#' override this assignment by providing an explicit `domain` argument to
#' [reactive()] or [observe()].
#'
#' For advanced usage, it's possible to override the default domain using
#' `withReactiveDomain`. The `domain` argument will be made the
#' default domain while `expr` is evaluated.
#'
#' Implementers of new reactive primitives can use `onReactiveDomainEnded`
#' as a convenience function for registering callbacks. If the reactive domain
#' is `NULL` and `failIfNull` is `FALSE`, then the callback will
#' never be invoked.
#'
#' @name domains
#' @param domain A valid domain object (for example, a Shiny session), or
#'   `NULL`
#' @param expr An expression to evaluate under `domain`
#' @param callback A callback function to be invoked
#' @param failIfNull If `TRUE` then an error is given if the `domain`
#'   is `NULL`
NULL

#
# Example 1
# ---
# ```
# obs1 <- observe({
# })
# shinyServer(function(input, output) {
#   obs2 <- observe({
#     obs3 <- observe({
#     })
#   })
# })
# # obs1 would have no domain, obs2 and obs3 would be owned by the session
# ```
#
# Example 2
# ---
# ```
# globalValues <- reactiveValues(broadcast="")
# shinyServer(function(input, output) {
#   sessionValues <- reactiveValues()
#   output$messageOutput <- renderText({
#     globalValues$broadcast
#     obs1 <- observe({...})
#   })
#   observe({
#     if (input$goButton == 0) return()
#     isolate( globalValues$broadcast <- input$messageInput )
#   })
# })
# # The observer behind messageOutput would be owned by the session,
# # as would all the many instances of obs1 that were created.
# ```
# ---
#
# Example 3
# ---
# ```
# rexpr1 <- reactive({
#   invalidateLater(1000)
#   obs1 <- observe({...})
# })
# observeSomething <- function() {
#   obs2 <- observe({...})
# })
# shinyServer(function(input, output) {
#   obs3 <- observe({
#     observeSomething()
#     rexpr1()
#   })
# })
# # rexpr1, the invalidateLater call, and obs1 would all have no owner;
# # obs2 and obs3 would be owned by the session.
# ```
