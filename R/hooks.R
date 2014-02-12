

# Call an application hook. Application hooks are provided so that front ends
# can know when a Shiny application is running:
#
# shiny.onAppStart -- called when an application begins running
# shiny.onAppStop -- called when an appliation stops
#
# Both hooks are passed the url where the application is accessible (appUrl).
# Note that the appUrl can be NULL if the application was run on a UNIX domain
# socket rather than a TCP/IP port/
callAppHook <- function(name, appUrl) {
  for (hook in getHooksList(paste0("shiny.", name)))
    hook(appUrl)
}

# The value for getHook can be a single function or a list of functions,
# This function ensures that the result can always be processed as a list
getHooksList <- function(name) {
  hooks <- getHook(name)
  if (!is.list(hooks))
    hooks <- list(hooks)
  hooks
}
