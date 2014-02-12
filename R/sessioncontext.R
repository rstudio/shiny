# Keeps the context associated with a ShinySession reference object for the
# duration of a request. Used to emit reactive evaluation information to the
# appropriate session when showcase mode is enabled.

.sessionContext <- new.env(parent=emptyenv())
.beginShowcaseSessionContext <- function(session) {
  assign("session", session, envir = .sessionContext)
}

.endShowcaseSessionContext <- function() {
  if (exists("session", where = .sessionContext))
    remove("session", envir = .sessionContext)
}

.getShowcaseSessionContext <- function() {
  if (exists("session", where = .sessionContext))
    .sessionContext$session
  else
    NULL
}
