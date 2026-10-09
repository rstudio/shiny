#' @include utils.R
#' @include resume-serialize.R
#' @include resume-options.R
#' @include resume-uploads.R
#' @include resume-writer.R
NULL

# Per-session registry behind resume. Gives reactiveVal() / reactiveValues()
# sources an identity, tracks what changed, writes the record, and on resume
# applies a record; saved state that does not fit, found at verify, closes
# the session for an inputs-only retry. It knows nothing about observers, outputs or conductors:
# those always run again.
SnapshotRegistry <- R6Class("SnapshotRegistry", cloneable = FALSE,
  public = list(
    initialize = function(session) {
      private$session <- session
      private$entries <- list()
      private$orphans <- list()
      private$retired <- character(0)
      private$informed <- character(0)
      private$ordinals <- fastmap::fastmap()
      snapshot_track_values(.subset2(session$input, "impl"), self, internal = TRUE)
      if (is.reactivevalues(session$clientData)) {
        snapshot_track_values(.subset2(session$clientData, "impl"), self, internal = TRUE)
      }
    },

    # Called by the constructors. `identity` is snapshot_source_label()'s
    # list, `apply(record)` sets the node from a record, `initial` is what a
    # revert restores. Returns the id, or NULL when the source has none.
    registerSource = function(node, kind, identity, domain = NULL, initial = NULL,
                              apply = function(record) NULL) {
      if (identical(identity$source, "internal")) {
        snapshot_mark_skipped(node, "internal")
        return(invisible(NULL))
      }
      if (identical(identity$source, "body")) {
        identity$label <- snapshot_body_label(identity$site, private$ordinal)
      }
      entry <- list(node = node, kind = kind, label = identity$label,
                    ns = private$namespaceOf(domain), apply = apply,
                    call = identity$call %||% paste0(kind, "()"))
      if (private$insideConductor()) {
        return(private$orphan(entry, "created inside a reactive()", counted = FALSE))
      }
      if (is.null(identity$label)) {
        # Not saved and not counted: only what the app author can fix blocks
        # saved state or ends a resume.
        return(private$orphan(entry, "no label", counted = FALSE))
      }
      id <- paste(entry$ns, kind, identity$label, sep = "|")
      if (id %in% private$retired) return(private$orphan(entry, "duplicate label", counted = TRUE))
      first <- private$entries[[id]]
      if (!is.null(first)) {
        # Sources sharing an id all lose their identity.
        private$entries[[id]] <- NULL
        private$retired <- c(private$retired, id)
        first$node$.snapshot$id <- NULL
        private$orphan(first, "duplicate label", counted = TRUE)
        return(private$orphan(entry, "duplicate label", counted = TRUE))
      }
      node$.snapshot$id <- id
      private$entries[[id]] <- entry
      if (private$phase %in% c("constructing", "restoring")) {
        private$initials[[id]] <- list(initial)
        record <- private$records[[id]]
        if (!is.null(record)) {
          private$applyRecord(id, entry, record)
        } else if (identical(private$phase, "constructing") && !id %in% private$omitted) {
          # Before verify, a source the record lacks is a trip, unless the
          # record says it left the source out on purpose. One created at
          # the first flush keeps its initial value.
          private$trip(id, node, "no saved value")
        }
      }
      invisible(id)
    },

    # ---- dirtiness and the writer ---------------------------------------

    markDirty = function() {
      if (!is.null(private$writer)) private$writer$markDirty()
      invisible()
    },

    # Rule 4 of the rules of reactives: a restored source written during the
    # first flush by an observer that ran again is probably being
    # accumulated into twice. `state` is the source's `.snapshot`.
    noteWrite = function(state, label) {
      if (!identical(private$phase, "restoring") || !hasCurrentContext()) return(invisible())
      # session$onResume() callbacks run in isolate() while restoring; their
      # writes rebuild state rather than accumulate into it.
      if (private$session$.resumeCallbacks$running()) return(invisible())
      if (isTRUE(state$skip) || !isTRUE(state$id %in% private$applied)) return(invisible())
      # isolate() opens its own context (type "isolate"), and that is how an
      # observer usually writes: observe({ input$x; isolate(count(count() + 1)) }).
      if (!getCurrentContext()$.reactType %in% c("observer", "isolate")) return(invisible())
      self$devmodeInform(paste0("rule4:", label), sprintf(
        "`%s` was written during the first flush of a resumed session by an observer that ran again. If that observer accumulates into it, use observeEvent(), whose handler is held back on resume (rule 4 of the rules of reactives in ?enableResume).",
        label))
      invisible()
    },

    configureWriter = function(store, key, gate, debounceMs) {
      private$writer <- SnapshotWriter$new(private$session, store, key, gate, debounceMs)
      private$session$onFlushed(function() self$scheduleWrite(), once = FALSE)
      invisible()
    },

    writerEnabled = function() !is.null(private$writer) && private$writer$enabled(),

    writerStore = function() if (!is.null(private$writer)) private$writer$store(),

    scheduleWrite = function() {
      if (!is.null(private$writer)) private$writer$schedule(function() private$record())
      invisible()
    },

    writeNow = function() {
      if (is.null(private$writer)) return(invisible(FALSE))
      private$writer$writeNow(function() private$record())
    },

    deleteSnapshot = function() {
      if (!is.null(private$writer)) private$writer$delete()
      invisible()
    },

    wroteSnapshot = function() !is.null(private$writer) && private$writer$wrote(),

    # snapshot_code_hash(), once per session: the files may change while it runs.
    codeHash = function() {
      if (is.null(private$codeHashValue)) {
        private$codeHashValue <- list(snapshot_code_hash(package = private$serverPackage))
      }
      private$codeHashValue[[1]]
    },

    # server.R reports the server function before any write or resume. The
    # package version joins the hash; in devmode, code the hash cannot see
    # is named once.
    noteServerFunction = function(package, srcFile) {
      private$serverPackage <- package
      private$codeHashValue <- NULL
      if (!is.null(private$session$.resumeSettings$appVersion)) return(invisible())
      dir <- snapshot_app_path()
      files <- app_r_files(dir)
      covered <- normalizePath(file.path(dir, c(files$top, files$helpers)), mustWork = FALSE)
      hasFile <- length(srcFile) == 1L && nzchar(srcFile)
      outside <- hasFile && !normalizePath(srcFile, mustWork = FALSE) %in% covered
      nothing <- !length(covered) && is.null(package)
      if (outside || nothing) {
        self$devmodeInform("code-hash", sprintf(
          "The resume code hash covers %s. The server function is defined in %s, which it does not see, so a deploy that changes it will not be detected before saved state is restored. Set `appVersion` in enableResume() and change it when a deploy changes what the app saves.",
          if (length(covered)) "the app's R files (the app directory and R/)" else "no files",
          if (hasFile) srcFile else "code the hash does not cover"))
      }
      invisible()
    },

    # ---- the record ----------------------------------------------------

    snapshot = function(report = TRUE) {
      settings <- private$session$.resumeSettings
      maxNode <- settings$maxNodeSize
      maxTotal <- settings$maxSize
      sources <- list()
      sizes <- numeric(0)
      rows <- list()
      blocked <- list()
      addRow <- function(kind, label, ns, restores, reason = NA_character_) {
        if (!report) return(invisible())
        rows[[length(rows) + 1L]] <<- list(kind = kind, label = label %||% NA_character_, namespace = ns,
          restores = restores, reason = if (restores) NA_character_ else reason)
      }
      block <- function(id, reason) blocked[[length(blocked) + 1L]] <<- list(id = id, reason = reason)
      unlabelled <- character(0)
      # Left out on purpose, so the next resume does not take their missing
      # saved values for sources the record never knew.
      omitted <- character(0)

      for (id in names(private$entries)) {
        entry <- private$entries[[id]]
        if (private$sourceDestroyed(entry)) {
          omitted <- c(omitted, id)
          next
        }
        st <- entry$node$.snapshot
        if (isTRUE(st$skip)) {
          if (!identical(st$skipReason, "internal")) {
            omitted <- c(omitted, id)
            addRow(entry$kind, entry$label, entry$ns, FALSE, st$skipReason)
          }
          next
        }
        res <- tryCatch(private$sourceRecord(entry, maxNode),
          error = function(e) list(reason = snapshot_check_failed(e)))
        if (is.null(res$reason)) {
          sources[[id]] <- res$record
          sizes[[id]] <- res$size
          addRow(entry$kind, entry$label, entry$ns, TRUE)
        } else {
          block(id, res$reason)
          addRow(entry$kind, entry$label, entry$ns, FALSE, res$reason)
          self$devmodeInform(paste0("blocked:", id), sprintf(
            "`%s` cannot be saved (%s), so the session resumes from inputs only until it is fixed or left out with excludeFromResume().",
            entry$label, res$reason))
        }
      }
      for (entry in private$orphans) {
        if (private$sourceDestroyed(entry)) next
        st <- entry$node$.snapshot
        if (isTRUE(st$skip)) {
          if (!identical(st$skipReason, "internal")) addRow(entry$kind, entry$label, entry$ns, FALSE, st$skipReason)
          next
        }
        addRow(entry$kind, entry$label, entry$ns, FALSE, entry$reason)
        if (isTRUE(entry$counted)) block(paste(entry$ns, entry$kind, entry$label %||% "<unnamed>", sep = "|"), entry$reason)
        if (identical(entry$reason, "no label")) unlabelled <- c(unlabelled, entry$call)
      }
      if (length(unlabelled)) {
        self$devmodeInform("no-label", sprintf(
          "%d sources with no label were not saved: %s. Assign each to a name or give it a label (rule 1 of the rules of reactives in ?enableResume).",
          length(unlabelled), paste(unlabelled, collapse = ", ")))
      }

      # Total cap: the largest sources are blocked until the rest fit.
      while (length(sizes) && sum(sizes) > maxTotal) {
        victim <- names(sizes)[which.max(sizes)]
        sizes <- sizes[names(sizes) != victim]
        sources[[victim]] <- NULL
        block(victim, "over maxSize")
        entry <- private$entries[[victim]]
        for (i in seq_along(rows)) {
          if (identical(rows[[i]]$kind, entry$kind) && identical(rows[[i]]$label, entry$label) && identical(rows[[i]]$namespace, entry$ns)) {
            rows[[i]]$restores <- FALSE
            rows[[i]]$reason <- "over maxSize"
          }
        }
      }

      fileInputs <- private$fileInputValues()
      plan <- snapshot_upload_plan(fileInputs, maxTotal - sum(sizes))
      for (name in plan$omitted) {
        fileInputs[[name]] <- NULL
        self$devmodeInform(paste0("upload-omitted:", name), sprintf(
          "The files uploaded to input$%s do not fit under maxSize, so after a resume input$%s is empty and what depends on it re-runs.",
          name, name))
      }
      userData <- private$userDataValues(maxNode, addRow)

      record <- list(
        format = snapshot_format_version,
        complete = length(blocked) == 0L,
        blocked = blocked,
        sources = sources,
        omitted = omitted,
        userData = userData,
        fileInputs = fileInputs,
        uploads = plan$keep
      )
      if (report) record$report <- snapshot_report_frame(rows)
      record
    },

    report = function() self$snapshot()$report,

    # devmode messages once per session; the process-wide 8 h throttle in
    # devmode_inform() is bypassed with .frequency = "always".
    devmodeInform = function(key, message) {
      if (key %in% private$informed) return(invisible())
      private$informed <- c(private$informed, key)
      devmode_inform(message, .frequency = "always")
      invisible()
    },

    # ---- resuming ------------------------------------------------------
    #
    # A resume runs in these steps: (1) the session reads, checks and decodes
    # the record, restores its uploads and claims it
    # (ShinySession$.resumeSession()); a problem there falls back to inputs
    # only; (2) the client's inputs, the record's file inputs and userData are
    # set; (3) the server function runs, and constructors apply their records;
    # (4) verify: saved state that does not fit closes the session, and the
    # client's retry starts from inputs only; (4a) the
    # session runs session$onResume() callbacks and (4b) sends `resumed`;
    # (5) the first flush, with observeEvent() first runs held back; (6) the
    # replacement record is written (finishResume()). The client's `update`s
    # queued while it was disconnected arrive after step 5 and are ordinary
    # input.

    # Steps 2-4. `record` is a decoded record whose identity checks passed and
    # whose uploads were restored; `live` the client's handler-applied inputs
    # as of the disconnect; `run` runs the server function once. Returns the
    # outcome, or raises `shiny_resume_failed`. Step 6 runs after the first
    # flush.
    resume = function(record, live, run) {
      private$records <- record$sources
      private$omitted <- record$omitted %||% character(0)
      private$initials <- list()
      private$applied <- character(0)
      private$claimed <- character(0)
      private$trips <- list()
      private$skipped <- character(0)
      private$outcomeValue <- NULL
      private$phase <- "constructing"
      snapshot_begin_hold_back(private$session)
      completed <- FALSE
      on.exit(if (!completed) private$leave(), add = TRUE)

      # Step 2: the client's inputs, as on an inputs-only resume; the record's
      # file inputs over them, since the browser cannot send a file again;
      # the live clientData; then userData, so the server function's
      # top-level guards see it.
      private$setInputs(live$inputs, force = FALSE)
      private$setInputs(record$fileInputs, force = FALSE)
      private$setClientData(live$clientData, force = FALSE)
      private$restoreUserData(record$userData)

      # Step 3: constructors apply their records as the server function runs.
      run()

      # Step 4. The server function has run with saved values, so resetting
      # them could not undo what it did with them. The session closes without
      # a stop (server.R's endOnError()); the record is already claimed, so
      # the client's retry starts from inputs only.
      cause <- tryCatch(private$verify(),
        error = function(e) paste0("saved state could not be checked: ", conditionMessage(e)))
      if (!is.null(cause)) {
        message(sprintf(
          "Resume failed after the server function ran: %s. The session restarted from inputs only.",
          cause))
        self$devmodeInform("misfit-appversion",
          "Saved state did not fit the app once the server function ran. If a deploy changed what the app saves, set or bump appVersion in enableResume().")
        rlang::abort(paste0("Saved state does not fit the app: ", cause), class = "shiny_resume_failed")
      }
      private$outcomeValue <- list(resumed = "snapshot", cause = NULL, values = length(private$applied))
      private$phase <- "restoring"
      private$session$onFlushed(function() self$finishResume(), once = TRUE)
      completed <- TRUE
      private$outcomeValue
    },

    # Step 6. The replacement record (the one read was deleted): after the
    # first flush, so sources rebuilt by hooks and first-flush observers are
    # in it.
    finishResume = function() {
      if (!identical(private$phase, "restoring")) return(invisible())
      self$writeNow()
      private$outcomeValue$values <- length(private$applied)
      unclaimed <- setdiff(names(private$records), private$claimed)
      private$outcomeValue$unclaimed <- unclaimed
      if (length(unclaimed)) {
        self$devmodeInform("unclaimed", sprintf(
          "Saved values for %s were not restored because nothing recreated them, probably because an observeEvent() handler creates them and handlers whose events already happened do not run again. Create them from state instead, or in session$onResume() (rule 3 of the rules of reactives in ?enableResume).",
          paste(unclaimed, collapse = ", ")))
      }
      private$outcomeValue$skipped <- private$skipped
      private$leave()
      invisible()
    },
    restoring = function() private$phase %in% c("constructing", "restoring"),
    noteInitSkipped = function(label) {
      private$skipped <- c(private$skipped, label)
      invisible()
    },
    noteEventObserver = function(reactId) {
      private$eventObservers <- c(private$eventObservers, reactId)
      invisible()
    },
    # Matched by observer (reactId), not by context, since an observer gets a
    # new context each run. `outputs` are the reactIds of the session's output
    # observers: re-rendering on resume has no side effect, so observeEvent()
    # is the wrong advice for them.
    actionObservers = function(outputs = character(0)) {
      snapshot_action_observers(.subset2(private$session$input, "impl"),
                                c(private$eventObservers, outputs))
    },
    # excludeFromResume() during a resume: a source whose saved value was
    # applied goes back to its constructor value, and its dependents
    # recompute. A source that lost its identity to a duplicate label has no
    # id and was never applied.
    excludeRestored = function(state) {
      id <- state$id
      if (is.null(id) || !id %in% private$applied) return(invisible())
      private$revertOne(private$entries[[id]], id)
      invisible()
    },
    outcome = function() private$outcomeValue
  ),

  private = list(
    session = NULL,
    entries = NULL,      # id -> list(node, kind, label, ns, apply)
    orphans = NULL,      # entries without an identity: list(..., reason, counted)
    ordinals = NULL,     # per-caller construction order of identical calls: fastmap
    retired = NULL,      # ids two or more sources shared; none of them gets it
    informed = NULL,
    codeHashValue = NULL,
    serverPackage = NULL,

    writer = NULL,

    # resuming
    phase = "idle",      # "idle" | "constructing" (server function running) | "restoring" (verified; first flush)
    records = NULL, omitted = character(0), initials = NULL, applied = character(0), claimed = character(0), trips = list(),
    skipped = character(0), eventObservers = character(0), outcomeValue = NULL,

    orphan = function(entry, reason, counted) {
      entry$reason <- reason
      entry$counted <- counted
      private$orphans[[length(private$orphans) + 1L]] <- entry
      invisible(NULL)
    },

    # The 1-based position of this construction among identical calls made in
    # one invocation (`env`) of the caller. The map holds `env`, so its
    # address cannot be reused while the session lives.
    ordinal = function(env, call) {
      key <- paste(rlang::obj_address(env), rlang::hash(call))
      seen <- private$ordinals$get(key)
      n <- if (is.null(seen)) 1L else seen$n + 1L
      private$ordinals$set(key, list(env = env, n = n))
      n
    },

    insideConductor = function() {
      hasCurrentContext() && identical(getCurrentContext()$.reactType, "observable")
    },

    # The module namespace relative to the root session: "" for the root,
    # "mod-" inside moduleServer("mod"). A MockShinySession's own ns() adds a
    # prefix to everything, so the root's is stripped.
    namespaceOf = function(domain) {
      if (is.null(domain)) return("")
      ns <- domain$ns("")
      root <- private$session$ns("")
      if (nzchar(root) && startsWith(ns, root)) substring(ns, nchar(root) + 1L) else ns
    },

    # ReactiveVal keeps its destroyed flag private; ReactiveValues exposes it.
    sourceDestroyed = function(entry) {
      if (identical(entry$kind, "reactiveVal")) isTRUE(entry$node$.snapshot$destroyed()) else isTRUE(entry$node$.destroyed)
    },

    # The record the writer stores: the snapshot plus the identity block.
    record = function() {
      rec <- self$snapshot(report = FALSE)
      rec$identity <- snapshot_identity(private$session)
      rec
    },

    walkReason = function(hit) {
      if (identical(hit, snapshot_too_large)) hit else paste0("value is not serializable: ", hit)
    },

    sourceRecord = function(entry, maxNode) {
      node <- entry$node
      if (identical(entry$kind, "reactiveVal")) {
        value <- node$.snapshot$value()
        hit <- snapshot_unserializable_path(value)
        if (!is.null(hit)) return(list(reason = private$walkReason(hit)))
        bytes <- serialize(value, NULL, xdr = FALSE)
        if (length(bytes) > maxNode) return(list(reason = "over maxNodeSize"))
        return(list(record = list(kind = "reactiveVal", bytes = bytes), size = length(bytes)))
      }
      order <- node$.nameOrder
      values <- node$.values$mget(order)
      for (key in order) {
        hit <- snapshot_unserializable_path(values[[key]], paste0("value$", key))
        if (!is.null(hit)) return(list(reason = private$walkReason(hit)))
      }
      bytes <- serialize(values, NULL, xdr = FALSE)
      if (length(bytes) > maxNode) return(list(reason = "over maxNodeSize"))
      list(record = list(kind = "reactiveValues", bytes = bytes, order = order), size = length(bytes))
    },

    # The browser keeps every input value except a file, so file inputs are
    # the only inputs a record holds.
    fileInputValues = function() {
      impl <- .subset2(private$session$input, "impl")
      out <- list()
      for (name in impl$.nameOrder) {
        if (!identical(impl$getMeta(name, "shiny.serializer"), serializerFileInput)) next
        value <- impl$.values$get(name)
        if (!is.null(value)) out[[name]] <- value
      }
      out
    },

    userDataValues = function(maxNode, addRow) {
      ud <- private$session$userData
      out <- list()
      exclude <- private$session$.resumeSettings$userDataExclude
      for (name in ls(ud, all.names = TRUE)) {
        if (name %in% exclude) {
          addRow("userData", name, "", FALSE, "userDataExclude")
          next
        }
        value <- get(name, envir = ud, inherits = FALSE)
        reason <- tryCatch(private$userDataReason(value, maxNode),
          error = function(e) snapshot_check_failed(e))
        addRow("userData", name, "", is.null(reason), reason)
        if (is.null(reason)) out[name] <- list(value)
      }
      out
    },

    userDataReason = function(value, maxNode) {
      hit <- snapshot_unserializable_path(value)
      if (!is.null(hit)) return(private$walkReason(hit))
      if (snapshot_serialized_size(value) > maxNode) return("over maxNodeSize")
      NULL
    },

    # ---- resume internals ----

    # A source without a usable record is a trip that ends the session if it
    # still counts at verify. Trips are collected and judged there: the
    # server function may mark the source with excludeFromResume() after its
    # constructor returned.
    trip = function(id, node, reason) {
      private$trips[[length(private$trips) + 1L]] <- list(id = id, node = node, reason = reason)
      invisible()
    },

    # The first trip that counts (its source was not marked since), as the
    # cause text, or NULL.
    gateCause = function() {
      for (t in private$trips) {
        if (isTRUE(t$node$.snapshot$skip)) next
        return(sprintf("%s (%s)", t$id, t$reason))
      }
      NULL
    },

    # A record that cannot be applied (corrupt bytes, a class whose package
    # is gone) must not raise into the user's constructor call.
    applyRecord = function(id, entry, record) {
      ok <- tryCatch({ entry$apply(record); TRUE },
        error = function(e) { private$trip(id, entry$node, paste0("saved value could not be applied: ", conditionMessage(e))); FALSE })
      if (ok) {
        private$applied <- c(private$applied, id)
        private$claimed <- c(private$claimed, id)
      }
      invisible(ok)
    },

    # Step 4: why saved state does not fit, or NULL. An unmarked source that
    # lost its identity to a shared label counts too.
    verify = function() {
      for (entry in private$orphans) {
        if (isTRUE(entry$counted) && !isTRUE(entry$node$.snapshot$skip)) {
          private$trip(paste(entry$ns, entry$kind, entry$label %||% "<unnamed>", sep = "|"), entry$node, entry$reason)
        }
      }
      private$gateCause()
    },

    revertOne = function(entry, id) {
      initial <- private$initials[[id]]
      if (!is.null(initial)) entry$node$.snapshot$revert(initial[[1]])
      private$applied <- setdiff(private$applied, id)
      invisible()
    },

    restoreUserData = function(values) {
      values <- values[setdiff(names(values), private$session$.resumeSettings$userDataExclude)]
      ud <- private$session$userData
      for (name in names(values)) {
        assign(name, values[[name]], envir = ud)
      }
      invisible()
    },

    setInputs = function(values, force) {
      if (!length(values)) return(invisible())
      impl <- .subset2(private$session$input, "impl")
      for (name in names(values)) impl$set(name, values[[name]], force = force)
      invisible()
    },

    setClientData = function(values, force) {
      if (!length(values) || !is.reactivevalues(private$session$clientData)) return(invisible())
      impl <- .subset2(private$session$clientData, "impl")
      for (name in names(values)) impl$set(name, values[[name]], force = force)
      if (is.function(private$session$manageHiddenOutputs)) private$session$manageHiddenOutputs()
      invisible()
    },

    leave = function() {
      private$phase <- "idle"
      private$records <- NULL
      private$omitted <- character(0)
      private$initials <- NULL
      invisible()
    }
  )
)

# The registry of `domain`'s root session, or NULL (resume off, no session,
# or a foreign domain).
snapshot_registry <- function(domain) {
  while (inherits(domain, "session_proxy")) domain <- .subset2(domain, "parent")
  if (!is.environment(domain)) return(NULL)
  reg <- tryCatch(.subset2(domain, ".snapshotRegistry"), error = function(e) NULL)
  if (!is.null(reg) && inherits(reg, "SnapshotRegistry")) reg
}

# A session answering a `resume` holds back observeEvent() first runs until
# its first flush ends, whether or not it has saved state, so a reconnect
# never replays past events. Resume-off sessions have no registry, so this
# lives on the session.
snapshot_begin_hold_back <- function(session) {
  if (isTRUE(session$.resumeHoldBack)) return(invisible())
  session$.resumeHoldBack <- TRUE
  session$onFlushed(function() session$.resumeHoldBack <- FALSE, once = TRUE)
  invisible()
}

snapshot_holding_back <- function(domain) {
  while (inherits(domain, "session_proxy")) domain <- .subset2(domain, "parent")
  if (!is.environment(domain)) return(FALSE)
  isTRUE(tryCatch(.subset2(domain, ".resumeHoldBack"), error = function(e) FALSE))
}

# Closure state is never saved, so the variables a held-back handler would
# have assigned keep their initial values; devmode names them.
snapshot_inform_closure_vars <- function(label, func) {
  vars <- snapshot_hazards(snapshot_user_body(func))
  if (!length(vars)) return(invisible())
  one <- length(vars) == 1L
  devmode_inform(sprintf(
    "%s was held back on resume; it assigns %s, whose %s not saved. Keep %s in a reactiveVal() or session$userData, or rebuild %s in session$onResume().",
    label, paste(vars, collapse = ", "), if (one) "value was" else "values were",
    if (one) "it" else "them", if (one) "it" else "them"
  ), .frequency = "always")
  invisible()
}

# The `.snapshot` state of a tracked source: an environment the registry and
# the node update in place. Untracked nodes keep `.snapshot = NULL`.
snapshot_node_state <- function(registry, ...) {
  state <- list2env(list(...), parent = emptyenv())
  state$registry <- registry
  state$id <- NULL
  state$skip <- FALSE
  state$skipReason <- NULL
  state
}

# Starts tracking a ReactiveVal. Built here rather than in
# ReactiveVal$initialize() so the closures do not keep the constructor's
# frame (and its initial value) alive.
snapshot_track_val <- function(impl, registry) {
  private <- impl$.__enclos_env__$private
  impl$.snapshot <- snapshot_node_state(registry,
    value = function() private$value,
    destroyed = function() isTRUE(private$.destroyed),
    apply = function(record) private$value <- record$value,
    revert = function(initial) {
      private$value <- initial
      # Something may have read the restored value already; it must see the revert.
      private$dependents$invalidate()
    }
  )
  invisible(impl$.snapshot)
}

# Starts tracking a ReactiveValues (a reactiveValues() source, or a session's
# input and clientData, which are `internal`: dirty-tracked, never recorded
# as sources).
snapshot_track_values <- function(impl, registry, internal = FALSE) {
  impl$.snapshot <- snapshot_node_state(registry,
    apply = function(record) {
      vals <- record$value
      order <- record$order
      vals <- lapply(order, function(key) vals[[key]])   # read all before setting any
      for (i in seq_along(order)) impl$.values$set(order[[i]], vals[[i]])
      impl$.nameOrder <- unique(c(order, impl$.nameOrder))
    },
    revert = function(initial) snapshot_values_revert(impl, initial)
  )
  if (internal) {
    impl$.snapshot$skip <- TRUE
    impl$.snapshot$skipReason <- "internal"
  }
  invisible(impl$.snapshot)
}

# Registers a reactiveValues() source after construction, because
# reactiveValues() only learns the assignment-derived label once the object
# exists. The record's keys win; keys the record lacks keep their initial value.
snapshot_register_values <- function(impl, registry, identity, domain, initial) {
  snapshot_track_values(impl, registry)
  registry$registerSource(impl, "reactiveValues", identity, domain, initial = initial,
    apply = impl$.snapshot$apply
  )
  invisible()
}

# Back to the constructor's initial values.
snapshot_values_revert <- function(impl, initial) {
  for (key in impl$.nameOrder) impl$.values$remove(key)
  impl$.nameOrder <- character(0)
  for (key in base::names(initial)) {
    impl$.values$set(key, initial[[key]])
    impl$.nameOrder[length(impl$.nameOrder) + 1] <- key
  }
  for (deps in impl$.dependents$values()) deps$invalidate()
  impl$.namesDeps$invalidate()
  impl$.valuesDeps$invalidate()
  impl$.allValuesDeps$invalidate()
  invisible()
}

snapshot_check_failed <- function(e) paste0("could not be checked: ", conditionMessage(e))

snapshot_session_report <- function(session) {
  reg <- session$.snapshotRegistry
  if (is.null(reg)) snapshot_report_frame(list()) else reg$report()
}

snapshot_report_frame <- function(rows) {
  df <- data.frame(
    kind = vapply(rows, `[[`, character(1), "kind"),
    label = vapply(rows, `[[`, character(1), "label"),
    namespace = vapply(rows, `[[`, character(1), "namespace"),
    restores = vapply(rows, `[[`, logical(1), "restores"),
    reason = vapply(rows, `[[`, character(1), "reason"),
    stringsAsFactors = FALSE
  )
  class(df) <- c("shiny_resume_report", "data.frame")
  df
}

#' @export
print.shiny_resume_report <- function(x, ...) {
  cat(sprintf("<resumeReport> %d of %d values restore\n", sum(x$restores), nrow(x)))
  bad <- x[!x$restores, , drop = FALSE]
  for (reason in unique(bad$reason)) {
    cat("\n", reason, ":\n", sep = "")
    sel <- bad$reason == reason
    cat(paste0("  ", bad$kind[sel], " ", ifelse(is.na(bad$label[sel]), "<unnamed>", bad$label[sel]), collapse = "\n"), "\n", sep = "")
  }
  invisible(x)
}
