#' @include utils.R
#' @include resume-serialize.R
#' @include resume-options.R
#' @include resume-uploads.R
NULL

# Per-session registry behind resume. Gives reactiveVal() / reactiveValues()
# sources an identity, tracks what changed, writes the record, and on resume
# applies a record; saved state that does not fit, found at verify, ends
# the session. It knows nothing about observers, outputs or conductors:
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
      if (!private$dirty) private$firstDirtyAt <- private$session$.now()
      private$dirty <- TRUE
      private$changedSinceSchedule <- TRUE
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
      private$store <- store
      private$key <- key
      private$gate <- gate
      private$debounceMs <- debounceMs
      private$session$onFlushed(function() self$scheduleWrite(), once = FALSE)
      invisible()
    },

    writerEnabled = function() {
      !is.null(private$store) && isTRUE(private$gate()) && !is.null(self$writerStore())
    },

    # The writer's store, resolving it if needed; NULL when unconfigured or
    # unavailable.
    writerStore = function() {
      if (is.function(private$store)) private$store <- private$store()
      private$store
    },

    # Called after every flush. Only a flush that followed a change
    # (re)schedules the write; the wait is capped at ten quiet intervals from
    # the first unsaved change so a session that never goes quiet still writes.
    scheduleWrite = function() {
      if (!private$changedSinceSchedule || !self$writerEnabled()) return(invisible())
      private$changedSinceSchedule <- FALSE
      private$cancelPendingWrite()
      waited <- private$session$.now() - private$firstDirtyAt
      delay <- max(0, min(private$debounceMs, 10 * private$debounceMs - waited))
      private$cancelWrite <- defineScheduler(private$session)(delay, function() {
        private$cancelWrite <- NULL
        self$writeNow()
      })
      invisible()
    },

    # Serializes and writes now if the gate allows; never throws. A failure
    # is logged once per session, and later writes are still attempted.
    writeNow = function() {
      private$cancelPendingWrite()
      ok <- tryCatch({
        if (!self$writerEnabled()) return(invisible(FALSE))
        # write() would refuse it too, but only after the uploads were
        # copied back into the files directory claim() deleted.
        if (private$store$claimed(private$key)) return(invisible(FALSE))
        rec <- self$snapshot(report = FALSE)
        rec$identity <- snapshot_identity(private$session)
        uploads <- rec$uploads
        if (length(uploads)) rec <- snapshot_write_uploads(rec, private$store$filesDir(private$key))
        rec$uploads <- NULL
        written <- !identical(private$store$write(private$key, rec), FALSE)
        if (written) {
          snapshot_prune_uploads(private$store$filesDir(private$key), as.character(unlist(uploads, use.names = FALSE)))
        }
        written
      }, error = function(e) {
        if (!private$writeFailureLogged) {
          private$writeFailureLogged <- TRUE
          message("Saved state could not be written: ", conditionMessage(e))
        }
        FALSE
      })
      private$dirty <- FALSE
      private$firstDirtyAt <- NULL
      private$changedSinceSchedule <- FALSE
      if (ok) private$wrote <- TRUE
      invisible(ok)
    },

    # Removes the record this session wrote, if any, and stops pending writes.
    deleteSnapshot = function() {
      private$cancelPendingWrite()
      if (private$wrote) {
        self$writerStore()$delete(private$key)
        private$wrote <- FALSE
      }
      invisible()
    },

    wroteSnapshot = function() private$wrote,

    # snapshot_code_hash(), once per session: the files may change while it runs.
    codeHash = function() {
      if (is.null(private$codeHashValue)) private$codeHashValue <- list(snapshot_code_hash())
      private$codeHashValue[[1]]
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
      addRow <- function(kind, label, ns, adoptable, reason = NA_character_) {
        if (!report) return(invisible())
        rows[[length(rows) + 1L]] <<- list(kind = kind, label = label %||% NA_character_, namespace = ns,
          adoptable = adoptable, reason = if (adoptable) NA_character_ else reason)
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
            rows[[i]]$adoptable <- FALSE
            rows[[i]]$reason <- "over maxSize"
          }
        }
      }

      inputs <- private$inputValues(addRow)
      plan <- snapshot_upload_plan(inputs$inputs, inputs$fileInputs, maxTotal - sum(sizes))
      for (name in plan$omitted) {
        inputs$inputs[name] <- list(NULL)
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
        inputs = inputs$inputs,
        clientData = inputs$clientData,
        fileInputs = inputs$fileInputs,
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
    # the record and claims it (ShinySession$.resumeSession()); a problem
    # there falls back to inputs only; (2) the record's inputs and userData
    # are restored; (3) the server function runs, and constructors apply
    # their records; (4) verify: saved state that does not fit ends the
    # session; (4.5) the client's inputs that differ from the record's are
    # merged, as state (mergeLiveInputs()); (4a) the session runs
    # session$onResume() callbacks and (4b) sends `resumed`; (5) the first
    # flush, with observeEvent() first runs held back; (6) the replacement
    # record is written (finishResume()). The client's `update`s queued while
    # it was disconnected arrive after step 5 and are ordinary input.

    # Steps 2-4.5. `record` is a decoded record whose identity checks passed;
    # `live` the client's handler-applied inputs; `run` runs the server
    # function once. Returns the outcome, or raises `shiny_resume_failed`.
    # Step 6 runs after the first flush.
    resume = function(record, live, run) {
      private$records <- record$sources
      private$omitted <- record$omitted %||% character(0)
      private$initials <- list()
      private$applied <- character(0)
      private$claimed <- character(0)
      private$trips <- list()
      private$liveSet <- live
      private$recordInputs <- record$inputs
      private$recordClientData <- record$clientData
      private$skipped <- character(0)
      private$outcomeValue <- NULL
      private$phase <- "constructing"
      snapshot_begin_hold_back(private$session)
      completed <- FALSE
      on.exit(if (!completed) private$leave(), add = TRUE)

      # Step 2: the record's inputs, the client's live clientData, then
      # userData, so the server function's top-level guards see it.
      private$setInputs(record$inputs, force = FALSE)
      private$setClientData(live$clientData, force = FALSE)
      private$restoreUserData(record$userData)

      # Step 3: constructors apply their records as the server function runs.
      run()

      # Step 4. The server function has run with saved values, so resetting
      # them could not undo what it did with them: saved state that does not
      # fit this code ends the session (server.R's endOnError()).
      cause <- tryCatch(private$verify(),
        error = function(e) paste0("saved state could not be checked: ", conditionMessage(e)))
      if (!is.null(cause)) {
        message(sprintf(
          "Resume failed after the server function ran: %s. The session ended; reloading the page starts fresh. Set or bump appVersion in enableResume() when a deploy changes what the app saves.",
          cause))
        rlang::abort(paste0("Saved state does not fit the app: ", cause), class = "shiny_resume_failed")
      }
      private$outcomeValue <- list(resumed = "snapshot", cause = NULL,
                                   values = length(private$applied), inputs = length(record$inputs))
      private$phase <- "restoring"
      # Step 4.5, inside the hold-back: the delta is record lag (values the
      # old session processed after its last write), so it is state. The
      # outage-era changes are not here; the client sends them as `update`s.
      private$mergeLiveInputs()
      private$session$onFlushed(function() self$finishResume(), once = TRUE)
      completed <- TRUE
      private$outcomeValue
    },

    # Step 6. The replacement record (the one read was deleted): after the
    # first flush, so sources rebuilt by hooks and first-flush observers are
    # in it, and with the merged inputs, the freshest baseline for the next
    # resume's delta.
    finishResume = function() {
      if (is.null(private$liveSet)) return(invisible())
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
    # excludeFromResume() on a source whose saved value was applied during
    # this resume. Before the app used that value, the source goes back to its
    # constructor value; after, nothing can undo what the app did with it.
    excludeRestored = function(state) {
      # A source that lost its identity to a duplicate label has no entry.
      entry <- if (is.null(state$id)) NULL else private$entries[[state$id]]
      if (isTRUE(state$used)) {
        rlang::abort(sprintf(
          "excludeFromResume() was called on `%s` after the app used its restored value. Call it before the first read, for example around the constructor: `x <- excludeFromResume(reactiveVal(...))`.",
          entry$label %||% state$id %||% "<unnamed>"), class = "shiny_resume_exclude_late")
      }
      if (!is.null(entry)) private$revertOne(entry, state$id)
      state$watch <- FALSE
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

    # writer
    store = NULL, key = NULL, gate = NULL, debounceMs = 1000,
    dirty = FALSE, firstDirtyAt = NULL, changedSinceSchedule = FALSE,
    cancelWrite = NULL, writeFailureLogged = FALSE, wrote = FALSE, pausedFlag = FALSE,

    # resuming
    phase = "idle",      # "idle" | "constructing" (server function running) | "restoring" (verified; first flush)
    records = NULL, omitted = character(0), initials = NULL, applied = character(0), claimed = character(0), trips = list(),
    liveSet = NULL, recordInputs = NULL, recordClientData = NULL,
    skipped = character(0), outcomeValue = NULL,
    watched = list(),

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

    cancelPendingWrite = function() {
      if (is.function(private$cancelWrite)) private$cancelWrite()
      private$cancelWrite <- NULL
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

    inputValues = function(addRow) {
      impl <- .subset2(private$session$input, "impl")
      inputs <- list()
      fileInputs <- character(0)
      for (name in impl$.nameOrder) {
        serializer <- impl$getMeta(name, "shiny.serializer")
        if (identical(serializer, serializerUnserializable)) {
          addRow("input", name, "", FALSE, "password input; restored from the client")
          next
        }
        inputs[name] <- list(impl$.values$get(name))
        addRow("input", name, "", TRUE)
        if (identical(serializer, serializerFileInput)) fileInputs <- c(fileInputs, name)
      }
      clientData <- list()
      if (is.reactivevalues(private$session$clientData)) {
        cd <- .subset2(private$session$clientData, "impl")
        clientData <- cd$.values$mget(cd$.nameOrder)
      }
      list(inputs = inputs, clientData = clientData, fileInputs = fileInputs)
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
        entry$node$.snapshot$watch <- TRUE
        private$watched <- c(private$watched, list(entry$node$.snapshot))
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

    # Step 4.5. A file input's live value is the browser's NULL and must not
    # clobber the restored upload. `force = TRUE`: the dependents of a value
    # that changed must recompute at the first flush even if a constructor
    # already set it.
    mergeLiveInputs = function() {
      live <- private$liveSet
      changed <- list()
      for (name in names(live$inputs)) {
        if (name %in% live$fileInputs) next
        if (!identical(live$inputs[[name]], private$recordInputs[[name]])) changed[name] <- list(live$inputs[[name]])
      }
      private$setInputs(changed, force = TRUE)
      changedCd <- list()
      for (name in names(live$clientData)) {
        if (!identical(live$clientData[[name]], private$recordClientData[[name]])) changedCd[name] <- list(live$clientData[[name]])
      }
      private$setClientData(changedCd, force = TRUE)
      invisible()
    },

    leave = function() {
      for (state in private$watched) { state$watch <- FALSE; state$used <- FALSE }
      private$watched <- list()
      private$phase <- "idle"
      private$records <- NULL
      private$omitted <- character(0)
      private$initials <- NULL
      private$liveSet <- NULL
      private$recordInputs <- NULL
      private$recordClientData <- NULL
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
  # While a resume runs: `watch` once a saved value was applied, `used` once
  # the app read or wrote it since (snapshot_note_use()).
  state$watch <- FALSE
  state$used <- FALSE
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
    adoptable = vapply(rows, `[[`, logical(1), "adoptable"),
    reason = vapply(rows, `[[`, character(1), "reason"),
    stringsAsFactors = FALSE
  )
  class(df) <- c("shiny_resume_report", "data.frame")
  df
}

#' @export
print.shiny_resume_report <- function(x, ...) {
  cat(sprintf("<resumeReport> %d of %d values and inputs restore\n", sum(x$adoptable), nrow(x)))
  bad <- x[!x$adoptable, , drop = FALSE]
  for (reason in unique(bad$reason)) {
    cat("\n", reason, ":\n", sep = "")
    sel <- bad$reason == reason
    cat(paste0("  ", bad$kind[sel], " ", ifelse(is.na(bad$label[sel]), "<unnamed>", bad$label[sel]), collapse = "\n"), "\n", sep = "")
  }
  invisible(x)
}
