#' @include utils.R
#' @include resume-serialize.R
#' @include resume-options.R
#' @include resume-uploads.R
NULL

# Per-session registry behind resume. Gives reactiveVal() / reactiveValues()
# sources an identity, tracks what changed, writes the record, and on resume
# applies a record under the all-or-nothing gate (spec 3-5). It knows nothing
# about observers, outputs or conductors: those always run again.
SnapshotRegistry <- R6Class("SnapshotRegistry", cloneable = FALSE,
  public = list(
    initialize = function(session) {
      private$session <- session
      private$entries <- list()
      private$orphans <- list()
      private$retired <- character(0)
      private$informed <- character(0)
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
      entry <- list(node = node, kind = kind, label = identity$label,
                    ns = private$namespaceOf(domain), apply = apply)
      if (private$insideConductor()) {
        return(private$orphan(entry, "created inside a reactive()", counted = FALSE))
      }
      if (is.null(identity$label)) {
        # Reported when the record is taken: disableResume() comes after construction.
        return(private$orphan(entry, "no label", counted = TRUE))
      }
      id <- paste(entry$ns, kind, identity$label, sep = "|")
      if (id %in% private$retired) return(private$orphan(entry, "duplicate label", counted = TRUE))
      first <- private$entries[[id]]
      if (!is.null(first)) {
        # Sources sharing an id all lose their identity (spec 3.2).
        private$entries[[id]] <- NULL
        private$retired <- c(private$retired, id)
        first$node$.snapshot$id <- NULL
        if (id %in% private$applied) private$revertOne(first, id)
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
        } else if (identical(private$phase, "constructing")) {
          # Before verify, a source the record lacks trips the gate; one
          # created at the first flush keeps its initial value (spec 3.2).
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
      if (isTRUE(state$skip) || !isTRUE(state$id %in% private$applied)) return(invisible())
      # isolate() opens its own context (type "isolate"), and that is how an
      # observer usually writes: observe({ input$x; isolate(count(count() + 1)) }).
      if (!getCurrentContext()$.reactType %in% c("observer", "isolate")) return(invisible())
      self$devmodeInform(paste0("rule4:", label), sprintf(
        "`%s` was written during the first flush of a resumed session by an observer that ran again. If that observer accumulates into it, use observeEvent(), whose handler is held back on resume (rule 4 of the rules of reactives).",
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
      !is.null(private$store) && !private$pausedFlag && isTRUE(private$gate()) && !is.null(self$writerStore())
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
        rec <- self$snapshot()
        rec$identity <- snapshot_identity(private$session)
        if (length(rec$uploads)) rec <- snapshot_write_uploads(rec, private$store$filesDir(private$key))
        rec$uploads <- NULL
        private$store$write(private$key, rec)
        TRUE
      }, error = function(e) {
        if (!private$writeFailureLogged) {
          private$writeFailureLogged <- TRUE
          message("Saved state could not be written: ", conditionMessage(e))
        }
        FALSE
      })
      private$dirty <- FALSE
      private$firstDirtyAt <- NULL
      if (ok) private$wrote <- TRUE
      invisible(ok)
    },

    # From a fatal error until the socket closes nothing is written, the
    # closing write included (spec 5.1, 8).
    pause = function() {
      private$pausedFlag <- TRUE
      private$cancelPendingWrite()
      invisible()
    },
    paused = function() private$pausedFlag,

    # Removes the record this session wrote, if any, and stops pending writes.
    deleteSnapshot = function() {
      private$cancelPendingWrite()
      if (private$wrote) {
        self$writerStore()$delete(private$key)
        private$wrote <- FALSE
      }
      invisible()
    },

    # After a close the client announced with `unload`: the record outlives
    # the session only briefly. In-process; a restart inside the window
    # leaves the record to the TTL (accepted, spec 5.1).
    scheduleDelete = function(seconds) {
      store <- self$writerStore()
      key <- private$key
      if (is.null(store) || !private$wrote) return(invisible())
      later::later(function() try(store$delete(key), silent = TRUE), seconds)
      invisible()
    },

    wroteSnapshot = function() private$wrote,
    storeKey = function() private$key,

    # ---- the record ----------------------------------------------------

    snapshot = function() {
      settings <- private$session$.resumeSettings
      maxNode <- settings$maxNodeSize
      maxTotal <- settings$maxSize
      sources <- list()
      sizes <- numeric(0)
      rows <- list()
      blocked <- list()
      addRow <- function(kind, label, ns, adoptable, reason = NA_character_) {
        rows[[length(rows) + 1L]] <<- list(kind = kind, label = label %||% NA_character_, namespace = ns,
          adoptable = adoptable, reason = if (adoptable) NA_character_ else reason)
      }
      block <- function(id, reason) blocked[[length(blocked) + 1L]] <<- list(id = id, reason = reason)

      for (id in names(private$entries)) {
        entry <- private$entries[[id]]
        if (private$sourceDestroyed(entry)) next
        st <- entry$node$.snapshot
        if (isTRUE(st$skip)) {
          if (!identical(st$skipReason, "internal")) addRow(entry$kind, entry$label, entry$ns, FALSE, st$skipReason)
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
            "`%s` cannot be saved (%s), so the session resumes from inputs only until it is fixed or marked with disableResume().",
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
        if (identical(entry$reason, "no label")) private$informNoLabel(entry)
      }

      # Total cap: the largest sources are blocked until the rest fit (spec 3.4).
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

      list(
        format = snapshot_format_version,
        complete = length(blocked) == 0L,
        blocked = blocked,
        sources = sources,
        userData = userData,
        inputs = inputs$inputs,
        clientData = inputs$clientData,
        fileInputs = inputs$fileInputs,
        uploads = plan$keep,
        report = snapshot_report_frame(rows)
      )
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

    # ---- resuming (spec 4.1 steps 2-6) ---------------------------------

    # Steps 2-4 of spec 4.1. `record` is a complete record whose identity
    # checks passed; `live` the client's handler-applied inputs; `run` runs
    # the server function once. The caller seeds the RestoreContext before
    # and sends `resumed` after. Returns the outcome; step 6 runs after the
    # first flush.
    resume = function(record, live, fresh, run) {
      private$records <- record$sources
      private$initials <- list()
      private$applied <- character(0)
      private$trips <- list()
      private$liveSet <- live
      private$recordInputs <- record$inputs
      private$recordClientData <- record$clientData
      private$fresh <- isTRUE(fresh)
      private$updated <- character(0)
      private$skipped <- character(0)
      private$reran <- list()
      private$outcomeValue <- NULL
      private$phase <- "constructing"
      completed <- FALSE
      on.exit(if (!completed) private$leave(), add = TRUE)

      # Step 2: the record's inputs, then the client's live clientData.
      private$setInputs(record$inputs, force = FALSE)
      private$setClientData(live$clientData, force = FALSE)

      # Step 3: constructors apply their records as the server function runs.
      run()

      # Step 4: verify. A marked source that got a record (it was unmarked
      # when the record was written) goes back to its initial value; an
      # unmarked source without an identity blocks.
      for (id in private$applied) {
        entry <- private$entries[[id]]
        if (!is.null(entry) && isTRUE(entry$node$.snapshot$skip)) private$revertOne(entry, id)
      }
      for (entry in private$orphans) {
        if (isTRUE(entry$counted) && !isTRUE(entry$node$.snapshot$skip)) {
          if (identical(entry$reason, "no label")) private$informNoLabel(entry)
          private$trip(paste(entry$ns, entry$kind, entry$label %||% "<unnamed>", sep = "|"), entry$node, entry$reason)
        }
      }
      cause <- private$gateCause()
      if (!is.null(cause)) {
        private$revertAll()
        private$phase <- "idle"           # every handler runs at the first flush
        private$outcomeValue <- list(resumed = "inputs", cause = cause,
                                     values = 0L, inputs = length(record$inputs))
      } else {
        ud <- private$session$userData
        for (name in names(record$userData)) assign(name, record$userData[[name]], envir = ud)
        private$phase <- "restoring"
        private$outcomeValue <- list(resumed = "snapshot", cause = NULL,
                                     values = length(private$applied), inputs = length(record$inputs))
        # The record we read was deleted (single use); write our own now
        # rather than after the debounce. A no-op without a writer (Mock).
        self$writeNow()
      }
      private$session$onFlushed(function() self$finishResume(), once = TRUE)
      completed <- TRUE
      private$outcomeValue
    },

    # Step 6. On an intact page, the client's inputs that differ from the
    # record's are applied now, skipping names an `update` changed since
    # step 2. On a fresh page only clientData and inputs the record lacks:
    # the widgets' reported values are defaults, not user changes.
    finishResume = function() {
      live <- private$liveSet
      if (is.null(live)) return(invisible())
      changed <- list()
      for (name in names(live$inputs)) {
        if (name %in% live$fileInputs || name %in% private$updated) next
        if (private$fresh && name %in% names(private$recordInputs)) next
        if (!identical(live$inputs[[name]], private$recordInputs[[name]])) changed[name] <- list(live$inputs[[name]])
      }
      private$setInputs(changed, force = TRUE)
      changedCd <- list()
      for (name in names(live$clientData)) {
        if (!identical(live$clientData[[name]], private$recordClientData[[name]])) changedCd[name] <- list(live$clientData[[name]])
      }
      private$setClientData(changedCd, force = TRUE)
      private$outcomeValue$skipped <- private$skipped
      private$outcomeValue$reran <- private$reran
      private$leave()
      invisible()
    },
    skippingInits = function() identical(private$phase, "restoring"),
    restoring = function() private$phase %in% c("constructing", "restoring"),
    noteUpdated = function(names) {
      if (self$restoring()) private$updated <- union(private$updated, names)
      invisible()
    },
    noteInitSkipped = function(label) {
      private$skipped <- c(private$skipped, label)
      invisible()
    },
    noteInitRan = function(label, why) {
      private$reran[[length(private$reran) + 1L]] <- list(label = label, why = why)
      invisible()
    },
    outcome = function() private$outcomeValue
  ),

  private = list(
    session = NULL,
    entries = NULL,      # id -> list(node, kind, label, ns, apply)
    orphans = NULL,      # entries without an identity: list(..., reason, counted)
    retired = NULL,      # ids two or more sources shared; none of them gets it
    informed = NULL,

    # writer
    store = NULL, key = NULL, gate = NULL, debounceMs = 1000,
    dirty = FALSE, firstDirtyAt = NULL, changedSinceSchedule = FALSE,
    cancelWrite = NULL, writeFailureLogged = FALSE, wrote = FALSE, pausedFlag = FALSE,

    # resuming
    phase = "idle",      # "idle" | "constructing" (server function running) | "restoring" (verified; first flush)
    records = NULL, initials = NULL, applied = character(0), trips = list(),
    liveSet = NULL, recordInputs = NULL, recordClientData = NULL, fresh = FALSE,
    updated = character(0), skipped = character(0), reran = list(), outcomeValue = NULL,

    orphan = function(entry, reason, counted) {
      entry$reason <- reason
      entry$counted <- counted
      private$orphans[[length(private$orphans) + 1L]] <- entry
      invisible(NULL)
    },

    informNoLabel = function(entry) {
      self$devmodeInform(paste0("no-label:", entry$kind, ":", entry$ns), sprintf(
        "A %s() created in namespace '%s' has no label, so the session cannot resume it. Assign it to a name or give it a label (rule 1 of the rules of reactives).",
        entry$kind, entry$ns))
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
      for (name in ls(ud, all.names = TRUE)) {
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

    # A source without a usable record trips the gate. Trips are collected
    # and judged at verify: the server function may mark the source with
    # disableResume() after its constructor returned.
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
      if (ok) private$applied <- c(private$applied, id)
      invisible(ok)
    },

    revertOne = function(entry, id) {
      initial <- private$initials[[id]]
      if (!is.null(initial)) entry$node$.snapshot$revert(initial[[1]])
      private$applied <- setdiff(private$applied, id)
      invisible()
    },

    revertAll = function() {
      for (id in private$applied) {
        entry <- private$entries[[id]]
        if (!is.null(entry)) private$revertOne(entry, id)
      }
      private$applied <- character(0)
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
      private$initials <- NULL
      private$liveSet <- NULL
      private$recordInputs <- NULL
      private$recordClientData <- NULL
      private$updated <- character(0)
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
    apply = function(record) {
      vals <- unserialize(record$bytes)
      order <- record$order
      vals <- lapply(order, function(key) vals[[key]])   # read all before setting any
      for (i in seq_along(order)) impl$.values$set(order[[i]], vals[[i]])
      impl$.nameOrder <- unique(c(order, impl$.nameOrder))
    }
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
