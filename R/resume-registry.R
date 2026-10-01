#' @include utils.R
#' @include resume-serialize.R
#' @include resume-options.R
NULL

# Per-session registry behind resume snapshots. Hands out
# stable ids to sources and observers, tracks which node created which, and
# walks the graph into a snapshot and adopts one on resume.
SnapshotRegistry <- R6Class("SnapshotRegistry", cloneable = FALSE,
  public = list(
    initialize = function(session) {
      private$session <- session
      private$nodes <- Map$new()
      private$byReactId <- Map$new()
      private$counts <- Map$new()
      private$hashLabelled <- Map$new()
      private$pending <- fastmap::fastqueue()
      private$children <- Map$new()
      private$conductorHashes <- Map$new()
      private$pendingConductors <- fastmap::fastqueue()
      private$resumedCallbacks <- Callbacks$new()

      inputImpl <- .subset2(session$input, "impl")
      private$addNode(inputImpl, "input", "input", "input", "", "top")
      snapshot_track_values(inputImpl, self, "input")
      if (is.reactivevalues(session$clientData)) {
        cdImpl <- .subset2(session$clientData, "impl")
        private$addNode(cdImpl, "clientData", "clientData", "clientData", "", "top")
        snapshot_track_values(cdImpl, self, "clientData")
      }
    },

    # Sources are registered at construction and get their final id at once.
    # `initial` is the constructor's initial value (or the initial key list
    # for reactiveValues); its shape is recorded for the positional-identity
    # adoption check. When adopting and the snapshot has a record for this
    # source, `apply(record)` sets the node's value from it. The node already
    # carries its snapshot_node_state().
    registerSource = function(node, kind, label, labelSource = "explicit", domain = NULL, initial = NULL,
                              apply = function(record) NULL) {
      creator <- private$creatingNode()
      ns <- private$namespaceOf(domain)
      id <- private$makeId(ns, creator$id, kind, label)
      private$addNode(node, id, kind, label, ns, creator$id, creator$adoptable, creator$reason,
        labelSource = labelSource, shape = snapshot_shape(kind, initial))
      node$.snapshot$id <- id
      if (!creator$adoptable) {
        self$devmodeInform(
          paste0("inside-reactive:", id),
          sprintf(
            "`%s` is created inside the reactive() `%s`, so it is never restored when the session resumes. Create reactives at the top of a scope (rule 2 of the rules of reactives).",
            label, creator$label
          )
        )
      }
      if (identical(labelSource, "hash")) {
        private$nudgeAmbiguous(ns, creator$id, kind, label)
      }
      record <- private$recordFor(id, creator$adoptable)
      if (!is.null(record) && private$applySourceRecord(id, kind, record, apply)) {
        private$markAdopted(id)
        if (identical(labelSource, "hash")) private$adoptedInitials[id] <- list(initial)
      }
      invisible(id)
    },

    # Observers register at construction but are finalized later: their
    # identity label and hash parts are complete only after wrappers such as
    # bindEvent() have run.
    registerObserver = function(node, domain = NULL) {
      creator <- private$creatingNode()
      private$pending$add(list(
        node = node,
        ns = private$namespaceOf(domain),
        creator = creator
      ))
      invisible()
    },

    # Finalizes `node` and every pending observer constructed before it, in
    # construction order, so occurrence numbers agree between sessions. With
    # node = NULL, finalizes everything.
    finalizeNodes = function(node = NULL) {
      while (private$pending$size()) {
        entry <- private$pending$remove()
        private$finalizeEntry(entry)
        if (!is.null(node) && identical(entry$node, node)) break
      }
      invisible()
    },

    ids = function() {
      self$finalizeNodes()
      sort(private$nodes$keys())
    },

    idOf = function(node) {
      if (is.null(node$.snapshot$id)) self$finalizeNodes(node)
      node$.snapshot$id
    },

    nodeEntry = function(id) {
      self$finalizeNodes()
      private$nodes$get(id)
    },

    # Bodies are hashed only when hasConductorHashes() needs them, or in
    # batches once enough queue up, so a session that creates reactives
    # dynamically and never resumes holds one entry per distinct body.
    # `body` is a conductor body, or a snapshot_lazy() of a full conductor
    # hash (snapshot_set_conductor_parts()).
    noteConductor = function(body) {
      private$pendingConductors$add(body)
      if (private$pendingConductors$size() >= 1000L) private$hashPendingConductors()
      invisible()
    },

    # TRUE when a reactive() with each of these body hashes exists in this
    # session. A presence check over the session's conductor bodies, not a
    # check that those conductors sit on this observer's path: two
    # conductors with the same body are indistinguishable here, by design.
    hasConductorHashes = function(hashes) {
      private$hashPendingConductors()
      all(vapply(hashes, function(h) private$conductorHashes$containsKey(h), logical(1)))
    },

    # The snapshot for this session under `policy`: source
    # values, observer records, inputs, clientData, userData, and the report.
    snapshot = function(policy = snapshot_policy_reconnect()) {
      self$finalizeNodes()
      private$startWalk()
      on.exit(private$walkMemo <- NULL, add = TRUE)
      settings <- private$session$.resumeSettings
      maxNode <- settings$maxNodeSize
      maxTotal <- settings$maxSize
      nodes <- list()
      sizes <- numeric(0)
      rows <- list()
      addRow <- function(id, kind, label, ns, adoptable, reason, hazard = character(0)) {
        rows[[length(rows) + 1L]] <<- list(
          id = id, kind = kind, label = label, namespace = ns, adoptable = adoptable,
          reason = if (adoptable) NA_character_ else reason,
          hazard = if (length(hazard)) paste(hazard, collapse = ", ") else NA_character_
        )
      }

      dead <- character(0)
      for (id in private$nodes$keys()) {
        entry <- private$nodes$get(id)
        node <- private$entryNode(entry)
        if (is.null(node)) {
          dead <- c(dead, id)
          next
        }
        if (entry$kind %in% c("input", "clientData")) {
          addRow(id, entry$kind, entry$label, entry$ns, TRUE, NA_character_)
          next
        }
        if (!entry$kind %in% policy$kinds) next
        if (entry$kind %in% c("observer", "output")) {
          rec <- private$observerRecord(id, entry, node)
          addRow(id, entry$kind, entry$label, entry$ns, rec$adoptable, rec$reason, rec$hazard)
          # Every observer record is kept, adoptable or not: it carries no
          # value, and the adopting session needs the reason to log a useful
          # fallback instead of "not in the snapshot".
          nodes[[id]] <- rec
          next
        }
        if (private$sourceDestroyed(entry, node)) next
        res <- private$sourceRecord(id, entry, node, maxNode)
        addRow(id, entry$kind, entry$label, entry$ns, is.null(res$reason), res$reason)
        if (is.null(res$reason)) {
          nodes[[id]] <- res$record
          sizes[[id]] <- res$size
        }
      }

      private$pruneDead(dead)

      # Total cap: largest-first eviction.
      while (length(sizes) && sum(sizes) > maxTotal) {
        victim <- names(sizes)[which.max(sizes)]
        nodes[[victim]] <- NULL
        sizes <- sizes[names(sizes) != victim]
        for (i in seq_along(rows)) {
          if (identical(rows[[i]]$id, victim)) {
            rows[[i]]$adoptable <- FALSE
            rows[[i]]$reason <- "evicted under maxSize"
          }
        }
      }

      inputs <- private$inputValues(policy, addRow)
      userData <- if (isTRUE(policy$userData)) private$userDataValues(maxNode, addRow) else list()

      list(
        format = 1L,
        inputs = inputs$inputs,
        clientData = inputs$clientData,
        # Names of fileInput() values, so adopt can check their upload
        # directories still exist.
        fileInputs = inputs$fileInputs,
        userData = userData,
        nodes = nodes,
        report = snapshot_report_frame(rows)
      )
    },

    report = function() {
      self$snapshot()$report
    },

    # One record per live observer/output, without values.
    observerRecords = function() {
      self$finalizeNodes()
      private$startWalk()
      on.exit(private$walkMemo <- NULL, add = TRUE)
      out <- list()
      for (id in private$nodes$keys()) {
        entry <- private$nodes$get(id)
        if (!entry$kind %in% c("observer", "output")) next
        node <- private$entryNode(entry)
        if (is.null(node)) next
        out[[id]] <- private$observerRecord(id, entry, node)
      }
      out
    },

    inAdoptMode = function() private$adopting,

    pendingAdopt = function(node) {
      private$adoptQueue[[length(private$adoptQueue) + 1L]] <- node
      invisible()
    },

    observerAdopted = function(reactId) {
      id <- private$byReactId$get(reactId)
      if (is.null(id)) return(FALSE)
      node <- private$entryNode(private$nodes$get(id))
      !is.null(node) && isTRUE(node$.snapshot$adopted)
    },

    onResumed = function(callback) {
      private$resumedCallbacks$register(callback)
    },

    # list(adopted, fallbacks, hazards) once a resume finished, else NULL.
    adoptOutcome = function() private$outcome,

    # `store` is a store or a function returning one (or NULL when none can
    # be created). A function is called at the first write the gate allows,
    # so a process with the feature off never creates the store directory.
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

    # Removes the snapshot this session wrote, if any, and stops pending writes.
    deleteSnapshot = function() {
      private$cancelPendingWrite()
      if (private$wrote) {
        self$writerStore()$delete(private$key)
        private$wrote <- FALSE
      }
      invisible()
    },

    markDirty = function() {
      if (!private$dirty) private$firstDirtyAt <- private$session$.now()
      private$dirty <- TRUE
      private$changedSinceSchedule <- TRUE
      invisible()
    },

    # Called after every flush. The quiet interval runs from the last change,
    # not the last flush, so only a flush that followed a
    # change (re)schedules the write.
    scheduleWrite = function() {
      if (!private$changedSinceSchedule || private$writeFailed || !self$writerEnabled()) {
        return(invisible())
      }
      private$changedSinceSchedule <- FALSE
      private$cancelPendingWrite()
      # A trailing-edge debounce alone never fires while changes keep coming
      # (a timer-driven source, a streaming plot), and a hard kill then loses
      # everything since the last quiet moment. Cap the wait at ten quiet
      # intervals from the first unsaved change.
      waited <- private$session$.now() - private$firstDirtyAt
      delay <- max(0, min(private$debounceMs, 10 * private$debounceMs - waited))
      private$cancelWrite <- defineScheduler(private$session)(delay, function() {
        private$cancelWrite <- NULL
        self$writeNow()
      })
      invisible()
    },

    # Serializes and writes now if the gate allows; never throws, because
    # adopt() calls it before registering steps 4-6. Errors are logged once
    # per session; snapshotting is best-effort.
    writeNow = function() {
      # Cancel first: a pending timer closes over this registry and so pins
      # the session, even when the gate now refuses the write.
      private$cancelPendingWrite()
      ok <- tryCatch({
        if (!self$writerEnabled()) return(invisible(FALSE))
        snap <- self$snapshot()
        snap$identity <- snapshot_identity(private$session)
        private$store$write(private$key, snap)
        TRUE
      }, error = function(e) {
        if (!private$writeFailed) {
          private$writeFailed <- TRUE
          message("Saved state could not be written: ", conditionMessage(e))
        }
        private$dirty <- FALSE
        private$firstDirtyAt <- NULL
        FALSE
      })
      if (ok) {
        private$dirty <- FALSE
        private$firstDirtyAt <- NULL
        private$wrote <- TRUE
      }
      invisible(ok)
    },

    wroteSnapshot = function() private$wrote,
    storeKey = function() private$key,

    # `live` is snapshot_live_inputs()'s list after
    # input handlers were applied; `run` runs the server function.
    adopt = function(snapshot, live, run, info) {
      private$adopted <- character(0)
      private$fallbacks <- list()
      private$abandonReason <- NULL
      private$outcome <- NULL

      # An upload directory lives in the writing process's tempdir(), which
      # dies with that process. A non-NULL input$<file> pointing at it would
      # pass req() and then fail on read, which is worse than the NULL a
      # fresh start gives. Drop such values; their
      # dependents fall back through sourceAdopted().
      for (name in snapshot$fileInputs) {
        paths <- snapshot$inputs[[name]]$datapath
        if (is.null(paths) || !all(file.exists(paths))) {
          snapshot$inputs[[name]] <- NULL
          private$droppedUploads <- c(private$droppedUploads, name)
          private$addFallback(paste0("input$", name), "input", "upload directory no longer exists")
        }
      }

      private$records <- snapshot$nodes
      private$snapshotInputs <- snapshot$inputs
      private$lastSnapshotClientData <- snapshot$clientData
      private$liveSet <- live
      private$adopting <- TRUE
      # If anything below throws, leave adopt mode so pending observers run
      # and the session is not stuck; the error still propagates.
      completed <- FALSE
      on.exit(if (!completed) private$leaveAdoptMode(), add = TRUE)

      # Step 1: the snapshot's inputs first, then the client's live clientData.
      private$setInputs(snapshot$inputs, force = FALSE)
      private$setClientData(live$clientData, force = FALSE)
      snapshot_seed_restore_context(private$session, snapshot$inputs)

      # Step 2: server function in adopt mode.
      run()

      # Step 3: commit, userData, onResumed.
      private$commitOrAbandon()
      ud <- private$session$userData
      for (name in names(snapshot$userData)) assign(name, snapshot$userData[[name]], envir = ud)
      private$fireResumed(info)

      # The snapshot we adopted was deleted on read (single use). Until this
      # session writes its own, a second drop would lose the state, so write
      # now rather than after the debounce. A no-op when
      # no writer is configured (Mock) or the gate is off.
      self$writeNow()

      # Steps 4-6 after the first flush. ShinySession fires onFlushed only
      # once the busy count reaches zero, so observers created while it is
      # above zero (e.g. by a constructor observer awaiting a promise) stay
      # pending until then.
      private$session$onFlushed(function() self$finishAdopt(), once = TRUE)
      completed <- TRUE
      info
    },

    finishAdopt = function() {
      if (!private$adopting) return(invisible())
      private$commitOrAbandon()
      live <- private$liveSet
      changed <- list()
      for (name in names(live$inputs)) {
        if (name %in% live$fileInputs) next
        if (!identical(live$inputs[[name]], private$snapshotInputs[[name]])) changed[name] <- list(live$inputs[[name]])
      }
      private$setInputs(changed, force = TRUE)
      snapCd <- private$lastSnapshotClientData
      changedCd <- list()
      for (name in names(live$clientData)) {
        if (!identical(live$clientData[[name]], snapCd[[name]])) changedCd[name] <- list(live$clientData[[name]])
      }
      private$setClientData(changedCd, force = TRUE)
      private$outcome <- list(
        adopted = private$adopted,
        fallbacks = private$fallbackFrame(),
        hazards = private$adoptedHazards()
      )
      private$leaveAdoptMode()
      invisible()
    },

    # Rung 2: no snapshot; seed the
    # restore context from the replayed inputs and run as init does.
    resumeFromInputs = function(live, run, info) {
      private$adopted <- character(0)
      private$fallbacks <- list()
      snapshot_seed_restore_context(private$session, live$inputs)
      private$setInputs(live$inputs, force = FALSE)
      private$setClientData(live$clientData, force = FALSE)
      run()
      private$outcome <- list(adopted = character(0), fallbacks = private$fallbackFrame(), hazards = character(0))
      private$fireResumed(info)
      info
    },

    commit = function() {
      self$finalizeNodes()
      private$verifySiblingCounts()
      queue <- private$adoptQueue
      private$adoptQueue <- list()
      if (!is.null(private$abandonReason)) {
        # A failed commit abandons adoption for the rest of the resume
        # so later commits adopt nothing.
        for (node in queue) private$abandon(node, private$abandonReason)
        return(invisible())
      }
      for (node in queue) private$decideAndWire(node)
      invisible()
    },

    # devmode messages once per session; the process-wide
    # 8 h throttle in devmode_inform() is bypassed with .frequency = "always".
    devmodeInform = function(key, message) {
      if (key %in% private$informed) return(invisible())
      private$informed <- c(private$informed, key)
      devmode_inform(message, .frequency = "always")
      invisible()
    }
  ),

  private = list(
    session = NULL,
    nodes = NULL,        # id -> list(node = weakref, kind, label, labelSource, shape, ns, parent, adoptable, reason, adopted)
    byReactId = NULL,    # observer reactId -> id (finalized observers only)
    counts = NULL,       # occurrence counters per (ns, parent, kind, label)
    hashLabelled = NULL, # count of hash-labelled sources per (ns, parent, kind)
    pending = NULL,      # observers awaiting finalization, construction order
    children = NULL,     # presence set of ids that created other nodes
    informed = character(0),
    adopting = FALSE,    # between adopt() and finishAdopt()
    records = NULL,      # the adopted snapshot's node records, while adopting
    adoptQueue = list(), # observers constructed in adopt mode, awaiting commit
    adopted = character(0),
    fallbacks = list(),  # id -> reason a node was not adopted
    adoptedInitials = list(), # hash-labelled adopted source id -> its initial value (for a revert)
    resumedCallbacks = NULL,
    outcome = NULL,
    liveSet = NULL,
    snapshotInputs = NULL,
    lastSnapshotClientData = NULL,
    droppedUploads = character(0),
    abandonReason = NULL, # set once a commit failed; adoption is then abandoned

    carriedPathHashes = list(),
    conductorHashes = NULL,   # presence set of conductor body hashes
    pendingConductors = NULL, # queue of conductor bodies not yet hashed

    store = NULL,        # writer: NULL until configureWriter()
    key = NULL,
    gate = NULL,
    debounceMs = 1000,
    dirty = FALSE,
    changedSinceSchedule = FALSE,
    firstDirtyAt = NULL,
    cancelWrite = NULL,
    wrote = FALSE,
    writeFailed = FALSE,

    # Within one walk, each conductor's source set is computed once and
    # shared by every observer whose paths cross it; otherwise layered
    # diamonds are walked exponentially. Keyed by conductor reactId.
    walkMemo = NULL,
    startWalk = function() {
      private$walkMemo <- new.env(parent = emptyenv())
    },

    hashPendingConductors = function() {
      for (entry in private$pendingConductors$as_list()) {
        hash <- if (inherits(entry, "snapshot_lazy")) entry() else snapshot_hash_expr(entry)
        private$conductorHashes$set(hash, TRUE)
      }
      private$pendingConductors$reset()
    },

    cancelPendingWrite = function() {
      if (is.function(private$cancelWrite)) private$cancelWrite()
      private$cancelWrite <- NULL
    },

    # ReactiveVal keeps its destroyed flag private; ReactiveValues exposes it.
    sourceDestroyed = function(entry, node) {
      if (identical(entry$kind, "reactiveVal")) node$.snapshot$destroyed() else isTRUE(node$.destroyed)
    },

    sourceRecord = function(id, entry, node, maxNode) {
      if (!isTRUE(entry$adoptable)) return(list(reason = entry$reason))
      if (isTRUE(node$.snapshot$skip)) return(list(reason = "disableResume()"))
      if (identical(entry$kind, "reactiveVal")) {
        value <- node$.snapshot$value()
        path <- snapshot_unserializable_path(value)
        if (!is.null(path)) return(list(reason = paste0("value is not serializable: ", path)))
        # Serialized once, here; the record carries the bytes so the store
        # only copies them instead of serializing every value a second time.
        bytes <- serialize(value, NULL, xdr = FALSE)
        size <- length(bytes)
        if (size > maxNode) return(list(reason = sprintf("value exceeds maxNodeSize (%d bytes)", size)))
        return(list(record = c(private$identityRecord(entry), list(hasValue = TRUE, bytes = bytes)), size = size))
      }
      order <- node$.nameOrder
      values <- node$.values$mget(order)
      for (key in order) {
        path <- snapshot_unserializable_path(values[[key]], paste0("value$", key))
        if (!is.null(path)) return(list(reason = paste0("value is not serializable: ", path)))
      }
      bytes <- serialize(values, NULL, xdr = FALSE)
      size <- length(bytes)
      if (size > maxNode) return(list(reason = sprintf("value exceeds maxNodeSize (%d bytes)", size)))
      list(record = c(private$identityRecord(entry), list(hasValue = TRUE, bytes = bytes, order = order)), size = size)
    },

    # Identity fields shared by every source record. `siblings` and `shape`
    # guard positional identity at adopt.
    identityRecord = function(entry) {
      list(
        kind = entry$kind, label = entry$label, ns = entry$ns,
        labelSource = entry$labelSource,
        siblings = private$counts$get(paste(entry$ns, entry$parent, entry$kind, entry$label, sep = "\r")),
        shape = entry$shape
      )
    },

    inputValues = function(policy, addRow) {
      impl <- private$entryNode(private$nodes$get("input"))
      inputs <- list()
      fileInputs <- character(0)
      for (name in impl$.nameOrder) {
        serializer <- impl$getMeta(name, "shiny.serializer")
        if (!isTRUE(policy$keepInput(name, serializer))) {
          addRow(paste0("input$", name), "input", name, "", FALSE, "password input; restored from the client")
          next
        }
        inputs[name] <- list(impl$.values$get(name))
        if (identical(serializer, serializerFileInput)) fileInputs <- c(fileInputs, name)
      }
      clientData <- list()
      cdEntry <- private$nodes$get("clientData")
      if (!is.null(cdEntry)) {
        cd <- private$entryNode(cdEntry)
        clientData <- cd$.values$mget(cd$.nameOrder)
      }
      list(inputs = inputs, clientData = clientData, fileInputs = fileInputs)
    },

    userDataValues = function(maxNode, addRow) {
      ud <- private$session$userData
      out <- list()
      for (name in ls(ud, all.names = TRUE)) {
        value <- get(name, envir = ud, inherits = FALSE)
        path <- snapshot_unserializable_path(value)
        if (!is.null(path)) {
          addRow(paste0("userData$", name), "userData", name, "", FALSE, paste0("value is not serializable: ", path))
          next
        }
        size <- snapshot_serialized_size(value)
        if (size > maxNode) {
          addRow(paste0("userData$", name), "userData", name, "", FALSE,
            sprintf("value exceeds maxNodeSize (%d bytes)", size))
          next
        }
        out[name] <- list(value)
      }
      out
    },

    observerRecord = function(id, entry, node) {
      ctx <- node$.ctx
      current <- !is.null(ctx) && !ctx$.invalidated
      deps <- if (current) private$sourcesOf(ctx) else list(sources = list(), pathHashes = character(0), adoptable = TRUE, reason = NA_character_, settled = TRUE)
      # A run that returned a promise is still collecting edges until it
      # settles, on the observer or on a conductor on its paths. Recording it
      # as not up to date makes adopt invalidate it, so it runs once with
      # complete dependencies rather than being wired to a partial set.
      upToDate <- current && isTRUE(ctx$.snapshot$settled) && isTRUE(deps$settled)
      if (!is.null(ctx) && isTRUE(ctx$.snapshot$adopted)) {
        deps$pathHashes <- c(deps$pathHashes, private$carriedPathHashes[[id]])
      }
      adoptable <- isTRUE(entry$adoptable) && deps$adoptable && !isTRUE(node$.snapshot$skip)
      reason <- if (isTRUE(node$.snapshot$skip)) "disableResume()" else if (!isTRUE(entry$adoptable)) entry$reason else deps$reason
      # Ever created a node, even one since garbage-collected (pruneDead()
      # keeps the key).
      createsNodes <- private$children$containsKey(id)
      if (adoptable && createsNodes) {
        adoptable <- FALSE
        reason <- "creates other reactives, so it re-runs on resume"
      }
      list(
        kind = entry$kind, label = entry$label, ns = entry$ns,
        bodyHash = rlang::hash(snapshot_observer_parts(node)),
        pathHashes = sort(unique(deps$pathHashes)),
        sources = deps$sources,
        upToDate = upToDate,
        destroyed = isTRUE(node$.destroyed),
        suspended = if (identical(entry$kind, "observer")) isTRUE(node$.suspended) else NA,
        createsNodes = createsNodes,
        hazard = snapshot_observer_hazards(node),
        adoptable = adoptable,
        reason = if (adoptable) NA_character_ else reason
      )
    },

    # Walks a context's recorded edges: sources are collected (with access
    # kind), conductors are traversed and their body hashes collected. Any
    # non-graph or out-of-session dependency makes the whole set unusable.
    sourcesOf = function(ctx, visited = character(0)) {
      out <- list(sources = list(), pathHashes = character(0), adoptable = TRUE, reason = NA_character_, settled = TRUE)
      state <- ctx$.snapshot
      if (is.null(state) || !isTRUE(state$adoptable)) {
        out$adoptable <- FALSE
        out$reason <- if (is.null(state)) "not tracked" else state$reason
        return(out)
      }
      for (dep in state$upstream) {
        so <- dep$.snapshotOwner
        owner <- rlang::wref_key(so$ref)
        if (is.null(owner)) next
        if (inherits(owner, "Observable")) {
          if (owner$.reactId %in% visited) next
          cctx <- owner$.ctx
          if (is.null(cctx)) {
            out$adoptable <- FALSE
            out$reason <- "depends on a reactive() that was invalidated when the snapshot was taken"
            return(out)
          }
          out$pathHashes <- c(out$pathHashes, snapshot_body_hash(owner))
          if (!isTRUE(cctx$.snapshot$settled)) out$settled <- FALSE
          inner <- private$walkMemo[[owner$.reactId]]
          if (is.null(inner)) {
            inner <- private$sourcesOf(cctx, c(visited, owner$.reactId))
            if (!is.null(private$walkMemo)) private$walkMemo[[owner$.reactId]] <- inner
          }
          if (!inner$adoptable) return(inner)
          out$settled <- out$settled && isTRUE(inner$settled)
          out$sources <- c(out$sources, inner$sources)
          out$pathHashes <- c(out$pathHashes, inner$pathHashes)
        } else {
          sid <- owner$.snapshot$id
          if (is.null(sid)) {
            out$adoptable <- FALSE
            out$reason <- "depends on a reactive outside the session"
            return(out)
          }
          out$sources[[length(out$sources) + 1L]] <- list(id = sid, part = so$part, key = so$key)
        }
      }
      out$sources <- unique(out$sources)
      out$pathHashes <- unique(out$pathHashes)
      out
    },

    addNode = function(node, id, kind, label, ns, parent, adoptable = TRUE, reason = NA_character_,
                       labelSource = "explicit", shape = NULL) {
      private$nodes$set(id, list(
        node = rlang::new_weakref(node),
        kind = kind, label = label, labelSource = labelSource, shape = shape,
        ns = ns, parent = parent,
        adoptable = adoptable, reason = reason, adopted = FALSE
      ))
      # The ids that created other nodes, so the walk's creates-nodes check is
      # O(1) per observer instead of a scan of every node.
      private$children$set(parent, TRUE)
    },

    entryNode = function(entry) rlang::wref_key(entry$node),

    # Drops the entries of garbage-collected nodes, so sessions that keep
    # creating and dropping observers do not grow these maps and every
    # snapshot does not walk dead entries. `counts` is kept, so an
    # occurrence number is never reused within a session; a live parent keeps
    # its `children` key, so it still counts as creating other reactives.
    pruneDead = function(dead) {
      if (!length(dead)) return(invisible())
      for (id in dead) private$nodes$remove(id)
      for (rid in private$byReactId$keys()) {
        if (private$byReactId$get(rid) %in% dead) private$byReactId$remove(rid)
      }
      for (id in dead) private$children$remove(id)
      invisible()
    },

    namespaceOf = function(domain) {
      if (is.null(domain)) "" else domain$ns("")
    },

    makeId = function(ns, parent, kind, label) {
      key <- paste(ns, parent, kind, label, sep = "\r")
      n <- (private$counts$get(key) %||% 0L) + 1L
      private$counts$set(key, n)
      sprintf("%s|%s|%s:%s#%d", ns, parent, kind, label, n)
    },

    # The observer whose body is running, "top" for the server function and
    # module bodies (and isolate() at top level), or a not-adoptable marker
    # for a reactive() body.
    creatingNode = function() {
      top <- list(id = "top", adoptable = TRUE, reason = NA_character_, label = "top")
      if (!hasCurrentContext()) return(top)
      ctx <- getCurrentContext()
      reactId <- ctx$.reactId
      if (!is.character(reactId) || length(reactId) != 1 ||
          identical(reactId, rLog$noReactId) || identical(reactId, rLog$dummyReactId)) {
        return(top)
      }
      id <- private$byReactId$get(reactId)
      if (!is.null(id)) {
        return(list(id = id, adoptable = TRUE, reason = NA_character_, label = id))
      }
      for (entry in private$pending$as_list()) {
        if (identical(entry$node$.reactId, reactId)) {
          self$finalizeNodes(entry$node)
          return(list(id = entry$node$.snapshot$id, adoptable = TRUE, reason = NA_character_, label = entry$node$.snapshot$id))
        }
      }
      list(
        id = paste0("context:", reactId), adoptable = FALSE,
        reason = "created inside a reactive()", label = ctx$.label
      )
    },

    finalizeEntry = function(entry) {
      node <- entry$node
      label <- node$.snapshot$label %||% rlang::hash(snapshot_observer_parts(node))
      id <- private$makeId(entry$ns, entry$creator$id, node$.snapshot$kind, label)
      private$addNode(node, id, node$.snapshot$kind, label, entry$ns, entry$creator$id,
        entry$creator$adoptable, entry$creator$reason)
      node$.snapshot$id <- id
      private$byReactId$set(node$.reactId, id)
    },

    recordFor = function(id, adoptable) {
      if (!private$adopting || !isTRUE(adoptable)) return(NULL)
      rec <- private$records[[id]]
      if (is.null(rec)) return(NULL)
      entry <- private$nodes$get(id)
      # Positional identity: a hash-labelled source adopts only when its
      # initial value has the shape the snapshot recorded. The sibling count
      # is checked at commit, once every sibling exists.
      if (identical(entry$labelSource, "hash") && !identical(rec$shape, entry$shape)) {
        private$addFallback(id, entry$kind, "positional identity: initial value shape differs")
        return(NULL)
      }
      rec
    },

    # A record that cannot be applied (corrupt bytes, a class whose package
    # is gone) must not raise into the user's constructor call: the source keeps its initial value and is not marked adopted, so
    # its dependents fall back through sourceAdopted().
    applySourceRecord = function(id, kind, record, apply) {
      tryCatch({
        apply(record)
        TRUE
      }, error = function(e) {
        private$addFallback(id, kind, paste0("adoption failed: ", conditionMessage(e)))
        FALSE
      })
    },

    markAdopted = function(id) {
      entry <- private$nodes$get(id)
      entry$adopted <- TRUE
      private$nodes$set(id, entry)
    },

    # Positional identity: every hash-labelled group that adopted must
    # have the occurrence count the snapshot recorded, otherwise `#n` may name
    # a different variable. On a mismatch the whole group reverts to its
    # initial values and is unmarked, so dependents fall back through the
    # consistency invariant in decideAndWire(). Runs at every commit: groups
    # under `top` are complete at the first, groups under a constructor
    # observer at the second.
    verifySiblingCounts = function() {
      for (id in names(private$adoptedInitials)) {
        entry <- private$nodes$get(id)
        if (is.null(entry)) {
          private$adoptedInitials[id] <- NULL
          next
        }
        rec <- private$records[[id]]
        key <- paste(entry$ns, entry$parent, entry$kind, entry$label, sep = "\r")
        now <- private$counts$get(key)
        if (identical(as.integer(now), as.integer(rec$siblings))) next
        node <- private$entryNode(entry)
        if (!is.null(node)) node$.snapshot$revert(private$adoptedInitials[[id]])
        entry$adopted <- FALSE
        private$nodes$set(id, entry)
        private$addFallback(id, entry$kind, sprintf(
          "positional identity: sibling count differs (snapshot %d, now %d)", rec$siblings, now
        ))
        private$adoptedInitials[id] <- NULL
      }
      invisible()
    },

    commitOrAbandon = function() {
      queue <- private$adoptQueue
      tryCatch(self$commit(), error = function(e) {
        # If commit throws, abandon adoption; every observer runs, including those already adopted. Sources keep their values.
        reason <- paste0("commit failed: ", conditionMessage(e))
        private$abandonReason <- reason
        adopted <- lapply(private$adopted, function(id) private$entryNode(private$nodes$get(id)))
        for (node in c(queue, Filter(Negate(is.null), adopted))) private$abandon(node, reason)
        private$adoptQueue <- list()
      })
    },

    abandon = function(node, reason) {
      if (isTRUE(node$.destroyed)) return(invisible())
      id <- node$.snapshot$id
      private$adopted <- setdiff(private$adopted, id)
      node$.snapshot$adopted <- FALSE
      ctx <- node$.ctx
      if (!is.null(ctx)) {
        if (!is.null(ctx$.snapshot)) ctx$.snapshot$adopted <- FALSE
        ctx$invalidate()
      }
      if (!is.null(id) && is.null(private$fallbacks[[id]])) private$addFallback(id, node$.snapshot$kind, reason)
      invisible()
    },

    leaveAdoptMode = function() {
      for (node in private$adoptQueue) if (!is.null(node$.ctx)) node$.ctx$invalidate()
      private$adopting <- FALSE
      private$records <- NULL
      private$adoptQueue <- list()
      private$adoptedInitials <- list()
      private$liveSet <- NULL
      private$snapshotInputs <- NULL
      private$lastSnapshotClientData <- NULL
      private$droppedUploads <- character(0)
      invisible()
    },

    addFallback = function(id, kind, reason) {
      private$fallbacks[[id]] <- list(kind = kind, reason = reason)
    },

    fireResumed = function(info) {
      withReactiveDomain(private$session, isolate(private$resumedCallbacks$invoke(info, onError = printError)))
    },

    setInputs = function(values, force) {
      if (!length(values)) return(invisible())
      impl <- private$entryNode(private$nodes$get("input"))
      for (name in names(values)) impl$set(name, values[[name]], force = force)
      invisible()
    },

    setClientData = function(values, force) {
      entry <- private$nodes$get("clientData")
      if (is.null(entry) || !length(values)) return(invisible())
      impl <- private$entryNode(entry)
      for (name in names(values)) impl$set(name, values[[name]], force = force)
      if (is.function(private$session$manageHiddenOutputs)) private$session$manageHiddenOutputs()
      invisible()
    },

    # If deciding or adopting one node throws, that node is treated as a
    # mismatch and runs.
    decideAndWire = function(node) {
      id <- node$.snapshot$id
      ctx <- node$.ctx
      reason <- tryCatch(
        private$adoptNode(node, id, ctx),
        error = function(e) paste0("adoption failed: ", conditionMessage(e))
      )
      if (is.null(reason)) return(invisible())
      private$addFallback(id, node$.snapshot$kind, reason)
      if (!is.null(ctx)) ctx$invalidate()
      invisible()
    },

    # Adopts `node` and returns NULL, or returns why it cannot be adopted.
    adoptNode = function(node, id, ctx) {
      rec <- private$records[[id]]
      if (isTRUE(node$.snapshot$skip)) return("disableResume()")
      if (is.null(rec)) return("not in the snapshot")
      if (!identical(rec$kind, node$.snapshot$kind)) return("kind changed")
      if (isTRUE(rec$destroyed)) {
        node$.snapshot$adopted <- TRUE
        node$destroy()
        private$adopted <- c(private$adopted, id)
        return(NULL)
      }
      if (!isTRUE(rec$adoptable)) return(rec$reason)
      if (!identical(rec$bodyHash, rlang::hash(snapshot_observer_parts(node)))) return("body changed")
      if (!self$hasConductorHashes(rec$pathHashes)) return("a reactive() on its dependency path changed")
      if (is.null(ctx)) return("not in the snapshot")
      for (src in rec$sources) {
        if (!private$sourceAdopted(src)) return("depends on a source that was not restored")
      }
      for (src in rec$sources) {
        owner <- private$entryNode(private$nodes$get(src$id))
        owner$.snapshot$wire(ctx, src$part, src$key)
      }
      if (identical(rec$kind, "observer")) {
        if (isTRUE(rec$suspended)) node$suspend() else node$resume()
      }
      ctx$.snapshot$adopted <- TRUE
      node$.snapshot$adopted <- TRUE
      private$carriedPathHashes[[id]] <- rec$pathHashes
      private$adopted <- c(private$adopted, id)
      if (!isTRUE(rec$upToDate)) ctx$invalidate()
      NULL
    },

    sourceAdopted = function(src) {
      if (identical(src$id, "input")) {
        # Every input key holds its snapshot value after step 1, or is NULL
        # in both sessions, or only the client has it and step 5 applies it,
        # invalidating this observer. The exception is
        # an upload adopt() dropped: its value changed and step 5 never
        # applies a fileInput.
        return(is.null(src$key) || !(src$key %in% private$droppedUploads))
      }
      if (identical(src$id, "clientData")) return(!is.null(private$nodes$get("clientData")))
      entry <- private$nodes$get(src$id)
      !is.null(entry) && isTRUE(entry$adopted) && !is.null(private$entryNode(entry))
    },

    fallbackFrame = function() {
      data.frame(
        id = as.character(names(private$fallbacks)),
        kind = vapply(private$fallbacks, `[[`, character(1), "kind", USE.NAMES = FALSE),
        reason = vapply(private$fallbacks, `[[`, character(1), "reason", USE.NAMES = FALSE),
        stringsAsFactors = FALSE
      )
    },

    adoptedHazards = function() {
      Filter(function(id) {
        rec <- private$records[[id]]
        !is.null(rec) && length(rec$hazard) > 0
      }, private$adopted)
    },

    nudgeAmbiguous = function(ns, parent, kind, label) {
      key <- paste(ns, parent, kind, sep = "\r")
      n <- (private$hashLabelled$get(key) %||% 0L) + 1L
      private$hashLabelled$set(key, n)
      if (n == 2L) {
        self$devmodeInform(
          paste0("ambiguous-source:", key),
          sprintf(
            "Two or more %s() calls in the same scope have neither a source reference nor a `label`, so resume tells them apart by position only. Give each a `label`%s (rule 4 of the rules of reactives).",
            kind,
            # An unassigned ExtendedTask holds its state in a reactiveVal()
            # and has no `label` argument.
            if (identical(kind, "reactiveVal")) "; if they come from ExtendedTask$new(), assign each ExtendedTask to a distinct name" else ""
          )
        )
      }
    }
  )
)

# The registry for a reactive domain, or NULL when there is no domain or it has
# no registry. Every reactive constructor calls this, so it is a plain field
# read on the root session; a module's session proxy is walked up directly
# rather than through `$.session_proxy`. The read never dispatches, so a
# custom domain whose `$` rejects unknown names (a Reference Class object, say)
# cannot make reactive constructors error; it, a domain that is not an
# environment, and one whose field is not a registry all have no registry.
snapshot_registry <- function(domain) {
  while (inherits(domain, "session_proxy")) domain <- .subset2(domain, "parent")
  if (!is.environment(domain)) return(NULL)
  reg <- .subset2(domain, ".snapshotRegistry")
  if (!is.null(reg) && inherits(reg, "SnapshotRegistry")) reg
}

# The `.snapshot` state of a tracked node: an environment, so the registry
# and the node update it in place. `registry` is the session's registry,
# `id` the node's identity once assigned, `skip` set by disableResume(); `...`
# adds per-class fields and functions (see the `.snapshot` field of each
# class). Untracked nodes keep `.snapshot = NULL` and pay for none of this.
snapshot_node_state <- function(registry, ...) {
  state <- list2env(list(...), parent = emptyenv())
  state$registry <- registry
  state$id <- NULL
  state$skip <- FALSE
  state
}

# A Dependents' owner: the tracked reactive whose readers it holds, and which
# part of it ("value" for ReactiveVal and Observable; for ReactiveValues
# "key" with `key`, "names", "asList" or "asListAll"). Held weakly: a
# recording context keeps the Dependents alive through its upstream list, and
# a strong owner would keep every upstream reactive alive as long as its
# readers.
snapshot_owner <- function(node, part = "value", key = NULL) {
  list(ref = rlang::new_weakref(node), part = part, key = key)
}

# Starts tracking a ReactiveVal: its snapshot state reaches the private value
# and Dependents. Built here rather than in ReactiveVal$initialize() so the
# closures do not keep the constructor's frame (and its initial value) alive.
snapshot_track_val <- function(impl, registry) {
  private <- impl$.__enclos_env__$private
  private$dependents$.snapshotOwner <- snapshot_owner(impl)
  impl$.snapshot <- snapshot_node_state(registry,
    value = function() private$value,
    destroyed = function() isTRUE(private$.destroyed),
    # A reactiveVal has one part, so `part` and `key` are ignored.
    wire = function(ctx, part, key) private$dependents$register(ctx = ctx),
    revert = function(initial) {
      private$value <- initial
      # A constructor observer may have read the adopted value during the
      # first flush; it must see the revert.
      private$dependents$invalidate()
    }
  )
  invisible(impl$.snapshot)
}

# Starts tracking a ReactiveValues (a reactiveValues() source, or a session's
# input and clientData): gives it its snapshot state and its existing
# Dependents their owners. Key Dependents created later get theirs on
# creation (ReactiveValues$get()).
snapshot_track_values <- function(impl, registry, id = NULL) {
  state <- snapshot_node_state(registry,
    wire = function(ctx, part, key) snapshot_values_wire(impl, ctx, part, key),
    revert = function(initial) snapshot_values_revert(impl, initial)
  )
  state$id <- id
  impl$.snapshot <- state
  impl$.namesDeps$.snapshotOwner <- snapshot_owner(impl, "names")
  impl$.valuesDeps$.snapshotOwner <- snapshot_owner(impl, "asList")
  impl$.allValuesDeps$.snapshotOwner <- snapshot_owner(impl, "asListAll")
  for (key in impl$.dependents$keys()) {
    deps <- impl$.dependents$get(key)
    deps$.snapshotOwner <- snapshot_owner(impl, "key", key)
  }
  invisible(state)
}

# Registers a reactiveValues() source. After construction, because
# reactiveValues() only learns the assignment-derived label once the object
# exists.
snapshot_register_values <- function(impl, registry, identity, domain, initial) {
  snapshot_track_values(impl, registry)
  registry$registerSource(impl, "reactiveValues", identity$label, identity$source, domain, initial = initial,
    apply = function(record) {
      vals <- unserialize(record$bytes)
      # Read every value before setting any, so a bad record changes nothing.
      order <- record$order
      vals <- lapply(order, function(key) vals[[key]])
      for (i in seq_along(order)) impl$.values$set(order[[i]], vals[[i]])
      impl$.nameOrder <- unique(c(order, impl$.nameOrder))
    }
  )
  invisible()
}

# Sentinel edge: make `ctx` a dependent of one part
# of a ReactiveValues without a read happening.
snapshot_values_wire <- function(impl, ctx, part, key = NULL) {
  switch(part,
    key = {
      # As ReactiveValues$get() does on a key's first read.
      if (!impl$.dependents$containsKey(key)) {
        rLog$defineKey(impl$.reactId, impl$.values$get(key), key, impl$.label, ctx$.domain)
        deps <- Dependents$new(rLog$keyIdStr(impl$.reactId, key))
        deps$.snapshotOwner <- snapshot_owner(impl, "key", key)
        impl$.dependents$set(key, deps)
      }
      impl$.dependents$get(key)$register(ctx = ctx)
    },
    names = {
      impl$.hasRetrieved$names <- TRUE
      impl$.namesDeps$register(ctx = ctx)
    },
    asList = {
      impl$.hasRetrieved$asList <- TRUE
      impl$.valuesDeps$register(ctx = ctx)
    },
    asListAll = {
      impl$.hasRetrieved$asListAll <- TRUE
      impl$.allValuesDeps$register(ctx = ctx)
    },
    stop("unknown reactiveValues part: ", part)
  )
  invisible()
}

# Undoes an adoption at commit: back to the constructor's initial values.
snapshot_values_revert <- function(impl, initial) {
  for (key in impl$.nameOrder) impl$.values$remove(key)
  impl$.nameOrder <- character(0)
  for (key in base::names(initial)) {
    impl$.values$set(key, initial[[key]])
    impl$.nameOrder[length(impl$.nameOrder) + 1] <- key
  }
  # A constructor observer may have read the adopted values during the first
  # flush; it must see the revert.
  for (deps in impl$.dependents$values()) deps$invalidate()
  impl$.namesDeps$invalidate()
  impl$.valuesDeps$invalidate()
  impl$.allValuesDeps$invalidate()
  invisible()
}

# A tracked reactive()'s path hash, computed on first use and memoised: the
# snapshot walk reads it for every observer whose path crosses this
# conductor, and most sessions never snapshot.
snapshot_body_hash <- function(observable) {
  state <- observable$.snapshot
  if (is.null(state$bodyHash)) {
    state$bodyHash <- snapshot_conductor_hash(fn_body(observable$.origFunc), state$hashParts)
  }
  state$bodyHash
}

# A tracked observer's hash parts and hazards, forced once and memoised.
snapshot_observer_parts <- function(node) {
  state <- node$.snapshot
  state$hashParts <- lapply(state$hashParts, snapshot_force)
  state$hashParts
}

snapshot_observer_hazards <- function(node) {
  state <- node$.snapshot
  state$hazard <- snapshot_force(state$hazard)
  state$hazard
}

# session$resumeReport() and session$onResumed() for ShinySession and
# MockShinySession. A session without a registry restores nothing, so its
# report is empty. It has resume off and never resumes (server.R answers a
# `resume` as `init`), so its callbacks would never fire.
snapshot_session_report <- function(session) {
  reg <- session$.snapshotRegistry
  if (is.null(reg)) snapshot_report_frame(list()) else reg$report()
}

snapshot_session_on_resumed <- function(session, callback) {
  if (!is.function(callback)) stop("`callback` must be a function taking one argument, `info`")
  reg <- session$.snapshotRegistry
  if (is.null(reg)) return(function() invisible(NULL))
  reg$onResumed(callback)
}

snapshot_report_frame <- function(rows) {
  df <- data.frame(
    id = vapply(rows, `[[`, character(1), "id"),
    kind = vapply(rows, `[[`, character(1), "kind"),
    label = vapply(rows, `[[`, character(1), "label"),
    namespace = vapply(rows, `[[`, character(1), "namespace"),
    adoptable = vapply(rows, `[[`, logical(1), "adoptable"),
    reason = vapply(rows, `[[`, character(1), "reason"),
    hazard = vapply(rows, `[[`, character(1), "hazard"),
    stringsAsFactors = FALSE
  )
  class(df) <- c("shiny_resume_report", "data.frame")
  df
}

# Called where a session hands out a URL that embeds its token (downloads,
# data objects). An adopted node would keep showing the old session's URL,
# which 404s, so whatever is running must re-run on resume.
snapshot_uses_session_url <- function() {
  if (hasCurrentContext()) {
    context_not_adoptable(getCurrentContext(), "uses a session URL (download or data object)")
  }
  invisible()
}

# TRUE inside the run of an output/observer that adopted its snapshot record.
snapshot_current_observer_adopted <- function() {
  if (!hasCurrentContext()) return(FALSE)
  reg <- snapshot_registry(getDefaultReactiveDomain())
  if (is.null(reg)) return(FALSE)
  reg$observerAdopted(getCurrentContext()$.reactId)
}

#' @export
print.shiny_resume_report <- function(x, ...) {
  cat(sprintf("<resumeReport> %d of %d nodes adoptable\n", sum(x$adoptable), nrow(x)))
  bad <- x[!x$adoptable, , drop = FALSE]
  for (reason in unique(bad$reason)) {
    cat("\n", reason, ":\n", sep = "")
    sel <- bad$reason == reason
    cat(paste0("  ", bad$kind[sel], " ", bad$label[sel], collapse = "\n"), "\n", sep = "")
  }
  hz <- x[!is.na(x$hazard), , drop = FALSE]
  if (nrow(hz)) {
    cat("\nAssign to closure variables (hazard):\n")
    cat(paste0("  ", hz$label, ": ", hz$hazard, collapse = "\n"), "\n", sep = "")
  }
  invisible(x)
}
