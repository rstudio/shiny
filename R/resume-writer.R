# The debounced record writer of one session. It owns the store, the key, the
# gate, the dirty flags and the timer; the registry builds the record.
SnapshotWriter <- R6Class("SnapshotWriter", cloneable = FALSE,
  public = list(
    initialize = function(session, store, key, gate, debounceMs) {
      private$session <- session
      private$storeField <- store
      private$keyField <- key
      private$gate <- gate
      private$debounceMs <- debounceMs
    },

    enabled = function() {
      !is.null(private$storeField) && isTRUE(private$gate()) && !is.null(self$store())
    },

    # The store, resolving it if needed; NULL when unconfigured or unavailable.
    store = function() {
      if (is.function(private$storeField)) private$storeField <- private$storeField()
      private$storeField
    },

    markDirty = function() {
      if (!private$dirty) private$firstDirtyAt <- private$session$.now()
      private$dirty <- TRUE
      private$changedSinceSchedule <- TRUE
      invisible()
    },

    # Called after every flush. Only a flush that followed a change
    # (re)schedules the write; the wait is capped at ten quiet intervals from
    # the first unsaved change so a session that never goes quiet still writes.
    # `record` is a zero-argument function returning the record to write.
    schedule = function(record) {
      if (!private$changedSinceSchedule || !self$enabled()) return(invisible())
      private$changedSinceSchedule <- FALSE
      self$cancel()
      waited <- private$session$.now() - private$firstDirtyAt
      delay <- max(0, min(private$debounceMs, 10 * private$debounceMs - waited))
      private$cancelWrite <- defineScheduler(private$session)(delay, function() {
        private$cancelWrite <- NULL
        self$writeNow(record)
      })
      invisible()
    },

    # Builds and writes the record now if the gate allows; never throws. A
    # failure is logged once per session, and later writes are still attempted.
    writeNow = function(record) {
      self$cancel()
      ok <- tryCatch({
        if (!self$enabled()) return(invisible(FALSE))
        store <- self$store()
        key <- private$keyField
        # write() would refuse it too, but only after the uploads were
        # copied back into the files directory claim() deleted.
        if (store$claimed(key)) return(invisible(FALSE))
        rec <- record()
        uploads <- rec$uploads
        if (length(uploads)) rec <- snapshot_write_uploads(rec, store$filesDir(key))
        rec$uploads <- NULL
        written <- !identical(store$write(key, rec), FALSE)
        if (written) {
          snapshot_prune_uploads(store$filesDir(key), as.character(unlist(uploads, use.names = FALSE)))
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
      if (ok) private$wroteFlag <- TRUE
      invisible(ok)
    },

    # Removes the record this session wrote, if any, and stops pending writes.
    delete = function() {
      self$cancel()
      if (private$wroteFlag) {
        self$store()$delete(private$keyField)
        private$wroteFlag <- FALSE
      }
      invisible()
    },

    wrote = function() private$wroteFlag,

    cancel = function() {
      if (is.function(private$cancelWrite)) private$cancelWrite()
      private$cancelWrite <- NULL
      invisible()
    }
  ),
  private = list(
    session = NULL, storeField = NULL, keyField = NULL, gate = NULL, debounceMs = 1000,
    dirty = FALSE, firstDirtyAt = NULL, changedSinceSchedule = FALSE,
    cancelWrite = NULL, writeFailureLogged = FALSE, wroteFlag = FALSE
  )
)
