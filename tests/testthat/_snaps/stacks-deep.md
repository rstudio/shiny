# deep stack capturing

    Code
      cat(sep = "\n", formatError(err))
    Output
      Error in onFinally: boom
          : stop
          : onFinally [test-stacks-deep.R#XXX]
          : onFulfilled
          : callback
          : <Anonymous>
          : onFulfilled
          : handleFulfill
          : <Anonymous>
          : execCallbacks
          : later::run_now
          : wait_for_it
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : test_that
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : source_file
          : FUN
          : lapply
          : test_files_serial
          : test_files
      From earlier call:
          : domain$wrapOnFulfilled
          : promiseDomain$onThen
          : action
          : promise
          : self$then
          : promise$finally
          : finally
          : onRejected [test-stacks-deep.R#XXX]
          : callback
          : <Anonymous>
          : onRejected
          : handleReject
          : <Anonymous>
          : execCallbacks
          : later::run_now
          : wait_for_it
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : test_that
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : source_file
          : FUN
          : lapply
          : test_files_serial
          : test_files
      From earlier call:
          : domain$wrapOnRejected
          : promiseDomain$onThen
          : action
          : promise
          : self$then
          : promise$catch
          : catch
          : %...!%
          : onFulfilled [test-stacks-deep.R#XXX]
          : callback
          : <Anonymous>
          : onFulfilled
          : handleFulfill
          : <Anonymous>
          : execCallbacks
          : later::run_now
          : wait_for_it
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : test_that
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : source_file
          : FUN
          : lapply
          : test_files_serial
          : test_files
      From earlier call:
          : domain$wrapOnFulfilled
          : promiseDomain$onThen
          : action
          : promise
          : promise$then
          : then
          : %...>%
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : test_that
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : source_file
          : FUN
          : lapply
          : test_files_serial
          : test_files

---

    Code
      cat(sep = "\n", formatError(err, full = TRUE))
    Output
      Error in onFinally: boom
          : h
          : .handleSimpleError
          : stop
          : onFinally [test-stacks-deep.R#XXX]
          : onFulfilled
          : withCallingHandlers
          : callback
          : force
          : reenter_promise_domain
          : <Anonymous>
          : onFulfilled
          : withVisible
          : private$doResolve
          : withCallingHandlers
          : doTryCatch
          : tryCatchOne
          : tryCatchList
          : base::tryCatch
          : tryCatch
          : resolve
          : handleFulfill
          : <Anonymous>
          : execCallbacks
          : later::run_now
          : wait_for_it
          : eval [test-stacks-deep.R#XXX]
          : eval
          : withCallingHandlers
          : doTryCatch
          : tryCatchOne
          : tryCatchList
          : tryCatch
          : doWithOneRestart
          : withOneRestart
          : withRestarts
          : test_code
          : test_that
          : eval [test-stacks-deep.R#XXX]
          : eval
          : withCallingHandlers
          : doTryCatch
          : tryCatchOne
          : tryCatchList
          : tryCatch
          : doWithOneRestart
          : withOneRestart
          : withRestarts
          : test_code
          : source_file
          : FUN
          : lapply
          : doTryCatch
          : tryCatchOne
          : tryCatchList
          : tryCatch
          : with_reporter
          : test_files_serial
          : test_files
      From earlier call:
          : domain$wrapOnFulfilled
          : promiseDomain$onThen
          : action
          : withCallingHandlers
          : doTryCatch
          : tryCatchOne
          : tryCatchList
          : base::tryCatch
          : tryCatch
          : promise
          : self$then
          : promise$finally
          : finally
          : onRejected [test-stacks-deep.R#XXX]
          : withCallingHandlers
          : callback
          : force
          : reenter_promise_domain
          : <Anonymous>
          : onRejected
          : withVisible
          : private$doResolve
          : withCallingHandlers
          : doTryCatch
          : tryCatchOne
          : tryCatchList
          : base::tryCatch
          : tryCatch
          : resolve
          : handleReject
          : <Anonymous>
          : execCallbacks
          : later::run_now
          : wait_for_it
          : eval [test-stacks-deep.R#XXX]
          : eval
          : withCallingHandlers
          : doTryCatch
          : tryCatchOne
          : tryCatchList
          : tryCatch
          : doWithOneRestart
          : withOneRestart
          : withRestarts
          : test_code
          : test_that
          : eval [test-stacks-deep.R#XXX]
          : eval
          : withCallingHandlers
          : doTryCatch
          : tryCatchOne
          : tryCatchList
          : tryCatch
          : doWithOneRestart
          : withOneRestart
          : withRestarts
          : test_code
          : source_file
          : FUN
          : lapply
          : doTryCatch
          : tryCatchOne
          : tryCatchList
          : tryCatch
          : with_reporter
          : test_files_serial
          : test_files
      From earlier call:
          : domain$wrapOnRejected
          : promiseDomain$onThen
          : action
          : withCallingHandlers
          : doTryCatch
          : tryCatchOne
          : tryCatchList
          : base::tryCatch
          : tryCatch
          : promise
          : self$then
          : promise$catch
          : catch
          : %...!%
          : onFulfilled [test-stacks-deep.R#XXX]
          : withCallingHandlers
          : callback
          : force
          : reenter_promise_domain
          : <Anonymous>
          : onFulfilled
          : withVisible
          : private$doResolve
          : withCallingHandlers
          : doTryCatch
          : tryCatchOne
          : tryCatchList
          : base::tryCatch
          : tryCatch
          : resolve
          : handleFulfill
          : <Anonymous>
          : execCallbacks
          : later::run_now
          : wait_for_it
          : eval [test-stacks-deep.R#XXX]
          : eval
          : withCallingHandlers
          : doTryCatch
          : tryCatchOne
          : tryCatchList
          : tryCatch
          : doWithOneRestart
          : withOneRestart
          : withRestarts
          : test_code
          : test_that
          : eval [test-stacks-deep.R#XXX]
          : eval
          : withCallingHandlers
          : doTryCatch
          : tryCatchOne
          : tryCatchList
          : tryCatch
          : doWithOneRestart
          : withOneRestart
          : withRestarts
          : test_code
          : source_file
          : FUN
          : lapply
          : doTryCatch
          : tryCatchOne
          : tryCatchList
          : tryCatch
          : with_reporter
          : test_files_serial
          : test_files
      From earlier call:
          : domain$wrapOnFulfilled
          : promiseDomain$onThen
          : action
          : withCallingHandlers
          : doTryCatch
          : tryCatchOne
          : tryCatchList
          : base::tryCatch
          : tryCatch
          : promise
          : promise$then
          : then
          : %...>%
          : withCallingHandlers [test-stacks-deep.R#XXX]
          : domain$wrapSync
          : promises::with_promise_domain
          : captureStackTraces
          : as.promise
          : catch
          : %...!%
          : eval [test-stacks-deep.R#XXX]
          : eval
          : withCallingHandlers
          : doTryCatch
          : tryCatchOne
          : tryCatchList
          : tryCatch
          : doWithOneRestart
          : withOneRestart
          : withRestarts
          : test_code
          : test_that
          : eval [test-stacks-deep.R#XXX]
          : eval
          : withCallingHandlers
          : doTryCatch
          : tryCatchOne
          : tryCatchList
          : tryCatch
          : doWithOneRestart
          : withOneRestart
          : withRestarts
          : test_code
          : source_file
          : FUN
          : lapply
          : doTryCatch
          : tryCatchOne
          : tryCatchList
          : tryCatch
          : with_reporter
          : test_files_serial
          : test_files

# deep stacks long chain

    Code
      cat(sep = "\n", stacktrace <- formatError(dserr))
    Output
      Error in onFulfilled: boom
          : stop
          : onFulfilled [test-stacks-deep.R#XXX]
          : callback
          : <Anonymous>
          : onFulfilled
          : handleFulfill
          : <Anonymous>
          : execCallbacks
          : later::run_now
          : wait_for_it
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : test_that
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : source_file
          : FUN
          : lapply
          : test_files_serial
          : test_files
      From earlier call:
          : domain$wrapOnFulfilled
          : promiseDomain$onThen
          : action
          : promise
          : promise$then
          : then
          : %...>%
          : J__ [test-stacks-deep.R#XXX]
          : onFulfilled
          : callback
          : <Anonymous>
          : onFulfilled
          : handleFulfill
          : <Anonymous>
          : execCallbacks
          : later::run_now
          : wait_for_it
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : test_that
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : source_file
          : FUN
          : lapply
          : test_files_serial
          : test_files
      From earlier call:
          : domain$wrapOnFulfilled
          : promiseDomain$onThen
          : action
          : promise
          : promise$then
          : then
          : %...>%
          : I__ [test-stacks-deep.R#XXX]
          : onFulfilled
          : callback
          : <Anonymous>
          : onFulfilled
          : handleFulfill
          : <Anonymous>
          : execCallbacks
          : later::run_now
          : wait_for_it
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : test_that
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : source_file
          : FUN
          : lapply
          : test_files_serial
          : test_files
      From earlier call:
          : domain$wrapOnFulfilled
          : promiseDomain$onThen
          : action
          : promise
          : promise$then
          : then
          : %...>%
          : H__ [test-stacks-deep.R#XXX]
          : onFulfilled
          : callback
          : <Anonymous>
          : onFulfilled
          : handleFulfill
          : <Anonymous>
          : execCallbacks
          : later::run_now
          : wait_for_it
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : test_that
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : source_file
          : FUN
          : lapply
          : test_files_serial
          : test_files
      From earlier call:
          : domain$wrapOnFulfilled
          : promiseDomain$onThen
          : action
          : promise
          : promise$then
          : then
          : %...>%
          : G__ [test-stacks-deep.R#XXX]
          : onFulfilled
          : callback
          : <Anonymous>
          : onFulfilled
          : handleFulfill
          : <Anonymous>
          : execCallbacks
          : later::run_now
          : wait_for_it
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : test_that
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : source_file
          : FUN
          : lapply
          : test_files_serial
          : test_files
      From earlier call:
          : domain$wrapOnFulfilled
          : promiseDomain$onThen
          : action
          : promise
          : promise$then
          : then
          : %...>%
          : F__ [test-stacks-deep.R#XXX]
          : onFulfilled
          : callback
          : <Anonymous>
          : onFulfilled
          : handleFulfill
          : <Anonymous>
          : execCallbacks
          : later::run_now
          : wait_for_it
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : test_that
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : source_file
          : FUN
          : lapply
          : test_files_serial
          : test_files
      From earlier call:
          : domain$wrapOnFulfilled
          : promiseDomain$onThen
          : action
          : promise
          : promise$then
          : then
          : %...>%
          : E__ [test-stacks-deep.R#XXX]
          : onFulfilled
          : callback
          : <Anonymous>
          : onFulfilled
          : handleFulfill
          : <Anonymous>
          : execCallbacks
          : later::run_now
          : wait_for_it
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : test_that
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : source_file
          : FUN
          : lapply
          : test_files_serial
          : test_files
      From earlier call:
          : domain$wrapOnFulfilled
          : promiseDomain$onThen
          : action
          : promise
          : promise$then
          : then
          : %...>%
          : D__ [test-stacks-deep.R#XXX]
          : onFulfilled
          : callback
          : <Anonymous>
          : onFulfilled
          : handleFulfill
          : <Anonymous>
          : execCallbacks
          : later::run_now
          : wait_for_it
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : test_that
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : source_file
          : FUN
          : lapply
          : test_files_serial
          : test_files
      From earlier call:
          : domain$wrapOnFulfilled
          : promiseDomain$onThen
          : action
          : promise
          : promise$then
          : then
          : %...>%
          : C__ [test-stacks-deep.R#XXX]
          : onFulfilled
          : callback
          : <Anonymous>
          : onFulfilled
          : handleFulfill
          : <Anonymous>
          : execCallbacks
          : later::run_now
          : wait_for_it
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : test_that
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : source_file
          : FUN
          : lapply
          : test_files_serial
          : test_files
      From earlier call:
          : domain$wrapOnFulfilled
          : promiseDomain$onThen
          : action
          : promise
          : promise$then
          : then
          : %...>%
          : B__ [test-stacks-deep.R#XXX]
          : onFulfilled
          : callback
          : <Anonymous>
          : onFulfilled
          : handleFulfill
          : <Anonymous>
          : execCallbacks
          : later::run_now
          : wait_for_it
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : test_that
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : source_file
          : FUN
          : lapply
          : test_files_serial
          : test_files
      From earlier call:
          : domain$wrapOnFulfilled
          : promiseDomain$onThen
          : action
          : promise
          : promise$then
          : then
          : %...>%
          : A__ [test-stacks-deep.R#XXX]
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : test_that
          : eval [test-stacks-deep.R#XXX]
          : eval
          : test_code
          : source_file
          : FUN
          : lapply
          : test_files_serial
          : test_files

