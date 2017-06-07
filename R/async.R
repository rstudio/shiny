#' @export
call_async <- function(func, ..., args = NULL) {
  if (missing(args))
    args <- list(...)
  rx <- callr::r_bg(rlang::as_function(func), args)

  promise::new_promise(function(resolve, reject) {
    task <- function() {
      if (rx$is_alive()) {
        later::later(task, 0.1)
      } else {
        tryCatch(
          resolve(rx$get_result()),
          error = reject
        )
      }
    }
    task()
  })
}
