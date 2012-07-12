# Return the current time, in milliseconds from epoch, with
# unspecified time zone.
now <- function() {
  .Call('getTimeInMillis')
}
