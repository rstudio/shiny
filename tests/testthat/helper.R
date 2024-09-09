# Helper function for checking that vectors have same contents, regardless of
# order. Can be removed once something similar is incorporated into testthat
# package. See
# https://github.com/hadley/testthat/issues/473
contents_identical <- function(a, b) {
  # Convert to named vectors - needed for sorting later.
  if (is.null(names(a))) {
    names(a) <- rep("", length(a))
  }
  if (is.null(names(b))) {
    names(b) <- rep("", length(b))
  }

  # Fast path for atomic vectors
  if (is.atomic(a) && is.atomic(b)) {
    # Sort first by names, then contents. This is so that the comparison can
    # handle duplicated names.
    a <- a[order(names(a), a)]
    b <- b[order(names(b), b)]

    return(identical(a, b))
  }

  # If we get here, we're on the slower path for lists

  # Check if names are the same. If there are duplicated names, make sure
  # there's the same number of duplicates of each.
  if (!identical(sort(names(a)), sort(names(b)))) {
    return(FALSE)
  }

  # Group each vector by names
  by_names_a <- tapply(a, names(a), function(x) x)
  by_names_b <- tapply(b, names(b), function(x) x)

  # Compare each group
  for (i in seq_along(by_names_a)) {
    subset_a <- by_names_a[[i]]
    subset_b <- by_names_b[[i]]

    unique_subset_a <- unique(subset_a)
    idx_a <- sort(match(subset_a, unique_subset_a))
    idx_b <- sort(match(subset_b, unique_subset_a))
    if (!identical(idx_a, idx_b)) {
      return(FALSE)
    }
  }

  TRUE
}
