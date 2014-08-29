# "...like a regular queue or stack data structure, but where additionally each
# element has a "priority" associated with it. In a priority queue, an element
# with high priority is served before an element with low priority. If two
# elements have the same priority, they are served according to their order in
# the queue." (http://en.wikipedia.org/wiki/Priority_queue)

PriorityQueue <- R6Class(
  'PriorityQueue',
  portable = FALSE,
  class = FALSE,
  public = list(
    # Keys are priorities, values are subqueues (implemented as list)
    .itemsByPriority = 'Map',
    # Sorted vector (largest first)
    .priorities = numeric(0),

    initialize = function() {
      .itemsByPriority <<- Map$new()
    },
    # Enqueue an item, with the given priority level (must be integer). Higher
    # priority numbers are dequeued earlier than lower.
    enqueue = function(item, priority) {
      priority <- normalizePriority(priority)

      if (!(priority %in% .priorities)) {
        .priorities <<- c(.priorities, priority)
        .priorities <<- sort(.priorities, decreasing=TRUE)
        .itemsByPriority$set(.key(priority), list(item))
      } else {
        .itemsByPriority$set(
          .key(priority),
          c(.itemsByPriority$get(.key(priority)), item)
        )
      }
      return(invisible())
    },
    # Retrieve a single item by 1) priority number (highest first) and then 2)
    # insertion order (first in, first out). If there are no items to be
    # dequeued, then NULL is returned. If it is necessary to distinguish between
    # a NULL value and the empty case, call isEmpty() before dequeue().
    dequeue = function() {
      if (length(.priorities) == 0)
        return(NULL)

      maxPriority <- .priorities[[1]]
      items <- .itemsByPriority$get(.key(maxPriority))
      firstItem <- items[[1]]
      if (length(items) == 1) {
        # This is the last item at this priority. Remove both the list and the
        # priority level.
        .itemsByPriority$remove(.key(maxPriority))
        .priorities <<- .priorities[-1]
      } else {
        # There are still items at this priority. Remove the current item from
        # the list, and save it.
        items <- items[-1]
        .itemsByPriority$set(.key(maxPriority), items)
      }
      return(firstItem)
    },
    # Returns TRUE if no items are in the queue, otherwise FALSE.
    isEmpty = function() {
      length(.priorities) == 0
    },
    # Translates a priority integer to a character that is suitable for using as
    # a key.
    .key = function(priority) {
      sprintf('%a', priority)
    }
  )
)

normalizePriority <- function(priority) {

  if (is.null(priority))
    priority <- 0

  # Cast integers to numeric to prevent any inconsistencies
  if (is.integer(priority))
    priority <- as.numeric(priority)

  if (!is.numeric(priority))
    stop('priority must be an integer or numeric')

  # Check length
  if (length(priority) == 0) {
    warning('Zero-length priority vector was passed; using 0')
    priority <- 0
  } else if (length(priority) > 1) {
    warning('Priority has length > 1 and only the first element will be used')
    priority <- priority[1]
  }

  # NA == 0
  if (is.na(priority))
    priority <- 0

  return(priority)
}

# pq <- PriorityQueue$new()
# pq$enqueue('a', 1)
# pq$enqueue('b', 1L)
# pq$enqueue('c', 1)
# pq$enqueue('A', 2)
# pq$enqueue('B', 2L)
# pq$enqueue('C', 2)
# pq$enqueue('d', 1)
# pq$enqueue('e', 1L)
# pq$enqueue('f', 1)
# pq$enqueue('D', 2)
# pq$enqueue('E', 2L)
# pq$enqueue('F', 2)
# # Expect ABCDEFabcdef
