# "...like a regular queue or stack data structure, but where additionally each
# element has a "priority" associated with it. In a priority queue, an element
# with high priority is served before an element with low priority. If two
# elements have the same priority, they are served according to their order in
# the queue." (http://en.wikipedia.org/wiki/Priority_queue)

PriorityQueue <- setRefClass(
  'PriorityQueue',
  fields = list(
    # Keys are priorities, values are subqueues (implemented as list)
    .itemsByPriority = 'Map',
    # Sorted vector (largest first)
    .priorities = 'integer'  
  ),
  methods = list(
    # Enqueue an item, with the given priority level (must be integer). Higher 
    # priority numbers are dequeued earlier than lower.
    # 
    # We insist on integers over numerics to avoid problems that may arise from 
    # loss of precision that is introduced when converting a numeric to a string
    # (which is performed when accessing the .itemsByPriority map).
    enqueue = function(item, priority) {
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
      as.character(priority)
    }
  )
)

# pq <- PriorityQueue$new()
# pq$enqueue('a', 1L)
# pq$enqueue('b', 1L)
# pq$enqueue('c', 1L)
# pq$enqueue('A', 2L)
# pq$enqueue('B', 2L)
# pq$enqueue('C', 2L)
# pq$enqueue('d', 1L)
# pq$enqueue('e', 1L)
# pq$enqueue('f', 1L)
# pq$enqueue('D', 2L)
# pq$enqueue('E', 2L)
# pq$enqueue('F', 2L)
# # Expect ABCDEFabcdef
