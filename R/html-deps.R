pathPattern <- "^(~|/|[a-zA-Z]:[/\\\\]|\\\\\\\\)"

createWebDependency <- function(dependency) {
  if (is.null(dependency))
    return(NULL)

  if (!is(dependency, "html_dependency"))
    stop("Unexpected non-html_dependency type")

  # Does it look like a path on disk? Register it as a resource and replace the
  # disk-based path with a relative URL
  if (grepl(pathPattern, dependency$path, perl = TRUE)) {
    prefix <- paste(dependency$name, "-", dependency$version, sep = "")
    addResourcePath(prefix, dependency$path)
    dependency$path <- prefix
  }

  return(dependency)
}

# Given a list of dependencies, choose the latest versions and return them as a
# named list in the correct order.
getNewestDeps <- function(dependencies) {
  result <- list()
  for (dep in dependencies) {
    if (!is.null(dep)) {
      other <- result[[dep$name]]
      if (is.null(other) || compareVersion(dep$version, other$version) > 0) {
        # Note that if the dep was already in the result list, then this
        # assignment preserves its position in the list
        result[[dep$name]] <- dep
      }
    }
  }
  return(result)
}

# Remove `remove` from `dependencies` if the name matches.
# dependencies is a named list of dependencies.
# remove is a named list of dependencies that take priority.
# If warnOnConflict, then warn when a dependency is being removed because of an
# older version already being loaded.
removeDeps <- function(dependencies, remove, warnOnConflict = TRUE) {
  matches <- names(dependencies) %in% names(remove)
  if (warnOnConflict) {
    for (depname in names(dependencies)[matches]) {
      loser <- dependencies[[depname]]
      winner <- remove[[depname]]
      if (compareVersion(loser$version, winner$version) > 0) {
        warning(sprintf(paste("The dependency %s %s conflicts with",
          "version %s"), loser$name, loser$version, winner$version
        ))
      }
    }
  }

  # Return only deps that weren't in remove
  return(dependencies[!matches])
}

DependencyContext <- setRefClass(
  "DependencyContext",
  fields = list(
    committed = 'Map'
  ),
  methods = list(
    commitDependency = function(dep) {
      committed$set(dep$name, dep$version)
    },
    resolveDependencies = function(deps) {
      # 1. Internally resolve the deps. This also turns unnamed lists into named
      # lists.
      deps <- getNewestDeps(deps)

      # 2. Remove deps that conflict with committed (with warning if version of
      # dep is newer than committed)
      deps <- removeDeps(deps, as.list(committed), warnOnConflict = TRUE)

      return(deps)
    }
  )
)
