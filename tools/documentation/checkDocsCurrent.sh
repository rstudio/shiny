#!/bin/bash

set -e

# Generate package docs in the working directory
echo "Document..."
Rscript -e "devtools::document(roclets=c('rd', 'collate', 'namespace'))"

if [ -n "$(git status --porcelain)" ]
then
  git status --porcelain
  >&2 echo "Please generate the Roxygen documentation and commit the updates."
  >&2 echo "The above files changed when we generated the Roxygen documentation. This most often occurs when a user changes the Roxygen documentation in an R file but doesn't regenerate the documentation before committing."
  exit 1
else
  echo "No difference detected; Roxygen docs are current."
fi


# Update htmltools docs
echo "Update Reexports..."
Rscript './tools/documentation/updateReexports.R'
echo "Check pkgdown..."
Rscript './tools/documentation/checkPkgdown.R'

if [ -n "$(git status --porcelain)" ]
then
  git status --porcelain
  >&2 echo "Please generate the reexports documentation and commit the updates."
  >&2 echo "The above files changed when we generated the reexports documentation by calling './tools/documentation/updateReexports.R'. This most often occurs when the documentation re-exported by shiny does not match the latest tagged package exports."
  exit 1
else
  echo "No difference detected; re-exported docs are current."
fi
