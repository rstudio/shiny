#!/bin/bash

set -e

# Generate package docs in the working directory
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
