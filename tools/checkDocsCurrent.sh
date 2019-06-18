#!/bin/bash

# Generate package docs in the working directory
Rscript -e "devtools::document(roclets=c('rd', 'collate', 'namespace'))"

# This command will return a zero exit code if there are uncommitted changes
test -n "$(git status --porcelain)"
if [ $? -eq 0 ]
then
  git status --porcelain
  echo "Please generate the Roxygen documentation and commit the updates."
  echo "The above files changed when we generated the Roxygen documentation. This most often occurs when a user changes the Roxygen documentation in an R file but doesn't regenerate the documentation before committing."
  exit 1
else
  echo "No difference detected; Roxygen docs are current."
fi
