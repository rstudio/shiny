#!/bin/bash

# Run JS build process
(cd "$(dirname "$0")" && yarn build)
if [ $? -ne 0 ]
then
  echo "Error generating JavaScript assets with yarn."
  exit 1
fi

# This command will return a zero exit code if there are uncommitted changes
test -n "$(git status --porcelain)"
if [ $? -eq 0 ]
then
  git status --porcelain
  echo "Please rebuild the JavaScript and commit the changes."
  echo "The above files changed when we built the JavaScript assets. This most often occurs when a user makes changes to the JavaScript sources but doesn't rebuild and commit them."
  exit 1
else
  echo "No difference detected; JavaScript build is current."
fi
