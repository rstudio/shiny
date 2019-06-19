#!/bin/bash

set -e

# Run JS build process
(cd "$(dirname "$0")" && yarn && yarn build)

if [ -n "$(git status --porcelain)" ]
then
  git status --porcelain
  >&2 echo "Please rebuild the JavaScript and commit the changes."
  >&2 echo "The above files changed when we built the JavaScript assets. This most often occurs when a user makes changes to the JavaScript sources but doesn't rebuild and commit them."
  exit 1
else
  echo "No difference detected; JavaScript build is current."
fi
