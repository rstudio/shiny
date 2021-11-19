#!/bin/bash -e
. ./tools/documentation/checkDocsCurrent.sh

echo "Updating package.json version to match DESCRIPTION Version"
Rscript ./tools/updatePackageJsonVersion.R
if [ -n "$(git status --porcelain package.json)" ]
then
  yarn build
  git commit ./inst package.json -m 'Sync package version (GitHub Actions)' || echo "No package version to commit"
else
  echo "No package version difference detected; package.json is current."
fi
