#!/bin/bash -e
. ./tools/documentation/checkDocsCurrent.sh

Rscript ./tools/updatePackageJsonVersion.R
if [ -n "$(git status --porcelain package.json)" ]
then
  yarn build
  git commit ./inst package.json -m 'Sync package version (GitHub Actions)' || echo "No package version to commit"
else
  echo "No package version difference detected; package.json is current."
fi
