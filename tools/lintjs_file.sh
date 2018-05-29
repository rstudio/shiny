#!/bin/bash -ex

# should be used with `npm run lintjs FILE`

if [[ "$#" == 0 ]]
then
  echo "please include a file to lint and make prettier: 'npm run lintjs FILE'"
  exit 1;
fi

echo "eslint..."
eslint --fix "$@"

echo "prettier..."
prettier --write "$@"

echo "flow..."
# flow status
