#!/bin/bash -e

# should be used with `npm run rlog-fix FILE`

if [[ "$#" == 0 ]]
then
  echo "please include a file to lint and make prettier: 'npm run rlog-fix FILE'"
  exit 1;
fi


eslint --fix "$@"

prettier --write "$@"
