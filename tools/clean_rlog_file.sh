#!/bin/bash -e

# should be used with `npm run rlog-fix FILE`

npm run lintjs "$@"

npm run rlog-build-only
