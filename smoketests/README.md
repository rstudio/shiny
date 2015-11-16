## Smoke tests

This directory contains application subdirectories that produce deterministic output on stdout/stderr. After flushing output once, each app exits (due to `session$onFlushed(stopApp)`).

`Rscript snapshot.R` runs each app and visits it using phantomjs. The resulting stdout/stderr output is written to an `R.out.save` file in the app directory.

`Rscript test.R` also runs each app, but instead of saving to `R.out.save`, the results are compared to the `R.out.save` and any discrepancy is reported as test failure.

### Prerequisites

`phantomjs` must be in your path (tested with phantomjs 1.9, but later versions should be fine). On Ubuntu this is simply `apt-get install phantomjs`. On Mac if you have homebrew you can do `brew install phantomjs`. Otherwise, see http://phantomjs.org/download.html.

### Adding tests

1. Create a new directory and put either an app.R file or ui.R/server.R pair.
2. Add the line `session$onFlushed(stopApp)` to your server function.
3. Add whatever logic to emit info to stdout/stderr.
4. Run `Rscript snapshot.R`.
5. Add the new directory to git.
