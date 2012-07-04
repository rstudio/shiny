source('R/react.R');
source('R/reactives.R');
source('R/shiny.R');
source('R/shinywrappers.R');

args <- commandArgs(trailingOnly=T)

if (length(args) == 0) {
  stop("Usage: shiny.sh <app_dir>")
}

app.path <- args[1]

app <- startApp(app=file.path(app.path, 'app.R'),
                 www.root=file.path(app.path, 'www'),
                 sys.www.root='./www',
                 port=8100L)
runApp(app)
