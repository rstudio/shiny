source('R/react.R');
source('R/shiny.R');

args <- commandArgs(trailingOnly=T)

if (length(args) == 0) {
  stop("Usage: shiny.sh <app_dir>")
}

app.path <- args[1]

app <- start.app(app=file.path(app.path, 'app.R'),
                 www.root=file.path(app.path, 'www'),
                 sys.www.root='./www')
run.app(app)
