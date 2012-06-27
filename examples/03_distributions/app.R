data <- Observable$new(function() {
  # Choose a distribution function
  dist <- switch(get.shiny.input('dist'),
                 norm = rnorm,
                 unif = runif,
                 lnorm = rlnorm,
                 exp = rexp,
                 rnorm)
  
  # Generate n values from the distribution function
  dist(max(1, get.shiny.input('n')))
})

define.shiny.plot('plot1', function() {
  dist <- get.shiny.input('dist')
  n <- get.shiny.input('n')
  
  hist(data$get.value(), 
       main=paste('r', dist, '(', n, ')', sep=''))
}, width=600, height=300)

define.shiny.table('table1', function() {
  data.frame(x=data$get.value())
})
