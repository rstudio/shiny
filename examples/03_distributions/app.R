data <- Observable$new(function() {
  # Choose a distribution function
  dist <- switch(get.input('dist'),
                 norm = rnorm,
                 unif = runif,
                 lnorm = rlnorm,
                 exp = rexp,
                 rnorm)
  
  # Generate n values from the distribution function
  dist(as.integer(get.input('n')))
})

define.plot('plot1', function() {
  dist <- get.input('dist')
  n <- get.input('n')
  
  hist(data$get.value(), 
       main=paste('r', dist, '(', n, ')', sep=''))
}, width=600, height=300)

define.table('table1', function() {
  data.frame(x=data$get.value())
})

define.output('summary1', function() {
  paste(capture.output(print(summary(data$get.value()))), collapse="\n")
})