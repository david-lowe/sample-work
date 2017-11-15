# A function taking the following arguments:
#     log.density.target: a function with one argument x giving the log of the target density at x.
#     log.density.envelope: a function with one argument x giving the log of the envelope density at x.
#     sample.envelope: a function taking no arguments and returning a sample from the envelope distribution.
#     alpha: a numeric such that the density of envelope distribution divided by alpha is greater than
#        the density of the target distribution for all x.
#     sample.size: a numeric given the number of samples to draw.
# The function returns sample.size realizations from the target distribution.

rejection.sampler <- function(log.density.target,log.density.envelope,sample.envelope,alpha,sample.size) {
  log.f <- log.density.target
  log.g <- log.density.envelope
  x <- matrix(0,ncol=length(sample.envelope()),nrow=sample.size,byrow=T)
  accept <- 0
  while(accept < sample.size){
    x.cand <- sample.envelope()
    u <- log(runif(1)) + log.g(x.cand) - log(alpha)
    if(u <= log.f(x.cand)){
      accept <- accept + 1
      x[accept,] <- x.cand
    }
  }
  x
}

