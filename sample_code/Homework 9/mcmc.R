data1 <- c(0.91,2.34,2.54,1.57,1.63,1.08,0.30,0.90,0.08,2.01,1.5,1.2)
beta <- 0.8
y <- rpois(length(data1),exp(beta*data1))
mu <- 0.4
sigma <- 0.2 
min <- -5
max <- 5

bayes.post.norm.log <- function(y,data1,mu,sigma){
  function(beta){
    sum(y*beta*data1-exp(beta*data1)) - (beta-mu)^2/(2*sigma^2)
  }
}

b.norm <- bayes.post.norm.log(y,data1,mu,sigma)
#x <- seq(-1,2,by=0.01)
#p <- numeric(length(x))
#for(i in 1:length(x)){
#  p[i] <- b(x[i])
#}
#p
#plot(x,exp(p),type="l")

draws <- numeric(100000)
s <- 0.23

doit.norm <- function(mu,sigma,s) {
  draws[1] <- rnorm(1,mu,sigma)
  accept <- 0
  for ( i in 2:length(draws) ) {
    proposal <- rnorm(1,draws[i-1],s)
    metropolis <- b.norm(proposal) - b.norm(draws[i-1])
    if ( log(runif(1)) < metropolis ) {
      draws[i] <- proposal
      accept <- accept + 1
    } else {
      draws[i] <- draws[i-1]
    }
  }
  list(draws,accept/length(draws))
}

d.norm <- doit.norm(mu,sigma,s)

bayes.post.unif.log <- function(y,data1,min,max){
  function(beta){
    sum(y*beta*data1-exp(beta*data1)) - log(max-min)
  }
}

b.unif <- bayes.post.unif.log(y,data1,min,max)

doit.unif <- function(mu,sigma,s) {
  draws[1] <- rnorm(1,mu,sigma)
  accept <- 0
  for ( i in 2:length(draws) ) {
    proposal <- rnorm(1,draws[i-1],s)
    metropolis <- b.unif(proposal) - b.unif(draws[i-1])
    if ( log(runif(1)) < metropolis ) {
      draws[i] <- proposal
      accept <- accept + 1
    } else {
      draws[i] <- draws[i-1]
    }
  }
  list(draws,accept/length(draws))
}

d.unif <- doit.unif(mu,sigma,s)

#0-1 Loss
estimate_mode <- function(s) {
  d <- density(s)
  d$x[which.max(d$y)]
}

estimate_mode(d.norm[[1]])
library(coda)
HPDinterval(mcmc(d.norm[[1]]),0.95)

estimate_mode(d.unif[[1]])
HPDinterval(mcmc(d.unif[[1]]),0.95)

#Squared Error
mean(d.norm[[1]])
mean(d.norm[[1]])+c(-1,1)*1.96*sd(d.norm[[1]])

mean(d.unif[[1]])
mean(d.unif[[1]])+c(-1,1)*1.96*sd(d.unif[[1]])

#Absolute
median(d.norm[[1]])
quantile(d.norm[[1]],c(.025,.975))

median(d.unif[[1]])
quantile(d.unif[[1]],c(.025,.975))
