# Make log likelihood, gradient, hessian
makeFunctions <- function(x){
  
  k <- x
  n <- length(x)
  
  log.likelihood <- function(theta){ 
    r <- theta[1]
    p <- theta[2]
    -sum(log(gamma(k+r)) - log(gamma(r))
    + log(1-p)*k + r*log(p))
  }
  
  gradient <- function(theta){
    r <- theta[1]
    p <- theta[2]
    -c((n*r)/(p) - sum(k)/(1-p), 
      sum(digamma(k+r)) - n*digamma(r) + n*log(p)) 
  }
  
  hessian <- function(theta){
    r <- theta[1]
    p <- theta[2]
    -matrix( c(sum(trigamma(k+r)) - n*trigamma(r),
              n/p,
              n/p,
              -sum(k)/((1-p)^2) - (n*r)/(p^2)
    )
    ,nrow=2,byrow=TRUE)
  }
  
  list(log.lik=log.likelihood,gradient=gradient,hessian=hessian)
  
}

#### MLE ####

source("rnbinom.R")
set.seed(153546515)
x <- r.nbinom(1000,4.2,.12)
f <- makeFunctions(x)

4.2*(1-.12)/.12
4.2*(1-.12)/(.12^2)

theta <- c(3,.4)
est <- suppressWarnings(optim(theta,f$log.lik)$par)


#### Newton Raphson ####

#theta <- c(3,.4)
#epsilon <- 1e-8
#count <- 0
#while(sqrt(sum(f$gradient(theta)^2)) > epsilon){
#  theta <- theta - solve(f$hessian(theta),f$gradient(theta))
#  count <- count+1
#}


#### MLE ####
# tried to find using Newton-Raphson but I couldn't figure out how to constrain p to (0,1) so after 
# trying to use the gradient and the hessian to find the MLE I decided it would be best to use optim

est <- optim(theta,function(m) -f$log.lik(m))$par

#### Method of Moments ####
x <- r.nbinom(1000,4.2,.12)

p <- mean(x)/var(x)
r <- (mean(x)^2)/(var(x)-mean(x))


#### Bayes Estimator ####
x <- r.nbinom(1000,4.2,.12)


# Prior for size (r): r ~ gamma(a,b)
# Prior for probability of success (p): p ~ beta(c,d)

makePosterior <- function(x,prior.param1,prior.param2){
  k <- x
  n <- length(x)
  sum.k <- sum(x)
  a <- prior.param1[1]
  b <- prior.param1[2]
  c <- prior.param2[1]
  d <- prior.param2[2]
  # posterior for r
  post.r <- function(r,p){ 
    sum(lgamma(k+r)) - n*lgamma(r)
      + n*r*log(p) + (a-1)*log(r) - b*r
  }
  post.p <- function(r,p){
    n*r*log(p) + log(1-p)*sum.k + (c-1)*log(p)
      + (d-1)*log(1-p)
  }

  list(post.r=post.r,post.p=post.p)

}


BayesEst <- function(prior.param1,prior.param2,tuning.param,n.iter=10000){
  func <- makePosterior(x,prior.param1,prior.param2)
  r.draws <- rep(0,n.iter)
  p.draws <- rep(0,n.iter)
  r.draws[1] <- (mean(x)^2)/(var(x)-mean(x)) #r.gamma(1,prior.param1[1],prior.param1[2])
  p.draws[1] <- mean(x)/var(x) #r.beta(1,prior.param2[1],prior.param2[2])
  r.accept <- 0
  p.accept <- 0

  suppressWarnings(
  for(i in 2:n.iter){
    r.star <- r.norm(1,r.draws[i-1],tuning.param[1])
    r.draws[i] <- r.draws[i-1]
#    cat(r.star,"\n")
    if(r.star > 0){
      g.r <- func$post.r(r.star,p.draws[i-1]) 
             - func$post.r(r.draws[i-1],p.draws[i-1])
#      cat("g ",g.r,"\n")
      if(log(runif(1)) < g.r){
        r.draws[i] <- r.star
        r.accept <- r.accept + 1
      }
    }
    p.star <- r.norm(1,p.draws[i-1],tuning.param[2])
    p.draws[i] <- p.draws[i-1]
    if(p.star > 0 && p.star < 1){
      g.p <- func$post.p(r.draws[i],p.star)
             - func$post.p(r.draws[i],p.draws[i-1])
      if(log(runif(1)) < g.p){
        p.draws[i] <- p.star
        p.accept <- p.accept + 1
      }
    }
  }
  )   

  list(r.draws=r.draws
      ,r.accept=(r.accept/length(r.draws))
      ,p.draws=p.draws
      ,p.accept=(p.accept/length(r.draws)))  

}

r.prior <- c((mean(x)^2)/(var(x)-mean(x)),1)
p.prior <- c(10*mean(x)/var(x),10)
tuning <- c(.01,.13)
draws <- BayesEst(r.prior,p.prior,tuning,40)

#library(coda)
mean(draws$r.draws)
plot(density(draws$r.draws))
plot(draws$r.draws,type='l')
draws$r.accept
acf(draws$r.draws)
effectiveSize(draws$r.draws)

mean(draws$p.draws)
plot(density(draws$p.draws))
plot(draws$p.draws,type='l')
draws$p.accept
acf(draws$p.draws)
effectiveSize(draws$p.draws)







