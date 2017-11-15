# log likelihood of poisson and its first and second derivatives
log.lik.0 <- function(y,x,beta) sum(y*beta*x - exp(beta*x))
log.lik.1 <- function(y,x,beta) sum(y*x - x*exp(beta*x))
log.lik.2 <- function(x,beta) sum(-(x^2)*exp(beta*x))
pois.func <- list(log.lik.0,log.lik.1,log.lik.2)

find.mle <- function(y,x,functions=pois.func,init.est,tol=0.000001){
  f0 <- functions[[1]]
  f1 <- functions[[2]]
  f2 <- functions[[3]]
  est <- init.est
  count <- 0
  while(abs(f1(y,x,est)) > tol){
    b <- f1(y,x,est)
    c <- f2(x,est)
    new.est <- est - b/c
    est <- new.est
    count <- count + 1
  }
  est
}

## Bootstrap
bootstrap.ci <- function(y,x,alpha=.05){
  mle <- find.mle(y,x,pois.func,mean(log(y+1)/x))
  betas <- sapply(1:10000,function(i){
             new.y <- sample(y,replace=T)
             new.x <- sample(x,replace=T)
             find.mle(new.y,new.x,pois.func,mean(log(new.y+1)/new.x))
  })
  ci <- quantile(betas,c(alpha/2,1-alpha/2))
  beta.est <- c(mle,ci)
  names(beta.est) <- c("MLE","Lower","Upper")
  beta.est
}

## Wilk's Method
bisection <- function(f,x0,x1,epsilon=.000001){
  if(f(x0)*f(x1)>0){stop("Please make a better guess (where the two values produce a positive and a negative value for the function).")}
  while(abs(x1-x0) >= epsilon){
    x2 <- (x0 + x1)/2
    if(f(x2)*f(x0) < 0){
      x1 <- x2
    } else{
      x0 <- x2
    }
  }
  mean(c(x0,x1))
}

wilks.ci <- function(y,x,alpha=.05){
  mle <- find.mle(y,x,pois.func,mean(log(y+1)/x))
  ci.guess <- c(mle-1,mle+1)
  find.ci <- function(beta){
    pois.func[[1]](y,x,mle) - pois.func[[1]](y,x,beta) - 0.5 * qchisq(1-alpha,1)
  }
  while(find.ci(ci.guess[1])*find.ci(mle) >= 0 || find.ci(ci.guess[2])*find.ci(mle) >= 0){
    ci.guess[1] <- ci.guess[1] - 1
    ci.guess[2] <- ci.guess[2] + 1
  }
  ll <- bisection(find.ci,ci.guess[1],mle)
  ul <- bisection(find.ci,mle,ci.guess[2]) 
  beta.est <- c(mle,ll,ul)
  names(beta.est) <- c("MLE","Lower","Upper")
  beta.est
}


## Test-Inversion 
test.inversion.ci <- function(y,x,alpha=0.05){
  mle <- find.mle(y,x,pois.func,mean(log(y+1)/x))
  mle.draws <- sapply(1:10000,function(i){ 
    new.y <- rpois(length(y),exp(mle*x))
    find.mle(new.y,x,pois.func,mle)
  })
  grid <- seq(min(mle.draws),max(mle.draws),diff(range(mle.draws))/60)
  n <- length(y)
  interval <- sapply(grid,function(beta,alpha,x) {
    samp <- sapply(1:1000,function(i) {
      new.y <- rpois(n,exp(x*beta))
      find.mle(new.y,x,pois.func,mean(log(new.y+1)/x))
    })
    crit.values <- quantile(samp,c(alpha/2,1-alpha/2))
    prod(crit.values - mle) < 0
  },alpha=alpha,x=x)
  ci <- range(grid[interval])
  if ( min(ci) <= min(grid) ) stop("It appears the grid doesn't go low enough.")
  if ( max(ci) >= max(grid) ) stop("It appears the grid doesn't go high enough.")
  beta.est <- c(mle,ci)
  names(beta.est) <- c("MLE","Lower","Upper")
  beta.est
}

## Creates the log posterior based on the specified prior and the data
makeLogPosterior <- function(y,x,log.likelihood,prior.dist,param1,param2){
  if(prior.dist == "normal"){
    function(beta) log.likelihood(y,x,beta) - ((beta - param1)^2)/(2*(param2^2))
  }else if(prior.dist=="uniform"){
    function(beta) log.likelihood(y,x,beta) - log(param2 - param1)
  }
}

mcmc.beta <- function(y,x,prior.dist,param1,param2,tuning.param){
  beta.draws <- rep(0,10000)
  accept <- 0
  if(prior.dist=="normal"){
    beta.draws[1] <- rnorm(1,param1,param2)
    beta.post <- makeLogPosterior(y,x,pois.func[[1]],"normal",param1,param2)
  }else if(prior.dist=="uniform"){
    beta.draws[1] <- runif(1,param1,param2)
    beta.post <- makeLogPosterior(y,x,pois.func[[1]],"uniform",param1,param2)
  }
  for(i in 2:length(beta.draws)){
    beta.star <- rnorm(1,beta.draws[i-1],tuning.param)
    r <- beta.post(beta.star) - beta.post(beta.draws[i-1])
    if(log(runif(1)) < r){
      beta.draw[i] <- beta.star
      accept <- accept + 1
    }else{
      beta.draws[i] <- beta.draws[i-1]
    }
  }
  list(beta.draws,accept/length(beta.draws))
}

# posreg function references all code above.

posreg <- function(y,x,method,alpha=.05){
  log.lik.0 <- function(y,x,beta) sum(y*beta*x - exp(beta*x))
  log.lik.1 <- function(y,x,beta) sum(y*x - x*exp(beta*x))
  log.lik.2 <- function(x,beta) sum(-(x^2)*exp(beta*x))
  pois.func <- list(log.lik.0,log.lik.1,log.lik.2)
  if(missing(y) || missing(x) || missing(method)){
    stop("Please provide a vector of x's, a vector of y's, the method you would like to use (e.g. c(\"mle\",\"wilks\")), and a value between 0 and 1 for alpha.")
  }
  if(method[1]=="mle"){
    if(method[2] == "bootstrap"){
      bootstrap.ci(y,x,alpha)
    } else if(method[2] == "wilks"){
      wilks.ci(y,x,alpha)
    } else if(method[2] == "test-inversion"){
      test.inversion.ci(y,x,alpha)
    } else {
      stop("Please check that you have entered a valid MLE estimation method:\n
           -\"wilks\"\n
           -\"bootstrap\"\n
           -\"test-inversion\"")
    }
  }else if(method[1]=="bayes"){
    param1 <- as.numeric(method[4])
    param2 <- as.numeric(method[5])
    if(method[3]=="normal"){
      draws <- mcmc.beta(y,x,"normal",param1,param2,.3)
    }else if(method[3]=="uniform"){
      draws <- mcmc.beta(y,x,"uniform",param1,param2,.3)
    }else{
      stop("Please check that you have entered a valid prior distribution:\n
           - \"normal\"\n
           - \"uniform\"")
    }
    if(method[2]=="0-1"){
      d.beta <- density(draws[[1]])
      beta.est <- d.beta$x[which.max(d.beta$y)]
      library(coda)
      beta.ci <- HPDinterval(mcmc(draws[[1]]),1-alpha)
    }else if(method[2]=="absolute"){
      beta.est <- median(draws[[1]])
      beta.ci <- quantile(draws[[1]],c(alpha/2,1-alpha/2))
    }else if(method[2]=="squared"){
      beta.est <- mean(draws[[1]])
      beta.ci <- beta.est + c(-1,1)*qnorm(1-alpha/2)*sd(draws[[1]])        
    }else{
      stop("Please check that you have entered a valid loss function:\n
           - \"0-1\"\n
           - \"absolute\"\n
           - \"squared\"\n")
    }
  result <- c(beta.est,beta.ci)
  names(result) <- c("Estimate","Lower","Upper")
  result
  }else{
    stop("Did not recognize the method indicated. Please be sure it matches one of the following formats:\n
          - c(\"mle\",<<CI-METHOD>>)\n
          - c(\"bayes\",<<LOSS>>,\"normal\",<<MEAN>>,<<VARIANCE>>)\n
          - c(\"bayes\",<<LOSS>>,\"uniform\",<<LOWER>>,<<UPPER>>)")
  }
}

#x <- c(0.91, 2.34, 2.54, 1.57, 1.63, 1.08, 0.30, 0.90, 0.08, 2.01, 1.5, 1.2)
#beta <- .5 
#y <- rpois(length(x),exp(x*beta)) 
#
#posreg(y,x,c("bayes","0-1","normal",.4,.2))
#posreg(y,x,c("bayes","squared","normal",.4,.2))
#posreg(y,x,c("bayes","absolute","normal",.4,.2))
#posreg(y,x,c("bayes","0-1","uniform",-5,5))
#posreg(y,x,c("bayes","squared","uniform",-5,5))
#posreg(y,x,c("bayes","absolute","uniform",-5,5))
#posreg(y,x,c("mle","bootstrap"))
#posreg(y,x,c("mle","wilks"))
#posreg(y,x,c("mle","test-inversion"))
