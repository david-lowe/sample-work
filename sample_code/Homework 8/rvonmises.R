rvonmises <- function(n,mu,nu,kappa1,kappa2,lambda){
  source("rejection-sampler.R")
  source("dvonmises.R")
  f <- function(x) dvonmises(x,mu,nu,kappa1,kappa2,lambda,TRUE,TRUE)
  alpha.f <- function(x) dvonmises(x,mu,nu,kappa1,kappa2,lambda,FALSE,TRUE)
  samp.env <- function() runif(2,-pi,pi)
  log.d.env <- function(y) -log(2*pi)
  if(kappa1*kappa2 > lambda^2){
    alpha.val <- alpha.f(c(mu,nu))
    rejection.sampler(f,log.d.env,samp.env,alpha.val,n)
  }else{
    psi.0 <- acos((kappa2/abs(lambda))*sqrt(((lambda^2)+(kappa1^2))/((lambda^2)+(kappa2^2)))) - nu
    phi.0 <- acos((kappa1/abs(lambda))*sqrt(((lambda^2)+(kappa2^2))/((lambda^2)+(kappa1^2)))) - mu
    if(lambda > 0){
      alpha.val <- max(alpha.f(c(phi.0,psi.0)),alpha.f(c(-phi.0,-psi.0)))
    }else{
      alpha.val <- max(alpha.f(c(-phi.0,psi.0)),alpha.f(c(phi.0,-psi.0)))
    }
    rejection.sampler(f,log.d.env,samp.env,alpha.val,n)
  }
}

