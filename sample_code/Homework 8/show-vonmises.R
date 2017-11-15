show.vonmises <- function(n,mu,nu,kappa1,kappa2,lambda,show.contours=TRUE){
  source("rvonmises.R")
  x <- rvonmises(n,mu,nu,kappa1,kappa2,lambda)
  library(MASS)
  contours.x <- kde2d(x[,1], x[,2], n=1000, lim=c(-pi,pi,-pi,pi))
  image(contours.x)
  if(show.contours){
    contour(contours.x,add=T)
  }
}

show.vonmises(10000,1,1,1,2,1)
