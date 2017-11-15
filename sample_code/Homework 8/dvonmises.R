dvonmises <- function(x,mu,nu,kappa1,kappa2,lambda,log=FALSE,use.normalizing.constant=TRUE){
  if(length(which(c(x,mu,nu) >= -pi)) < 4 ||
     length(which(c(x,mu,nu) < pi)) < 4 ||
     kappa1 <= 0 || kappa2 <= 0 || missing(lambda)){
    stop("Please provide a value for each parameter within the parameter space:
         x = a vector of two angles in (-pi,pi]
         mu in (-pi,pi]
         nu in (-pi,pi]
         kappa1 > 0
         kappa2 > 0
         lambda in (-infinity,infinity)")
  } 
  if(use.normalizing.constant){
    m <- seq(0,100)
    C <- 1/((4*(pi^2))*sum(choose(2*m,m)*(((lambda^2)/(4*kappa1*kappa2))^m)*besselI(kappa1,m)*besselI(kappa2,m)))
  }else{
    C <- 1
  }
  if(log){
    if(use.normalizing.constant){
      C <- log(C)
    }else{
      C <- 0
    }
    C+(kappa1*cos(x[1]-mu) + kappa2*cos(x[2]-nu) + lambda*sin(x[1]-mu)*sin(x[2]-nu))
  }else{
    C*exp(kappa1*cos(x[1]-mu) + kappa2*cos(x[2]-nu) + lambda*sin(x[1]-mu)*sin(x[2]-nu))
  }
}

