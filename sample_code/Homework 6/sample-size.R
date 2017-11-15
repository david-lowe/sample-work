source("bisection.R")

sample.size <- function(p,width,alpha,n.Samples,dist){
  if(p > 1 || p < 0){stop("\"p\" must be a value between 0 and 1.")}
  if(width < 0){stop("\"width\" must be greater than 0")}
  if(alpha > 1 || alpha < 0){stop("\"alpha\" must be a value between 0 and 1.")}
  if(n.Samples < 0){stop("\"n.Samples\" must be greater than 0")}
  w <- function(x){
    if(dist == "normal"){
      dat <- replicate(n.Samples,rnorm(x))
    }else if(dist == "exponential"){
      dat <- replicate(n.Samples,rexp(x))
    }else{
      stop("The distribution input must be either \"normal\" or \"exponential.\"")
    }
    s <- apply(dat,2,sd)
    width.samp <- 2*qt((1-alpha/2),x-1)*s/sqrt(x)
    mean(width.samp <= width) - p
  }
  n <- ceiling(bisection(w,2,100))
  cat("A sample size of n=",n," is required to create a ",100*(1-alpha),"% confidence interval that is smaller than the specified width, ",width,", with probability ",p," for ",dist,"ly distributed data.",sep="")
}

## I chose the bisection method mainly because I could control better
## which values of n were possible. The secant method produced infinite
## and negative values which are invalid values for n. The bisection
## method maintains positive values for n, and although it is much
## more computationally intensive, it doesn't produce unacceptable 
## values and will eventually find the correct n. 


