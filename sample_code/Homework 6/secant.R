func <- function(x) {
  3*x + 4
}


secant <- function(f,x0,x1,epsilon=.000001){
  while(abs(x1-x0) >= epsilon){
    x.new <- x1-(f(x1)*(x1-x0))/(f(x1)-f(x0)) 
    x0 <- x1
    x1 <- x.new
  }
  x1
}

secant(func,-10,100,.0001)



w <- function(n){
  n.Samples <- 100000
  width.samp <- rep(0,n.Samples)
  for(i in 1:n.Samples){
    if(dist == "normal"){
      s <- sd(rnorm(n))
    }else if(dist == "exponential"){
      s <- sd(rexp(n))
    }else{
      stop("The distribution input must be either \"normal\" or \"exponential.\"")
    }
    width.samp[i] <- diff(c(-1,1)*qt((1-alpha/2),n-1)*s/sqrt(n))
  }
  quantile(width.samp,p)
}

secant(w,50,4)
