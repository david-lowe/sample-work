func <- function(x) {
  3*x + 4
}


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

bisection(func,-10,100,.0001)









