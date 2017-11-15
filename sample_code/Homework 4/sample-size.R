sample.size <- function(p,width,alpha,n.Samples,dist){
  if(p > 1 || p < 0){stop("\"p\" must be a value between 0 and 1.")}
  if(width < 0){stop("\"width\" must be greater than 0")}
  if(alpha > 1 || alpha < 0){stop("\"alpha\" must be a value between 0 and 1.")}
  if(n.Samples < 0){stop("\"n.Samples\" must be greater than 0")}
  prop <- width + 1
  n <- 1
  while(prop > width){
    n <- n + 1
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
    prop <- quantile(width.samp,p)
  }
  cat("A sample size of n=",n," is required to create a ",100*(1-alpha),"% confidence interval that is smaller than the specified width, ",width,", with probability ",p," for ",dist,"ly distributed data.",sep="")
}
