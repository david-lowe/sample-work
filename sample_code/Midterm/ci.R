## sets initial parameterization
sig2 <- 1
n.iter <- 100000
alpha <- .05

## function set up to produce [n.iter] confidence intervals using both methods
sig2.ci <- function(sig2, n, n.iter, alpha, dist="normal"){
  
  # defines the sample distribution and finds the variance of n.iter samples
  if(dist=="exponential"){
    samp <- replicate(n.iter,rexp(n,sig2))
  }else{
    samp <- replicate(n.iter,rnorm(n,0,sig2))
  }
  S2 <- apply(samp,2,var)

  # Method 1: equal tailed
  m1.a <- qchisq(alpha/2,n-1)    # finds 'a' for method 1
  m1.b <- qchisq(1-(alpha/2),n-1)    # finds 'b' for method 1
  m1.ci <- cbind((n-1)*S2/m1.b,(n-1)*S2/m1.a)    # creates confidence intervals for sig2 using the calculated 'a' and 'b'
  colnames(m1.ci) <- c("m1.LL","m1.UL")
  
  # Method 2: minimum width
  m2.a <- optimize(function(a){
                     (1/a) - (1/qchisq((1-alpha)+pchisq(a,n-1),n-1))
                   }, interval=c(0,qchisq(alpha,n-1)), maximum=F)$minimum
  ## optimize finds the 'a' that minimizes the function g(a) = 1/a - 1/b with the constraint that Pr(a <= Y <= b) = 1 - alpha (Y ~ X²(n-1))
  m2.b <- qchisq((1-alpha)+pchisq(m2.a,n-1),n-1)
  m2.ci <- cbind((n-1)*S2/m2.b,(n-1)*S2/m2.a)
  colnames(m2.ci) <- c("m2.LL","m2.UL")
  ci <- data.frame(m1.ci,m2.ci)    # creates a dataset with the confidence intervals
  ci$m1.coverage <- 0
  ci$m2.coverage <- 0
  ci$m1.coverage[(which(ci[,1] < sig2 & ci[,2] > sig2))] <- 1    # specifies which intervals contain sig2
  ci$m2.coverage[(which(ci[,3] < sig2 & ci[,4] > sig2))] <- 1    # specifies which intervals contain sig2
  ci$m1.width <- ci[,2] - ci[,1]    # calculates interval width
  ci$m2.width <- ci[,4] - ci[,3]    # calculates interval width 
  ci
}

ci.n10 <- sig2.ci(sig2=sig2,n=10,n.iter=n.iter,alpha=alpha)    # finds interval for n=10
ci.n25 <- sig2.ci(sig2=sig2,n=25,n.iter=n.iter,alpha=alpha)    # finds interval for n=25
ci.n100 <- sig2.ci(sig2=sig2,n=100,n.iter=n.iter,alpha=alpha)    # finds interval for n=100

# 1. Verify Method 1 and Method 2 produce similar coverage
m1.coverage <- mean(ci.n10$m1.coverage)
m2.coverage <- mean(ci.n10$m2.coverage)

m1.coverage     # estimated coverage for method 1 of normals
m2.coverage     # estimated coverage for method 2 of normals

m1.coverage.mc <- m1.coverage + c(-1,1)*qnorm(1-(alpha/2))*sd(ci.n10$m1.coverage)/sqrt(n.iter)
m2.coverage.mc <- m2.coverage + c(-1,1)*qnorm(1-(alpha/2))*sd(ci.n10$m2.coverage)/sqrt(n.iter)

m1.coverage.mc    # MC error for method 1 of the coverage simulation of normals  
m2.coverage.mc    # MC error for method 2 of the coverage simulation of normals

## calculates the interval widths for each sample size
width.ratio.n10 <- mean(ci.n10$m1.width)/mean(ci.n10$m2.width)
width.ratio.n25 <- mean(ci.n25$m1.width)/mean(ci.n25$m2.width)
width.ratio.n100 <- mean(ci.n100$m1.width)/mean(ci.n100$m2.width)

width.ratio.n10    # width ratio for n=10
width.ratio.n25    # width ratio for n=25
width.ratio.n100    # width ratio for n=100
## Monte Carlo error is not very useful or practical in a ratio calculation

## does the same simulation with exponential draws
ci.n10.exp <- sig2.ci(sig2=sig2,n=10,n.iter=n.iter,alpha=alpha,dist="exponential")

m1.coverage.exp <- mean(ci.n10.exp$m1.coverage)
m2.coverage.exp <- mean(ci.n10.exp$m2.coverage)

m1.coverage.exp    # estimated coverage for method 1 of exponential draws
m2.coverage.exp    # estimated coverage for method 2 of exponential draws

m1.coverage.exp.mc <- m1.coverage.exp + c(-1,1)*qnorm(1-(alpha/2))*sd(ci.n10.exp$m1.coverage)/sqrt(n.iter)
m2.coverage.exp.mc <- m2.coverage.exp + c(-1,1)*qnorm(1-(alpha/2))*sd(ci.n10.exp$m2.coverage)/sqrt(n.iter)

m1.coverage.exp.mc    # MC error for method 1 of the coverage simulation of exponential draws
m2.coverage.exp.mc    # MC error for method 2 of the coverage simulation of exponential draws

