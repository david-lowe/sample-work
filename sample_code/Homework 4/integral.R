# actual function
g <- function(x) 1-2/pi*(1-x)^2*exp(x)/x*asin(sqrt(x))

# chosen pdf
p <- function(x) (20/13)*((x/2)+(2/5))

# inverse cdf
Finv <- function(x) (1/5)*(-4+sqrt(65*x + 16))

h <- function(x) g(x)/p(x) * (x>=0) * (x<=1)

B <- 10000000
x <- Finv(runif(B))

est <- mean(h(x))
ci <- est + c(-1,1)*qnorm(.975)*sd(h(x))/sqrt(B)

cat(sprintf("The estimated area under the given function, g(x), where x is in [0,1] is %4.5f with a 95%% Monte Carlo error of (%4.5f,%4.5f).",est,ci[1],ci[2]))

