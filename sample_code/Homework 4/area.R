## {(x,y): x^2 + y^2 ≤ 0.5, x^2 ≤ y ≤ x }
## x, y must be between 0 and 1

N <- 1000000
x <- runif(N,0,1)
y <- runif(N,0,1)
area <- ifelse(x > y & y > x^2 & x^2 + y^2 <= .5, 1, 0)

est <- mean(area)
ci <- est + c(-1,1)*qnorm(.975)*sd(area)/sqrt(N)

cat(sprintf("The estimated area of this region is approximately %4.5f with a 95%% Monte Carlo error of (%4.5f,%4.5f).",est,ci[1],ci[2]))

