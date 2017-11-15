new.urn <- function(n.balls=2,black.proportion) {
  c('black'=n.balls*black.proportion,
    'white'=n.balls*(1-black.proportion))
}

proportion <- function(urn) {
  urn['black']/sum(urn)
}

draw.ball <- function(urn) {
  prob.black <- proportion(urn)
  drawn.ball <- c('white','black')[1*(runif(1)<prob.black)+1]
  urn[drawn.ball] <- urn[drawn.ball] + 1
  urn
}

# Initial proportion of black balls
prop <- .6
theta <- prop
n.balls.to.start <- 10
n.draws <- 100
precision <- 10000

doit <- function(precision,prop) {
  draws <- numeric(precision)
  for ( i in 1:precision ) {
    urn <- new.urn(n.balls.to.start,prop)
    for ( j in 1:n.draws ) {
      urn <- draw.ball(urn)
    }
    draws[i] <- proportion(urn)
  }
  draws
}



# 1. If the level of significance is 0.05, what is the rejection 
# region for this problem? 

draws.5 <- doit(precision,.5)
n.hyp <- qnorm(1-.05)*sd(draws.5) + mean(draws.5)
ci.5 <- n.hyp + c(-1,1)*sd(draws.5)/sqrt(precision)

cat(sprintf("The estimated rejection region is the area to the right of %4.5f with a 95%% Monte Carlo error of (%4.5f,%4.5f).",n.hyp,ci.5[1],ci.5[2]))

# 2. If the true proportion of black balls is 0.6, what is the 
# power of the test for the null hypothesis that the proportion 
# of black balls is 0.5, versus the one-side alternative that 
# the proportion is greater than 0.5? 

draws.6 <- doit(precision,.6)
power.6 <- mean(draws.6>=n.hyp)
ci.6 <- mean(draws.6>=n.hyp) + c(-1,1)*sd(draws.6)/sqrt(precision)

cat(sprintf("The estimated power when the true proportion is 0.6 is %4.5f with a 95%% Monte Carlo error of (%4.5f,%4.5f).",power.6,ci.6[1],ci.6[2]))

# 3. What is the power if the true proportion is 0.7? 

draws.7 <- doit(precision,.7)
power.7 <- mean(draws.7>=n.hyp)
ci.7 <- mean(draws.7>=n.hyp) + c(-1,1)*sd(draws.7)/sqrt(precision)

cat(sprintf("The estimated power when the true proportion is 0.7 is %4.5f with a 95%% Monte Carlo error of (%4.5f,%4.5f).",power.7,ci.7[1],ci.7[2]))

plot(density(draws.5),ylim=c(0,4))
lines(density(draws.6),col="red")
lines(density(draws.7),col="blue")
abline(v=n.hyp,lwd=2)

