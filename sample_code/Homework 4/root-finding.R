### Root Finding Methods ###

# Example function: Expected number of clusters from Ewens distribution.
enc <- function(n,alpha) {
  result <- numeric(length(alpha))
  for ( i in seq(along=alpha) ) {
    result[i] <- sum(alpha[i]/(1:n+alpha[i]-1))
  }
  return(result)
}

# When there are 100 items and the mass parameter is 2, how many clusters
# do we expect?
enc(100,2)

# Helper function for inverting 'enc' function.
mkFunc <- function(n,target) {
  function(alpha) {
    enc(n,alpha) - target
  }
}

# When there are 100 items and we expect 6 clusters, what mass parameter
# should we use?

n <- 100
target.ENC <- 6
f <- mkFunc(n,target.ENC)




# Method 0: Iteratively guess!
# Note: Not very much fun.
f(5)
f(3)
f(1)
f(2)
f(1.5)
f(1.2)
f(1.25)
f(1.23)
f(1.235)
f(1.236)
f(1.2355)
f(1.2352)
f(1.2351)




# Method 1: Grid method lays down a grid and evaluates the function at all points.
# Notes:
#   a. Accuracy depends on the fineness of grid.
#   b. Requires many evaluations of the function.
#   c. Conceptually scales beyond univariate functions,
#      but the curse of dimensionality is a practical
#      limitation.
x <- seq(0.001,10,length=10000)   # Grid
fx <- f(x)                        # How many times is f called?  Once, but...
root <- x[which.min(abs(fx))]
root
f(root)
plot(x,fx,type="l")
abline(h=0,lty=2)
abline(v=root,lty=2)

system.time({
  f(x)
  root <- x[which.min(abs(fx))]
})




# Method 2: Bisection method sequentially reduces the width of window
# where the root is known to lie.
# Steps:
#   1. Select starting values x_0 < x_1 such that f(x_0)*f(x_1) < 0
#   2. Set x_2 = (x_0 + x_1)/2
#   3. If f(x_2)*f(x_0) < 0 set x_1 = x_2 else set x_0 = x_2
#   4. Repeat 2-3 until |x_1 - x_0| <= epsilon
#   5. Return (x_0 + x_1)/2
# Notes:
#   a. Is not easy to generalize beyond one-dimension.
#   b. Much more accurate and efficient than the grid method.


# You will provide your own implementation in your homework. :)
source("bisection.R")

# bisection function arguments:
#   1. a function taking one argument.
#   2. a value less than the root.
#   3. a value greater than the root.

# Let's make sure its working.
a <- bisection(f,0.01,10)
a

actual.ENC <- enc(100,a)
target.ENC - actual.ENC

# How many times is f called?
.count <- 0
trace(f, tracer=function() .count <<- .count +1, print=FALSE)
bisection(f,0.01,10)
.count
untrace(f)
system.time(bisection(f,0.01,10))

# Let's try it out for other inputs.
bisection(mkFunc(10,6),0.01,10)
bisection(mkFunc(1000,6),0.01,10)




# Method 3: Newton-Raphson method
# Steps:
#   1. Let x_0 be an initial guess for the root.
#   2. Compute x_{n+1} = x_n - f(x_n) / f'(x_n), where f' is the derivative of f.
#   3. Repeat step 2 for n = 0, 1, ... until |x_n - x_{n-1}| <= epsilon
#   4. Return x_n
# Notes:
#   a. Much more computational efficient than the bisection method.
#   b. Just as accurate as the bisection method.
#   d. Requires the derivative of the function.
#   c. Generalizes well to high dimensions (if you can do the math!).

# Oops, it doesn't work for this example because we don't know the derivative of f.
# But, for the sake of demonstration, consider this example instead.

g      <- function(x) x^3 - 3*x + 1
gPrime <- function(x) 3*x^2 - 3
x <- seq(-2,2,length=1000)
plot(x,g(x),type="l")
x <- 0.5    # Try these different starting values:  -1.5, 0, 0.5, 1.5, -1, 1
diff <- Inf
while ( abs(diff) > 0.0000001 ) {
  xNew <- x - g(x)/gPrime(x)
  diff <- xNew - x
  x <- xNew
}
x
g(x)
abline(h=0,lty=2)
abline(v=x,lty=2)




# Method 4: Secant method.  Like the Newton-Raphson method, but numerically
# approximate the derivative.
# Steps:
#   1. Make two initial guesses x_0 and x_1 for the root.
#   2. Compute x_{n+1} = x_n - f(x_n) ( x_n - x_{n-1} ) / ( f(x_n) - f(x_{n-1}) )
#   3. Repeat step 2 for n = 1, 2, ... until |x_n - x_{n-1}| <= epsilon
#   4. Return x_n
# Notes:
#   a. As computational efficient as the Newton-Raphson method.
#   b. Just as accurate as the bisection and Newton-Raphson method.
#   d. Does **not** require the derivative of the function.
#   c. Generalizes well to high dimensions and doesn't require a lot of math.

# You will provide your own implementation in your homework. :)
source("secant.R")

# secant function arguments:
#   1. a function taking one argument.
#   2. a guess as the value of the root.
#   3. another guess for the root.

# Back to the original example, since we don't need to derivative.
a <- secant(f,2,3)
a

actual.ENC <- enc(100,a)
target.ENC - actual.ENC


# How many times is f called?
.count <- 0
trace(f, tracer=function() .count <<- .count +1, print=FALSE)
secant(f,2,3)
.count
untrace(f)
system.time(bisection(f,0.01,10))


