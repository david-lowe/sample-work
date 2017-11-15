install.packages("foreach")
install.packages("doMC")
library(foreach)
library(doMC)

# set parameters
n <- c(10,15,30,50)
theta1 <- c(0,1,2,3)
theta2 <- 2
dist <- c("normal","beta")
n.iter <-  2000 # number of permutations and parametric tests performed (used to build the t distribution)
n.cores <- 1

# create a grid with all possible combinations
dist.comb <- expand.grid(n=n,theta1=theta1,theta2=theta2,dist=dist)

# function produces the actual data that we will be comparing against
get.real.data <- function(n,theta1,theta2,dist){
  if(dist=="normal"){
    rnorm(n,theta1,theta2)
  } else if(dist=="beta"){
    rbeta(n,.1,1+theta1)
  }
}

# gets a distribution for every setting in the parameter grid
real.data <- lapply(1:nrow(dist.comb),function(i){
  get.real.data(dist.comb[i,1],dist.comb[i,2],dist.comb[i,3],dist.comb[i,4])
})

# gets the real t-statistics that will be used to compare the permutation test and parametric test
real.t <- lapply(1:nrow(dist.comb),function(i) abs(t.test(real.data[[i]])$statistic))
real.size <- lapply(1:nrow(dist.comb),function(i) (t.test(real.data[[i]])$p.value)/2)

# permutes each sample data set n.iter times and produces the test statistic of each sample
perm.t <- function(data,samp){
  mclapply(1:n.iter, function(i){
    m <- sample(c(TRUE,FALSE),length(data[[samp]]),replace=TRUE)
    data[[samp]][m] <- data[[samp]][m]*(-1)
    t.test(data[[samp]])$statistic
  },mc.cores=n.cores)
}

t.perm.dist <- lapply(1:nrow(dist.comb),function(i) perm.t(real.data,i))

# creates t-statistics for n.iter samples of each distribution combination of size n
get.param.data <- function(n,theta1,theta2,dist){
  mclapply(1:n.iter, function(j){
    if(dist=="normal"){
      dat <- rnorm(n,theta1,theta2)
    } else if(dist=="beta"){
      dat <- rbeta(n,theta2/10,1+theta1)
    }
    t.test(dat)$statistic
},mc.cores=n.cores)}

t.parametric.dist <- lapply(1:nrow(dist.comb),function(i) get.param.data(dist.comb[i,1],
                                                                         dist.comb[i,2],
                                                                         dist.comb[i,3],
                                                                         dist.comb[i,4]))

# replicates the null hypothesis statistics to compare against the permutation and parametric distributions
null.t <- rep(c(unlist(real.t[1:length(n)])),nrow(dist.comb)/length(n))

# finds what proportion of the parametric distributions fall to the right of the t-statistic
power.t <- sapply(1:nrow(dist.comb), function(i){
  mean(t.parametric.dist[[i]] >= null.t[i])
})
names(power.t) <- "power.t"

# finds what proportion of the permutations distributions fall to the right of the t-statistic and to the right of the actual data
perm.size <- sapply(1:nrow(dist.comb), function(i) unlist(mean(t.perm.dist[[i]] >= real.t[[i]])))
size.t <- cbind(unlist(real.size),perm.size)
real.size <- round(unlist(real.size),4)
names(real.size) <- "real.size"

final.table <- cbind(dist.comb,real.size,perm.size,power.t)
final.table

a <- matrix(c(1,2,2,3,3,4,4,5),4,2,byrow=TRUE)
n <- as.data.frame(c(30,60,100))
merge(n,a)
