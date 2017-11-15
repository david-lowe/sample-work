library(parallel)
n.cores <- 1
alpha <- .05
alt <- c(0,0,1,2,3)
n <- c(15,30,80)
N <- 100

norm.data <- lapply(n, function(i){
  mclapply(1:N, function(j){
    matrix(rnorm(length(alt)*i,alt),byrow=T,ncol=length(alt),nrow=i)
  }, mc.cores=n.cores)
})

beta.data <- lapply(n, function(i){
  mclapply(1:N, function(j){
    matrix(rbeta(length(alt)*i,.1,1+alt),byrow=T,ncol=length(alt),nrow=i)
  }, mc.cores=n.cores)
})

t.stat <- function(data,mu){
  t.stat <- t.test(data[,1],data[,mu],paired=T)
  p.values <- t.stat$p.value
}

pwr <- lapply(1:length(n), function(i){
  p.values <- lapply(2:length(alt), function(j){
    mcmapply(function(k){
      t.stat(k,j)
    },norm.data[[i]], mc.cores=n.cores)
  })
  mclapply(p.values, function(l){
    mean(l < alpha/2 | l > (1-alpha/2))
  }, mc.cores=n.cores)
})

## breaks here
perm.t.test <- function(dat,mu){
  nsim2 <- 100
  permute <- matrix(sample(c(TRUE,FALSE),nrow(dat)*nsim2,replace=TRUE),nrow=nsim2,ncol=n)
  dat <- dat[,c(1,mu)]
  
  some.datasets <- lapply(permute,function(l) {
    dat[permute[i,],] <- dat[permute[i,],c(2,1)]
    dat
  })
  perm.t.stat <- sapply(some.datasets, function(x) t.test(x[,1],x[,2],paired=T)$statistic)
}

norm.c.vals <- matrix(0,nrow=length(n),ncol=2)
for(i in 1:3){
  norm.c.vals[i,] <- quantile(sapply(norm.data[[i]],function(k){
    t.test(k[,1],k[,2],paired=T)$statistic
    }), c(alpha/2,1-alpha/2)
  )
}

norm.power <- function(t.stat,N){
  mean(t.stat < norm.c.vals[N,1] | t.stat > norm.c.vals[N,2])
}

norm.perm.power <- matrix(0,nrow=length(n),ncol=4)
for(i in 1:3){  # n's
  norm.perm.power[i,] <- mcmapply(function(j){
    ts <- sapply(norm.data[[i]],function(x) perm.t.test(x,j))
    norm.power(ts,i)
  },2:5,mc.cores=n.cores)
}




