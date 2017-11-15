r.norm <- function(n,mu=0,sigma=1){
  u1 <- runif(n%/%2,0,1)
  u2 <- runif(n%/%2,0,1)  
  x1 <- mu + sigma*sqrt(-2*log(u1))*cos(2*pi*u2)
  x2 <- mu + sigma*sqrt(-2*log(u1))*sin(2*pi*u2)
  
  if(n%%2==1){
    u1 <- runif(1,0,1)
    u2 <- runif(1,0,1)
    x1 <- c(x1, mu + sigma*sqrt(-2*log(u1))*cos(2*pi*u2))
  }
  
  x <- c(x1,x2)
  x
}

r.pois <- function(n,lambda){
  x <- rep(0,n)
  for(i in 1:n){
    u <- runif(1,0,1)
    j <- 1
    x.i <- 0
    while(u > exp(-lambda)){
      u.i <- runif(1,0,1)
      u <- u*u.i
      j <- j + 1
    }
    x.i <-  j - 1
    x[i] <- x.i
  }  
  x
}

# rgamma
r.gamma <- function(n, shape, rate=1, scale=1/rate){
  x <- rep(-1,n)
  if(shape >= 1){
    for(i in 1:n){
      while(x[i]==-1){
        d <- shape - (1/3)
        c <- 1/sqrt(9*d)
        z <- r.norm(1,0,1)
        u <- runif(1,0,1)
        v <- (1+c*z)^3
        if(z > (-1/c) && log(u) < (.5*z^2 + d - d*v + d*log(v))){
          x[i] <- (d*v)*scale
        }
      }
    }
  } else{
    for(i in 1:n){
      while(x[i]==-1){
        d <- shape - (1/3) + 1
        c <- 1/sqrt(9*d)
        z <- r.norm(1,0,1)
        u1 <- runif(1,0,1)
        u2 <- runif(1,0,1)
        v <- (1+c*z)^3
        if(z > (-1/c) && log(u1) < (.5*z^2 + d - d*v + d*log(v))){
          x[i] <- ((d*v)*u2^(1/shape))*scale
        }
      }
    }
  }
  x
}

r.nbinom <- function(n,s,p){
  x <- rep(0,n)
  for(i in 1:n){
    lambda <- r.gamma(1,s,scale=(1-p)/p)
    x[i] <- r.pois(1,lambda)
  }
  x
}


r.beta <- function(n,a,b,ncp=0){
  if(ncp != 0){stop("Non-Centrality Parameter must be set to 0")}
  y <- r.gamma(n,a,1)
  z <- r.gamma(n,b,1)
  y/(y + z)
}
