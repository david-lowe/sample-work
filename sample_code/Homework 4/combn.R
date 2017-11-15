comb <- function(N,K){
  if(K%%1 != 0 || K < 1 || K > N){stop("K must be an integer greater than or equal to one, and must be less than N.")}
  if(N%%1 != 0 || N < 1){stop("N must be an integer greater than or equal to one.")}
  if(K == 1 && N > 1){
    x <- t(combn(1:N,K))
    y <- data.frame(x[sample(1:nrow(x),nrow(x)),])
    colnames(y) <- rownames(y) <- NULL
    print(y, row.names = FALSE)
  }else{
    x <- (combn(1:N,K))
    y <- split(x,rep(1:ncol(x),each=nrow(x)))
    z <- t(sapply(y,sample))
    a <- z[sample(nrow(z)),]
    if(N==K){
      b <- as.data.frame(t(a))
    }else{
      b <- as.data.frame(a)
    }
    names(b) <- rep(" ",ncol(b))
    print(b, row.names=FALSE)
  }
}

N <- 10
K <- 3
comb(N,K)

