source("poisson-regression.R")
library(foreach)
library(doParallel)
x <- c(0.91, 2.34, 2.54, 1.57, 1.63, 1.08, 0.30, 0.90, 0.08, 2.01, 1.5, 1.2)
beta <- seq(0,.8,.05)
n.iter <- 1000

y <- lapply(beta,function(b){
       lapply(1:n.iter,function(j) rpois(length(x),exp(x*b)))
})

registerDoParallel(4)

beta.hat <- foreach(b=1:length(beta)) %:% 
              foreach(j=1:n.iter, .combine='rbind') %dopar% {
                wilks <- posreg(y[[b]][[j]],x,c("mle","wilks"),alpha=.05)[1]
                squared <- posreg(y[[b]][[j]],x,c("bayes","squared","normal",.4,.2),alpha=.05)[1]
                zero.one <- posreg(y[[b]][[j]],x,c("bayes","0-1","uniform",-5,5),alpha=.05)[1]
                c(wilks,squared,zero.one)
              }

bias <- lapply(1:length(beta),function(b){
          apply(beta.hat[[b]],2,function(z) z - beta[b])
})

mse <- foreach(b=1:length(beta)) %:% 
         foreach(j=1:n.iter, .combine='rbind') %dopar% {
           wilks <- sum((exp(beta.hat[[b]][[j,1]]*x)-y[[b]][[j]])^2)/(length(x)-1)
           squared <- sum((exp(beta.hat[[b]][[j,2]]*x)-y[[b]][[j]])^2)/(length(x)-1)
           zero.one <- sum((exp(beta.hat[[b]][[j,3]]*x)-y[[b]][[j]])^2)/(length(x)-1)
           c(wilks,squared,zero.one)
         }


beta.est <- lapply(1:length(beta),function(b){
              apply(beta.hat[[b]],2,mean)
})

bias.est <- lapply(1:length(beta),function(b){
              apply(bias[[b]],2,mean)
})

mse.est <- lapply(1:length(beta),function(b){
             apply(mse[[b]],2,mean)
})

wilks.results <- matrix(0,nrow=length(beta),ncol=4,byrow=TRUE)
squared.results <- matrix(0,nrow=length(beta),ncol=4,byrow=TRUE)
zero.one.results <- matrix(0,nrow=length(beta),ncol=4,byrow=TRUE)

for(i in 1:length(beta)){
  wilks.results[i,] <- c(beta[i],beta.est[[i]][[1]],bias.est[[i]][[1]],mse.est[[i]][[1]])
  squared.results[i,] <- c(beta[i],beta.est[[i]][[2]],bias.est[[i]][[2]],mse.est[[i]][[2]])
  zero.one.results[i,] <- c(beta[i],beta.est[[i]][[3]],bias.est[[i]][[3]],mse.est[[i]][[3]])
}

bias.bounds <- range(wilks.results[,3],squared.results[,3],zero.one.results[,3])
mse.bounds <- range(wilks.results[,4],squared.results[,4],zero.one.results[,4])

pdf("bias.pdf")
plot(beta,wilks.results[,3],xlab=expression(beta),ylab="Bias",pch=15,cex=1.5,col="orange",ylim=bias.bounds,main="Bias Comparison")
points(beta,squared.results[,3],pch=17,cex=1.5,col="red")
points(beta,zero.one.results[,3],pch=19,cex=1.5,col="blue")
legend("topright",c("Wilk's MLE","Squared Error","0-1"),cex=1.5,pch=c(15,17,19),col=c("orange","red","blue"))
dev.off()

pdf("mse.pdf")
plot(beta,wilks.results[,4],xlab=expression(beta),ylab="MSE",pch=15,cex=1.5,col="orange",ylim=mse.bounds,main="Mean Squared Error \n Comparison")
points(beta,squared.results[,4],pch=17,cex=1.5,col="red")
points(beta,zero.one.results[,4],pch=19,cex=1.5,col="blue")
legend("topleft",c("Wilk's MLE","Squared Error","0-1"),cex=1.5,pch=c(15,17,19),col=c("orange","red","blue"))
dev.off()


