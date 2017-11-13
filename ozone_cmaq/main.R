setwd("C:/Users/David Lowe/Desktop/STAT 536/Midterm")

library(maps)
library(LatticeKrig)
library(nlme)
library(car)
library(xtable)

ozone <- read.csv("Ozone.csv",header=TRUE)[,2:4]
cmaq <- read.csv("CMAQ.csv",header=TRUE)[,c(1:2,4)]
predloc <- read.csv("PredLocs.csv",header=TRUE)

ozone <- data.frame(ozone[,1:2],'ozone'=ozone[,3]) # ,'cmaq'=NA
cmaq <- data.frame(cmaq[,1:2],'cmaq'=cmaq[,3]) # 'ozone'=NA,


###########################################
########## Exploring the Data #############
###########################################

quilt.plot(ozone$Longitude,ozone$Latitude,ozone$ozone,xlab="Longitude",ylab="Latitude")
map('state',add=TRUE)

quilt.plot(cmaq$Longitude,cmaq$Latitude,cmaq$cmaq,xlab="Longitude",ylab="Latitude")
map('state',add=TRUE)


##############################################
####### Principal Component Regression #######
##############################################



###########################################
######## CMAQ Distance Selection ##########
###########################################

K <- 20
nearest.K <- matrix(0,nrow=nrow(ozone),ncol=K)
ord.K <- matrix(0,nrow=nrow(ozone),ncol=K)
for(i in 1:nrow(ozone)){
  dists <- rdist.earth(ozone[i,1:2],cmaq[,1:2],miles=TRUE)
  ord <- order(dists)[1:K]
  ord.K[i,] <- sort(as.vector(dists))[1:K]
  nearest.K[i,] <- cmaq[ord,3]
}

colnames(nearest.K) <- paste("c",1:K,sep="")
x.mat <- as.matrix(nearest.K)
dat <- cbind(ozone,x.mat)
avg.dist <- t(t(colMeans(ord.K)))
print(xtable(t(avg.dist[1:6,]),digits=2), include.rownames=FALSE)

mod.fit <- t(sapply(1:ncol(x.mat),function(i){
  fit <- lm(ozone~as.matrix(x.mat[,1:i]),dat)
  fit.aic <- AIC(fit)
  fit.bic <- BIC(fit)
  fit.rsq <- summary(fit)$r.squared
  fit.adjr2 <- summary(fit)$adj.r.squared
  results <- cbind(fit.aic,fit.bic,fit.rsq,fit.adjr2)
  results
}))

colnames(mod.fit) <- c("AIC","BIC","R2","AdjR2")
par(mfrow=c(2,1))
plot(mod.fit[,1],type="l",lwd=2,col="steelblue3",xlab="Number of Closest CMAQ measurements",ylab="Information Criterion")
lines(mod.fit[,2],type="l",lwd=2,col="firebrick3")
abline(v=which.min(mod.fit[,2]),lty=2,col="springgreen3")
plot(mod.fit[,3],type="l",lwd=2,col="steelblue3",xlab="Number of Closest CMAQ measurements",ylab=expression(R^2))
lines(mod.fit[,4],type="l",lwd=2,col="firebrick3")
abline(v=which.min(mod.fit[,2]),lty=2,col="springgreen3")
par(mfrow=c(1,1))

# choosing first 6 as the most useful for understanding ozone

x.mat <- x.mat[,1:6]
dat <- cbind(ozone,x.mat)
final.fit <- lm(ozone~c1+c2+c3+c4+c5+c6,dat)
avPlots(final.fit)
vif.cmaq <- t(t(vif(final.fit)))
colnames(x.mat) <- c("CMAQ_1","CMAQ_2","CMAQ_3","CMAQ_4","CMAQ_5","CMAQ_6")
pairs(x.mat,pch=19,cex=.5)

###########################################
### Choosing the Number of Components #####
###########################################

pc.vals <- eigen(cov(x.mat))$values
cumsum(pc.vals)/sum(pc.vals) # one component explains 94% of the X's
pc <- eigen(cov(x.mat))$vectors[,1]
z <- as.matrix(x.mat)%*%pc
# assess linearity 


###########################################
########### Fitting the Model #############
###########################################

fit.pcr <- gls(ozone~z,correlation=corExp(form=~Longitude+Latitude,nugget=TRUE),method="ML",data=dat)
summary(fit.pcr)


###########################################
########## Verifying Assumptions ##########
###########################################

plot(dat$c1,dat$ozone,pch=19,cex=.7,
     xlab=expression(paste(1^{st}," Closest CMAQ Measurement",sep="")),ylab="Ozone")


###########################################
############# Beta Estimates ##############
###########################################

bhat <- c(fit.pcr$coefficients[1],as.vector(t(t(pc))%*%fit.pcr$coefficients[2]))
bhat.se <- c(sqrt(vcov(fit.pcr)[1,1]),sqrt(diag(t(t(pc))%*%vcov(fit.pcr)[2,2]%*%t(pc))))
n <- nrow(dat)
p <- length(bhat)
bhat.ul <- bhat + qt(.975,n-p-1)*bhat.se
bhat.ll <- bhat - qt(.975,n-p-1)*bhat.se
bhat.summary <- cbind(bhat,bhat.ll,bhat.ul)
print(xtable(bhat.summary,digits=4), include.rownames=FALSE)

###########################################
###### Prediction Cross Validation ########
###########################################

cv.pc <- function(pcx){
  train <- sample(1:nrow(dat),nrow(dat)*.75)
  test.dat <- dat[-train,]
  train.dat <- dat[train,]
  z.train <- as.matrix(pcx[train,])
  cv.mod <- gls(ozone~z.train,
                correlation=corExp(form=~Longitude+Latitude,nugget=TRUE),
                method="ML",data=train.dat)
  Zstar <- cbind(1,pcx[-train,])
  bhat <- t(t(cv.mod$coefficients))
  Z <- cbind(1,z[train,])
  Y <- train.dat[,3]
  sig2 <- (cv.mod$sigma)^2
  phi <- coef(cv.mod$modelStruct$corStruct,unconstrained=FALSE)[1]
  nug <- coef(cv.mod$modelStruct$corStruct,unconstrained=FALSE)[2]
  N <- nrow(train.dat) #Number of observed time periods
  K <- nrow(test.dat) #number of time periods forward
  R <- exp(-as.matrix(dist(rbind(train.dat[,1:2],test.dat[,1:2])))/phi)
  R <- (1-nug)*R+nug*diag(N+K) ##Exp correlation with nugget
  pred.mn <- Zstar%*%bhat + R[N+(1:K), (1:N)]%*%solve(R[(1:N),(1:N)])%*%
    (Y-Z%*%bhat) #conditional mean of MVN
  pred.var <- sig2*(R[N+(1:K),N+(1:K)]-R[N+(1:K), (1:N)]%*%solve(R[(1:N),(1:N)])
                    %*%R[(1:N),N+(1:K)]) # conditional variance of MVN
  pred.lb <- pred.mn - qnorm(.975)*sqrt(diag(pred.var))
  pred.ub <- pred.mn + qnorm(.975)*sqrt(diag(pred.var))
  pred.vals <- cbind(test.dat$ozone,pred.mn,pred.lb,pred.ub)
  coverage <- mean(pred.vals[,1]>pred.vals[,3] & pred.vals[,1]<pred.vals[,4])
  int.width <- mean(pred.vals[,4] - pred.vals[,3])
  bias <- mean(pred.vals[,2] - pred.vals[,1])
  rmse <- sqrt(mean((pred.vals[,2]-pred.vals[,1])^2))
  cbind(coverage,int.width,bias,rmse)
}



cv.diag <- function(cv.sim){
  par(mfrow=c(2,2),mar=c(5,4,1,1))
  # coverage
  hist(cv.sim[,1],xlab="Coverage",main=NULL,freq=FALSE)
  # interval width
  hist(cv.sim[,2],xlab="Interval Width",main=NULL,freq=FALSE)
  # bias
  hist(cv.sim[,3],xlab="Bias",main=NULL,freq=FALSE)
  # msep
  hist(cv.sim[,4],xlab=expression(sqrt(MSE)),main=NULL,freq=FALSE)
  par(mfrow=c(1,1))
  colMeans(cv.sim)
}

n.iter <- 500
a <- proc.time()[3]
cvpc.sim <- data.frame(matrix(0,byrow=TRUE,nrow=n.iter,ncol=4))
for(i in 1:n.iter){
  cvpc.sim[i,] <- cv.pc(z)
}
colnames(cvpc.sim) <- c("coverage","iw","bias","rmse")
save.image(file="midterm.RData")
b <- proc.time()[3]

load("midterm.RData")
cv.diag(cvpc.sim)


###########################################
######## Final Ozone Predictions ##########
###########################################

K <- 6
nearest.K <- matrix(0,nrow=nrow(predloc),ncol=K)
for(i in 1:nrow(predloc)){
  dists <- rdist(predloc[i,],cmaq[,1:2])
  ord <- order(dists)[1:K]
  nearest.K[i,] <- cmaq[ord,3]
}

colnames(nearest.K) <- paste("c",1:K,sep="")
pred.dat <- cbind(predloc,nearest.K)
pred.x.mat <- dat[,4:ncol(pred.dat)]

Xstar <- cbind(1, nearest.K)
X <- cbind(1,x.mat)
Y <- dat$ozone
sig2 <- (fit.pcr$sigma)^2
phi <- coef(fit.pcr$modelStruct$corStruct,unconstrained=FALSE)[1]
nug <- coef(fit.pcr$modelStruct$corStruct,unconstrained=FALSE)[2]
N <- nrow(dat) #Number of observed time periods
K <- nrow(pred.dat) #number of time periods forward
R <- exp(-as.matrix(dist(rbind(dat[,1:2],pred.dat[,1:2])))/phi)
R <- (1-nug)*R+nug*diag(N+K) ##Exp correlation with nugget
pred.mn <- Xstar%*%t(t(bhat)) + R[N+(1:K), (1:N)]%*%solve(R[(1:N),(1:N)])%*%
  (Y-X%*%t(t(bhat))) #conditional mean of MVN
pred.var <- sig2*(R[N+(1:K),N+(1:K)]-R[N+(1:K), (1:N)]%*%solve(R[(1:N),(1:N)])
                  %*%R[(1:N),N+(1:K)]) # conditional variance of MVN
pred.lb <- pred.mn - qnorm(.975)*sqrt(diag(pred.var))
pred.ub <- pred.mn + qnorm(.975)*sqrt(diag(pred.var))
pred.vals <- cbind(pred.mn,pred.lb,pred.ub)


quilt.plot(ozone$Longitude,ozone$Latitude,ozone$ozone,nx=53,ny=53)
map('state',add=TRUE)

quilt.plot(predloc[,1],predloc[,2],pred.mn,nx=53,ny=53,zlim=c(22,106))
map('state',add=TRUE)

quilt.plot(predloc[,1],predloc[,2],pred.lb,nx=53,ny=53,zlim=c(10,106))
map('state',add=TRUE)

quilt.plot(predloc[,1],predloc[,2],pred.ub,nx=53,ny=53,zlim=c(22,106))
map('state',add=TRUE)
