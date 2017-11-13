emp <- read.table("C:/Users/David/Desktop/STAT 536/Employee/Employee.txt",header=TRUE)[,-1]

mod.age <- lm(emp$Age~.,data=emp)
mod.ten <- lm(emp$Tenure~.,data=emp)
mod.wb <- lm(emp$WellBeing~.,data=emp)
mod.js <- lm(emp$JobSat~.,data=emp)
mod.jp <- lm(emp$JobPerf~.,data=emp)
mod.iq <- lm(emp$IQ~.,data=emp)

par(mfrow=c(3,2),mar=rep(2.5,4))
qqnorm(mod.age$residuals,main = "Age ~ Others")
qqline(mod.age$residuals,col="red")
qqnorm(mod.ten$residuals,main = "Tenure ~ Others")
qqline(mod.ten$residuals,col="red")
qqnorm(mod.wb$residuals,main = "Wellbeing ~ Others")
qqline(mod.wb$residuals,col="red")
qqnorm(mod.js$residuals,main = "Job Satisfaction ~ Others")
qqline(mod.js$residuals,col="red")
qqnorm(mod.jp$residuals,main = "Job Performance ~ Others")
qqline(mod.jp$residuals,col="red")
qqnorm(mod.iq$residuals,main = "IQ ~ Others")
qqline(mod.iq$residuals,col="red")


library(mice)
library(xtable)
xtable(md.pattern(emp))
length(emp[,1])
apply(na.omit(emp),2,min)
apply(na.omit(emp),2,max)


makeFunctions <- function(new.dataset){
  
  mu <- as.vector(colMeans(new.dataset,na.rm=TRUE))
  v <- cov(new.dataset,use='pairwise.complete.obs')
  
  # wb is missing
  cov1 <- t(as.vector(v[-3,3]))
  v1.inv <- solve(v[-3,-3])
  mu1 <- mu[3]
  mu1.cond <- mu[-3]
  var.wb <- var(new.dataset[,3],na.rm=TRUE) - cov1%*%v1.inv%*%t(cov1)
  
  cond1 <- function(vec){
    mu.wb <- mu1 + cov1%*%v1.inv%*%(vec[-3] - mu1.cond)
    list(mu.wb,var.wb)
  }

  # js is missing
  cov2 <- t(as.vector(v[-4,4]))
  v2.inv <- solve(v[-4,-4])
  mu2 <- mu[4]
  mu2.cond <- mu[-4]
  var.js <- var(new.dataset[,4],na.rm=TRUE) - cov2%*%v2.inv%*%t(cov2)
  
  cond2 <- function(vec){
    mu.js <- mu2 + cov2%*%v2.inv%*%(vec[-4] - mu2.cond)
    list(mu.js,var.js)
  }
  
  #jp is missing
  cov3 <- t(as.vector(v[-5,5]))
  v3.inv <- solve(v[-5,-5])
  mu3 <- mu[5]
  mu3.cond <- mu[-5]
  var.jp <- var(new.dataset[,5],na.rm=TRUE) - cov3%*%v3.inv%*%t(cov3)
  
  cond3 <- function(vec){
    mu.jp <- mu3 + cov3%*%v3.inv%*%(vec[-5] - mu3.cond)
    list(mu.jp,var.jp)
  }
  
  # wb and jp are missing
  cov4 <- t(as.matrix(v[-c(3,5),c(3,5)])) 
  v4.inv <- solve(v[-c(3,5),-c(3,5)])
  mu4 <- mu[c(3,5)]
  mu4.cond <- mu[-c(3,5)]
  var.wb.jp <- var(new.dataset[,c(3,5)],na.rm=TRUE) - cov4%*%v4.inv%*%t(cov4)
  
  cond4 <- function(vec){
    mu.wb.jp <- mu4 + cov4%*%v4.inv%*%(vec[-c(3,5)] - mu4.cond)
    list(mu.wb.jp,var.wb.jp)
  }
  
  list(cond1=cond1,cond2=cond2,cond3=cond3,cond4=cond4)
}


a <- proc.time()[3]
library(mvtnorm)
miss.dat <- emp
n.iter <- 100
mult.imp <- lapply(1:n.iter,function(i){
  func <- makeFunctions(miss.dat)
  miss.dat <- emp
  for(i in 1:nrow(miss.dat)){
    rw <- as.matrix(miss.dat[i,])
    if(length(which(is.na(rw))) > 0){
      if(length(which(is.na(rw))) == 2){
        params <- func$cond4(rw)
      } else if(which(is.na(rw)) == 3){
        params <- func$cond1(rw)
      } else if(which(is.na(rw)) == 4){
        params <- func$cond2(rw)
      } else if(which(is.na(rw)) == 5){
        params <- func$cond3(rw)
      } 
      miss.dat[i,which(is.na(rw))] <- rmvnorm(1,params[[1]],params[[2]])
    } 
  }
  dat <- miss.dat
  new.mod <- summary(lm(JobPerf~.,data=miss.dat))
  coef <- new.mod$coef[,c(1,2)]
  r2 <- new.mod$r.squared
  list(dat=dat,coef=coef,r2=r2,rmse=rmse)
})
b <- proc.time()[3]
b-a

coef <- matrix(0,nrow=n.iter,ncol=ncol(emp))
se <- matrix(0,nrow=n.iter,ncol=ncol(emp))
r2 <- rep(0,n.iter)

for(i in 1:n.iter){
  coef[i,] <- t(mult.imp[[i]]$coef[,1])
  se[i,] <- t(mult.imp[[i]]$coef[,2])
  r2[i] <- mult.imp[[i]]$r2
}

coef.means <- colMeans(coef)
v.w <- apply(se,2,function(x) sum(x^2)/n.iter)
v.b <- rep(0,ncol(coef))
for(i in 1:ncol(coef)){
  v.b[i] <- sum((coef[,i] - coef.means[i])^2)/(n.iter-1)
}

v.t <- v.w + v.b + v.b/n.iter
se.pooled <- sqrt(v.t)

par(mfrow=c(3,2),mar=rep(2.5,4))

plot(coef[,1],type='l',main="expression(beta[0])")
plot(coef[,2],type='l',main=expression(beta[1]))
plot(coef[,3],type='l',main=expression(beta[2]))
plot(coef[,4],type='l',main=expression(beta[3]))
plot(coef[,5],type='l',main=expression(beta[4]))
plot(coef[,6],type='l',main=expression(beta[5]))

# Histogram of the Betas
hist(coef[,1],main="Intercept")
hist(coef[,2],main="Age")
hist(coef[,3],main="Tenure")
hist(coef[,4],main="Wellbeing")
hist(coef[,5],main="Job Satisfaction")
hist(coef[,6],main="IQ")

hist(r2)

fmi <- (v.b + v.b/n.iter)/v.t

t <- coef.means/se.pooled
df <- (n.iter - 1)*(1/fmi^2)
pvals <- round(1-pt(abs(t),df),digits=4)*2
t.star <- qt(.975,df)


ci.coef <- matrix(0,nrow=6,ncol=2)
for(i in 1:6){
  ci.coef[i,1] <- coef.means[i] - t.star[i]*se.pooled[i]
  ci.coef[i,2] <- coef.means[i] + t.star[i]*se.pooled[i]
}


save.image("C:/Users/David/Desktop/STAT 536/Employee/myworkspace.RData")
load("C:/Users/David/Desktop/STAT 536/Employee/myworkspace.RData")


# model diagnostics
library(car)
base.mod <- lm(JobPerf~.,data=emp)
avPlots(base.mod,main=NULL,cex.lab=1.5)
plot(base.mod,main=NULL,cex.lab=1.5)



