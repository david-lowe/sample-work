gene <- read.table("https://mheaton.byu.edu/Courses/Stat536/Case%20Studies/GeneExpression/Data/GeneExpression2.txt",header=TRUE)
x=model.matrix(Malignant~.,gene)[,-1]
y=gene$Malignant

library(glmnet)
grid=10^seq(10,-5,length=500)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
set.seed(1)
train=sample(1:nrow(x), nrow(x)/10)
test=(-train)
y.test=y[test]
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

# The Lasso

library(glmnet)
grid=10^seq(10,-5,length=500)
n.iter <- 1000
lam.vec <- rep(0,n.iter)
msep <- rep(0,n.iter)

for(i in 1:n.iter){
  lasso.mod=glmnet(x,y,alpha=1,lambda=grid)
  train=sample(1:nrow(x), nrow(x)*.9)
  test=(-train)
  y.test=y[test]
  cv.out=cv.glmnet(x[train,],y[train],alpha=1)
  bestlam=cv.out$lambda.min
  lam.vec[i] <- bestlam
  lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
  msep[i] <- mean((lasso.pred-y.test)^2)
}

# fixing lambda to .0255

bestlam <- median(lam.vec)
hist(msep)

out=glmnet(x,y,alpha=1,lambda=bestlam)
coef.hat <- predict(out,type="coefficients",s=bestlam)[1:ncol(gene),]


n.iter <- 10000
coef <- matrix(NA,byrow=TRUE,nrow=n.iter,ncol=ncol(gene))

set.seed(22117)
for(i in 1:n.iter){
  rows <- sample(1:nrow(gene),nrow(gene),replace=TRUE)
  new.x <- x[rows,]
  new.y <- y[rows]
  out <- glmnet(new.x,new.y,alpha=1,lambda=bestlam)
  coef[i,] <- predict(out,type="coefficients",s=bestlam)[1:ncol(gene),]
}

coef.matrix <- matrix(0,byrow=TRUE,nrow=ncol(gene),ncol=4)
for(i in 1:ncol(gene)){
  bounds <- 2*coef.hat[i] - quantile(coef[,i],c(.975,.025)) 
  coef.matrix[i,1:3] <- c(mean(coef[,i]),bounds)
  coef.matrix[i,4] <- !isTRUE(bounds[1] <= 0 && bounds[2] >= 0)
}

sum(coef.matrix[,4] == 1)
coef.sig <- as.factor(c("Intercept", colnames(gene)[which(coef.matrix[,4] == 1)])[-2])

coef.table <- data.frame(coef.sig,coef.matrix[which(coef.matrix[,4]==1),1:3])
colnames(coef.table) <- c("Gene","Estimate","Lower 2.5%","Upper 97.5%")


library(xtable)
xtable(coef.table, digits=4)

# 1,3,6,8,11,12,16,20,21,31,348,4511
# > colnames(gene[,-1])[which(coef.matrix[,4]==1)]
# [1] "X1260_s_at"  "X31639_f_at" "X32179_s_at" "X32783_at"   "X33921_at"   "X34243_i_at" "X36311_at"  
# [8] "X38087_s_at" "X38263_at"   "X41693_r_at" "X41319_at"   "X33175_at"





###################################
###### Logit Transformation #######
###################################


library(glmnet)

x=model.matrix(Malignant~.,gene)[,-1]
y=log(gene$Malignant/(1-gene$Malignant))

grid=10^seq(10,-5,length=500)
n.iter <- 100
lam.vec <- rep(0,n.iter)

for(i in 1:n.iter){
  lasso.mod=glmnet(x,y,alpha=1,lambda=grid)
  #  plot(lasso.mod, xvar = "lambda", label = TRUE)
  #  plot(lasso.mod)
  #set.seed(1)
  train=sample(1:nrow(x), nrow(x)*.9)
  test=(-train)
  y.test=y[test]
  cv.out=cv.glmnet(x[train,],y[train],alpha=1)
  #  plot(cv.out)
  bestlam=cv.out$lambda.min
  lam.vec[i] <- bestlam
}

bestlam <- mean(lam.vec)

out=glmnet(x,y,alpha=1,lambda=bestlam)
coef.hat <- predict(out,type="coefficients",s=bestlam)[1:ncol(gene),]

a <- proc.time()[3]
n.iter <- 10000
coef <- matrix(NA,byrow=TRUE,nrow=n.iter,ncol=ncol(gene))
for(i in 1:n.iter){
  rows <- sample(1:nrow(gene),nrow(gene),replace=TRUE)
  new.x <- x[rows,]
  new.y <- y[rows]
  out=glmnet(new.x,new.y,alpha=1,lambda=bestlam)
  coef[i,] <- predict(out,type="coefficients",s=bestlam)[1:ncol(gene),]
}
b <- proc.time()[3]
b-a

coef.matrix <- matrix(0,byrow=TRUE,nrow=ncol(gene),ncol=4)
for(i in 1:ncol(gene)){
  bounds <- 2*coef.hat[i] - quantile(coef[,i],c(.975,.025)) 
  coef.matrix[i,1:3] <- c(mean(coef[,i]),bounds)
  coef.matrix[i,4] <- !isTRUE(bounds[1] <= 0 && bounds[2] >= 0)
}

sum(coef.matrix[,4] == 1)
coef.sig <- colnames(gene[,-1])[which(coef.matrix[,4] == 1)]

coef.table <- data.frame(coef.sig,coef.matrix[which(coef.matrix[,4]==1),1:3])
colnames(coef.table) <- c("Gene","Estimate","Lower 2.5%","Upper 97.5%")
library(xtable)
xtable(coef.table)



plot(gene$X1002_f_at, gene$X1003_s_at,xlab="X1002_f_at",ylab="X1003_s_at",pch=19)
cor(gene$X1002_f_at, gene$X1003_s_at)


