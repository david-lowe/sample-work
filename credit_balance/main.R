credit <- read.csv("C:/Users/David Lowe/Documents/BYU/Old Classes/STAT 536/Credit/Credit.csv",header=T)
credit.log <- data.frame(credit[,1:ncol(credit)-1],"Balance"=log(credit[,11]))
pairs(credit.log)

plot(credit$Limit,credit$Rating,xlab="Limit",ylab="Rating",main="Limit vs. Rating")
vif(lm(Balance~.,data=credit))

avPlots(lm(Balance~.,data=credit))


summary(lm(Balance~.,data=credit.log[,-2])) # Adjusted R2 of .7678 (exluding Limit)
summary(lm(Balance~.,data=credit.log[,-3])) # Adjusted R2 of .7758 (exluding Rating)

interaction.plot(credit$Income,credit$Student,credit$Balance,type="p",col=c("skyblue","red"),pch=19,cex=.5,legend=TRUE)

#install.packages("leaps")
library(leaps)
library(car)
subsets <- regsubsets(Balance~.,data=credit[,-3],method=c("exhaustive","forward","backward"))
summary(subsets)
plot(subsets,scale="r2") # suggests Income, Limit, Student
plot(subsets,scale="adjr2") # suggests Income, Limit, Student
plot(subsets,scale="Cp") # suggests Income, Limit, Cards, Age, Student
plot(subsets,scale="bic") # suggests Limit, Cards, Age, Student

mod1 <- lm(Balance~Income+Limit+Cards+Age+Student+Student*Income,data=credit)
summary(mod1)
round(confint(mod1),4)

plot(mod1)
hist(mod1$residuals,xlim=c(-40,40),main="Histogram of Residuals",xlab="Residuals",freq=FALSE)




plot(fitted(mod1),rstudent(mod1),ylim=c(-3,3),xlab="Fitted Residuals",ylab="Studentized Residuals",main="Fitted Values vs. Residuals")
abline(h=0,col="blue")


## plot residuals
plot(rstudent(mod1))

## cross validation
cv <- function(){
  test.rows <- sample(1:nrow(credit),round(.1*nrow(credit)))
  test <- credit[test.rows,]
  training <- credit[-test.rows,]
  cv.mod <- lm(Balance~Income+Limit+Cards+Age+Student,data=training)
  pred <- predict.lm(cv.mod,test,type="response",interval="prediction")
  pred.vals <- cbind(test$Balance,pred)
  coverage <- mean(pred.vals[,1]>pred.vals[,3] & pred.vals[,1]<pred.vals[,4])
  int.width <- mean(pred.vals[,4] - pred.vals[,3])
  bias <- mean(pred.vals[,2] - pred.vals[,1])
  mse <- var(pred.vals[,2]) + bias^2
  cbind(coverage,int.width,bias,mse)
}

n.iter <- 10000
cv.sim <- data.frame(matrix(0,byrow=TRUE,nrow=n.iter,ncol=4))
for(i in 1:n.iter){
  cv.sim[i,] <- cv()
}



colnames(cv.sim) <- c("Coverage","Int_Width","Bias","MSE")
par(mfrow=c(2,2),mar=rep(2,4))
hist(cv.sim$Coverage)
hist(cv.sim$Int_Width)
hist(cv.sim$Bias)



sec <- 12529190
min <- sec/60
hours <- min/60
days <- hours/24
weeks <- days/7
months <- weeks/4






