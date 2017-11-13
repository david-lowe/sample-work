
# REQUIRED LIBRARIES
####################

library(splines)
library(ggplot2)
library(xtable)


# READ IN THE DATA
##################

ag <- read.csv("irrigation.csv",header = TRUE)


# EXPLORATORY DATA ANALYSIS
###########################

## plots the data with loess curve
splot <- ggplot(ag, aes(x=CWSI, y=SWC)) + geom_point() + stat_smooth() + theme_bw()
splot


# ANALYSIS
##########

## Cross Validation to get number of knots from minimized predicted mean squared error
n.iter <- 10000
msep <- matrix(0,n.iter,10)
for(j in 1:n.iter){
  train <- sample(1:nrow(ag), round(.85*nrow(ag)))
  for(i in 1:10){
    X <- ns(intercept=TRUE, ag[,2], df=i+1)
    X.pred <- X[-train,]
    X.train <- X[train,]
    bhat <- solve(t(X.train) %*% X.train) %*% t(X.train) %*% ag[train,3]
    pred <- X.pred %*% bhat
    msep[j,i] <- mean((ag[-train,3]-pred)^2)
  }
}

## plots average MSEP against knots
plot(0:9, colMeans(msep), type="b", pch=19, xlab="Knots", ylab="MSEP")


## fit the correct model with 1 knot
fit <- lm(SWC~ns(CWSI,df=2), data=ag)
summary(fit)

## prediction plot with actual values 
pred.grid <- seq(0,1,length=1001)
pred <- predict.lm(fit, 
                   newdata=data.frame(CWSI=pred.grid),
                   type="response",
                   interval="prediction")
plot(ag$CWSI, ag$SWC, 
     xlab="CWSI", ylab="SWC", 
     ylim=c(21,30), xlim=c(0,1), 
     cex=.75, pch=19, col="grey50")
lines(pred.grid, pred[,1], 
      col="dodgerblue4", lwd=2)
lines(pred.grid, pred[,2], 
      col="seagreen3",lwd=2,lty=2)
lines(pred.grid, pred[,3], 
      col="seagreen3",lwd=2,lty=2)
abline(v=diff(range(ag$CWSI))/2 + min(ag$CWSI),lty=3)
legend("topright",
       c("Best Fit Line","95% Prediction Interval","Knot Point"),
       lty=c(1,2,3),
       lwd=c(2,2,1),
       cex=.85,
       col=c("dodgerblue4","seagreen3","black"))


# CHECK ASSUMPTIONS
###################

res <- rstudent(fit)

## Normality
hist(res, breaks=5, xlab="Studentized Residuals", main=NULL, freq=FALSE)
curve(dnorm(x,0,1), type="l", add=TRUE)
qqnorm(res, main=NULL)
qqline(res)

# Equal Variance
plot(fitted(fit), res, xlab="Fitted Values", ylab="Studentized Residuals")
abline(h=0, lty=3)

# Independence
plot(res, ylab="Studentized Residuals")
abline(h=0, lty=3)


# MODEL DIAGNOSTICS
###################

## cross-validation
cv <- function(){
  train <- sample(1:nrow(ag), round(.85*nrow(ag)))
  train.dat <- ag[train,]
  test.dat <- ag[-train,]
  cv.mod <- lm(SWC ~ ns(CWSI,df=2), data=train.dat)
  pred <- predict.lm(cv.mod, test.dat, type="response", interval="prediction")
  pred.vals <- cbind(test.dat$SWC, pred)
  coverage <- mean(pred.vals[,1] > pred.vals[,3] & pred.vals[,1] < pred.vals[,4])
  int.width <- mean(pred.vals[,4] - pred.vals[,3])
  bias <- mean(pred.vals[,2] - pred.vals[,1])
  rmse <- sqrt(mean((pred.vals[,2]-pred.vals[,1])^2))
  cbind(coverage, int.width, bias, rmse)
}

n.iter <- 10001
cv.sim <- data.frame(matrix(0, byrow=TRUE, nrow=n.iter, ncol=4))
for(i in 1:n.iter){
  cv.sim[i,] <- cv()
}
colnames(cv.sim) <- c("Coverage", "Int. Width", "Bias", "RMSE")

## covarage
hist(cv.sim[,1], xlab="Coverage", main=NULL, freq=FALSE)
mean(cv.sim[,1])

## interval width
hist(cv.sim[,2], xlab="Interval Width", main=NULL, freq=FALSE)
mean(cv.sim[,2])

## bias
hist(cv.sim[,3], xlab="Bias", main=NULL, freq=FALSE)
mean(cv.sim[,3])

## msep
hist(cv.sim[,4], xlab=expression(sqrt(MSE)), main=NULL, freq=FALSE)
mean(cv.sim[,4])


# INTERPRETATION
################

## Plot with demonstration on how to use best fit line
plot(pred.grid, pred[,1],
     xlab="CWSI", ylab="SWC",
     ylim=c(21,30), xlim=c(0,1), xaxs="i",
     type="l", lwd=2, cex=.75, col="dodgerblue4")
polygon(c(pred.grid,rev(pred.grid)),c(pred[,2],rev(pred[,3])),
        col="grey80",border=NA)
lines(pred.grid, pred[,1], col="dodgerblue4", lwd=2)
segments(0, pred[1,2], 0, pred[1,3], col="black")
segments(pred.grid[438], 0, pred.grid[438], pred[438,3], lty=3, col="red")
segments(0, pred[438,1], pred.grid[438], pred[438,1], lwd=2, lty=8, col="red")
segments(0, pred[438,2], pred.grid[438], pred[438,2], lty=3, col="red")
segments(0, pred[438,3], pred.grid[438], pred[438,3], lty=3, col="red")
segments(pred.grid[438], pred[438,2], pred.grid[438], pred[438,3], lwd=2, col="red")
legend("topright",
       legend=c("Best Fit Line","95% Prediction Interval",
       "Predicted SWC Line","Predicted SWC 95% Interval"),
       lty=c(1,1,8,3),
       lwd=c(2,8,2,1),
       col=c("dodgerblue4","grey80","red","red"),
       cex=.85)



## chart with SWC predictions 
pred.tab.grid <- seq(0, 1, length=21)
pred.tab <- as.data.frame(predict.lm(fit, newdata=data.frame(CWSI=pred.tab.grid),
                                     type="response", interval="prediction"))
pred.tab$add <- pred.tab[1,1] - pred.tab[,1]
pred.table <- cbind(pred.tab.grid, pred.tab)
colnames(pred.table) <- c("CWSI","Estimated SWC","Lower 2.5%","Upper 97.5%","Needed Water (SWC Units)")
print(xtable(pred.table), include.rownames=FALSE)






