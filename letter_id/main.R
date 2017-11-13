setwd("C:/Users/David Lowe/Documents/BYU/Old Classes/STAT 536/Letter")
letter <- read.table("letter-recognition.txt",header=TRUE,sep=",")
load("letter-RF.RData")

library(randomForest)
library(tree)

nms <- names(ADS)[c(-4,-14)]
ADS[,nms] <- lapply(ADS[,nms],factor)

# EDA

xtable(t(table(letter$letter)[14:26]),row.names=FALSE)


boxplot(letter$xege~letter$letter,ylab="Mean Edge Count",xlab="Letter",main="Mean Edge Count by Letter")

boxplot(letter$xege~letter$letter,ylab="Mean Edge Count",xlab="Letter",main="Mean Edge Count by Letter")

plot(jitter(letter[,"xege"],2),jitter(letter[,"yege"],2),col=clrs[as.numeric(letter$letter)],pch=19,cex=.5,xlab="Mean Edge Count on x-axis",ylab="Mean Edge Count on y-axis",main="Mean Edge Count on \nX and Y Axes for Each Letter ")


cor(letter[,"xbox"],letter[,"ybox"])

plot(jitter(letter[,"xbox"],2),jitter(letter[,"ybox"],2),pch=19,cex=.5,xlab="xbox",ylab="ybox",main="Correlation = 0.758")
cor(letter[,"xbox"],letter[,"ybox"])

train <- sample(1:nrow(letter), nrow(letter)/2)
letter.test <- letter[-train,"letter"]

n.tr <- seq(25,250,length.out=10)

pct.cor.ntr <- matrix(0,10,10)
for(j in 1:10){
  train <- sample(1:nrow(letter), nrow(letter)/2)
  letter.test <- letter[-train,"letter"]
  for(i in 1:10){
    rf.letter <- randomForest(letter~.,data=letter,subset=train,mtry=3,importance=TRUE,n.trees=n.tr[i])
    yhat.rf <- predict(rf.letter,newdata=letter[-train,])
    pct.cor[j,i] <- mean(yhat.rf==letter.test)
  }
}


n.tr <- seq(25,500,length.out=20)
pct.cor <- rep(0,20)

for(i in 1:20){
  rf.letter <- randomForest(letter~.,data=letter,subset=train,mtry=3,importance=TRUE,n.trees=n.tr[i])
  yhat.rf <- predict(rf.letter,newdata=letter[-train,])
  pct.cor[i] <- mean(yhat.rf==letter.test)
}

letter.rf <- rf.letter
plot(rf.letter)
letter.rf2 <- randomForest(letter~.,data=letter,subset=train,mtry=3,importance=TRUE,n.trees=n.tr[i],proximity=TRUE)
getTree(rf.letter, 500, labelVar=TRUE)
MDSplot(letter.rf2, letter$letter,k=16)


importance(letter.rf2)
varImpPlot(letter.rf2)

train <- sample(1:2500,1000)
sub <- sample(1:nrow(letter),2500)
letter.sub <- letter[sub,]
letter.rf2 <- randomForest(letter~.,data=letter.sub,subset=train,mtry=3,importance=TRUE,n.trees=500,proximity=TRUE)
getTree(rf.letter, 500, labelVar=TRUE)
letter.mds <- MDSplot(letter.rf2, letter.sub$letter,k=16)



nlevs <- 26
palette <- if (require(RColorBrewer) && nlevs < 50) brewer.pal(nlevs, "Set1")

clrs <- rainbow(26)

for(j in 1:16){
  for(i in 1:16){
    labs <- c(i,j)
    plot(letter.mds$points[,labs[1]],letter.mds$points[,labs[2]], col = clrs[as.numeric(letter.sub$letter)], pch = 19,cex=.5,xlab=names(letter.sub)[labs[1]+1],ylab=names(letter.sub)[labs[2]+1])
  }
}


legend(x,y,levels(fac),col=brewer.pal(nlevs, 'Set1'), pch=pch)



n.tr <- seq(25,500,length.out=20)
pct.cor <- rep(0,20)



train <- sample(1:nrow(letter), nrow(letter)/2)
letter.test <- letter[-train,"letter"]

for(i in 1:20){
  letter.rf <- randomForest(letter~.,data=letter,subset=train,mtry=3,importance=TRUE,n.trees=500)
  yhat.rf <- predict(letter.rf,newdata=letter[-train,])
  pct.cor[i] <- mean(yhat.rf==letter.test)
}

library(reprtree)

model <- randomForest(Species ~ ., data=iris, importance=TRUE, ntree=500, mtry = 2, do.trace=100)

reprtree:::plot.getTree(letter.rf,3,labelVar=TRUE)

options(repos='http://cran.rstudio.org')
have.packages <- installed.packages()
cran.packages <- c('devtools','plotrix','randomForest','tree')
to.install <- setdiff(cran.packages, have.packages[,1])
if(length(to.install)>0) install.packages(to.install)

library(devtools)
if(!('reprtree' %in% installed.packages())){
  install_github('araastat/reprtree')
}
for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))




# test number of variables to sample

train.dat <- replicate(10,sample(1:nrow(letter),.5*nrow(letter)))


pct.cor.nv <- matrix(0,10,10)

for(j in 1:10){
  train <- train.dat[,j]
  letter.test <- letter[-train,"letter"]
  for(i in 1:10){
    letter.rf <- randomForest(letter~.,data=letter,subset=train,mtry=i,importance=TRUE)
    yhat.rf <- predict(letter.rf,newdata=letter[-train,])
    pct.cor.nv[j,i] <- mean(yhat.rf==letter.test)
  }
}


# test number of trees per forest

n.tr <- seq(25,500,length.out=20)
pct.cor.nt <- matrix(0,10,20)

for(j in 1:10){
  train <- train.dat[,j]
  letter.test <- letter[-train,"letter"]
  for(i in 1:20){
    letter.rf <- randomForest(letter~.,data=letter,subset=train,mtry=3,importance=TRUE,ntree=n.tr[i])
    yhat.rf <- predict(letter.rf,newdata=letter[-train,])
    pct.cor.nt[j,i] <- mean(yhat.rf==letter.test)
  }
}

plot(colMeans(pct.cor.nv),type="l",xlab="Number of Variables Sampled",ylab="Percent Accuracy",main="Percent Accuracy by Number \n of Variables Selected",col="green3",lwd=2)
abline(v=3,lty=2)
points(colMeans(pct.cor.nv),lwd=2,col="gold2")


plot(n.tr,colMeans(pct.cor.nt),type="l",xlab="Number of Trees per Forest",ylab="Percent Accuracy",main="Percent Accuracy by Number \n of Trees",col="tomato",lwd=2)
abline(v=125,lty=2)
points(n.tr,colMeans(pct.cor.nt),lwd=2,col="slateblue")


train.dat <- replicate(100,sample(1:nrow(letter),.5*nrow(letter)))

cv.rf <- lapply(1:100,function(i){
  train <- train.dat[,j]
  letter.test <- letter[-train,"letter"]
  letter.rf <- randomForest(letter~.,data=letter,subset=train,mtry=3,importance=TRUE,ntree=125)
  yhat.rf <- predict(letter.rf,newdata=letter[-train,])
  pct.cor <- mean(yhat.rf==letter.test)
  err.rate <- letter.rf$err.rate[125,1]
  list(pct.cor=pct.cor,err.rate=err.rate)
})

pct.cor <- rep(0,ncol(train.dat))
err.rt <- rep(0,ncol(train.dat))
for(i in 1:ncol(train.dat)){
  pct.cor[i] <- cv.rf[[i]]$pct.cor
  err.rt[i] <- cv.rf[[i]]$err.rate
}

pct <- c(mean(pct.cor),quantile(pct.cor,c(.025,.975)))
err <- c(mean(err.rt),quantile(err.rt,c(.025,.975)))

names(pct)[1] <- c("Avg. Accuracy")
names(err)[1] <- c("Avg. Error Rate")

xtable(t(pct),row.names=FALSE,digits=4)
xtable(t(err),row.names=FALSE,digits=4)

save.image("letter-RF.RData")



set.seed(17)
letter.rf <- randomForest(letter~.,data=letter,mtry=3,importance=TRUE,ntree=125)

importance(letter.rf)
varImpPlot(letter.rf,type=2,main="Variable Importance")

rf.tree <- getTree(letter.rf,k=5,labelVar=TRUE)

library("party")
plot(rf.tree, type="simple")

xtable(letter.rf$confusion,digits=4)


library(randomForest)
library(reprtree)

options(repos='http://cran.rstudio.org')
have.packages <- installed.packages()
cran.packages <- c('devtools','plotrix','randomForest','tree')
to.install <- setdiff(cran.packages, have.packages[,1])
if(length(to.install)>0) install.packages(to.install)

library(devtools)
if(!('reprtree' %in% installed.packages())){
  install_github('araastat/reprtree')
}
for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))
library(reprtree)

reprtree:::plot.getTree(letter.rf,3,depth=5,main="Tree Trunk")


boot.dat <- replicate(100,sample(1:nrow(letter),nrow(letter),replace=TRUE))

boot.pct <- rep(0,100)
boot.err <- rep(0,100)
cv.rf <- lapply(1:100,function(i){
  train <- boot.dat[,j]
  letter.test <- letter[-train,"letter"]
  letter.rf <- randomForest(letter~.,data=letter,subset=train,mtry=3,importance=TRUE,ntree=125)
  yhat.rf <- predict(letter.rf,newdata=letter[-train,])
  boot.pct <- mean(yhat.rf==letter.test)
  boot.err <- letter.rf$err.rate[125,1]
  list(pct.cor=boot.pct,err.rate=boot.err)
})

pct.cor <- rep(0,ncol(train.dat))
err.rt <- rep(0,ncol(train.dat))
for(i in 1:ncol(train.dat)){
  pct.cor[i] <- cv.rf[[i]]$pct.cor
  err.rt[i] <- cv.rf[[i]]$err.rate
}

hist(pct.cor)
hist(err.rt)

sumx <- 1:10
p2 <- .6
p1 <- .5
sumx*log(p2/p1) +10*log((1-p2)/(1-p1)) - sumx*log((1-p2)/(1-p1))

