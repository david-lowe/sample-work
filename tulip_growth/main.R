library(splines)

setwd("C:/Users/David Lowe/Desktop/STAT 536/Final")
tulip <- read.csv("Tulips.csv",header=TRUE)
tulip <- tulip[which(tulip$Population != 12),]
tulip$CT2 <- tulip$ChillingTime^2
tulip$Season <- ifelse(tulip$DayCollected<=90,"Winter",ifelse(tulip$DayCollected<=181,"Spring",ifelse(tulip$DayCollected<=273,"Summer","Fall")))




############################################################
################# Principle Components #####################
############################################################


pc.vals <- eigen(cov(cbind(tulip$ChillingTime,tulip$CT2)))$values
cumsum(pc.vals)/sum(pc.vals) # one component explains 94% of the X's
pc <- eigen(cov(cbind(tulip$ChillingTime,tulip$CT2)))$vectors[,1]
z <- as.matrix(cbind(tulip$ChillingTime,tulip$CT2))%*%pc

fit1 <- glm(Germinated ~ -1 + as.factor(Population) + z + z:as.factor(Population),data=tulip, family="binomial")
summary(fit1)

1-fit1$deviance/fit1$null.deviance

bhat <- c(fit1$coefficients[1:11],as.vector(t(t(pc))%*%fit1$coefficients[12:22]))

bhat.se <- c(sqrt(diag(vcov(fit1)[1:11,1:11])))
for(i in 12:22){
  bhat.se <- c(bhat.se,sqrt(diag(t(t(pc))%*%vcov(fit1)[i,i]%*%t(pc))))
}
results <- data.frame(Estimate=bhat,SE=bhat.se)
pop <- paste("Pop",1:11,sep="")
pop2 <- paste("Pop",rep(1:11,each=2),sep="")
ct.z <- paste("CT",rep(1:2,11),sep="")
rownames(results) <- c(pop,paste(pop2,":",ct.z,sep=""))
low <- results$Estimate - pnorm(.975)*results$SE
up <- results$Estimate + pnorm(.975)*results$SE

results <- data.frame(results,Lower=low,Upper=up)
results <- exp(results)[,c(1,3,4)]

############################################################
############################################################
############################################################



pairs(tulip)

xtable(table(tulip$YearCollected,tulip$Population),row.names=FALSE)
xtable(table(tulip$DayCollected,tulip$Population),row.names=FALSE)


pop <- aggregate((as.numeric(tulip$Germinated)-1),by=list(tulip$Population),mean)
ct <- aggregate((as.numeric(tulip$Germinated)-1),by=list(tulip$ChillingTime),mean)
yc <- aggregate((as.numeric(tulip$Germinated)-1),by=list(tulip$YearCollected),mean)
dc <- aggregate((as.numeric(tulip$Germinated)-1),by=list(tulip$DayCollected),mean)
zt <- aggregate((as.numeric(tulip.ord$Germinated)-1),by=list(tulip.ord$ztime),mean)

plot(dc[,1],dc[,2],type="l",xlab="Day of Year Collected",ylab="Proportion Germinated")
abline(v=unique(tulip$DayCollected))

plot(yc[,1],yc[,2],type="l",xlab="Year Collected",ylab="Proportion Germinated")
abline(v=unique(tulip$YearCollected))

plot(ct[,1],ct[,2],type="l",xlab="Chilling Time (Weeks)",ylab="Proportion Germinated")
plot(pop[,1],pop[,2],type="h",lwd=2,xlab="Population",ylab="Proportion Germinated")
points(pop[,1],pop[,2],lwd=2)

plot(zt[,1],zt[,2],type="l",xlab="Aggregated Time",ylab="Proportion Germinated")

ns(tulip$Germinate~tulip$YearCollected,df=4)

for(i in 1:(ncol(tulip)-1)){
  scatter.smooth(tulip[,i],tulip$Germinated,xlab=names(tulip)[i],ylab="Germinated")
  points(jitter(tulip[,i]),jitter(as.numeric(tulip$Germinated)))
}


plot(jitter(tulip$ChillingTime),jitter(as.numeric(tulip$Germinated)-1),xlab="Chilling Time (Weeks)",ylab="Germinated",main="Smoothed Fit of Chilling Time on Germination")
lines(loess.smooth(tulip$ChillingTime,(as.numeric(tulip$Germinated)-1)),lwd=2,col="tomato")


plot(jitter(tulip$YearCollected),jitter(as.numeric(tulip$Germinated)-1),xlab="Year Collected",ylab="Germinated",main="Smoothed Fit of Year Collected on Germination")
lines(loess.smooth(tulip$YearCollected,(as.numeric(tulip$Germinated)-1)),lwd=2,col="tomato")


ct <- aggregate((as.numeric(tulip$Germinated)-1),by=list(tulip$ChillingTime,tulip$Population),mean)

ggplot(ct, aes(x = ct$Group.1, y = ct$x)) +
  geom_line() +
  facet_wrap( ~ Group.2) +
  ggtitle("Germination vs. Chilling Time by Population") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Chilling Time (Weeks)",y="Proportion Germinated") +
  scale_x_continuous(breaks=seq(0,12,by=2))


for(i in 1:(ncol(tulip)-1)){
  boxplot(tulip[,i]~tulip$Germinated,ylab=names(tulip)[i],xlab="Germinated")
}

fit.null <- glm(Germinated~ChillingTime,data=tulip,family="binomial")
fit.full <- glm(Germinated~.,data=tulip,family="binomial")

varSelectF <- step(fit.null,scope=list(lower=fit.null,upper=fit.full),direction="forward")

varSelectB <- step(fit.full,scope=list(lower=fit.null,upper=fit.full),direction="backward")

varSelectF$formula
varSelectB$formula

year.spline <- ns(as.numeric(tulip$YearCollected),df=3)





fit <- glm(Germinated ~ -1 + as.factor(Population) + ChillingTime + I(ChillingTime^2) + ChillingTime:as.factor(Population),data=tulip, family="binomial")
summary(fit)
exp(summary(fit)$coefficients)
1-fit$deviance/fit$null.deviance

bhat <- summary(fit)$coef
bhat.low <- bhat[,1] - pnorm(.975)*bhat[,2]
bhat.high <- bhat[,1] + pnorm(.975)*bhat[,2]

bhat.est <- round(cbind(exp(cbind(bhat[,1],bhat.low,bhat.high)),bhat[,4]),digits=6)
rownames(bhat.est) <- c(paste("$Pop_",1:11,"$",sep=""),"$CT$","$CT^2$",paste("$Pop_",2:11," \times CT$",sep=""))
colnames(bhat.est) <- c("Estimate","Lower","Upper","pval")
xtable(bhat.est,digits=3)

plotCI(bhat,y=NULL,uiw,liw=uiw,ui=NULL,li=NULL,err="y")

pred <- predict.glm(fit, newdata=tulip, se.fit=TRUE,type="link") 
low <- pred$fit - pnorm(.975)*pred$se.fit
up <- pred$fit + pnorm(.975)*pred$se.fit

pred <- as.vector(exp(pred$fit)/(1+exp(pred$fit)))
low <- exp(low)/(1+exp(low)) 
up <- exp(up)/(1+exp(up)) 

pred.ct <- aggregate((as.numeric(tulip$Germinated)-1),by=list(tulip$ChillingTime,tulip$Population),mean)

clrs <- rainbow(11)
plot(ct$Group.1,ct$x,col=clrs[ct$Group.2])
for(i in 1:11){
  lines(ct$Group.1[which(ct$Group.2==i)],ct$x[which(ct$Group.2==i)],col=clrs[ct$Group.2[which(ct$Group.2==i)]])
}

ct <- aggregate((as.numeric(tulip$Germinated)-1),by=list(tulip$ChillingTime),mean)

ggplot(ct, aes(x = ct$Group.1, y = ct$x)) +
  geom_line() +
  facet_wrap( ~ Group.2) +
  ggtitle("Germination vs. Chilling Time by Population") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Chilling Time (Weeks)",y="Proportion Germinated") +
  scale_x_continuous(breaks=seq(0,12,by=2))



pred.diag <- function(preds,cutoff){
  pred.germ <- rep(0,length(preds))
  pred.germ[preds>=cutoff] <- 1
  c.mat <- t(table(pred.germ,tulip$Germinated))
  accuracy <- sum(c.mat[1,1],c.mat[2,2])/length(preds)
  sensitivity <- c.mat[2,2]/sum(c.mat[2,])
  specificity <- c.mat[1,1]/sum(c.mat[1,])
  ppv <- c.mat[2,2]/sum(c.mat[,2])
  npv <- c.mat[1,1]/sum(c.mat[,1])
  fp <- c.mat[1,2]/sum(c.mat[1,])
  c(accuracy,sensitivity,specificity,ppv,npv,fp)
}

cut <- seq(min(pred)+.01,.9,by=.01)
cutoff.diag <- matrix(0,nrow=length(cut),ncol=6,byrow=TRUE)
for(i in 1:length(cut)){
  cutoff.diag[i,] <- pred.diag(pred,cut[i])
}
cutoff.sum <- data.frame(cut,cutoff.diag)
colnames(cutoff.sum) <- c("cutoff","accuracy","sensitivity","specificity","PPV","NPV","FP")

clr <- c("firebrick2","springgreen2","lightslateblue")
plot(cutoff.sum$cutoff,cutoff.sum$accuracy,xlab="Cut-off Point",ylab="Proportion",ylim=c(0,1),type="l",main="Cut-off Point Diagnostics",cex=.5,pch=19,lwd=2,col=clr[1])
lines(cutoff.sum$cutoff,cutoff.sum$sensitivity,type="l",cex=.5,pch=19,lty=2,lwd=2,col=clr[2])
lines(cutoff.sum$cutoff,cutoff.sum$specificity,type="l",cex=.5,pch=19,lty=2,lwd=2,col=clr[3])
abline(v=.39,lty=3)
legend("bottomleft",legend=toupper(names(cutoff.sum)[c(2:4)]),lty=c(1,2,2),lwd=2,col=clr,cex=.85)


plot(cutoff.sum$cutoff,cutoff.sum$accuracy,xlab="Cut-off Point",ylab="Proportion",ylim=c(0,1),type="l",main="Cut-off Point Diagnostics",cex=.5,pch=19,lwd=2,col=clr[1])
lines(cutoff.sum$cutoff,cutoff.sum$PPV,type="l",cex=.5,pch=19,lty=2,lwd=2,col=clr[2])
lines(cutoff.sum$cutoff,cutoff.sum$NPV,type="l",cex=.5,pch=19,lty=2,lwd=2,col=clr[3])
abline(v=.39,lty=3)
legend("bottomleft",legend=toupper(names(cutoff.sum)[c(2,5,6)]),lty=c(1,2,2),lwd=2,col=clr,cex=.85)


maxes <- cutoff.sum[apply(cutoff.sum,2,which.max),]

xtable(t(maxes[2,]*100),digits=2)

a <- cutoff.sum[order(cutoff.sum$FP),]

plot(c(0,a$FP,1),c(0,a$sensitivity,max(a$sensitivity)),xlab="False Positive Rate",ylab="Sensitivity",main="ROC Curve",type="l",xlim=c(0,1),ylim=c(0,1),col="royalblue",lwd=2)
segments(0,0,1,1,lty=2)

library(flux)
auc(c(0,a$FP,1),c(0,a$sensitivity,max(a$sensitivity)))





pred.germ <- rep(0,length(pred))
pred.germ[pred>=.39] <- 1

ct.pred <- aggregate(pred.germ,by=list(tulip$ChillingTime,tulip$Population),mean)

clrs <- rainbow(11)
plot(ct.pred$Group.1,ct.pred$x,col=clrs[ct$Group.2])
for(i in 1:11){
  lines(ct$Group.1[which(ct$Group.2==i)],ct$x[which(ct$Group.2==i)],col=clrs[ct$Group.2[which(ct$Group.2==i)]])
}

ggplot(ct.pred, aes(x = ct$Group.1, y = ct$x)) +
  geom_line() +
  facet_wrap( ~ Group.2) +
  ggtitle("Germination vs. Chilling Time by Population") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Chilling Time (Weeks)",y="Proportion Germinated") +
  scale_x_continuous(breaks=seq(0,12,by=2))




pred.germ <- rep(0,length(pred))
pred.germ[pred>=.39] <- 1

ct.pred2 <- aggregate(pred.germ,by=list(tulip$ChillingTime),mean)

plot(ct.pred2$Group.1,ct.pred2$x,type="l",ylab="Predicted Proportion Germinated",xlab="Chilling Time (Weeks)",main="Predicted Germanation by Chilling Time")

booty <- function(pred.dat){
  boot <- tulip[sample(1:nrow(tulip),nrow(tulip),replace=TRUE),]
  fit <- glm(Germinated ~ -1 + as.factor(Population) + ChillingTime + I(ChillingTime^2) + ChillingTime:as.factor(Population),data=boot, family="binomial")
  
  pred.d <- predict.glm(fit, newdata=pred.dat, se.fit=TRUE,type="link")
  low <- pred.d$fit - pnorm(.975)*pred.d$se.fit
  up <- pred.d$fit + pnorm(.975)*pred.d$se.fit
  
  pred.d <- as.vector(exp(pred.d$fit)/(1+exp(pred.d$fit)))
  low <- exp(low)/(1+exp(low)) 
  up <- exp(up)/(1+exp(up)) 
  
  pred.d <- data.frame(Pop=pred.dat$Population,CT=pred.dat$ChillingTime,pred.d,low,up)
  
  pred.d
}

pop.pred <- rep(1:11,each=2)

pred.dat <- data.frame(Population=rep(1:11,each=2),ChillingTime=rep(c(8,10),11))

n.iter <- 10000
diff <- matrix(0,nrow=22*n.iter,ncol=5)
for(i in 1:n.iter){
  diff[(i*22 - 21):(i*22),] <- as.matrix(booty(pred.dat))
}

diff.pop <- aggregate(diff[,3],by=list(diff[,1],diff[,2]),mean)
aggregate(diff[,3],by=list(diff[,2]),mean)

quant8 <- matrix(0,nrow=11,ncol=3,byrow=TRUE)
quant10 <- matrix(0,nrow=11,ncol=3,byrow=TRUE)
for(i in 1:11){
  dat8 <- diff[diff[,1]==i & diff[,2]==8,]
  dat10 <- diff[diff[,1]==i & diff[,2]==10,]
  dif <- dat8[,3] - dat10[,3]
  quant8[i,] <- c(mean(dif),quantile(dif,c(.025,.975)))
  quant10[i,] <- c(mean(dif),quantile(dif,c(.025,.975)))
}
dat10 <- diff[diff[,2]==10,]
dat8 <- diff[diff[,2]==8,]
cum.dif <- dat8[,3] - dat10[,3]
cum.quant <- t(c(mean(cum.dif),quantile(cum.dif,c(.025,.975))))
names(cum.quant) <- c("Mean","Lower","Upper")

rownames(quant8) <- paste("Pop. ",1:11,sep="")
colnames(quant8) <- c("Mean","Lower","Upper")

xtable(t(t(100*cum.quant)),row.names=FALSE)
xtable(quant8*100,digits=2)

hist(dat8[,3])
hist(dat10[,3])
hist(dat10[,3]-dat8[,3])



days <- seq(0,13,length.out=92)
days2 <- days^2
pop.pred <- rep(c(1:11),each=92)
pred.day <- data.frame(Population=pop.pred,ChillingTime=rep(days,11))

pred.d <- predict.glm(fit, newdata=pred.day, se.fit=TRUE,type="link") 
low <- pred.d$fit - pnorm(.975)*pred.d$se.fit
up <- pred.d$fit + pnorm(.975)*pred.d$se.fit

pred.d <- as.vector(exp(pred.d$fit)/(1+exp(pred.d$fit)))
low <- exp(low)/(1+exp(low)) 
up <- exp(up)/(1+exp(up)) 

pred.d <- data.frame(Pop=pred.day$Population,CT=pred.day$ChillingTime,pred.d,low,up)


clrs <- rainbow(11,s=.7)
plot(NULL,xlim=c(0,12),ylim=c(0,1),xlab="Chilling Time (Weeks)",ylab="Probability of Germination",main="Probability of Germination by \nWeeks of Chilling Time")
for(i in 1:11){
  lines(pred.d$CT[which(pred.d$Pop==i)],pred.d$pred.d[which(pred.d$Pop==i)],col=clrs[pred.d$Pop[which(pred.d$Pop==i)]],lwd=2)
}
legend("topleft",legend=1:11,col=clrs,lwd=2,cex=.5)

ggplot(pred.d, aes(x = CT, y = pred.d)) +
  geom_line() +
  facet_wrap( ~ Pop) +
  ggtitle("Germination vs. Chilling Time by Population") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Chilling Time (Weeks)",y="Proportion Germinated") +
  scale_x_continuous(breaks=seq(0,12,by=2))








boot.tulip <- function(pred.dat){
  boot <- tulip[sample(1:nrow(tulip),nrow(tulip),replace=TRUE),]
  fit <- glm(Germinated ~ -1 + as.factor(Population) + ChillingTime + I(ChillingTime^2) + ChillingTime:as.factor(Population),data=boot, family="binomial")

  pred.d <- predict.glm(fit, newdata=pred.dat, se.fit=TRUE,type="link")
  low <- pred.d$fit - pnorm(.975)*pred.d$se.fit
  up <- pred.d$fit + pnorm(.975)*pred.d$se.fit
  
  pred.d <- as.vector(exp(pred.d$fit)/(1+exp(pred.d$fit)))
  low <- exp(low)/(1+exp(low)) 
  up <- exp(up)/(1+exp(up)) 
  
  pred.d <- data.frame(Pop=pred.day$Population,CT=pred.day$ChillingTime,pred.d,low,up)
  
  pred.d[pred.d$pred.d %in% aggregate(pred.d$pred.d,by=list(pred.d$Pop),max)[,2],]
}


days <- seq(0,12,length.out=85)
pop.pred <- rep(c(1:11),each=85)
pred.day <- data.frame(Population=pop.pred,ChillingTime=rep(days,11))

pred.d <- predict.glm(fit, newdata=pred.day, se.fit=TRUE,type="link")
low <- pred.d$fit - pnorm(.975)*pred.d$se.fit
up <- pred.d$fit + pnorm(.975)*pred.d$se.fit

pred.d <- as.vector(exp(pred.d$fit)/(1+exp(pred.d$fit)))
low <- exp(low)/(1+exp(low)) 
up <- exp(up)/(1+exp(up)) 

pred.d <- data.frame(Pop=pred.day$Population,CT=pred.day$ChillingTime,pred.d,low,up)

opt.wk <- pred.d[pred.d$pred.d %in% aggregate(pred.d$pred.d,by=list(pred.d$Pop),max)[,2],]



n.iter <- 10000
boot.results <- matrix(0,nrow=n.iter,ncol=11,byrow=TRUE)
for(i in 1:n.iter){
  num.weeks <- boot.tulip(pred.day)
  boot.results[i,] <- t(num.weeks)[2,]
}

for(i in 1:11){
  hist(boot.results[,i],main=paste("Population ",i,sep=""))
}

head(boot.results)

opt.num.wks <- t(apply(boot.results,2,function(x) quantile(x,c(.025,.975))))

opt.num.wks <- data.frame(Population=1:11,opt.wk=opt.wk[,2],lower=opt.num.wks[,1],upper=opt.num.wks[,2])

opt.days <- cbind(1:11,round(opt.num.wks[,2],digits=2),opt.num.wks[,2:4]*7)

xtable(opt.days,row.names=FALSE)



y<-runif(10)
err<-runif(10)
plotCI(1:10,y,err)
plotCI(1:10,y,err,2*err,lwd=2,col="red",scol="blue")
err.x<-runif(10)
err.y<-runif(10)
plotCI(1:10,y,err.y,pt.bg=par("bg"),pch=21)
plotCI(1:10,y,err.x,pt.bg=par("bg"),pch=21,err="x",add=TRUE)
data(warpbreaks)
attach(warpbreaks)
wmeans<-by(breaks,tension,mean)
wsd<-by(breaks,tension,sd)
## note that barplot() returns the midpoints of the bars, which plotCI
##  uses as x-coordinates
plotCI(barplot(wmeans,col="gray",ylim=c(0,max(wmeans+wsd))),wmeans,wsd,add=TRUE)
## using labels instead of points
labs<-sample(LETTERS,replace=TRUE,size=10)
plotCI(1:10,y,err,pch=NA,gap=0.02)
text(1:10,y,labs)

