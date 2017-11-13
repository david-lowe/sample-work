setwd("C:/Users/David Lowe/Desktop/STAT 536/Crash")
crash <- read.table("Crash.txt",header=TRUE)

nms <- names(crash)[-c(1,12)]
crash[,nms] <- lapply(crash[,nms],factor)

names(crash) <- tolower(names(crash))



hour <- as.data.frame(model.matrix(~-1+as.factor(hour),data=crash))
names(hour) <- paste("hr",0:23,sep="")


ltcond <- as.data.frame(model.matrix(~-1+as.factor(lgt_cond),data=crash))
ltcond$c4_5 <- ltcond[,4] + ltcond[,5]
ltcond <- ltcond[,c(1:3,6)]
names(ltcond) <- paste("ltcond",c("Day","DarkNL","DarkL","LowLight"),sep="_")

mat <- as.data.frame(model.matrix(~-1+as.factor(weather),data=crash))
mat$c2_10 <- mat[,2] + mat[,10]
mat$c3_4_11 <- mat[,3] + mat[,4] + mat[,9]
mat$c6_7 <- mat[,6] + mat[,7]
weather <- mat[,c(1,11,12,5,8,13)]
names(weather) <- paste("Wthr",c("Clear","Rain","Snow","Fog","Cloudy","Windy"),sep="_")

mat <- as.data.frame(model.matrix(~-1+as.factor(typ_int),data=crash))
mat$c5_6 <- mat[,5] + mat[,6]
mat$c3_10 <- mat[,3] + mat[,8]
typeInt <- mat[,c(1:2,10,4,9,7)]
names(typeInt) <- paste("TypInt",c("NI","4W","T","Y","RA","5P"),sep="_")

mat <- as.data.frame(model.matrix(~-1+as.factor(rest_use),data=crash))
mat$c5_6 <- mat[,2] + mat[,3]
RU <- mat[,c(1,8,4:7)]
names(RU) <- paste("RestUse",c("NA","Partial","Full","MH","NU","Uk"),sep="_")

# air_bag = air bag deployment
# 00 Not Applicable
# 01 Deployed-Front
# 02 Deployed-Side (door, seatback)
# 03 Deployed-Curtain (roof)
# 07 Deployed-Other (knee, air belt, etc.)
# 08 Deployed-Combination
# 09 Deployment-Unknown Location
# 20 Not Deployed

mat <- as.data.frame(model.matrix(~-1+as.factor(air_bag),data=crash))
mat$c5_6 <- mat[,4] + mat[,5]
air_bag <- mat[,c(1:3,9,6:8)]
names(air_bag) <- paste("AirBag",c("NA","Front","Side","Other","Comb","Uk","ND"),sep="_")

VTW <- as.data.frame(model.matrix(~-1+as.factor(vtrafway),data=crash))
names(VTW) <- paste("VTW",c("2WND","2WDU","2WDB","1WND","2WT","EntEx"),sep="_")

mat <- as.data.frame(model.matrix(~-1+as.factor(vnum_lan),data=crash))
mat$c5_6 <- mat[,5] + mat[,6] + mat[,7]
numln <- mat[,c(1:4,8)]
names(numln) <- paste("NumLn",1:5,sep="_")

align <- as.data.frame(model.matrix(~-1+as.factor(valign),data=crash))
names(align) <- paste("align",c("S","R","L","Uk"),sep="_")

mat <- as.data.frame(model.matrix(~-1+as.factor(vsurcond),data=crash))
mat$c5_6 <- mat[,3] + mat[,4] + mat[,8]
mat$c7_8 <- mat[,5] + mat[,6] + mat[,7] + mat[,9]
sCond <- mat[,c(1,2,10,11)]
names(sCond) <- paste("sCond",c("Dry","Wet","Snow","Other"),sep="_")



crash1 <- data.frame(hour,ltcond,weather,alcohol=crash$alcohol,typeInt,RU,air_bag,VTW,spd_lim=crash$vspd_lim,align,sCond,severity=crash$severity)

nms <- names(crash1)[-61]
crash1[,nms] <- lapply(crash1[,nms],factor)
crash1[,61] <- as.numeric(crash1[,61])


fit.null <- glm(severity~1,data=crash1,family=binomial)
fit.full <- glm(severity~.,data=crash1,family=binomial)

varSelectF <- step(fit.null,scope=list(lower=fit.null,upper=fit.full),direction="forward")

varSelectB <- step(fit.full,scope=list(lower=fit.null,upper=fit.full),direction="backward")

varSelectF$formula # 26 variables, AIC=10925.2
varSelectB$formula # 27 variables, AIC=10931.4

avPlots(varSelectF)
avPlots(varSelectB)

vif(varSelectF)

crash.glm <- glm(varSelectF$formula,data=crash1,family=binomial)

pred <- predict.glm(crash.glm, newdata=crash1[,-ncol(crash1)], se.fit=TRUE,type="link") 
low <- pred$fit - pnorm(.975)*pred$se.fit
up <- pred$fit + pnorm(.975)*pred$se.fit

pred <- as.vector(exp(pred$fit)/(1+exp(pred$fit)))
low <- exp(low)/(1+exp(low)) 
up <- exp(up)/(1+exp(up)) 


pred.diag <- function(preds,cutoff){
  pred.sev <- rep(0,length(preds))
  pred.sev[preds>=cutoff] <- 1
  c.mat <- t(table(pred.sev,crash.dat$crash.SEVERITY))
  accuracy <- sum(c.mat[1,1],c.mat[2,2])/length(preds)
  sensitivity <- c.mat[2,2]/sum(c.mat[2,])
  specificity <- c.mat[1,1]/sum(c.mat[1,])
  ppv <- c.mat[2,2]/sum(c.mat[,2])
  npv <- c.mat[1,1]/sum(c.mat[,1])
  fp <- c.mat[1,2]/sum(c.mat[1,])
  c(accuracy,sensitivity,specificity,ppv,npv,fp)
}

cut <- seq(min(pred$fit)+.01,.9,by=.01)
cutoff.diag <- matrix(0,nrow=length(cut),ncol=6,byrow=TRUE)
for(i in 1:length(cut)){
  cutoff.diag[i,] <- pred.diag(pred$fit,cut[i])
}
cutoff.sum <- data.frame(cut,cutoff.diag)
colnames(cutoff.sum) <- c("cutoff","accuracy","sensitivity","specificity","PPV","NPV","FP")

clr <- c("firebrick2","springgreen2","coral1","lightslateblue","orchid3")
plot(cutoff.sum$cutoff,cutoff.sum$accuracy,xlab="Cut-off Point",ylab="Proportion",ylim=c(0,1),type="l",main="Cut-off Point Diagnostics",cex=.5,pch=19,lwd=2,col=clr[1])
lines(cutoff.sum$cutoff,cutoff.sum$sensitivity,type="l",cex=.5,pch=19,lty=2,lwd=2,col=clr[2])
lines(cutoff.sum$cutoff,cutoff.sum$specificity,type="l",cex=.5,pch=19,lty=2,lwd=2,col=clr[3])
lines(cutoff.sum$cutoff,cutoff.sum$PPV,type="l",cex=.5,pch=19,lty=4,lwd=2,col=clr[4])
lines(cutoff.sum$cutoff,cutoff.sum$NPV,type="l",cex=.5,pch=19,lty=4,lwd=2,col=clr[5])
legend("bottomleft",legend=toupper(names(cutoff.sum)[-c(1,ncol(cutoff.sum))]),lty=c(1,2,2,4,4),lwd=2,col=clr,cex=.6)


plot(cutoff.sum$FP,cutoff.sum$sensitivity,type="l",xlim=c(0,1),ylim=c(0,1),col="midnightblue",lwd=2)
segments(0,0,1,1,lty=2)

library(flux)
auc(cutoff.sum$FP,cutoff.sum$sensitivity)



################################################
############### Michael's Code #################
################################################

library(car)
crash <- crash[,c(9,2,3,4,5,6,7,8,10,11,12,13,14)]
crash[,-c(1,11)] <- lapply(crash[,-c(1,11)],factor)
X <- as.data.frame(model.matrix(SEVERITY~ .,data=crash))
orig.X <- X
X$LGT_COND4 <- X$LGT_COND4 + X$LGT_COND5 # low light conditions
X <- X[,-which(names(X)%in%c("LGT_COND5"))]
X$WEATHER3 <- X$WEATHER3 + X$WEATHER4 + X$WEATHER11 # Snowy-ish
X$WEATHER2 <- X$WEATHER2 + X$WEATHER12 # Would lead to wet roads
X$WEATHER6 <- X$WEATHER6 + X$WEATHER7 # Windy conditions
X <- X[,-which(names(X)%in%c("WEATHER4","WEATHER11","WEATHER12","WEATHER7"))]
X$TYP_INT5 <- X$TYP_INT5 + X$TYP_INT6 # Circular traffic patterns
X$TYP_INT3 <- X$TYP_INT3 + X$TYP_INT10 # because L is close to T
X <- X[,-which(names(X)%in%c("TYP_INT6","TYP_INT10"))]
X$REST_USE1 <- X$REST_USE1 + X$REST_USE2 # partial belt used
X$AIR_BAG3 <- X$AIR_BAG3 + X$AIR_BAG7 # Specialty airbags
X <- X[,-which(names(X)%in%c("REST_USE2","AIR_BAG7"))]
X$VNUM_LAN5 <- X$VNUM_LAN5 + X$VNUM_LAN6 + X$VNUM_LAN7 # 5 or more lanes
X$VSURCOND3 <- X$VSURCOND3 + X$VSURCOND10 + X$VSURCOND4 # Wintery conditions
X$VSURCOND5 <- X$VSURCOND5 + X$VSURCOND8 + X$VSURCOND11 + X$VSURCOND6 # Unique Situations
X <- X[,-which(names(X)%in%c("VNUM_LAN6","VNUM_LAN7","VSURCOND10","VSURCOND4","VSURCOND8","VSURCOND11","VSURCOND6"))]
new.X <- as.matrix(X)

crash.dat <- data.frame(new.X[,-1],crash$SEVERITY)
nms <- names(crash.dat[,-c(58)])
crash.dat[,nms] <- lapply(crash.dat[,nms],factor)
crash.dat[,58] <- as.numeric(crash.dat[,58])

null <- glm(crash.SEVERITY ~ 1,data=crash.dat,family=binomial)
full <- glm(crash.SEVERITY ~ .,data=crash.dat,family=binomial)
varSelectF <- step(null, scope=list(lower=null, upper=full), direction="forward")
varSelectF$formula
vif(varSelectF)

crash.glm <- glm(crash.SEVERITY ~ AIR_BAG20 + REST_USE7 + ALCOHOL2 + VSPD_LIM + LGT_COND3 + REST_USE5 + VALIGN3 + REST_USE8 + HOUR3 + VNUM_LAN3 + HOUR1 + VALIGN4 + AIR_BAG1 + AIR_BAG8 + AIR_BAG9 + HOUR4 + HOUR7 + TYP_INT2 + TYP_INT3 + WEATHER10,family="binomial",data=crash.dat)

pred <- predict.glm(crash.glm, newdata=crash.dat[,-65], se.fit=TRUE,type="link") 
low <- pred$fit - pnorm(.975)*pred$se.fit
up <- pred$fit + pnorm(.975)*pred$se.fit

pred <- as.vector(exp(pred$fit)/(1+exp(pred$fit)))
low <- exp(low)/(1+exp(low)) 
up <- exp(up)/(1+exp(up)) 


pred.diag <- function(preds,cutoff){
  pred.sev <- rep(0,length(preds))
  pred.sev[preds>=cutoff] <- 1
  c.mat <- t(table(pred.sev,crash.dat$crash.SEVERITY))
  accuracy <- sum(c.mat[1,1],c.mat[2,2])/length(preds)
  sensitivity <- c.mat[2,2]/sum(c.mat[2,])
  specificity <- c.mat[1,1]/sum(c.mat[1,])
  ppv <- c.mat[2,2]/sum(c.mat[,2])
  npv <- c.mat[1,1]/sum(c.mat[,1])
  fp <- c.mat[1,2]/sum(c.mat[1,])
  c(accuracy,sensitivity,specificity,ppv,npv,fp)
}

cut <- seq(.23,.9,by=.01)
cutoff.diag <- matrix(0,nrow=length(cut),ncol=6,byrow=TRUE)
for(i in 1:length(cut)){
  cutoff.diag[i,] <- pred.diag(pred,cut[i])
}
cutoff.sum <- data.frame(cut,cutoff.diag)
colnames(cutoff.sum) <- c("cutoff","accuracy","sensitivity","specificity","PPV","NPV","FP")

clr <- c("cornflowerblue","orchid3","coral1","black")
plot(cutoff.sum$cutoff,cutoff.sum$accuracy,xlab="Cut-off Point",ylab="Proportion",ylim=c(0,1),type="l",cex=.5,pch=19,lwd=3,col=clr[1])
lines(cutoff.sum$cutoff,cutoff.sum$sensitivity,type="l",cex=.5,pch=19,lty=2,lwd=3,col=clr[2])
lines(cutoff.sum$cutoff,cutoff.sum$specificity,type="l",cex=.5,pch=19,lty=3,lwd=3,col=clr[3])
abline(v=.35,lty=3)
legend("bottomright",legend=c(toupper(names(cutoff.sum)[c(2,3,4)]),"CUT-OFF"),lty=c(1,2,3,3),lwd=c(3,3,3,1),col=clr)




plot(cutoff.sum$cutoff,cutoff.sum$accuracy,xlab="Cut-off Point",ylab="Proportion",ylim=c(0,1),type="l",cex=.5,pch=19,lwd=3,col=clr[1])
lines(cutoff.sum$cutoff,cutoff.sum$PPV,type="l",cex=.5,pch=19,lty=4,lwd=3,col=clr[2])
lines(cutoff.sum$cutoff,cutoff.sum$NPV,type="l",cex=.5,pch=19,lty=4,lwd=3,col=clr[3])
abline(v=.35,lty=3)
legend("bottomright",legend=c(toupper(names(cutoff.sum)[c(2,5,6)]),"CUT-OFF"),lty=c(1,2,3,3),lwd=c(3,3,3,1),col=clr)


plot(cutoff.sum$FP,cutoff.sum$sensitivity,type="l",xlim=c(0,1),ylim=c(0,1),col="midnightblue",lwd=2)
segments(0,0,1,1,lty=2)

library(flux)
auc(cutoff.sum$FP,cutoff.sum$sensitivity)





















