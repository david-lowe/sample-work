gene <- read.table("https://mheaton.byu.edu/Courses/Stat536/Case%20Studies/GeneExpression/Data/GeneExpression2.txt",header=TRUE)

gene2 <- gene[order(gene[,1]),]
  
length(names(gene)) ## too many variables (5150)
nrow(gene) ## only 102 rows
str(gene) ## all numeric

plot(gene[,1],pch=19,cex=.7,main="Malignancy",ylab="Malignant",xlab="Patient")
abline(v=50.5,lty=2)

vars <- sample(2:5150,6)
par(mfrow=c(3,2),mar=c(4,4.2,3,2))
for(i in vars){
  plot(gene[,i],pch=19,cex=1,cex.lab=1.2,main=paste("Gene: ",colnames(gene)[i]),ylab=colnames(gene)[i],xlab="Patient")
}

vars <- sample(2:5150,2)
par(mfrow=c(1,2),mar=c(4,4.2,3,2))
for(i in vars){
  hist(gene[,i],cex.lab=1.2,main=paste("Gene: ",colnames(gene)[i]),xlab=colnames(gene)[i],freq=FALSE)
}


max(gene[,2:5150])

j <- sample(1:ncol(gene),50)
for(i in 1:50){
  plot(gene[,j[i]],type='l',main=j[i])
  Sys.sleep(2)
}

effect <- c(2330,330,1124,4649,968,2827)

effect <- c(9,14,20,21,1124)

par(mfrow=c(2,2),mar=rep(2,4))
plot(gene[,1124],type='l')


# 78-79 dips
par(mfrow=c(3,2),mar=rep(2,4))
for(i in 1:6){
  plot(gene[,effect[i]],type='l',main=paste("Gene: ",names(gene)[effect[i]]),ylab=names(gene)[effect[i]],xlab="Patient")
}
  


  
  
  
  