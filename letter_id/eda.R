setwd("C:/Users/David Lowe/Desktop/STAT 536/Letter")
letter <- read.table("letter-recognition.txt",header=TRUE,sep=",")

for(i in 2:ncol(letter)){
  boxplot(letter[,i]~letter$letter,ylab=names(letter)[i],xlab="Letter")
}


boxplot(letter$xege~letter$letter,ylab="Mean Edge Count",xlab="Letter",main="Mean Edge Count by Letter")





























