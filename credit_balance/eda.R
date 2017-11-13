credit <- read.csv("C:/Users/David/Desktop/STAT 536/MLR/Credit.csv",header=T)
attach(credit)
pairs(credit)
plot(Income,Balance)
plot(Limit,Balance)
plot(Rating,Balance)
plot(Cards,Balance)
plot(Age,Balance)
plot(Education,Balance)
boxplot(Balance~Gender,ylab="Balance")
boxplot(Balance~Student)
boxplot(Balance~Married)
boxplot(Balance~Ethnicity)

par(mfrow=c(3,1),mar=c(4,5,1,5))
plot(Income,Limit)
plot(Limit,Rating)
plot(Rating,Income)







