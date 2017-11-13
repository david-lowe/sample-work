ag <- read.csv("C:/Users/David Lowe/Desktop/STAT 536/Irrigation/irrigation.csv",header=TRUE)
names(ag)

install.packages("ggplot2")
library(ggplot2)

splot <- ggplot(ag, aes(x=CWSI, y=SWC)) + geom_point() + stat_smooth() + theme_bw()
splot

splot.log <- ggplot(ag, aes(x=log(CWSI), y=SWC)) + 
             geom_point() + stat_smooth() + theme_bw()
splot.log

splot.sqrt <- ggplot(ag, aes(x=sqrt(CWSI), y=SWC)) + 
              geom_point() + stat_smooth() + theme_bw() + 
              labs(x = expression(sqrt(CWSI)))
splot.sqrt












