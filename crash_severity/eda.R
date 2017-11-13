crash <- read.table("C:/Users/David Lowe/Desktop/STAT 536/Crash/Crash.txt",header=TRUE)
# pairs(crash[,2:14])
crash <- crash[,c(9,2:8,10:14)]
names(crash) <- tolower(names(crash))

for(i in 2:13){
  scatter.smooth(crash[,i],crash$severity,xlab=names(crash)[i],ylab="SEVERITY",main=paste("Smooth Curve of ",names(crash)[i]," vs. SEVERITY",p=""))
}

scatter.smooth(crash$vspd_lim,crash$severity,main="Smoothed Curve of Speed Limit vs. Severity",xlab="Speed Limit",ylab="Severity",pch=19,cex=.1,col="grey100")
points(jitter(crash$vspd_lim),jitter(crash$severity),pch=3,cex=.5)


aggregate(crash$SEVERITY,by=list(crash$HOUR),mean)

mn_ct <- function(x){
  cbind(mean(x),round(sum(x)),round(length(x)))
}

lapply(2:13, function(i){
    aggregate(crash$SEVERITY,by=list(crash[,i]),mn_ct)
  })



# [2,] "HOUR"    
# [3,] "LGT_COND"
# [4,] "WEATHER" 
# [5,] "ALCOHOL" 
# [6,] "TYP_INT" 
# [7,] "REST_USE"
# [8,] "AIR_BAG" 
# [9,] "VTRAFWAY"
# [10,] "VNUM_LAN"
# [11,] "VSPD_LIM"
# [12,] "VALIGN"  
# [13,] "VSURCOND"

# lgt_cond = Light Condition
# 1 Daylight
# 2 Dark - Not Lighted
# 3 Dark - Lighted
# 4 Dawn
# 5 Dusk

# COMBINATIONS

# 4 & 5

ltcond <- as.data.frame(model.matrix(~-1+as.factor(lgt_cond),data=crash))
ltcond$c4_5 <- ltcond[,4] + ltcond[,5]
ltcond <- ltcond[,c(1:3,6)]
names(ltcond) <- paste("ltcond",c("Day","DarkNL","DarkL","LowLight"),sep="_")

# weather
# 1 Clear
# 2 Rain
# 3 Sleet or Hail
# 4 Snow
# 5 Fog, Smog, Smoke
# 6 Severe Crosswinds
# 7 Blowing Sand, Soil, Dirt
# 10 Cloudy
# 11 Blowing Snow
# 12 Freezing Rain or Drizzle


# COMBINATIONS

# 2 & 12
# 3 & 4 & 11
# 6 & 7

mat <- as.data.frame(model.matrix(~-1+as.factor(weather),data=crash))
mat$c2_10 <- mat[,2] + mat[,10]
mat$c3_4_11 <- mat[,3] + mat[,4] + mat[,9]
mat$c6_7 <- mat[,6] + mat[,7]
weather <- mat[,c(1,11,12,5,8,13)]
names(weather) <- paste("Wthr",c("Clear","Rain","Snow","Fog","Cloudy","Windy"),sep="_")



# typ_int = type of intersection
# 1 Not an Intersection
# 2 Four-Way Intersection
# 3 T-Intersection
# 4 Y-Intersection
# 5 Traffic Circle
# 6 Roundabout
# 7 Five-Point, or More
# 10 L-Intersection

# COMBINATIONS

# 5 & 6
# ask about 10

mat <- as.data.frame(model.matrix(~-1+as.factor(typ_int),data=crash))
mat$c5_6 <- mat[,5] + mat[,6]
typeInt <- mat[,c(1:4,9,7:8)]
names(typeInt) <- paste("TypInt",c("NI","4W","T","Y","RA","5P","L"),sep="_")



# rest_use = restraint/helmet use
# 00 Not Applicable
# 07 None Used
# 03 Shoulder and Lap Belt Used
# 01 Shoulder Belt Only Used
# 02 Lap Belt Only Used
# 08 Restraint Used - Type Unknown
# 05 DOT-Compliant Motorcycle Helmet

# COMBINATIONS

# 1 & 2
# Maybe drop NA (0)

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

# COMBINATIONS

# 3 & 7

mat <- as.data.frame(model.matrix(~-1+as.factor(air_bag),data=crash))
mat$c5_6 <- mat[,4] + mat[,5]
air_bag <- mat[,c(1:3,9,6:8)]
names(air_bag) <- paste("AirBag",c("NA","Front","Side","Other","Comb","ND"),sep="_")



# vtrafway = vehicle traffic way
# 1 Two-Way, Not Divided
# 2 Two-Way, Divided, Unprotected (Painted > 4 Feet) Median
# 3 Two-Way, Divided, Positive Median Barrier
# 5 Two-Way, Not Divided With a Continuous Left-Turn Lane
# 4 One-Way Trafficway
# 6 Entrance/Exit Ramp

# COMBINATIONS


VTW <- as.data.frame(model.matrix(~-1+as.factor(vtrafway),data=crash))
names(VTW) <- paste("VTW",c("2WND","2WDU","2WDB","1WND","2WT","EntEx"),sep="_")

# vnum_lan = number of lanes
# 1 One lane
# 2 Two lanes
# 3 Three lanes
# 4 Four lanes
# 5 Five lanes
# 6 Six lanes
# 7 Seven or more lanes

# COMBINATIONS

# 5+

mat <- as.data.frame(model.matrix(~-1+as.factor(vnum_lan),data=crash))
mat$c5_6 <- mat[,5] + mat[,6] + mat[,7]
numln <- mat[,c(1:4,8)]
names(numln) <- paste("NumLn",1:5,sep="_")


# vspd_lim = speed limit
# 05-80 Actual Speed Limit (in 5 mph increments)




# valign = roadway alignment
# 1 Straight
# 2 Curve-Right
# 3 Curve-Left
# 4 Curve - Unknown Direction

align <- as.data.frame(model.matrix(~-1+as.factor(valign),data=crash))
names(align) <- paste("align",c("S","R","L","Uk"),sep="_")


# vsurcond = surface condition
# 01 Dry
# 02 Wet
# 03 Snow
# 10 Slush
# 04 Ice/Frost
# 06 Water (Standing, Moving)
# 05 Sand
# 11 Mud, Dirt, Gravel
# 08 Other

# COMBINATIONS

# 3 & 10 & 4?
# 5 & 6 & 8 & 11

mat <- as.data.frame(model.matrix(~-1+as.factor(vsurcond),data=crash))
mat$c5_6 <- mat[,3] + mat[,4] + mat[,8]
mat$c7_8 <- mat[,5] + mat[,6] + mat[,7] + mat[,9]
sCond <- mat[,c(1,2,10,11)]
names(sCond) <- paste("sCond",c("Dry","Wet","Snow","Other"),sep="_")



X <- data.frame(crash$hour,ltcond,weather,crash$alcohol,typeInt,RU,air_bag,VTW,crash$vspd_lim,align,sCond)
X.new <- as.matrix(as.numeric(X))


glm(severity~as.matrix(X.new),data=crash,family="binomial")












library(car)
crash <- read.table("crashes.txt",header=TRUE)
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











