#Get current working directory
setwd('/Users/tashaygreen/Downloads')

# Before reading raw data:
# Here are the definitions for data in each column
# col_1=Timestamp; col_2=Station; col_3: District; col_4=Freeway #; col_5=Directon (NSEW)
# col_6=Lane Type; col_7=Station Length; col_8=Samples; col_9=% Observed; col_10=Total Flow
# col11=Avg Occupancy; col_12=Ave Speed; 
# the remaining columns show individual info (Lane N Samples, Flow, Avg Occ, Avg Speed,Obverved(=1observed; 0=imputed))

Filename <- "d04_text_station_5min_2013_12_18.txt.gz"
PeMS <- read.delim(gzfile(Filename), sep="," ,header=FALSE)
dim(PeMS)
PeMS[1,]


############################
#  Question 1: Simple plot
############################
# Selecting a loop detector station with a given ID, for instance SelectedID=404922 (lanes=3)
SelectedID <- 404916   
StationData_1 <- subset(PeMS,PeMS [,2]==SelectedID)

SelectedID <- 404922  
StationData_2 <- subset(PeMS,PeMS [,2]==SelectedID)

# Temporal distribution of flow, average speed, and average occupancy for station 404916
dev.off()
dev.new(width=8.4, height=7.0)
par(mfrow=c(2,3))

TimeInterval <- 5
LowTime <- 0s
UpTime <- length(Filename)*24*60/TimeInterval
plot(ts(StationData_1[,1]),StationData_1[,10],type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(StationData_1[,10])*1.1),xlab="Timestamp (Interval=5min)",ylab="Flow (veh/5min)",main="Time vs. Flow",col="blue")
plot(ts(StationData_1[,1]),StationData_1[,12],type="l",xlim=c(LowTime,UpTime),ylim=c(0,100),xlab="Timestamp (Interval=5min)",ylab="Average Speed (mph)",main="Time vs. Average Speed",col="blue")
plot(ts(StationData_1[,1]),StationData_1[,11]*100,type="l",xlim=c(LowTime,UpTime),ylim=c(0,100),xlab="Timestamp (Interval=5min)",ylab="Average Occupancy (%)",main="Time vs. Average Occupancy",col="blue")
plot(StationData_1[,10],StationData_1[,12],xlim=c(0,max(StationData_1[,10])*1.1),ylim=c(0,100),xlab="Flow (veh/5min)",ylab="Average Speed (mph)",main="Flow vs. Average Speed",col="blue",pch=19,cex=0.8)
plot(StationData_1[,10],StationData_1[,11]*100,xlim=c(0,600),ylim=c(0,100),xlab="Flow (veh/5min)",ylab="Average Occupancy (%)",main="Flow vs. Average Occupancy",col="blue",pch=19,cex=0.8)
plot(StationData_1[,12],StationData_1[,11]*100,xlim=c(0,100),ylim=c(0,100),xlab="Average Speed (mph)",ylab="Average Occupancy (%)",main="Average Occupancy vs. Average Speed",col="blue",pch=19,cex=0.8)


# Temporal distribution of flow, average speed, and average occupancy for station 404922
dev.off()
dev.new(width=8.4, height=7.0)
par(mfrow=c(2,3))

TimeInterval <- 5
LowTime <- 0
UpTime <- length(Filename)*24*60/TimeInterval
plot(ts(StationData_2[,1]),StationData_2[,10],type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(StationData_2[,10])*1.1),xlab="Timestamp (Interval=5min)",ylab="Flow (veh/5min)",main="Time vs. Flow",col="blue")
plot(ts(StationData_2[,1]),StationData_2[,12],type="l",xlim=c(LowTime,UpTime),ylim=c(0,100),xlab="Timestamp (Interval=5min)",ylab="Average Speed (mph)",main="Time vs. Average Speed",col="blue")
plot(ts(StationData_2[,1]),StationData_2[,11]*100,type="l",xlim=c(LowTime,UpTime),ylim=c(0,100),xlab="Timestamp (Interval=5min)",ylab="Average Occupancy (%)",main="Time vs. Average Occupancy",col="blue")
plot(StationData_2[,10],StationData_2[,12],xlim=c(0,max(StationData_2[,10])*1.1),ylim=c(0,100),xlab="Flow (veh/5min)",ylab="Average Speed (mph)",main="Flow vs. Average Speed",col="blue",pch=19,cex=0.8)
plot(StationData_2[,10],StationData_2[,11]*100,xlim=c(0,600),ylim=c(0,100),xlab="Flow (veh/5min)",ylab="Average Occupancy (%)",main="Flow vs. Average Occupancy",col="blue",pch=19,cex=0.8)
plot(StationData_2[,12],StationData_2[,11]*100,xlim=c(0,100),ylim=c(0,100),xlab="Average Speed (mph)",ylab="Average Occupancy (%)",main="Average Occupancy vs. Average Speed",col="blue",pch=19,cex=0.8)


#############################################
#  Question 2: Cumulative and Oblique Curves #
#############################################

## Station 1 
dev.off()
dev.new(width=8.4, height=7.0)
par(mfrow=c(2,2))
# Assume capacity is 1000 for each lane
lanes=5  # Number of lanes of the roadway at a given direction
obliqueQ <- 1000*lanes/12*(1:288)
plot(ts(StationData_1[,1]),cumsum(StationData_1[,10]),type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(obliqueQ,cumsum(StationData_1[,10]))*1.5),xlab="Timestamp (Interval=5min)",ylab="Cumulative Flow N (t) (veh/5min)",main="Time vs. Flow",col="blue")
lines(1:288,obliqueQ,lwd=2,col=2)
plot(ts(StationData_1[,1]),cumsum(StationData_1[,10])-obliqueQ,type="l",xlim=c(LowTime,UpTime),ylim=c(min(cumsum(StationData_1[,10])-obliqueQ)*1.1,max(cumsum(StationData_1[,10])-obliqueQ)*1.1),xlab="Timestamp (Interval=5min)",ylab="Oblique Flow: N(t)-q*t (veh/5min)",main="Time vs. Flow",col="blue")

# Assume normal occupancy is 6%
obliqueO <- 0.06*(1:288)*100
plot(ts(StationData_1[,1]),cumsum(StationData_1[,11])*100,type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(cumsum(StationData_1[,11]))*100*1.5),xlab="Timestamp (Interval=5min)",ylab="Cumulative Occupancy O(t) (%)",main="Time vs. Average Occupancy",col="blue")
lines(1:288,obliqueO,lwd=2,col=2)
plot(ts(StationData_1[,1]),cumsum(StationData_1[,11])*100-obliqueO,type="l",xlim=c(LowTime,UpTime),ylim=c(min(cumsum(StationData_1[,11])*100-obliqueO)*1.1,max(cumsum(StationData_1[,11])*100-obliqueO)*1.1),xlab="Timestamp (Interval=5min)",ylab="Oblique Occupancy: O(t)-occ*t (%)",main="Time vs. Average Occupancy",col="blue")



## Station 2 
dev.off()
dev.new(width=8.4, height=7.0)
par(mfrow=c(2,2))
# Assume capacity is 1000 for each lane
lanes=5  # Number of lanes of the roadway at a given direction
obliqueQ <- 1000*lanes/12*(1:288)
plot(ts(StationData_2[,1]),cumsum(StationData_2[,10]),type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(obliqueQ,cumsum(StationData_2[,10]))*1.5),xlab="Timestamp (Interval=5min)",ylab="Cumulative Flow N (t) (veh/5min)",main="Time vs. Flow",col="blue")
lines(1:288,obliqueQ,lwd=2,col=2)
plot(ts(StationData_2[,1]),cumsum(StationData_2[,10])-obliqueQ,type="l",xlim=c(LowTime,UpTime),ylim=c(min(cumsum(StationData_2[,10])-obliqueQ)*1.1,max(cumsum(StationData_2[,10])-obliqueQ)*1.1),xlab="Timestamp (Interval=5min)",ylab="Oblique Flow: N(t)-q*t (veh/5min)",main="Time vs. Flow",col="blue")

# Assume normal occupancy is 6%
obliqueO <- 0.06*(1:288)*100
plot(ts(StationData_2[,1]),cumsum(StationData_2[,11])*100,type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(cumsum(StationData_2[,11]))*100*1.5),xlab="Timestamp (Interval=5min)",ylab="Cumulative Occupancy O(t) (%)",main="Time vs. Average Occupancy",col="blue")
lines(1:288,obliqueO,lwd=2,col=2)
plot(ts(StationData_2[,1]),cumsum(StationData_2[,11])*100-obliqueO,type="l",xlim=c(LowTime,UpTime),ylim=c(min(cumsum(StationData_2[,11])*100-obliqueO)*1.1,max(cumsum(StationData_2[,11])*100-obliqueO)*1.1),xlab="Timestamp (Interval=5min)",ylab="Oblique Occupancy: O(t)-occ*t (%)",main="Time vs. Average Occupancy",col="blue")


#############################################
#  Question 3: Traffic Condition Inferences
#############################################

## Based on the plots, the following inferences can be made: 
  ## The observed cumulative occupancy curve and cumulative flow curve approach the reference
  ## line but never exceed it.  Which lead me to believe that there may not be any queuing occuring 
  ## between the two detector stations. 
  ## As seen on the time vs avg speed plots, around 215 timestamp, there is a significant 
  ## decline in speed for station 404922. There must be an event that causes backup between the
  ## two stations. 
  ## Lastly, as seen on the oblique occupancy curve, traffic accumulates faster at station 404922
  ## than 404916 during the later timestamps events. 
