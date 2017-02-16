#Get current working directory
setwd('/Users/tashaygreen/Downloads')

#Read in data
TrafficData <- read.table("HW1_Dataset.csv", header = T, sep = ',')
TrafficData <- read.csv("HW1_Dataset.csv")
TrafficData <- read.delim("HW1_Dataset.csv", header = T, sep = ',')

s1 <-"There are "
s2 <-" observations in this dataset."
rows <- toString(nrow(TrafficData))

s3 <-"There are "
s4 <- " columns in this dataset."
colCount<- length(TrafficData)

#a) What are the dimensions of the data set and what are the variable or column names?
print(paste(s1,rows,s2, sep = ""))
print(paste(s3,colCount,s4, sep = ""))
print(colnames(TrafficData))

#b) What are the maximum actual travel time and maximum historical travel time?
TrafficData['Actual_Time'] <- TrafficData$Segment_Length.mile / TrafficData$Actual_Speed.mph
TrafficData['Historical_Time'] <- TrafficData$Segment_Length.mile /TrafficData$Historical_Speed.mph
max(TrafficData$Actual_Time, na.rm=TRUE)
max(TrafficData$Historical_Time, na.rm=TRUE) 

#c) What is the actual travel time variance for the measurement in each hour? Note you have three-hours of observations.
first<-TrafficData[1:12,]
second<-TrafficData[13:24,]
third<-TrafficData[25:36,]

var1<- var(first$Actual_Time)
var2<- var(second$Actual_Time)
var3<- var(third$Actual_Time)

#d) In which five-minute intrval do you find the maximum difference to be between the actual and the historical travel times? Create a simple plot to show the difference in each five-minute interval. 
plot(TrafficData$Actual_Time, TrafficData$Historical_Time, xlab = "Actual Time", ylab = "Historical Time" )

#e) Regress actual travel time on historical travel time, and show the regress results. 
reg<- lm(Actual_Time ~ Historical_Time, data = TrafficData)
abline(reg)
summary(reg)
