#Get current working directory
setwd('/Users/tashaygreen/Downloads')

#Read in data
SampleData <- read.table("HWData.csv", header = T, sep = ',')
SampleData <- read.csv("HWData.csv")
SampleData <- read.delim("HWData.csv", header = T, sep = ',')

#Compute the cumulative number of vehicles at each time period. Write a function to perform the computation.
cumulative<-function(volume)
{
  cumsum(volume) 
}

volume<- SampleData$volume

SampleData['CumulativeVolume']<-cumulative(volume)

#Draw a cumulative curve (x-axis = time, y-axis = cum # veh)
plot(SampleData$CumulativeVolume,main='Time vs Cumulative Vehicles', xlab = "Time (minutes)", ylab = "Cumulative Volume",lty=2, col=4, xaxt='n')

#Draw a scatter plot (x-axis = volume, y-axis = speed). Highlight minimum speed with color red.
minimum<-subset(SampleData, speed==min(SampleData$speed))
plot(SampleData$volume, SampleData$speed, main='Volume vs. Speed',xlab = "Volume (veh/15min)", ylab = "Speed (mph)" )
points(minimum,col='red', pch=17)

