# Question 1

library(data.table)

DT = data.table("Procedure1"=c(22.6,27.4,32.2,37.6,42.8,37.1,32.4,27.3,32.2,24.4,42.5,37.6,22.9,47.6),
                "Procedure2"=c(22.6,32.8,30.9,42.3,37.4,32.6,37.6,22.7,37.1,22.4,47.5,32.3,27.0,42.1))

#a) Calculate the mean and standard deviation of speeds for the two data sets separately.
m1<-mean(DT$Procedure1)
m2<-mean(DT$Procedure2)
sd1<-sd(DT$Procedure1)
sd2<-sd(DT$Procedure2)

#b) Are the speeds calculated using procedure 1 higher than 25 mph at a level of significance 0.05? 
t.test(DT$Procedure1, mu=25, alternative = 'greater')

#Results: Yes, with a p-value less than 0.05, we can conclude that the speeds
#are higher than 25mph at a level of significance 0.05.


#c) In order to find whether the data from both procedures are the same, 
#   conduct tests on the mean at a level of significance of 0.05. 
#   State your hypotheses and assume that the variances are equal. 
#   Do the necessary calculations and present your results.

#Null: There is no significant difference in the speeds calculated using
#procedures 1 & 2. 
#Alternate: There is a significant difference in the speeds calculate using
#procedures 1 & 2. 

t.test(DT$Procedure1, DT$Procedure2, paired=TRUE, var.equal=TRUE)

#Results: Cannot reject the Null. Since the p-value is larger than 0.05, 
#we cannot conclude that a significant difference exists between the two
#procedures. 


#d) Would it have been more appropriate to use the paired-t test in (b) rather than a 
#   simple t-test and why?

#A paired t test would be more appropriate since the speed observations for procedure 1
#and procedure 2 are paired and we are interested in comparing the results of the 
#two procedures. 


#e) Regress speeds measured by procedure 1 against speeds measured by procedure 2. 
#   Show the regression results and discuss on the hypothesis tests used in the regression model. 
plot(DT$Procedure1, DT$Procedure2, xlab='Procedure1 Speed', ylab='Procedure2 Speed', title(main = 'Procedure1 vs Procedure2 Speeds'))
mod <- lm(DT$Procedure1~ DT$Procedure2)
abline(mod)
summary(mod)

#Null: There is a significant difference in the speeds calculated using
#procedures 1 & 2. 
#Alternate: There is no significant difference in the speeds calculate using
#procedures 1 & 2. 

#Results: The linear regression accounts for ~66% of the variance in speeds and fits the 
#data relatively well. With a p-value less than 0.05, we can reject the Null that 
#there is significant variance in the speeds of procedure 1 and procedure 2. 




#Question 2

# Get current working directory
setwd('/Users/tashaygreen/Downloads')

# Read in data
SpeedData <- read.table("speed.csv", header = T, sep = ',')
SpeedData <- read.csv("speed.csv")
SpeedData <- read.delim("speed.csv", header = T, sep = ',')

#a) Use the dataset “speed.csv” to validate the relationship between space mean speed and time mean speed.
SpeedData['validation']= SpeedData$time.mean.speed - ((var(SpeedData$time.mean.speed))/ SpeedData$time.mean.speed)
SpeedData

#Null: The calculated space mean speed is equal to the calculated 
#space mean speed at a level of confidence 0.05.
#Alternate: The calculated space mean speed is not equal to the calculated 
#space mean speed at a level of confidence 0.05. 

t.test(SpeedData$validation, SpeedData$space.mean.speed)

#Results: With a p value greater than 0.05, we cannot reject the Null hypothesis. 
#Therefore, we can conclude that the calculated and observed values for space mean speed
#are the same at level of significance 0.05. 
#We are able to validate the relationship between space mean speed and time mean speed. 
