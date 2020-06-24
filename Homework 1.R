library (ggplot2)
library(readxl)

## Homework 1

setwd("C:/Users/Cindy/Documents/DSC 465/Homework/Homework 1")


# Problem 2
Intel <- read.csv("Intel-1998.csv")
head(Intel)
names(Intel)
ggplot(Intel, aes(x = Volume)) + 
  geom_histogram(bins=20,  fill="blue", color = "black")# binwidth is specific to the histogram


# Problem 3
perception <- read.csv("PerceptionExperiment.csv")
head(perception)
dim(perception)

## 3a). Creating 'Error' variable
Error = perception$Response - perception$TrueValue
Error

## 3b). Histogram
ggplot(perception, aes(x = Error)) + 
  geom_histogram(bins=15,  fill="blue", color = "black")# binwidth is specific to the histogram

## 3c). Bar Graph

median.Error = median(Error)
ggplot(perception, aes(x = Test, y = median.Error)) + geom_bar(stat="summary")

ggplot(perception, aes(x = Test, y = median.Error, fill = Test)) + 
  geom_bar(stat="summary", fun.y=median)

ggplot(perception, aes(reorder(Test,abs(Error)), y=abs(Error), fill=Test)) +
  geom_boxplot() +
  labs(title="Boxplot of Absolute Error by Perception Test", x="Test", y = "Absolute Error")

sortAbsError = sort(abs(Error))
sortAbsError

mean()





# Problem 4 - Infant Data
InfantData <- read.csv("InfantData.csv")
head(InfantData)

## 4a). Scatter plot
ggplot(InfantData, aes(x = Height.in, y = Weight.lbs, color = Sex)) + 
  geom_point() + labs(title="Height and Weight of Infants", x="Height (inches)", y = "Weight (in lbs)")

## 4b). Scatter plot with regression lines
ggplot(InfantData, aes(x = Height.in, y = Weight.lbs, color = Sex)) + 
  geom_point(alpha=.4)+ geom_smooth(method = "lm", se = F) + 
  labs(title="Height and Weight of Infants (with trend lines)",
     x="Height (inches)", y = "Weight (in lbs)")

## 4c). On word document

# Problem 5 - on Tableau

# Problem 6c). Native American Reservation Box Plot

reservation <- read.csv("reservation70-00.csv")
head(reservation)

ggplot(reservation, aes(reorder(Year,Population), y=Population, fill=reservation)) +
  geom_boxplot()

# Problem 7 - Earthquake Data
earthquake <- read.csv("Problem 7 Earthquake.csv")
earthquake

ggplot(earthquake, aes(x = Year, y = Magnitude, color = Magnitude)) + 
  geom_point(alpha=.4)+ geom_smooth(method = "lm", se = F) + labs(title="Earthquake Frequency in the Bar Area")
