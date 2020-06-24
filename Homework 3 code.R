library(ggplot2)
library(lubridate)
library(stringr)
library(hexbin)

setwd("C:/Users/Cindy/Documents/DSC 465/Homework/Homework 3")

#Problem 2:
water = read.csv("PortlandWaterLevel2003 - for R.csv")

head(water)
water = water[-1,]
head(water)
class(water$Time)

WaterLevel=water$WL
day <- as.Date(water$Date, "%m/%d/%Y")
water$new_time = strptime(water$Time, format="%H:%M")
water$new_time = as.POSIXlt(water$new_time)
water$new_time
class(water$new_time)
water$hour = hour(water$new_time)
head(water)


ggplot(data=water, aes(x=hour, y=day, fill=WL)) + scale_fill_distiller(palette = "PRGn") +
  labs(title="Portland Water Level HeatMap for 2003", x="Hour", y = "Month", fill="Water Level") +
  geom_tile()

## Using Spectral Color"
ggplot(data=water, aes(x=hour, y=day, fill=WL)) + scale_fill_distiller(palette = "Spectral")  +
  labs(title="Portland Water Level HeatMap for 2003", x="Hour", y = "Month", fill="Water Level") +
  geom_tile() + scale_x_continuous(breaks=seq(0,24)) +
  scale_y_date(date_breaks = "months" , date_labels = "%b %d")


## Problem 3
chicagocrime = read.csv("ChicagoCrime2018.csv")
head(chicagocrime)

class(chicagocrime$Date)
head(chicagocrime$Date)
## Problem 3a).
chicagocrime$Date = strptime(chicagocrime$Date, format="%m/%d/%Y %I:%M:%S %p")
chicagocrime$Date = as.POSIXlt(chicagocrime$Date)
chicagocrime$Date
class(chicagocrime$Date)
chicagocrime$Month = month(chicagocrime$Date)
chicagocrime$Hour = hour(chicagocrime$Date)

#chicagocrime$Date

library(stringr)
#p = str_split_fixed((chicagocrime$Date), " ", 2)
#head(p)
#new_date = p[,1]
#new_date
#Time = p[,2]
#Time
#chicagocrime$new_date = as.Date(chicagocrime$Date)
#new_date
#class(new_date)
# chicagocrime$Month <- months.Date(new_date)
#chicagocrime$Hour = hour(chicagocrime$Date)
#class(chicagocrime$Hour)
#head(chicagocrime)
#chicagocrime$Hour
#subcrime = subset(chicagocrime, subset = Primary.Type %in% c("ASSAULT","", "NARCOTICS", "THEFT"))
#subcrime


## Subsetting the data
Assault = subset(chicagocrime, Primary.Type == "ASSAULT")
Kidnapping = subset(chicagocrime, Primary.Type == "KIDNAPPING")
Narcotics = subset(chicagocrime, Primary.Type == "NARCOTICS")
Theft = subset(chicagocrime, Primary.Type == "THEFT")
HOMICIDE = subset(chicagocrime, Primary.Type == "HOMICIDE")


#_____________________________________________________________________________________________#

## Problem 3a.) ##

##Bar Graphs by Month

ggplot(Assault, aes(x=Month)) + geom_bar(fill='tomato') + labs(title="Assault Count by Month") + scale_x_discrete(limits = month.abb)
ggplot(Kidnapping, aes(x=Month)) + geom_bar(fill='royalblue1') + labs(title="Kidnapping Count by Month") +scale_x_discrete(limits = month.abb)
ggplot(Narcotics, aes(x=Month)) + geom_bar(fill='green4') + labs(title="Narcotics Count by Month") + scale_x_discrete(limits = month.abb)
ggplot(Theft, aes(x=Month)) + geom_bar(fill='goldenrod') + labs(title="Theft Count by Month")+ scale_x_discrete(limits = month.abb)

#_____________________________________________________________________________________________#


## Problem 3b.) ##

# Rose Plot by Month

## Assault Rose Plot
ggplot(Assault, aes(x = Month)) + 
  geom_histogram(breaks = seq(0, 12),fill = "tomato1", color="grey") + 
  coord_polar(start = 0) + ylab("Count") + ggtitle("Assault Count by Month") +
  scale_x_continuous("", limits = c(0, 12), breaks = seq(12), labels = seq(12))

## Kidnapping Rose Plot
ggplot(Kidnapping, aes(x = Month)) + 
  geom_histogram(breaks = seq(0, 12),fill = "royalblue1", color="grey") + 
  coord_polar(start = 0) +  ylab("Count") + ggtitle("Kidnapping Count by Month") + 
  scale_x_continuous("", limits = c(0, 12), breaks = seq(12), labels = seq(12))

## Narcotics Rose Plot
ggplot(Narcotics, aes(x = Month)) + 
  geom_histogram(breaks = seq(0, 12),fill = "green4", color="grey") + 
  coord_polar(start = 0) +  ylab("Count") + ggtitle("Narcotics Count by Month") + 
  scale_x_continuous("", limits = c(0,12), breaks = seq(12), labels = seq(12))

## Theft Rose Plot
ggplot(Theft, aes(x = Month)) + 
  geom_histogram(breaks = seq(0, 12),fill = "goldenrod", color="grey") + 
  coord_polar(start = 0) + ylab("Count") + ggtitle("Theft Count by Month") + 
  scale_x_continuous("", limits = c(0, 12), breaks = seq(12), labels = seq(12))


#_____________________________________________________________________________________________#

## Problem 3d.) ##

head(chicagocrime)

ggplot(HOMICIDE, aes(x=Longitude, y=Latitude) ) + geom_hex() + theme_bw() +
  ggtitle("Homicide Rate") 


#_____________________________________________________________________________________________#

## Problem 3e.) ##

##Bar Graphs by Hour

ggplot(Assault, aes(x=Hour)) + geom_histogram(binwidth=3, fill='tomato1') + labs(title="Assault Count by Hour") 
ggplot(Kidnapping, aes(x=Hour)) + geom_bar(fill='royalblue1') + labs(title="Kidnapping Count by Hour") +scale_x_continuous()
ggplot(Narcotics, aes(x=Hour)) + geom_bar(fill='green4') + labs(title="Narcotics Count by Hour")  +scale_x_continuous()
ggplot(Theft, aes(x=Hour)) + geom_bar(fill='goldenrod') + labs(title="Theft Count by Hour") +scale_x_continuous()


# Rose Plot by Hour

## Assault Rose Plot
ggplot(Assault, aes(x = Hour)) + 
  geom_bar(width=1, fill = "tomato1", color="grey") + 
  coord_polar(start = 0) + ylab("Count") + ggtitle("Assault Count by Hour") +
  scale_x_continuous("", breaks = seq(0,24), labels = seq(0,24))


## Kidnapping Rose Plot
ggplot(Kidnapping, aes(x = Hour)) + 
  geom_bar(width=1, fill = "royalblue1", color="grey") + 
  coord_polar(start = 0)  + ylab("Count") + ggtitle("Kidnapping Count by Hour") +
  scale_x_continuous("", breaks = seq(0,24), labels = seq(0,24))

## Narcotics Rose Plot
ggplot(Narcotics, aes(x = Hour)) + 
  geom_bar(width=1, fill = "green4", color="grey") + 
  coord_polar(start = 0) + ylab("Count") + ggtitle("Narcotics Count by Hour") +
  scale_x_continuous("", breaks = seq(0,24), labels = seq(0,24))


## Theft Rose Plot
ggplot(Theft, aes(x = Hour)) + 
  geom_bar(width=1, fill = "goldenrod", color="grey") + 
  coord_polar(start = 0) +  ylab("Count") + ggtitle("Theft Count by Hour") +
  scale_x_continuous("", breaks = seq(0,24), labels = seq(0,24))







## Test ##

#count(is.na(Assault$Hour))
#which(is.na(Assault$Hour))
#miss = subset(Month, Month==101121)
#miss
#chicagocrime$Date[101121]
#chicagocrime$Date[101120]
#Month <- month(new_date)

#chicagocrime$Date[101119]
#chicagocrime$Date[101122]
