library (ggplot2)
library(plyr)
source("GeomSplitViolin.R")

## Homework 2

setwd("C:/Users/Cindy/Documents/DSC 465/Homework")


## Problem 3

## reading in data
perception <- read.csv("PerceptionExperiment2007-2015Fall.csv")
head(perception)
dim(perception)

## Creating 'Error' variable
perception$Error = perception$Response - perception$TrueValue
perception$Error

perception$absError = abs(perception$Error)

n = nrow(perception) 

###############################################################################

## Problem 3a).
medError = ddply(perception, "Test", summarize, median=median(Error))
medError

# Next, we sort the medians by their value.  We cannot simply "reorder"
# because we are not reordering a single column as before, but a whole data
# frame. So, we use the "row" indexing operator with the "order" function

medError = medError[order(medError$median), ]
head(medError)


# Dotchart
par(mfrow=c(1,1))
medError$color[medError$median < 0.0] <- "red"
medError$color[medError$median >= 0.0] <- "blue"
#medError$color[medError$median == 0.0] <- "black"
dotchart(medError$median, medError$Test,col = medError$color, 
  pch=16,main="Perception Test Dot Plot", xlab="Median Error")

  

## Problem 3b).

ggplot(perception, aes(reorder(Test,absError), y=absError)) + geom_boxplot(aes(fill=Test))+
  geom_jitter(aes(x=as.numeric(Test) + rnorm(n, 0, .05), y=absError), color="black", alpha=.5, width=0.25, size=1.5) +
  labs(title="Boxplot of Absolute Error by Perception Test", x="Test", y = "Absolute Error") +
  guides(fill=FALSE)  ## FINAL CODE FOR 3b


###############################################################################
  
# Problem 3d). - violin plots
require(ggplot2)

ggplot(perception, aes(reorder(Test, Error), y=Error, fill=Test)) + 
  geom_violin() + ylab("Error") + guides(fill=FALSE) + 
  labs(title="Perception Test Violin Plot", x="Test", y = "Error")

VDNA = subset(perception, Test=="Vertical Distance, Non-Aligned")
hist(VDNA$Error, col="blue")
mean(VDNA$Error)
median(VDNA$Error)

VDA = subset(perception, Test=="Veritcal Distance, Aligned")
hist(VDA$Error, col="blue")
mean(VDA$Error)
median(VDA$Error)

# with JITTERING _ DNU
ggplot(perception, aes(reorder(Test, Error), y=Error, fill=Test)) + 
  geom_violin() + geom_jitter(color="gray2", width = 0.1)

ggplot(perception, aes(reorder(Test, Error))) + geom_violin(aes(fill=Test)) + 
  geom_point(aes(x=as.numeric(Test) + rnorm(n, 0, .05), y=Error), color="darkgreen", alpha=.06, size=2) + 
  coord_flip()

###############################################################################

ggplot(perception, aes(x=Test, y=Error)) + geom_boxplot() +
  geom_point(aes(x=as.numeric(Test) + rnorm(4416, 0, .03), y=Error), color="darkgreen", alpha=.2, size=2, width=0.3)


################################################################################


# Problem 3e). - subsetting data
modifiedPerception = subset(perception, perception$Subject >= 56 & perception$Subject<= 73)
head(modifiedPerception)
dim(modifiedPerception)
modifiedPerception

# i.
ggplot(modifiedPerception, aes(x=absError, fill=factor(Display))) +
  geom_histogram(alpha=.5, position="Identity", color = "black") +
  scale_fill_brewer(palette="Dark2") +
  labs(title = "Perception Test Histogram")


par(mfrow=c(2, 1))  

Display1 = subset(modifiedPerception, Display=="1")
Display2 = subset(modifiedPerception, Display=="2")
hist(Display1$Error, col="lightgreen")
hist(Display2$absError, col="cyan")


ggplot(modifiedPerception, aes(x=factor(Display), y=Error, fill=factor(Display))) +
  geom_boxplot() + labs(title = "Boxplots for Display", x = "Display") +
  guides(fill=FALSE) + scale_fill_brewer(palette="Dark2")

ggplot(modifiedPerception, aes(x=factor(Display), y=Error, fill=factor(Display))) + 
  geom_violin() + ylab("Error") + guides(fill=FALSE) + 
  labs(title="Perception Test Violin Plot", x="Display", y = "Error")


# ii.

# Now, we can do better than this by splitting the violins in each group
ggplot(modifiedPerception, aes(x=Test, y=absError, fill=factor(Display))) +
  geom_split_violin() + guides(fill=guide_legend(title="Display")) +
  coord_flip()# We can even display the boxplots over each side in the split violin plot

ggplot(modifiedPerception, aes(reorder(Test, Error), y=Error, fill=factor(Display))) + 
  geom_split_violin() + guides(fill=guide_legend(title="Display")) +
  labs(title = "Perception Test Violin Plot w.r.t to Display", x="Test")


###############################################################################

## Problem 3f). was done in Tableau

## Problem 3g).

library(dplyr)
reducedPerception <- filter(perception,  !(perception$Subject %in% 56:73))
reducedPerception  ## new perception data without subjects 56-73
dim(reducedPerception)


ggplot(reducedPerception, aes(reorder(Test, Error), y=Error, fill=factor(Display))) + 
  geom_split_violin() + guides(fill=guide_legend(title="Display")) +
  labs(title = "Grouped Violin Plot with Split Violins", 
       subtitle="observations 56-73 removed", x="Test") #KEEP


## Problem 6
library(readxl)
MessierData <- read_excel("MessierDataCleanHeaders.xlsx")
View(MessierData)   


ggplot(MessierData, aes(x=Kind, y=Distance)) +
  geom_boxplot() + scale_y_log10(labels=scales::comma)
