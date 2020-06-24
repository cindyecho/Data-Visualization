library(ggplot2)
library(lubridate)
library(stringr)
library(hexbin)


setwd("C:/Users/Cindy/Documents/DSC 465/Homework/Homework 4")


#Problem 3:
require(grDevices) # for colors

terrain_df = read.csv("terrain1.csv")
head(terrain_df)
help(package=colorspace)
## Heat Map"
ggplot(data=terrain_df, aes(x=x, y=y, fill=z)) + 
  scale_fill_gradientn(colors=terrain.colors(10))  +
  labs(title="Heat Map of Terrain Data", x="Longitude", y = "Latitude", fill="Altitude") +
  geom_tile() 

## Contour Plot
#ggplot(terrain_df, aes(x = x, y = y, z = z, fill=z)) + 
#  geom_tile() + scale_fill_gradientn(colors=terrain.colors(10)) + 
#  geom_contour(color="black") +
#  labs(title="Contour Plot of Terrain Data", x="Longitude", y = "Latitude", fill="Altitude") 


ggplot(data=terrain_df, aes(x=x, y=y, fill=z))  + 
  geom_tile() + scale_fill_gradientn(colors=terrain.colors(10)) + 
  labs(title="Contour Plot of Terrain Data Overlay", x="Longitude", y = "Latitude", fill="Altitude") +
  geom_contour(aes(x = x, y = y, z = z), color="black")
