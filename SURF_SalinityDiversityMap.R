#Clears workplace
rm(list=ls())

#install and load librarys
install.packages("leaflet")
install.packages("usmap")
install.packages("ggmap")
install.packages("viridis")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggrepel")
install.packages("ggpubr")
install.packages("ggsn")
install.packages("magick")
install.packages("maptools")
install.packages("ggimage")
install.packages("here")
install.packages("lemon")

library(leaflet)
library(usmap)
library(ggplot2)
library(ggmap)
library(readxl) 
library(extrafont)
font_import()
loadfonts(device="win") 

library(viridis)
library(tidyverse)
library(lubridate)
library(data.table)
library(ggrepel)
library(ggpubr)
library(ggsn)
library(magick)
library(maps)
library(maptools)
library(ggplot2)
library(grid)
library(ggimage)
library(ggmap)
library(here)
library(lemon)


#Stores coordinates bottom right corner and top left corner 
myLocation <- c(-71.5316, 41.4, -71.15, 41.838874)

#Sets map type
myMap <- get_map(location = myLocation, source = "stamen", maptype = "terrain-background")
bb <- attr(myMap, "bb")
bb2 <- data.frame(long = unlist(bb[c(2, 4)]), lat = unlist(bb[c(1, 3)]))

#gets working directory
getwd()

#Downloads data from excel
avgShan = read.csv ("AvgSannonLat.csv")
ShannonMap = data.frame(avgShan)

#Creates map
mapShannon <- ggmap(myMap) + 
  labs(x = 'Longitude') + #Labels
  geom_point(aes(x = long, y = lat, size = 3, colour = avg.shannon), data = ShannonMap) + #Adds points to map
  scale_colour_viridis(direction = -1, option = "A") + #Scale colour
  theme(text=element_text(family="Times New Roman", face="bold", size=12)) + #Changes font
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank()) +
  theme(legend.position = c(0.83, 0.2)) +
  labs(colour = "Shannon") + #rename legend labels
  guides(color = guide_colorbar(order = 1),fill = guide_legend(order = 0)) +
  scale_size(guide = 'none')
mapShannon

reposition_legend(d, 'top left')

ggsave(filename = "SURF_SalinityDiversityMap", device = "pdf", width = 10, height = 8)

mapSalinity <- ggmap(myMap) + 
  labs(x = 'Longitude', y = 'Latitude') + #Labels
  geom_point(aes(x = long, y = lat, size = 3, colour = avg.salinity), data = ShannonMap) + #Adds points to map
  scale_colour_viridis(direction = -1, option = "A") + #Scale colour
  theme(text=element_text(family="Times New Roman", face="bold", size=12)) + #Changes font
  theme(legend.position = c(0.83, 0.2)) + 
  scale_size(guide = 'none') +
  labs(colour = "Salinity")  #rename legend labels
mapSalinity

