###########################################################
#  R Script to create map of field sites                  #
#  Created by Piper                                       #
#  Created on 12 Feb 2018                                 #
#  Updated on 16 April 2018                               #
###########################################################

# clear workspace
rm(list=ls())


# Load Packages
library("maps")
library("mapdata")
library("dplyr")
library("RColorBrewer")

# datasets
data(us.cities)
sites <- read.csv("Data/SiteData.csv")
sites <- arrange(sites, Mean)
x <- rep(1, 20)
y <- seq(10,19.5, 0.5)


# colors
colfunc <- colorRampPalette(c("blue4", "yellow", "tomato3")) # flip for map
cols <- colfunc(20)  



# Create plot
png("Plots/Survey Map.png", width = 1450, height = 2100, res = 300)
layout(matrix(c(1,1,2), ncol=3), widths=c(3,1))

# prepare border
par(mar = c(5,5,4,2))
plot(sites$Long, sites$Latitude,cex = 0.25, xlab = expression(paste('Longitude')), ylab = expression(paste('Latitude')), xlim= c(-126, -116), ylim = c(32,49), type = 'n', cex.axis = 1.25, cex.lab = 1.5)

rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
       "slategray1")

#create main map
map("state", region = c('washington', 'oregon', 'california', 'nevada', 'arizona', 'utah', 'idaho', 'montana'), fill = TRUE, col = 'darkseagreen3', add = TRUE, border = TRUE)

map("worldHires","mexico", col="gray60", fill=TRUE, add=TRUE)  #add the adjacent parts of mexico

map("worldHires","canada", col="gray60", fill=TRUE, add=TRUE)  #add the adjacent parts of canada

# add city markers
Cities <- us.cities[us.cities$pop > 500000,]
City.Points <- Cities[Cities$long < -122.4 | Cities$long == -118.41,]
text(-122.2, 45.1, labels = "Portland", cex = 1.15)
text(-120.4, 38.2, labels = "San", cex = 1.15)
text(-120.2, 37.55, labels = "Francisco", cex = 1.15)
text(-116.9, 34.9, labels = "Los", cex = 1.15)
text(-117.1, 34.3, labels = "Angeles", cex = 1.15)

# Add survey sites
points(sites$Long, sites$Latitude, pch=19, col=cols, cex=1)


# Add names
sites$lab.lat <- NA
sites$lab.lat <- ifelse(sites$Site == "EC" | sites$Site == "FC" | sites$Site == "LC", (sites$Latitude+0.2), sites$Latitude)
sites$lab.lat <- ifelse(sites$Site ==  "PP" | sites$Site == "CB", (sites$Latitude+0.15), sites$lab.lat)
sites$lab.lat <- ifelse(sites$Site == "HZ" | sites$Site == "CC", (sites$Latitude-0.1), sites$lab.lat)
sites$lab.lat <- ifelse(sites$Site == "BC" | sites$Site == "PG" | sites$Site == "CT", (sites$Latitude-0.25), sites$lab.lat)
sites$lab.lat <- ifelse(sites$Site == "SB" | sites$Site == "DP" | sites$Site == "SC", (sites$Latitude-0.4), sites$lab.lat)
sites$lab.lat <- ifelse(sites$Site == "CC", (sites$lab.lat-0.275), sites$lab.lat)
sites$lab.lat <- ifelse(sites$Site == "LC", (sites$lab.lat-0.24), sites$lab.lat)
sites$lab.lat <- ifelse(sites$Site == "CM", (sites$Latitude+0.15), sites$lab.lat)
sites$lab.lat <- ifelse(sites$Site == "CT", (sites$Latitude-0.35), sites$lab.lat)
sites$lab.lat <- ifelse(sites$Site == "EC", (sites$Latitude-0.05), sites$lab.lat)
sites$lab.lat <- ifelse(sites$Site == "CM", (sites$Latitude-0.05), sites$lab.lat)
sites$lab.lat <- ifelse(sites$Site == "SC", (sites$Latitude-0.15), sites$lab.lat)
sites$lab.lat <- ifelse(sites$Site == "FC", (sites$Latitude+0.035), sites$lab.lat)
sites$lab.lat <- ifelse(sites$Site == "SR", (sites$Latitude-0.025), sites$lab.lat)
sites$lab.lat <- ifelse(sites$Site == "DP", (sites$Latitude-0.315), sites$lab.lat)

sites$lab.long <- NA
sites$lab.long <- ifelse(sites$Site == "FC" | sites$Site == "LC" | sites$Site == "BC"  | sites$Site == "PG", (sites$Long+0.15), sites$Long)
sites$lab.long <- ifelse(sites$Site == "SC" | sites$Site == "SB" | sites$Site == "CT", (sites$Long+0.35), sites$lab.long)
sites$lab.long <- ifelse(sites$Site == "DP", (sites$Long+0.475), sites$lab.long)
sites$lab.long <- ifelse(sites$Site == "SC", (sites$Long+0.09), sites$lab.long)
sites$lab.long <- ifelse(sites$Site == "CC", (sites$Long-0.1), sites$lab.long)


text((sites$lab.long - 0.7), sites$lab.lat, labels = sites$Site, cex = 0.75)


par(mar = c(5,0.005,4,3))
plot(x, y, pch = NA, cex = 3, xaxt="n", yaxt ="n", xlab = "", ylab= "", ylim = c(10.5,19.7))
rect(x-1,y,x+1,y+1, col = cols, border = NA)
mtext(expression(paste('Temperature (', degree, 'C)')), side=4, line=2, cex = 2)


dev.off()




