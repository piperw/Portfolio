###############################################################
#   Script to map arctic species distributions                #
#   Created 12/10/2020 by Piper Wallingford                   #
#   Most recent update on 1/15/2021                           #
###############################################################


# clear working space
rm(list=ls())

# load packages
library('dplyr')
library('ggplot2')
library('ggforce')
library('rworldmap')
library('mapproj')
library('geosphere')
library('raster')
library('plotrix')
library('marmap')
library('stringr')
library('wesanderson')
library('animation')


# Read in Data
arctic <- read.csv("Data/Arctic_phytoplankton_occurence_data.csv")

arctic[,4:6] <- NULL  
colnames(arctic) <- c("Species", "Longitude", "Latitude")   

world <- getMap()


# Plot occurence data on world map
png("Plots/Distributions.png", width = 2400, height = 1600, res = 300) 
ggplot() +
  geom_polygon(data = world, # plot world map
               aes(x = long, y = lat, group = group),
               fill = "gray", colour = "darkgray") +
  coord_map("ortho", orientation=c(60, 90, 0)) +    # rotate orientation
  scale_y_continuous(breaks = (-4:4) * 30) +     # set lat intervals
  scale_x_continuous(breaks = (-4:4) * 45) +    # set long intervals
  geom_point(data = subset(arctic, Latitude > 66.5), aes(x=Longitude, y=Latitude, color = Species), size = 0.35) +    # add data points
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "none") +
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=1),
        panel.ontop = element_blank()) 
dev.off()


# Make it spin! 
rotateMap <- function(angle){   # function to roate map
  ggplot() +
    geom_polygon(data = world, # plot world map
                 aes(x = long, y = lat, group = group),
                 fill = "gray", colour = "darkgray") +
    coord_map("ortho", orientation=c(61, angle, 0)) +    # rotate orientation
    scale_y_continuous(breaks = (-4:4) * 30) +     # set lat intervals
    scale_x_continuous(breaks = (-4:4) * 45) +    # set long intervals
    geom_point(data = subset(arctic, Latitude > 66.5), aes(x=Longitude, y=Latitude, color = Species), size = 0.35) +    # add data points
    labs(x = "Longitude", y = "Latitude") +
    theme(legend.position = "none") +
    theme(panel.border = element_rect(colour = "gray", fill=NA, size=1),
          panel.ontop = element_blank()) 
}

saveGIF({     # takes a couple of minutes 
  ani.options(nmax = 360)
  for(i in seq(0,360)){
    print(rotateMap(i))
  }
}, interval = 0.1, outdir="/Users/Piper/Documents/Dropbox/UCLA/Projects/Arctic Phytoplankton/Arctic Species/Plots/", movie.name = "Arctic Species.gif")




#### Adding arctic circle - thing that haven't worked: ####

# 1. Add horizontal line with geom_hline - plots as polygon 
ggplot() +
  geom_polygon(data = world, 
               aes(x = long, y = lat, group = group),
               fill = "gray", colour = "darkgray") +
  coord_map("ortho", orientation=c(60, 90, 0)) +
  scale_y_continuous(breaks = (-4:4) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  geom_hline(yintercept = 66.75) +
  geom_point(data = subset(arctic, Latitude > 66.5), aes(x=Longitude, y=Latitude, color = Species), size = 0.35) +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "none") +
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=1),
        panel.ontop = element_blank()) 


# 2. Use geom_circle from ggforce package - plots as lemniscate 
ggplot() +
  geom_polygon(data = world, 
               aes(x = long, y = lat, group = group),
               fill = "gray", colour = "darkgray") +
  coord_map("ortho", orientation=c(60, 90, 0)) +
  scale_y_continuous(breaks = (-4:4) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  geom_circle(aes(x0 = 30, y0 = 90, r = 33.25)) + # x0, y0 = center coordiantes, r = radius
  geom_point(data = subset(arctic, Latitude > 66.5), aes(x=Longitude, y=Latitude, color = Species), size = 0.35) +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "none") +
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=1),
        panel.ontop = element_blank()) 


# 3. Generating continuous line with geom_path - cannot add geom_point 
        # Error: Discrete value supplied to continuous scale

distantCircle <- function(x, radius) {     # function to add lat circle
  resul <- do.call("rbind", lapply(0:360, function(bearing) {
    res <- destPoint(p = x, b = bearing, d = radius)
    rownames(res) <- NULL
    return(data.frame(res))
  }))
  resul$dist <- radius / 1000
  return(resul)}

arctic.circle <- distantCircle(x = c(0.0000001,89.9999999), radius = 2600*1000)

ggplot() +
  geom_polygon(data = world, 
               aes(x = long, y = lat, group = group),
               fill = "gray", colour = "darkgray") +
  coord_map("ortho", orientation=c(60, 90, 0)) +
  scale_y_continuous(breaks = (-4:4) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  geom_path(data = arctic.circle, aes(x = lon, y = lat, group = dist, col = dist), linetype = 2) +
  #geom_point(data = subset(arctic, Latitude > 66.5), aes(x=Longitude, y=Latitude, color = Species), size = 0.35) +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "none") +
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=1),
        panel.ontop = element_blank()) 
dev.off()

