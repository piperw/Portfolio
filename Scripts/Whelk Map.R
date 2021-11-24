###########################################################
#  Project: map of whelk species                          #
#  Created by Piper Wallingford                           #
#  Created on 7 July 2020                                 #
###########################################################

# clear workspace
rm(list=ls())

# Load Packages
library("maps")
library("mapdata")
library("raster")
library("plotrix")
library("marmap")
library(sf)   
library(spData)        
library(spDataLarge) 
library("tmaptools")
library("tigris")
library("ggplot2")
library("scatterpie")
library("ggtree")
library("png")


# Whelk Data
whelks <- read.csv("Data/Whelks.csv")
whelks$Density <- round(whelks$Density, 2)


png(paste('Plots/LC.png'), width = 800, height = 800)
ggplot(whelks[whelks$Site == "LC",], aes(x = "", y = Density, fill = Species)) +
  geom_col() +
  scale_fill_manual(values = c("aquamarine4","mediumpurple3","peachpuff","lemonchiffon")) +
  #geom_text(aes(label = Density), position = position_stack(vjust = 0.5), size = 7) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "none")
dev.off()

LC <- readPNG('Plots/LC.png')


png(paste('Plots/SH.png'), width = 800, height = 800)
ggplot(whelks[whelks$Site == "SH",], aes(x = "", y = Density, fill = Species)) +
  geom_col() +
  scale_fill_manual(values = c("aquamarine4","mediumpurple3","peachpuff","lemonchiffon")) +
  #geom_text(aes(label = Density), position = position_stack(vjust = 0.5), size = 7) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "none")
dev.off()

SH <- readPNG('Plots/SH.png')


png(paste('Plots/TI.png'), width = 800, height = 800)
ggplot(whelks[whelks$Site == "TI",], aes(x = "", y = Density, fill = Species)) +
  geom_col() +
  scale_fill_manual(values = c("aquamarine4","mediumpurple3","peachpuff","lemonchiffon")) +
  #geom_text(aes(label = Density), position = position_stack(vjust = 0.5), size = 7) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "none")
dev.off()

TI <- readPNG('Plots/TI.png')


png(paste('Plots/CR.png'), width = 800, height = 800)
ggplot(whelks[whelks$Site == "CR",], aes(x = "", y = Density, fill = Species)) +
  geom_col() +
  scale_fill_manual(values = c("aquamarine4","mediumpurple3","peachpuff","lemonchiffon")) +
  #geom_text(aes(label = Density), position = position_stack(vjust = 0.5), size = 7) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "none")
dev.off()

CR <- readPNG('Plots/CR.png')


png(paste('Plots/SC.png'), width = 400, height = 400)
ggplot(whelks[whelks$Site == "SC",], aes(x = "", y = Density, fill = Species)) +
  geom_col() +
  scale_fill_manual(values = c("aquamarine4","mediumpurple3","peachpuff","lemonchiffon")) +
  #geom_text(aes(label = Density), position = position_stack(vjust = 0.5), size = 7) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  ) +
  theme(legend.position = "none")
dev.off()

SC <- readPNG('Plots/SC.png')



# California Counties
CA <- counties("California", cb = TRUE)
counties <- CA[CA$NAME == "Los Angeles" | CA$NAME == "San Bernardino" | CA$NAME == "San Diego" | CA$NAME == "Orange" | CA$NAME == "Ventura" | CA$NAME == "Riverside" | CA$NAME == "Imperial" | CA$NAME == "Kern" | CA$NAME == "San Luis Obispo" | CA$NAME == "Santa Barbara",]

# Mexico States
Mex <- world[world$name_long == "Mexico",]


png(paste('Plots/Whelk Map.png'), width = 2400, height = 2400)
ggplot() +
  geom_sf(data = counties, color="black", fill = "darkolivegreen3", size=0.25) +
  geom_sf(data = Mex, color = "black", fill = "gray", size=0.25) +
  annotate(geom = "text", x = -117, y = 32.8, label = "San Diego", color = "black", size = 25) +
  annotate(geom = "text", x = -118.25, y = 34, label = "Los Angeles", color = "black", size = 25) +
  annotate(geom = "text", x = -117.75, y = 33.65, label = "Irvine", color = "black", size = 25) +
  annotate(geom = "text", x = -118.35, y = 33.6, label = "Corona", color = "navy", size = 20) +
  annotate(geom = "text", x = -118.2, y = 33.5, label = "Shaw's", color = "navy", size = 20) +
  annotate(geom = "text", x = -118.115, y = 33.38, label = "Treasure Island", color = "navy", size = 20) +
  annotate(geom = "text", x = -117.625, y = 33, label = "Cardiff", color = "navy", size = 20) +
  annotate(geom = "text", x = -117.625, y = 32.85, label = "Scripps", color = "navy", size = 20) +
  #annotation_raster(SC, ymin = 33.599, ymax= 36.08,xmin = -117.91, xmax = -118) +
  xlim(-118.75,-116.7) +
  ylim(32.45,34) + 
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() + 
  theme(axis.text = element_text(size = 35)) +
  theme(axis.title = element_text(size = 50)) +
  guides(fill = "none") +
  theme(panel.background = element_rect(fill = "aliceblue")) 
dev.off()
