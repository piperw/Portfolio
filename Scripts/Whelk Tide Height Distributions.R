###########################################################
#  Whelk Tide Height Distributions                        #
#  Created by Piper Wallingford                           #
#  Created on 20 September 2018                           #
###########################################################


# clear workspace
rm(list=ls())


# Load Packages
library("tidyverse")
library("ggplot2")
library("ggeffects")
library("lme4")



#### Read in data ####
Data <- read.csv("Data/SurveyData.csv") 
TH.MPA <- read.csv("Data/TH Distributions PA.csv")


# Data Summary

# calculate densities
Data$Mdens = Data$M.lugubris/Data$Area
Data$Adens = Data$A.spirata/Data$Area
Data$Ndens = Data$N.emarginata/Data$Area
Data$Other.dens = (Data$Ceres + Data$Conus + Data$Max + Data$Olive)/Data$Area
Data$Native.dens = (Data$A.spirata + Data$N.emarginata)/Data$Area

# Assign Regions
Data$Region <- NULL
Data$Region <- ifelse(Data$Site == "HP" | Data$Site == "TI", "Expanding", "Established")
Data$Region <- ifelse(Data$Site == "CC" | Data$Site == "LC" | Data$Site == "SH", "Absent", Data$Region)


# Split by Mex  presence
Data$M.PA <- NULL
Data$M.PA = ifelse(Data$M.lugubris!=0, 1, 0)
Data$N.PA <- NULL
Data$N.PA = ifelse(Data$Native!=0, 1, 0)


# Tidy 
Data <- Data %>% 
  mutate(Season=as.factor(Season), Site=as.factor(Site), Transect=as.factor(Transect), Region = as.factor(Region), TH=as.numeric(TH))
Data$Region <- ordered(Data$Region, levels = c("Absent", "Expanding", "Established"))


# Summarize
Trnsct.Data <- Data %>% #average across transects
  group_by(Season, Site, TH, Region) %>%
  summarize(mean.native= mean(Native.dens),
            se.nat = sd(Native.dens)/sqrt(n()),
            mean.mex = mean(Mdens),
            se.mex = sd(Mdens)/sqrt(n())) %>%
  mutate(N.PA=ifelse(mean.native>0,1,0)) %>%
  mutate(M.PA=ifelse(mean.mex>0,1,0))


#### Data Analysis ####

# Presence/Absence Hurdle Models
binomial2 <- glm(N.PA ~ M.PA * TH, data = Trnsct.Data, family = binomial(link = logit))
gamma2 <- glm(mean.native ~ M.PA * TH, data = Trnsct.Data[Trnsct.Data$N.PA == 1,], family = Gamma(link = log))



#### Plot - Presence of Native Whelks in the Presence of Non-Native Whelks ####
png("Plots/THdistributions.png", width = 2400, height = 1600, res = 300)
ggplot(data=TH.MPA, aes(x = TH, y = Density, fill = Species, color = Species)) +
  facet_grid(~MPA) +
  geom_point(aes(y = Density, color = Species)) +
  geom_errorbar(aes(ymin=Density-se, ymax=Density+se, color = Species), width = 0.05) +
  geom_ribbon(aes(ymin = 0, ymax = Density, color = Species), alpha = 0.6) +
  scale_color_manual(values = c("mediumpurple3","aquamarine4")) + 
  scale_fill_manual(values = c("mediumpurple3","aquamarine4")) +
  xlab("Tidal Elevation (m)") +
  ylab(expression(paste('Density (per m'^{2},')'))) +
  scale_y_continuous(expand = c(0.0,0.05)) +
  scale_x_continuous(expand = c(0.001,0.001)) +
  theme(strip.background = element_blank(),
        strip.text.y = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        panel.border = element_rect(colour = "gray", fill=NA, size=1),
        panel.background = element_blank(), 
        panel.grid = element_blank(), 
        panel.spacing.x = unit(1,"cm"),
        axis.text.y=element_text(size = 14),
        axis.text.x=element_text(size=14),
        axis.title=element_text(size=16)) + 
  theme(legend.position = c(0.05, 0.95), 
        legend.key = element_rect(colour = 'white', fill = 'white', size = 0.5),
        legend.justification = c("left", "top"), 
        legend.box.just = "left",
        legend.title.align=0.35,
        legend.title=element_text(size=12),
        legend.text=element_text(size=12, face = "italic"))
dev.off()



#### Plot - Presence of Native Whelks in the Presence of Non-Native Whelks ####
png("Plots/Presence.png", width = 2400, height = 1600, res = 300)
plot(ggpredict(binomial2, c("TH","M.PA"), interactive = TRUE, se = TRUE, facet = TRUE)) + 
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) +
  scale_color_manual(name = expression(paste(italic("Mexacanthina "), "Presence")), labels = c("Absent", "Present"), values = c("aquamarine4","mediumpurple4")) +
  scale_fill_manual(values = c("aquamarine4","mediumpurple4")) +
  labs(x = "Tidal Elevation (m)", y = "Probability of Native Whelk Presence") +
  theme(strip.background = element_blank(),
        strip.text.y = element_text(size = 12),
        panel.border = element_rect(colour = "gray", fill=NA, size=1),
        panel.background = element_blank(), 
        panel.grid = element_blank(), 
        panel.spacing.x = unit(0,"line"),
        axis.text.y=element_text(size = 14),
        axis.text.x=element_text(size=14),
        axis.title=element_text(size=16)) +
  theme(axis.line.x = element_line(color="gray", size = 0.25),
        legend.position = c(0.95,0.95), 
        legend.justification = c("right", "top"), 
        legend.box.just = "right",
        legend.title.align=0.35,
        legend.title=element_text(size=14),
        legend.key = element_rect(fill = NA),
        legend.text=element_text(size=12)) 
dev.off()



#### Plot - Density of Native Whelks in the Presence of Non-Native Whelks ####
png("Plots/Density.png", width = 2400, height = 1600, res = 300)
plot(ggpredict(gamma2, terms = c("TH", "M.PA"), interactive = TRUE, se = TRUE, facet = TRUE)) + 
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) +
  scale_color_manual(name = expression(paste(italic("Mexacanthina "), "Presence")), labels = c("Absent", "Present"), values = c("aquamarine4","mediumpurple4")) +
  scale_fill_manual(values = c("aquamarine4","mediumpurple4")) +
  labs(x = "Tidal Elevation (m)", y = "Density of Native Whelks") +
  theme(strip.background = element_blank(),
        strip.text.y = element_text(size = 12),
        panel.border = element_rect(colour = "gray", fill=NA, size=1),
        panel.background = element_blank(), 
        panel.grid = element_blank(), 
        panel.spacing.x = unit(0,"line"),
        axis.text.y=element_text(size = 14),
        axis.text.x=element_text(size=14),
        axis.title=element_text(size=16)) +
  theme(axis.line.x = element_line(color="gray", size = 0.25),
        legend.position = c(0.95,0.95), 
        legend.justification = c("right", "top"), 
        legend.box.just = "right",
        legend.title.align=0.35,
        legend.title=element_text(size=14),
        legend.key = element_rect(fill = NA),
        legend.text=element_text(size=12)) 
dev.off()
