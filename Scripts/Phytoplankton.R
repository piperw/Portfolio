#########################################################
#    Code to calculate surface of 2020 LxT plots        #
#    Created by Piper Wallingford 2/2021                #
#    Most Recently Updated 10/2021                      #
#########################################################


# clear working space
rm(list=ls())


# Load packages
library('lubridate')
library('dplyr')
library('reshape2')
library('stringr')
library('ggplot2')
library('ggpubr')
library('bbmle')
library('mleTools')
library('mgcv')
library('growthTools')
library('tidyr')
library("tidyverse")
library('gridExtra')
library('praise')
library('wesanderson')
#library('reshape')


# helper function
getopt<-function(x,y){
  x[y==max(y)] # not resilient to ties
}


#### Read in environmental data ####

temp.tmp <- read.csv("Data/2020/Block3-4_Temps_2020-12-02.csv", stringsAsFactors = FALSE)
colnames(temp.tmp)[9]<-"Temp."
temp <- temp.tmp %>%
  group_by(Treatment) %>%
  summarise(Temp = mean(Temp.),
            se.temp = sd(Temp.)/sqrt(n())) # averaged temp per treatment 
colnames(temp)[1] <- "Temp.lvl"
temp$Temp <- round(temp$Temp, digits =2)

lgt.tmp <- read.csv("Data/2020/Block3-4_lights_2020-10-12.csv", stringsAsFactors = FALSE)
light <- lgt.tmp %>%
  group_by(Light.lvl) %>%
  summarise(Light = mean(PPFD),
            se.light = sd(PPFD)/sqrt(n()))
light$Light <- round(light$Light, digits =2)

rm(lgt.tmp, temp.tmp)


#### Phytoplankton Data ####
# Species
cr <- read.csv("Data/2020/CR_LxT_Surface_2020-12-01.csv", stringsAsFactors = FALSE)
cr <- cr %>%
  group_by(Species,Date,Time,TGB_number,Sample_ID,Replicate,Treatment,Light.lvl) %>%
  summarise(RFU = mean(RFU_raw))  
cr<-cr[!is.na(cr$RFU),]

cv.co <- read.csv("Data/2020/CV.CO_LxT_Surface_2020-12-16.csv", stringsAsFactors = FALSE)
cv.co <- cv.co %>% 
  group_by(Species,Date,Time,TGB_number,Sample_ID,Replicate,Treatment,Light.lvl) %>%
  summarise(RFU = mean(RFU_raw))  
cv.co<-cv.co[!is.na(cv.co$RFU),]

species <- rbind(cr,cv.co)
species <- species %>%
  mutate(ln.fluor = log(RFU))
colnames(species)[7] <- "Temp.lvl"
rm(cr, cv.co)

# merge data
data <- merge(species, temp)
data <- merge(data, light)
  colnames(data)[6] <- "Block"
  colnames(data)[7] <- "Well"


# format date and time
data$Date <- as.Date(data$Date, "%m/%d/%Y")
data$date.time<-paste(data$Date,data$Time)
data$date.time<-ymd_hm(data$date.time)

tmp.tab <- data %>% 
  group_by(Temp.lvl, Temp, Light.lvl, Light, Species, Replicate) %>% 
  summarise(minT=min(date.time))

tmp.data <- merge(data, tmp.tab)

tmp.data$dtime<-as.numeric(difftime(tmp.data$date.time,tmp.data$minT,units = 'days'))
data <- tmp.data[,c(3, 9, 10, 4, 2, 5, 13, 1, 6, 14, 11, 12, 7, 8, 15, 16, 17)]

# check range of time spans
range(data$dtime)
rm(tmp.tab, tmp.data, light, species, temp)


#### Estimate Growth Rates ####

# set up id column
data$id<-paste(data$Species, data$Well, data$Replicate, data$Temp.lvl, data$Temp, data$Light.lvl, data$Light)
head(data)

# Function to calculate the correct lnf0
funky<-function(x,y){
  x[which(y==min(y))]
}

# For full data set:   # plot set to F to run faster
data.1 <- data %>% 
  group_by(Species, Well, Replicate, Temp.lvl, Temp,  Light.lvl, Light) %>% 
  do(grs=get.growth.rate(.$dtime, .$ln.fluor, id=.$id, plot.best.Q=F, fpath=fpath2, method=c('linear')), lnf0=funky(.$ln.fluor,.$dtime))


# extract growth rate estimates from dplyr structure 
data.2 <- data.1 %>% 
  summarise(Species, Well, Replicate, Temp.lvl, Temp, Light.lvl, Light, mu=grs$best.slope, best.model=grs$best.model, rsqr=grs$best.model.rsqr, lnf0=lnf0)
head(data.2)

data.2$id<-paste(data.1$Species, data.2$Light, data.2$Temp, data.2$Well, data.2$Replicate)
head(data.2)



#### Fit models to each temperature curve  ####

# Fit double exponential model to each temperature curve
# Parameters
  # topt = Optimum temperature
  # b1 = Birth rate at 0 Celsius
  # b2 = Temperature scaling of birth rate, > 0
  # d0 = Temperature-independent death rate
  # d2 = Temperature scaling of death rate, > 0 (and d2 > b2)

de.fits <- data.2 %>% 
  group_by(Species, Light) %>%
  do(tpcs=get.decurve.tpc(.$Temp,.$mu))

de.fits <- de.fits %>% 
  summarise(Species, Light, topt = tpcs$topt, tmin = tpcs$tmin, tmax = tpcs$tmax, topt.lwr = tpcs$cf_ciFI['topt', 1], topt.upr = tpcs$cf_ciFI['topt', 2], rsqr = tpcs$rsqr, b1 = tpcs$cf$b1, b2 = tpcs$cf$b2, d0 = tpcs$cf$d0, d2 = tpcs$cf$d2, umax=tpcs$umax, umax.lwr=tpcs$umax_ci[1],umax.upr=tpcs$umax_ci[2])



#### Separate Species #### 
# C. reinhardtii
data.cr <- subset(data.2, Species=='C. reinhardtii') %>%
  filter(Temp < 43)

# C. ovata
data.co <- subset(data.2, Species=='C. ovata') %>%
  filter(Temp < 43)


#### Fit interactive model ([double exponential] : [Eiler-Peeters]) ####
# Double exponential w/ Eiler-Peeters
txl.de.ep <-function(b1, b2, d0, d2, topt, ua, Lopt, temperature, light){
  Lm <- (light/((ua*light^2)/(Lopt^2) + (1 - (2*(ua/Lopt)))*light + ua))
  p.mu<-b1*exp(b2*temperature)*Lm - 
    (d0 + ((b1*b2)/d2)*exp((b2 - d2)*topt)*exp(d2*temperature))
  p.mu<-ifelse(p.mu < -0.5,-0.5,p.mu)
  p.mu
}


#### C. reinhardtii  ####

# Maximum Likelihood Fit  

m2.cr.0 <- mle2(mu ~ dnorm(mean=txl.de.ep(exp(b1.g), exp(b2.g), exp(d0.g), exp(b2.g)+exp(delta), 10*topt.g, exp(ua.g), 100*Lopt, temperature=data.cr$Temp, light=data.cr$Light), sd=s), start=list(b1.g=log(0.63272319), b2.g=log(0.05015293), d0.g=log(0.02), delta=log(0.27891286), topt.g=3.46530498, ua.g=log(11.20552069), Lopt=1.97841736, s=0.39881316), control=list(maxit=5000,reltol=1e-14), data=data.cr)


pds<-predict(m2.cr.0)
plot(data.cr$mu~pds, data=data.cr)
abline(0,1)
get.R2(pds, data.cr$mu)
# 0.8090803



# Plot Surface
gamdata.cr<-c()
vals0<-expand.grid(temperature=seq(min(data.cr$Temp), max(data.cr$Temp), length.out = 1000), light=seq(0, max(data.cr$Light), length.out = 1000))
vals<-expand.grid(temperature=seq(min(data.cr$Temp), max(data.cr$Temp), length.out = 1000), light=seq(0, max(data.cr$Light), length.out = 1000))

pds.m20<-txl.de.ep(0.6109902, 0.05102389, 0.1532929, 0.05102389+0.2779066, 34.675153, 11.64041, 190.48037, temperature=vals0$temperature, light=vals0$light)
pds.m2<-txl.de.ep(0.6109902, 0.05102389, 0.1532929, 0.05102389+0.2779066, 34.675153, 11.64041, 190.48037, temperature=vals$temperature, light=vals$light)


gamdata0<-cbind(vals0, mu.m2=pds.m20)
gamdata.cr<-cbind(vals, mu.m2=pds.m2)

gamdata.cr.topt <- gamdata.cr %>% 
  group_by(light) %>% 
  summarize(topt=getopt(temperature, mu.m2))

gamdata.cr.Lopt <- gamdata.cr %>% 
  group_by(temperature) %>% 
  summarize(Lopt=getopt(light, mu.m2))

png("Plots/C.r topt.png", width = 1600, height = 1200, res = 300)
ggplot(gamdata.cr, aes(x=temperature, y=light))+
  geom_tile(aes(fill=mu.m2)) +
  geom_contour(aes(z=mu.m2),colour='white',bins=25) +
  geom_point(data=gamdata.cr.topt, aes(x=topt), size=0.5, color='red') +
  geom_point(data=gamdata.cr.Lopt, aes(y=Lopt), size=0.5, color='blue') +
  geom_contour(aes(z=mu.m2),colour='black', size=2, breaks=0) +
  scale_x_continuous('Temperature (°C)', expand=c(0,0))+
  scale_y_continuous(bquote('Light ('*mu*'E'*m^-2*s^-1*')'), expand=c(0,0)) +
  scale_fill_distiller(type='div', limits=c(-0.5,3.5), palette = 'Spectral')+
  geom_point(data=data.cr, aes(x=Temp, y=Light, color = mu)) +
  scale_color_gradientn(colors = c("black", "grey", "white")) +
  labs(fill = "Growth Rate", color = "Observed") +
  ggtitle(bquote('C. reinhardtii (' *R^2*' = 0.81)')) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        plot.title = element_text(size=14, hjust = 0.5),
        strip.background = element_blank(),
        strip.text.y = element_blank())  
dev.off()



#### C. ovata  ####

# Maximum Likelihood fit
m2.co <- mle2(mu ~ dnorm(mean=txl.de.ep(exp(b1.g), exp(b2.g), exp(d0.g), exp(b2.g)+exp(delta), 10*topt.g, exp(ua.g), 100*Lopt, temperature=data.co$Temp, light=data.co$Light), sd=s), start=list(b1.g=log(1.25), b2.g=log(0.12), d0.g=log(0.9), delta=log(0.007), topt.g=3.3, ua.g=log(25), Lopt=1.50, s=2), control=list(maxit=5000,reltol=1e-14), data=data.co)


pds<-predict(m2.co)
plot(data.co$mu~pds, data=data.co)
abline(0,1)   # negative mu values not captured 

get.R2(pds, data.co$mu)
# 0.7387949 


# Plot Surface
gamdata.co<-c()
vals0<-expand.grid(temperature=seq(min(data.co$Temp), max(data.co$Temp), length.out = 1000), light=seq(0, max(data.co$Light), length.out = 1000))
vals<-expand.grid(temperature=seq(min(data.co$Temp), max(data.co$Temp), length.out = 1000), light=seq(0, max(data.co$Light), length.out = 1000))

pds.m20<-txl.de.ep(0.247846, 0.07074078, 0.1443377, 0.07074078+0.2799697, 35.678712, 8.278909, 106.06402, temperature=vals0$temperature, light=vals0$light)
pds.m2<-txl.de.ep(0.247846, 0.07074078, 0.1443377, 0.07074078+0.2799697, 35.678712, 8.278909, 106.06402, temperature=vals$temperature, light=vals$light)


gamdata0<-cbind(vals0, mu.m2=pds.m20)
gamdata.co<-cbind(vals, mu.m2=pds.m2)

gamdata.co.topt <- gamdata.co %>% 
  group_by(light) %>% 
  summarize(topt=getopt(temperature, mu.m2))

gamdata.co.Lopt <- gamdata.co %>% 
  group_by(temperature) %>% 
  summarize(Lopt=getopt(light, mu.m2))


png("Plots/C.o topt.png", width = 1600, height = 1200, res = 300)
ggplot(gamdata.co, aes(x=temperature, y=light))+
  geom_tile(aes(fill=mu.m2)) +
  geom_contour(aes(z=mu.m2),colour='white',bins=25) +
  geom_point(data=gamdata.co.topt, aes(x=topt), size=0.5, color='red') +
  geom_point(data=gamdata.co.Lopt, aes(y=Lopt), size=0.5, color='blue') +
  geom_contour(aes(z=mu.m2),colour='black', size=2, breaks=0) +
  scale_x_continuous('Temperature (°C)', expand=c(0,0))+
  scale_y_continuous(bquote('Light ('*mu*'E'*m^-2*s^-1*')'), expand=c(0,0)) +
  scale_fill_distiller(type='div', limits=c(-0.5,3.5), palette = 'Spectral')+
  geom_point(data=data.cr, aes(x=Temp, y=Light, color = mu)) +
  scale_color_gradientn(colors = c("black", "grey", "white")) +
  labs(fill = "Growth Rate", color = "Observed") +
  ggtitle(bquote('C. ovata (' *R^2*' = 0.74)')) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        plot.title = element_text(size=14, hjust = 0.5),
        strip.background = element_blank(),
        strip.text.y = element_blank())  
dev.off()


