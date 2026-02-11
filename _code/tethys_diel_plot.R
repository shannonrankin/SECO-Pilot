#Create diel plot of acoustic events from drifters

#Required inputs: 
#1. DriftPositions.rda (r data file with date, lat, lon for each station)
#2. Pamguard database with event info
#3. Define station number and species to plot
#4. Define folder to save plots

#Load Required Packages
library(PAMpal)
library(RSQLite)
library(tidyverse)
library(beepr)
library(viridis)
library(suncalc)
library(lubridate)
library(ggnewscale)
library(scales)
library(lunar)
library(here)
#############################################################################
#Define required inputs
#1.Deployment Locations
data <- read_ods(here("data", "SE_DeployDetails.ODS"), sheet = 1)
#2. Pamguard database
DriftDB <-file.choose()
#3. Define station number and species to plot
Deployment_ID = 7
SPLabels<-c('UD')
#4. Define folder to save plots
savedir<-here("output", "dielPlots")
#############################################################################

#Load data from Pamguard Database
sqlite <- dbDriver("SQLite")
conn <- dbConnect(sqlite,DriftDB)   
Events <- dbReadTable(conn, "Click_Detector_OfflineEvents")         #read offline events
Events$eventType<-gsub(" ", "", Events$eventType, fixed = TRUE)

Events<-filter(Events,eventType %in% SPLabels)
Events$DateTime<-strptime(Events$UTC,format="%Y-%m-%d %H:%M:%OS",tz="UTC")
Events$DateTime<-as.POSIXct(Events$DateTime,tz="UTC")
Events$PT<-with_tz(Events$DateTime,tz="US/Pacific")

Events<-Events %>%
  mutate(interval=floor_date(PT,unit="hour")+minutes(floor(minute(PT)/10)*10), Hour=hour(PT)) 

Events$Time<-strftime(Events$interval,format="%H:%M:%S")
Events$Date<-as.Date(Events$PT)
Events$Hour<-hour(Events$PT)

#Retrieve sunrise and sunset SunTimes
Clips <- dbReadTable(conn, "Sound_Acquisition")  

#Filter track info to only contain data from station of interest 
data.n.trunc<-filter(AllTracks,station==station.numbers)
Times<-seq(from=as.POSIXct(Clips$UTC[2]),
           by=60*60,
           to=as.POSIXct(Clips$UTC[nrow(Clips)]))

SunDF<-data.frame(date=as.Date(Times),lat=mean(data.n.trunc$lat),lon=mean(data.n.trunc$long))

#Reduce data.frame to only use one lat/lon per day
SunDF<-SunDF[!duplicated(SunDF$date),]
SunTimes<-getSunlightTimes(data=SunDF,keep = c( "sunrise", "sunset"), tz = "America/Los_Angeles")

SunTimes$sunriseH<-hour(SunTimes$sunrise)
SunTimes$sunsetH<-hour(SunTimes$sunset)

#Get Lunar Illumination
MoonTimes<-data.frame(Times)
MoonTimes$Hour<-hour(MoonTimes$Times)
MoonTimes$LunarIllum<-lunar.illumination(MoonTimes$Times)
MoonTimes$Date<-as.Date(MoonTimes$Times)
MoonTimes<-select(MoonTimes,Date,Hour,LunarIllum)
MoonTimes<-unique(MoonTimes)


#Create the plot
# ggplot(Events,aes(Hour,Date,fill=eventType))+scale_fill_viridis(discrete=TRUE)+
#   geom_rect(data = SunTimes,inherit.aes=FALSE,
#              aes(xmin = 0, xmax =sunriseH, ymin = date, 
#                  ymax = date+days(1)), fill = "gray",alpha=.5)+
#   geom_rect(data = SunTimes,inherit.aes=FALSE,
#             aes(xmin = sunsetH, xmax =23, ymin = date, 
#                 ymax = date+days(1)), fill = "gray",alpha=.5)+
#     geom_rect(data = MoonTimes,inherit.aes=FALSE,
#             aes(xmin = 0, xmax =Hour+1, ymin = Date, 
#                 ymax = Date+days(1),fill=LunarIllum))+
#   theme_bw()+  xlab('Local Time')+labs(fill="Species")+
#   ggtitle(paste('Drift ', station.numbers[1], '\n Delphinid Acoustic Encounters: Hourly bins',sep=""))

ggplot()+geom_rect(data = SunTimes,inherit.aes=FALSE,
                   aes(xmin = 0, xmax =sunriseH, ymin = date, 
                       ymax = date+days(1)), fill = "gray",alpha=.6)+
  geom_rect(data = SunTimes,inherit.aes=FALSE,
            aes(xmin = sunsetH, xmax =24, ymin = date, 
                ymax = date+days(1)), fill = "gray",alpha=.6)+
  geom_rect(data = MoonTimes,inherit.aes=FALSE,
            aes(xmin = Hour, xmax =Hour+1, ymin = Date, 
                ymax = Date+days(1),fill=LunarIllum),alpha=0.3)+
  scale_fill_gradientn(colours = c(muted("red"),"white"),values=c(0,1),guide="colourbar")+
  geom_rect(data = SunTimes,inherit.aes=FALSE,
            aes(xmin = sunriseH, xmax =sunsetH, ymin = date, 
                ymax = date+days(1)), fill = "white",alpha=0.6)+
  new_scale_fill()+
  geom_tile(data=Events,aes(Hour,Date,fill=eventType))+
  scale_fill_viridis(discrete=TRUE)+theme_bw()


setwd(savedir)

ggsave(filename=paste('DeployID', station.numbers[1],'_',paste0(SPLabels[1:length(SPLabels)],collapse="_"),'Lunar','.png',sep=""),device='png')
