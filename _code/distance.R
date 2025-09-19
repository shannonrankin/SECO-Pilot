# https://distancesampling.org/

#########
# Ready #
#########
library(Distance)
library(here)
here()

source(here::here("_code/_common.R"))


#############
# data prep #
#############
# mydistanceData <- "DDtest_20250717_030700.WAV.Table.1.selections.txt" #give name of the distance dataset
# distdata <- read_tsv(here("data", mydistanceData))  # my data, must have distance, ID, and then add group size
# glimpse(distdata)

birdnet_SPP_LFRR <- read_csv(here("output", "birdnet_SPP_LFRR.csv"))



  
######################
# detection function #
######################
#https://workshops.distancesampling.org/duke-spatial-2015/practicals/1-detection-functions-solutions.html

df_hn <- ds(data=distdata, truncation=1000, key="hn", adjustment=NULL)
summary(df_hn)

ddf.gof(df_hn$ddf, asp=1)
plot(df_hn)