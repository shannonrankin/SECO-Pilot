## Code for processing birdnet analysis

## TO DO
# library(birdnetTools)

# 
# Calculate species specific confidence thresholds for BirdNet detections  birdnet_calc_threshold( )
# 
# Code Name vs Scientific Name vs Common Name [birdnetR/labels]
# 
# 
# Remove any detections before or after recording start/end date/time
# 
# Save as a csv in SECO-Pilot/data folder

#########
# Ready #
#########
library(here)
here()

source(here::here("_code/_common.R"))

##########################################
# Rename BirdNet_SelectionTable in place #
##########################################
# Root path where your FolderName1, FolderName2, ... are located
root_path <- "C:/Users/Shannon/Documents/audiomoth/Rails/birdnet"

rename_birdnet_files(root_path) # to add 
add_deploymentID(root_path)

# Merge files
root_path <- "your/root/path"  # Replace with your root path
dest_path <- here::here("raw") # Destination folder


dest_path <- here::here("raw") # Destination folder

copy_birdnet_files (root_path, dest_path) #Note, This will fail if the files already exist. Eventually fix this.


#############
# Data Prep #
#############
data <- birdnet_combine(here("raw")) # merge birdnet data saved in data/birdnet_output folder
deployments <- read_rds(here("data", "deployTable.rds"))
head(data)

#Next: 

birdnet_All <- birdnet_add_datetime(data, tz = "UTC") %>%  #add UTC dateTime columns
  rename(
    datetime_UTC = datetime) %>% 
    mutate (datetime_local = with_tz(datetime_UTC, "America/Los_Angeles") ) %>%
  separate(
    col = deployment_ID,
    into = c("Location-Site", "config", "deployID_date"),
    sep = "_",
    remove = FALSE
  )%>%
  separate("Location-Site", into = c("Location", "Site"), sep = "-" ) %>%
  left_join(
    deployments %>%
      select(deployment_ID, latitude, longitude),
    by = "deployment_ID"
  ) %>%
  select(-date, -year, -month, -mday, -yday, -hour, -minute, -deployID_date)
head(birdnet_All)

birdnet_top10 <- birdnet_All %>%
  # Count the number of detections per species
  count(Common.Name, sort = TRUE) %>%
  # Get the top 10 species
  slice_head(n = 10) %>%
  # Join back to the original data to get all rows for these species
  left_join(birdnet_All, by = "Common.Name")

birdnet_TJE <- filter(birdnet_All, Location == "TJE")

#Consider top 10 and these species (?) Bell's Sparrow, Beldings Savannah Sparrow, California Least Tern, western Snowy Plover, California Gnatcatcher, Least Bell's Vireo
# Get the top 10 species, then add "Bell's Vireo" if not already present
priority <- birdnet_TJE %>%
  count(Common.Name, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(Common.Name) %>%
  union("Bell's Vireo")
# Filter the original data for these species
birdnet_priority <- birdnet_TJE %>%
  filter(Common.Name %in% priority)


birdnet_top10_TJE <- filter(birdnet_top10, Location == "TJE")
birdnet_LFRR <- birdnet_All %>% filter(Common.Name == "Ridgway's Rail" & Location == c('TJE', 'SPP')) 

write_csv(birdnet_All, here("output", "birdnet_All.csv"))
write_csv(birdnet_TJE, here("output", "birdnet_TJE.csv"))
write_csv(birdnet_top10_TJE, here("output", "birdnet_TJE.csv"))
write_csv(birdnet_top10, here("output", "birdnet_top10.csv"))
write_csv(birdnet_LFRR, here("output", "birdnet_LFRR.csv"))
saveRDS(birdnet_All, file="data/birdnet_All.rds")

############
# Env Data #
############


##################
# PAMverse Plots #
##################

myData <- here("output", "birdnet_SPP_LFRR.csv")

#runDetectionExplorer(myData)

loadDetectionData(x=myData, source='csv', detectionType='detection', wide=FALSE, 
                  tz='UTC', 
                  columnMap=list(UTC='datetime_UTC', species='Common.Name', Latitude='latitude', Longitude='longitude'))


plotAcousticScene(x=myData, combineYears=FALSE) #Note: this would only be good for TJ

plotPolarDetections(x=myData, group='species', facet='species', bin='detection/hour', quantity='count')

plotDetectionBoxplot(x=myData, group='species', facet='species', bin='hour/day', combineYears=FALSE)


###################
### Hourly Bins ###
###################

