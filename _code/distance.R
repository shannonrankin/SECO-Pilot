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

# Load initial data
dd_deployTable <- readRDS(here("data/deployTable_DD.rds")) %>%
                            select(site, dist2SPP1,dist2SPP2)
myData <- readRDS(here("data", "birdnet_LFRR.rds"))

last_DD01_date <- myData %>%
  filter(Site == "DD01") %>%
  summarise(last_date = max(datetime_UTC)) %>%
  pull(last_date)
last_DD01_date

#########
# To Do #
#########
# Next step is to:
# 1. Review files in source (below) to:
# 2. Eliminate all false calls
# 3. Determine the source of the calls as DD01 or DD02 and annotate as such
# 4. Use this info to modify the source and detectionData files below
# 5. Redo the detection function


# First, Identify Source Site (DD01 vs DD02)
sourceSites <- c("DD01", "DD02")
source <- myData %>%
  filter(Site %in% sourceSites)%>%
  filter(
    !(datetime_UTC > last_DD01_date)
  ) %>%
  mutate(
    # Extract 15 characters before ".WAV"
    file_datetime_chr = str_sub(Begin.Path, -19, -5),
    # Convert to datetime
    file_datetime = as.POSIXct(file_datetime_chr, format = "%Y%m%d_%H%M%S", tz = "UTC")
  ) %>%
  arrange(datetime_UTC)

glimpse(source)

#distance dataset
distanceData <- myData %>%
  mutate(
    source = "DD01",  # Will want to replace this when I address the true source
    groupSize = 1,
    detection_ID = row_number()
  ) %>%
  filter(Site %in% dd_deployTable$site) %>%
  filter(
    !(datetime_UTC > last_DD01_date)
  ) %>%
  left_join(dd_deployTable, by = "site") %>%
  mutate(
    dist2source = case_when(
      as.character(source) == "DD01" ~ dist2SPP1,
      as.character(source) == "DD02" ~ dist2SPP2,
      is.na(source) ~ NA_real_
    )
  )
saveRDS(distanceData, file = here("data/distanceData.rds"))

glimpse(distanceData)
  
  
#Time series plot of #detections by site
test2 <- test %>%
  filter(site %in% sourceSites)
file2plot <- distanceData
ddPlot <- file2plot %>%
  # Count detections per Site and time (e.g., per day)
  count(Site, date = as.Date(datetime_local)) %>%
  # Plot
  ggplot(aes(x = date, y = n, color = Site)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = "Number of Detections", color = "Site") +
  theme_minimal()

ddPlot







  
#####count()######################
# detection function #
######################
#https://workshops.distancesampling.org/duke-spatial-2015/practicals/1-detection-functions-solutions.html

# df_hn <- ds(data=distdata, truncation=200, key="hn", adjustment=NULL)
distdata <- distanceData %>%
  rename(
    distance = dist2source,
    size = groupSize,
    object = detection_ID
  )

df_hn <- ds(data = distdata, truncation = 200, key = "hn", adjustment = NULL)
summary(df_hn)
 
ddf.gof(df_hn$ddf, asp=1)
plot(df_hn)
