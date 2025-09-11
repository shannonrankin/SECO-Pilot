## Deployment Tables
#libraries
library(readODS)
library(tidyverse)
library(here)
here()

## Update secondary deployDetails sheets in Repo
configDetails <- read_ods(here("data", "SE_DeployDetails.ODS"), sheet = 2) 
saveRDS(configDetails, file = "data/configDetails.rds")

inventory <- read_ods(here("data", "SE_DeployDetails.ODS"), sheet = 4) 
saveRDS(inventory, file = "data/inventory.rds")

Location_Site <- read_ods(here("data", "SE_DeployDetails.ODS"), sheet = 5) 
saveRDS(Location_Site, file = "data/Location_Site.rds")

##########################
#### Deployment Table ####
##########################

goodCols <- c("deployment_ID","location", "site", "config_type", "deploy_date", "latitude", "longitude", "retrieve_date")

deployments_all <- read_ods(here("data", "SE_DeployDetails.ODS"), sheet = 1)

deployments <- as_tibble(deployments_all) %>%
  select(all_of(goodCols)) 
saveRDS(deployments, file="data/deployTable.rds")

deployments <- as_tibble(deployments_all) %>%
  select(all_of(goodCols)) %>%
  filter(location %in% c("SPP-DD", "SPP", "TJE", "SGR"))
saveRDS(deployments, file="data/deployTable_railPilot.rds")

saveRDS(deployments_all, file="data/deployDetails.rds" )

  
  
  # record_start_Local = %>% #extract record start time in local time
  # record_end_Local = %>% #extract record end time in local time
  # record_length_hr =  %>%  # Add recording length in hrs after I fix the start/end record times 
  # record_length_day =  %>% # Add recording length in days
  # record_limit_type =   # Determine if recorder was battery or storage limited
  

