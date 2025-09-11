# Purpose of this script is to quality check data collected

# Next:
# Check that all files have recording percursor (missing on SPP-DD10_48c_20250716)


##########################
#### Select Root Path ####
##########################
root_path <- "C:/Users/Shannon/Documents/audiomoth/Rails/birdnet"


###############
#### Ready ####
###############
library(here)
here()
source(here::here("_code/_common.R"))
deployDetails <- read_rds(here("data", "deployDetails.rds"))
inventory <- read_rds(here("data", "inventory.rds"))



######################################################
### check that wav files have recording precursor  ###
######################################################
# Review specifically identified folder to determine 
# If wav files have device_ID appended to name
# User-reviewed modification of wav files if needed
# Note, this only works on a per folder basis!

### !!! BE CAREFUL !!! ###


#Folder Name, where the folder name is the deployment_ID
folder_name <- "SPP-DD10_48c_20250716"


rename_wav_files(root_path, folder_name, deployDetails, inventory)

# You must review the printed data to ensure info is correct
# If it is correct, select YES to rename all wav files in the folde




###########################################
### check for birdnet selection tables  ###
###########################################
# Recursively check the root_path directories for birdNET_SelectionTables. 
# If they are not found, create them by merging all birdnet files in that folder

check_birdnet_selectionTables(root_path)
