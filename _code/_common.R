#Load Libraries
library(tidyverse)
library(janitor)
library(glue)
library(Rraven)  #Note, if not installed, use--> remotes::install_github("maRce10/Rraven")
library(warbleR)  #Note, if not installed, use --> remotes::install_github("maRce10/warbleR")
library(birdnetTools)  #Note, if not installed, use--> pak::pak("birdnet-team/birdnetTools")
library(PAMmisc)
library(PAMpal)
library(PAMscapes)
library(fs)
library(stringr)
library(DT)
library(leaflet)
library(geosphere)
library(kableExtra)
library(readODS)
library(soundecology)
library(knitr)
library(kableExtra)
library(htmltools)

############################
# Update SPP-DD for source #
############################
# First, review files in Raven and get extra measures

# Review SPP-DD01 and SPP-DD02 get list of all LFRR calls

# For each call, 


#######################
# recordingDataCheck  #
#######################
#For new recordings, check that (1) correct folder name (location, site, config_type match the DeployDetails.ods), and (2) the device_ID from the wav filename matches the recorder_ID/device_ID from the DeployDetails.ods

library(tidyverse)
library(readODS)

recordingDataCheck <- function(folderPath, ods_file) {
  
  folderName <- basename(folderPath)
  
  # Read the ODS sheets
  deploy_details <- read_ods(ods_file, sheet = "DeployDetails")
  inventory <- read_ods(ods_file, sheet = "Inventory")
  
  # Get list of WAV files
  wav_files <- list.files(folderPath, pattern = "\\.(wav|WAV)$", full.names = FALSE)
  
  # Parse folderName components
  folder_components <- str_split(folderName, "_", simplify = TRUE)
  folder_location_site <- folder_components[1]
  folder_config_type <- folder_components[2]
  folder_components[3] <- str_remove(folder_components[3], "\\.(wav|WAV)$")#NEW
  folder_date <- folder_components[3]#NEW
  
  # Parse location-site from folderName
  location_site_parts <- str_split(folder_location_site, "-", simplify = TRUE)
  folder_location <- location_site_parts[1]
  folder_site <- location_site_parts[2]
  
  # Check 1: Verify folderName matches DeployDetails
  deploy_row <- deploy_details %>%
    filter(deployment_ID == folderName)
  
  if (nrow(deploy_row) == 0) {
    cat("Error: deployment_ID", folderName, "not found in DeployDetails\n")
    return(invisible(NULL))
  }
  
  mismatches <- c()
  
  if (deploy_row$location != folder_location) {
    mismatches <- c(mismatches, paste0("location: expected ", deploy_row$location, ", found ", folder_location))
  }
  if (deploy_row$site != folder_site) {
    mismatches <- c(mismatches, paste0("site: expected ", deploy_row$site, ", found ", folder_site))
  }
  if (deploy_row$config_type != folder_config_type) {
    mismatches <- c(mismatches, paste0("config_type: expected ", deploy_row$config_type, ", found ", folder_config_type))
  }  
  if (length(mismatches) == 0) {
    cat("folderName matches DeployDetails\n")
  } else {
    cat("Mismatch found:\n")
    cat(paste(mismatches, collapse = "\n"), "\n")
  }
  
  # Check 2: Verify device_ID in wav files
  recorder_id <- deploy_row$recorder_ID
  
  device_row <- inventory %>%
    filter(recorder_ID == recorder_id)
  
  if (nrow(device_row) == 0) {
    cat("Warning: recorder_ID", recorder_id, "not found in Inventory\n")
    return(invisible(NULL))
  }
  
  expected_device_id <- device_row$device_ID
  
  # Parse device_ID from first wav file
  if (length(wav_files) > 0) {
    wav_device_id <- str_split(wav_files[1], "_", simplify = TRUE)[1]
    
    if (wav_device_id == expected_device_id) {
      cat("device_ID in wav file matches device_ID/recorder_ID in DeployDetails\n")
    } else {
      cat("Warning: device_ID for wav file does not match DeployDetails\n")
      cat("  Expected:", expected_device_id, "\n")
      cat("  Found:", wav_device_id, "\n")
    }
  } else {
    cat("No WAV files found in folder\n")
  }
  
  return(invisible(NULL))
}

####################
# recordingRename  #
####################
# WARNING: Run recordingDataCheck function BEFORE RUNNING THIS FUNCTION!!! Rename all wav files in a folder by replacing the device_ID (first section of the filename) with the folderName. 

# Function 2: Rename Files
recordingRename <- function(folderPath) {
  
  folderName <- basename(folderPath) %>% str_remove("_[^_]+$")
  
  # Get list of WAV files
  wav_files <- list.files(folderPath, pattern = "\\.(wav|WAV)$", full.names = FALSE)
  
  if (length(wav_files) == 0) {
    cat("No WAV files found in folder\n")
    return(invisible(NULL))
  }
  
  # Rename files
  for (file in wav_files) {
    # Parse filename components
    file_parts <- str_split(file, "_", simplify = TRUE)
    
    # Remove device_ID (first component) and replace with folderName
    new_filename <- paste0(folderName, "_", 
                           paste(file_parts[-1], collapse = "_"))
    
    # Construct full paths
    old_path <- file.path(folderPath, file)
    new_path <- file.path(folderPath, new_filename)
    
    # Rename file
    file.rename(old_path, new_path)
  }
  
  # Display results
  cat("Folder:", folderName, "\n")
  cat("First five new filenames:\n")
  new_files <- list.files(folderPath, pattern = "\\.(wav|WAV)$", full.names = FALSE)
  cat(paste(head(new_files, 5), collapse = "\n"), "\n")
  
  return(invisible(NULL))
}

###############################
# recordingRenameNOdevice_ID  #
###############################
recordingRenameNOdevice_ID <- function(folderPath) {
  
  folderName <- basename(folderPath)
  
  # Extract location-site and config (first two parts)
  prefix <- folderName |>
    str_split("_", simplify = TRUE) |>
    (\(x) paste(x[1], x[2], sep = "_"))()
  
  # Get WAV files
  files <- list.files(
    folderPath,
    pattern = "\\.(wav|WAV)$",
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    message("No WAV files found.")
    return(invisible(NULL))
  }
  
  # Prevent double-prefixing
  old_names <- basename(files)
  
  needs_prefix <- !str_starts(old_names, paste0(prefix, "_"))
  
  files_to_rename <- files[needs_prefix]
  old_names <- old_names[needs_prefix]
  
  new_names <- paste0(prefix, "_", old_names)
  
  file.rename(
    from = files_to_rename,
    to   = file.path(folderPath, new_names)
  )
  
  invisible(
    tibble(
      old = basename(files_to_rename),
      new = new_names
    )
  )
}


###########################
# Create Video Dataframe  #
###########################
create_video_list <- function(folder = "videos") {
  # Get list of mp4 files in the specified folder
  video_files <- list.files(here(folder), pattern = "\\.mp4$", full.names = FALSE)
  
  # Create dataframe and extract information from filenames
  videoList <- tibble(filename = video_files) %>%
    mutate(filename_no_ext = str_remove(filename, "\\.mp4$")) %>%
    separate(filename_no_ext, into = c("species", "location", "date"), 
             sep = "_", remove = TRUE) %>%
    mutate(species = str_replace_all(species, "-", " ")) %>%
    select(filename, species, location, date)
  
  # Read the captions.csv file
  captions_df <- read_csv(here(folder, "captions.csv"))
  
  # Join the captions to the videoList by filename
  videoList <- videoList %>%
    left_join(captions_df, by = "filename")

  saveRDS(videoList, here("videos/videoList.rds"))
  return(videoList)
}


###########################################
# Function for 2 column Dashboard layout  #
###########################################
library(htmltools)
library(ggplot2)
library(knitr)

two_col_card <- function(left_content,
                         title = "About this figure",
                         text  = "",
                         left_width = 7,
                         right_width = 5,
                         width = 7,
                         height = 5,
                         dpi = 100) {
  
  # Save ggplot to a temporary file
  imgfile <- tempfile(fileext = ".png")
  
  ggsave(
    filename = imgfile,
    plot     = left_content,
    width    = width,
    height   = height,
    dpi      = dpi
  )
  
  # Convert to base64 embedded image
  img64 <- knitr::image_uri(imgfile)
  
  # Build two-column layout
  htmltools::div(
    class = "row g-3",
    htmltools::div(
      class = paste0("col-lg-", left_width),
      tags$img(src = img64, style = "width:100%; height:auto;")
    ),
    htmltools::div(
      class = paste0("col-lg-", right_width),
      htmltools::div(
        class = "card",
        htmltools::div(
          class = "card-body",
          tags$h5(class = "card-title", title),
          tags$p(class = "card-text", text)
        )
      )
    )
  )
}






#######################################
# check for birdnet selection tables  #
#######################################

check_birdnet_selectionTables <- function(root_path) {
  # Find all subfolders containing "birdnetLocal"
  subfolders <- list.dirs(root_path, recursive = TRUE, full.names = TRUE) %>% 
    str_subset("birdnetLocal")
  
  if (length(subfolders) == 0) {
    warning(glue("No folders containing 'birdnetLocal' found in {root_path}"))
    return(invisible(NULL))
  }
 
   
##########################################
# rename birdnet files to add foldername #
##########################################
rename_birdnet_files <- function(root_path) {
  # Get all subfolders recursively
  all_subfolders <- list.dirs(root_path, recursive = TRUE, full.names = TRUE)
  
  for (bn in all_subfolders) {
    # Only process subfolders containing "birdnetlocal_" (case-insensitive)
    if (grepl("birdnetlocal_", basename(bn), ignore.case = TRUE)) {
      
      # List files inside this subfolder
      files_in_bn <- list.files(bn, full.names = TRUE)
      
      # Look for BirdNET_SelectionTable.txt (case-insensitive match)
      target_file <- files_in_bn[grepl("^birdnet_selectiontable\\.txt$", 
                                       basename(files_in_bn), ignore.case = TRUE)]
      
      if (length(target_file) == 1) {
        # Extract everything AFTER the first underscore
        folder_name <- basename(bn)
        parts <- unlist(strsplit(folder_name, "_", fixed = TRUE))
        
        if (length(parts) >= 2) {
          suffix <- paste(parts[-1], collapse = "_")  # everything after first "_"
          
          # Build new file name
          new_name <- paste0("BirdNET_SelectionTable_", suffix, ".txt")
          new_path <- file.path(bn, new_name)
          
          # If file already exists, ask user before overwriting
          if (file.exists(new_path)) {
            answer <- readline(
              paste0("File already exists: ", new_path, 
                     "\nDo you want to overwrite it? (y/n): ")
            )
            
            if (tolower(answer) == "y") {
              file.remove(new_path)
              file.rename(target_file, new_path)
              message("Overwritten: ", target_file, " -> ", new_path)
            } else {
              message("Skipped: ", new_path)
            }
          } else {
            # Rename directly if no conflict
            file.rename(target_file, new_path)
            message("Renamed: ", target_file, " -> ", new_path)
          }
        } else {
          message("Skipping ", bn, " (no suffix after underscore)")
        }
      }
    }
  }
}


###########################################
# add deploymentID column to birdnet file #
###########################################

add_deploymentID <- function(root_path) {
  # Find all renamed BirdNET_SelectionTable_xxx.txt files
  all_files <- list.files(root_path, pattern = "^BirdNET_SelectionTable_.*\\.txt$",
                          recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  
  for (f in all_files) {
    # Extract xxx part (everything after first "_")
    fname <- basename(f)
    parts <- unlist(strsplit(fname, "_", fixed = TRUE))
    
    if (length(parts) >= 3) {
      deploymentID <- paste(parts[-(1:2)], collapse = "_")  # drop "BirdNET" + "SelectionTable"
      deploymentID <- sub("\\.txt$", "", deploymentID, ignore.case = TRUE)  # remove extension
      
      # Read file
      df <- tryCatch(
        read.delim(f, sep = "\t", stringsAsFactors = FALSE),
        error = function(e) {
          message("Could not read file: ", f, "\nError: ", e$message)
          return(NULL)
        }
      )
      
      if (!is.null(df)) {
        # Add deployment_ID column
        df$deployment_ID <- deploymentID
        
        # Write file back
        write.table(df, f, sep = "\t", row.names = FALSE, quote = FALSE)
        message(sprintf("Updated file: %s, deployment_ID: %s", f, deploymentID))
      }
    }
  }
}

######################################################
# copy birdnet selection tables to github raw folder #
######################################################
copy_birdnet_files <- function(root_path, dest_path) {
  # Find all files recursively
  all_files <- dir_ls(root_path, recurse = TRUE)
  
  # Filter for files containing "BirdNET_SelectionTable_" (case insensitive)
  files_to_copy <- all_files[str_detect(basename(all_files), regex("birdnet_selectiontable_", ignore_case = TRUE))]
  
  # Ensure destination folder exists
  dir_create(dest_path)
  
  # Copy each file
  walk(files_to_copy, ~ file_copy(.x, dest_path))
  
  # Print summary
  message("Copied ", length(files_to_copy), " files to ", dest_path)
  
  # Return the list of copied files (invisibly)
  invisible(files_to_copy)
}


####################################
# Function to process each folder  #
#################################### 
process_folder <- function(folder) {
  folder_name <- basename(folder)
  output_file <- file.path(folder, "birdNET_SelectionTable.txt")
  
  # Case 1: Already exists
  if (file.exists(output_file)) {
    message(glue("{folder_name}: birdNET_SelectionTable.txt already exists."))
    return(invisible(NULL))
  }
  
  # Case 2: Look for BirdNET.selection.table files
  files_to_merge <- list.files(folder, pattern = "BirdNET\\.selection\\.table", 
                               full.names = TRUE, ignore.case = FALSE)
  
  if (length(files_to_merge) > 0) {
    merged <- map_dfr(files_to_merge, ~ read_tsv(.x, show_col_types = FALSE))
    write_tsv(merged, output_file)
    message(glue("{folder_name}: created birdNET_SelectionTable.txt by merging {length(files_to_merge)} file(s)."))
  } else {
    # Case 3: Nothing found
    warning(glue("{folder_name}: no birdNET_SelectionTable.txt and no BirdNET.selection.table files found."))
  }
}

# Apply to all subfolders
walk(subfolders, process_folder)

message("✅ Finished processing all folders in ", root_path)
}


#####################################
# append device_ID to wav file name #
#####################################
rename_wav_files <- function(root_path, folder_name, deployDetails, inventory) {
  # Build full folder path
  folder_path <- file.path(root_path, folder_name)
  
  if (!dir.exists(folder_path)) {
    stop(glue("Folder {folder_name} not found in {root_path}"))
  }
  
  # Step 1: match deployment_ID in deployDetails
  deployment_row <- deployDetails %>%
    filter(deployment_ID == folder_name)
  
  if (nrow(deployment_row) == 0) {
    stop(glue("No matching deployment_ID found in deployDetails for {folder_name}"))
  }
  
  recorder_ID <- deployment_row$recorder_ID[1]
  
  # Step 2: match recorder_ID in inventory
  inventory_row <- inventory %>%
    filter(recorder_ID == recorder_ID)
  
  if (nrow(inventory_row) == 0) {
    stop(glue("No matching recorder_ID found in inventory for {recorder_ID}"))
  }
  
  device_ID <- inventory_row$device_ID[1]
  
  # Step 3: get wav files
  wav_files <- list.files(folder_path, pattern = "\\.wav$", full.names = TRUE, ignore.case = TRUE)
  
  preview <- if (length(wav_files) > 0) {
    new_names <- file.path(
      folder_path,
      ifelse(startsWith(basename(wav_files), paste0(device_ID, "_")),
             basename(wav_files),
             paste0(device_ID, "_", basename(wav_files)))
    )
    head(basename(new_names), 3)  # preview first 3 new names
  } else {
    "No wav files found"
  }
  
  # Step 4: print summary BEFORE asking
  summary_text <- glue(
    "Folder: {folder_name}\n",
    "Deployment_ID: {deployment_row$deployment_ID}\n",
    "Recorder_ID: {recorder_ID}\n",
    "Device_ID: {device_ID}\n",
    "Sample new filenames:\n  {paste(preview, collapse = '\n  ')}\n\n"
  )
  
  # Force flush to console immediately
  writeLines(summary_text, con = stdout())
  flush.console()
  
  # Step 5: confirm action
  response <- readline(prompt = "Do you want to rename all wav files in this folder? (yes/no): ")
  
  if (tolower(response) != "yes") {
    message("No Changes Made")
    return(invisible(NULL))
  }
  
  if (length(wav_files) == 0) {
    message("No wav files found in ", folder_name)
    return(invisible(NULL))
  }
  
  # Step 6: actually rename
  new_names <- file.path(
    folder_path,
    ifelse(startsWith(basename(wav_files), paste0(device_ID, "_")),
           basename(wav_files),
           paste0(device_ID, "_", basename(wav_files)))
  )
  
  file.rename(wav_files, new_names)
  
  message(glue("Renamed {length(wav_files)} wav files in {folder_name}"))
}
