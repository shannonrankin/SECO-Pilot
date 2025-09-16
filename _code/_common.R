# Load Libraries
library(tidyverse)
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

######################################################
# save birdnet selection tables to github raw folder #
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
  
  # Function to process each folder
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
