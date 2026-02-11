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
library(glue)
library(DT)
library(leaflet)
library(geosphere)
library(kableExtra)
library(readODS)
library(soundecology)
library(knitr)
library(kableExtra)
library(htmltools)
library(tuneR)
library(seewave)

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
add_deploymentID <- function(root_path, lowFreq, hiFreq) {
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
        
        # Update Low.Freq..Hz. and High.Freq..Hz. columns if they exist
        if (!"Low.Freq..Hz." %in% colnames(df)) {
          warning("Column 'Low.Freq..Hz.' not found in file: ", f)
        } else {
          df$Low.Freq..Hz. <- lowFreq
        }
        
        if (!"High.Freq..Hz." %in% colnames(df)) {
          warning("Column 'High.Freq..Hz.' not found in file: ", f)
        } else {
          df$High.Freq..Hz. <- hiFreq
        }
        
        # Write file back
        write.table(df, f, sep = "\t", row.names = FALSE, quote = FALSE)
        message(sprintf("Updated file: %s, deployment_ID: %s", f, deploymentID))
      }
    }
  }
}


# add_deploymentID("your/path/here", lowFreq = 50, hiFreq = 10000)

# add_deploymentID <- function(root_path) {
#   # Find all renamed BirdNET_SelectionTable_xxx.txt files
#   all_files <- list.files(root_path, pattern = "^BirdNET_SelectionTable_.*\\.txt$",
#                           recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
#   
#   for (f in all_files) {
#     # Extract xxx part (everything after first "_")
#     fname <- basename(f)
#     parts <- unlist(strsplit(fname, "_", fixed = TRUE))
#     
#     if (length(parts) >= 3) {
#       deploymentID <- paste(parts[-(1:2)], collapse = "_")  # drop "BirdNET" + "SelectionTable"
#       deploymentID <- sub("\\.txt$", "", deploymentID, ignore.case = TRUE)  # remove extension
#       
#       # Read file
#       df <- tryCatch(
#         read.delim(f, sep = "\t", stringsAsFactors = FALSE),
#         error = function(e) {
#           message("Could not read file: ", f, "\nError: ", e$message)
#           return(NULL)
#         }
#       )
#       
#       if (!is.null(df)) {
#         # Add deployment_ID column
#         df$deployment_ID <- deploymentID
#         
#         # Write file back
#         write.table(df, f, sep = "\t", row.names = FALSE, quote = FALSE)
#         message(sprintf("Updated file: %s, deployment_ID: %s", f, deploymentID))
#       }
#     }
#   }
# }

##################################################
# Process birdnet selectiontable (add measures)  #
##################################################
# Main function to process BirdNET selection table and add acoustic measurements
process_birdnet_table <- function(selection_table_path) {
  
  # Read the BirdNET selection table
  cat("Reading BirdNET selection table...\n")
  birdnet_data <- read.delim(selection_table_path, stringsAsFactors = FALSE)
  
  # Initialize error log
  error_log <- tibble(
    Row = integer(),
    Begin.Path = character(),
    File.Offset = numeric(),
    Error.Type = character(),
    Error.Message = character(),
    Timestamp = character()
  )
  
  # Initialize columns for spectro_analysis and sig2noise results
  spectro_cols <- c("meanfreq", "sd", "freq.median", "freq.Q25", "freq.Q75", 
                    "freq.IQR", "time.median", "time.Q25", "time.Q75", 
                    "time.IQR", "skew", "kurt", "sp.ent", "time.ent", 
                    "entropy", "sfm", "meandom", "mindom", "maxdom", 
                    "dfrange", "modindx", "startdom", "enddom", "dfslope", 
                    "meanpeakf")
  
  snr_cols <- c("SNR")
  
  # Add amplitude measurement columns
  amplitude_cols <- c("rms_amplitude", "peak_amplitude", "mean_amplitude", 
                      "median_amplitude", "sd_amplitude", "dB_rms", "dB_peak")
  
  # Add empty columns to the dataframe
  for (col in c(spectro_cols, snr_cols, amplitude_cols)) {
    birdnet_data[[col]] <- NA_real_
  }
  
  cat(paste0("Processing ", nrow(birdnet_data), " detections...\n\n"))
  
  # Add progress tracking
  start_time <- Sys.time()
  progress_interval <- 100  # Report progress every 100 rows
  
  # Process each row
  for (i in 1:nrow(birdnet_data)) {
    
    # Show progress at intervals
    if (i %% progress_interval == 0) {
      elapsed <- difftime(Sys.time(), start_time, units = "mins")
      rate <- i / as.numeric(elapsed)
      est_remaining <- (nrow(birdnet_data) - i) / rate
      cat(sprintf("Progress: %d/%d (%.1f%%) | Elapsed: %.1f min | Est. remaining: %.1f min\n", 
                  i, nrow(birdnet_data), (i/nrow(birdnet_data))*100, 
                  as.numeric(elapsed), est_remaining))
    }
    
    tryCatch({
      
      # Extract information from current row
      wav_path <- birdnet_data$Begin.Path[i]
      start_time <- birdnet_data$File.Offset..s.[i]
      end_time <- start_time + 3  # 3 second duration
      
      # Only print detailed info for first few rows or at intervals
      if (i <= 3 || i %% progress_interval == 0) {
        cat(paste0("Row ", i, "/", nrow(birdnet_data), ": ", basename(wav_path), 
                   " (", start_time, "-", end_time, "s)\n"))
      }
      
      # Check if file exists
      if (!file.exists(wav_path)) {
        stop("WAV file not found")
      }
      
      # Read the WAV file
      wave_obj <- readWave(wav_path, from = start_time, to = end_time, units = "seconds")
      
      # Get sampling rate
      samp_rate <- wave_obj@samp.rate
      
      # Create temporary directory and save extracted segment
      temp_dir <- tempdir()
      temp_wav_name <- paste0("temp_", i, "_", basename(wav_path))
      temp_wav_path <- file.path(temp_dir, temp_wav_name)
      writeWave(wave_obj, temp_wav_path)
      
      # Create a temporary selection table for warbleR functions
      # Set reasonable frequency limits if not specified
      low_freq <- birdnet_data$Low.Freq..Hz.[i] / 1000  # Convert to kHz
      high_freq <- birdnet_data$High.Freq..Hz.[i] / 1000  # Convert to kHz
      
      # Ensure frequencies are within valid range
      if (is.na(low_freq) || low_freq < 0) low_freq <- 0
      if (is.na(high_freq) || high_freq > (samp_rate/2000)) high_freq <- samp_rate/2000
      
      temp_selec <- data.frame(
        sound.files = temp_wav_name,
        selec = 1,
        start = 0,  # Since we've already extracted the segment
        end = 3,
        bottom.freq = low_freq,
        top.freq = high_freq,
        stringsAsFactors = FALSE
      )
      
      # Run spectro_analysis with default parameters
      spectro_results <- spectro_analysis(temp_selec, path = temp_dir, 
                                          bp = c(0, 22), wl = 512, 
                                          threshold = 15, parallel = 1)
      
      # Run sig2noise with default parameters - wrap in separate tryCatch
      # Try different approaches if the first one fails
      snr_results <- NULL
      
      # Try with default mar
      tryCatch({
        snr_results <- sig2noise(temp_selec, path = temp_dir, 
                                 mar = 0.04, parallel = 1)
      }, error = function(e) {
        # Try with larger margin
        tryCatch({
          snr_results <<- sig2noise(temp_selec, path = temp_dir, 
                                    mar = 0.1, parallel = 1)
        }, error = function(e2) {
          # Try with even larger margin
          tryCatch({
            snr_results <<- sig2noise(temp_selec, path = temp_dir, 
                                      mar = 0.2, parallel = 1)
          }, error = function(e3) {
            # sig2noise failed, continue without it
          })
        })
      })
      
      # Calculate amplitude measurements on frequency-filtered signal
      tryCatch({
        # Apply bandpass filter using the BirdNET frequency range
        low_freq_hz <- birdnet_data$Low.Freq..Hz.[i]
        high_freq_hz <- birdnet_data$High.Freq..Hz.[i]
        
        # Convert to normalized frequency (0-1 range where 1 = Nyquist)
        nyquist <- samp_rate / 2
        
        # Ensure frequencies are valid
        if (!is.na(low_freq_hz) && !is.na(high_freq_hz) && 
            low_freq_hz < high_freq_hz && high_freq_hz <= nyquist) {
          
          # Apply bandpass filter
          filtered_wave <- ffilter(wave_obj, from = low_freq_hz, to = high_freq_hz, 
                                   bandpass = TRUE, output = "Wave")
          
          # Extract the filtered signal as numeric vector
          signal <- filtered_wave@left
          
          # Calculate various amplitude measures
          # 1. RMS (Root Mean Square) amplitude
          rms_amp <- sqrt(mean(signal^2))
          birdnet_data$rms_amplitude[i] <- rms_amp
          
          # 2. Peak amplitude (maximum absolute value)
          peak_amp <- max(abs(signal))
          birdnet_data$peak_amplitude[i] <- peak_amp
          
          # 3. Mean amplitude (of absolute values)
          mean_amp <- mean(abs(signal))
          birdnet_data$mean_amplitude[i] <- mean_amp
          
          # 4. Median amplitude (of absolute values)
          median_amp <- median(abs(signal))
          birdnet_data$median_amplitude[i] <- median_amp
          
          # 5. Standard deviation of amplitude
          sd_amp <- sd(abs(signal))
          birdnet_data$sd_amplitude[i] <- sd_amp
          
          # 6. RMS in decibels (relative to max possible amplitude)
          if (rms_amp > 0) {
            birdnet_data$dB_rms[i] <- 20 * log10(rms_amp)
          }
          
          # 7. Peak in decibels
          if (peak_amp > 0) {
            birdnet_data$dB_peak[i] <- 20 * log10(peak_amp)
          }
          
        } else {
          # Invalid frequency range, skip amplitude calculations
        }
        
      }, error = function(e) {
        cat("  WARNING: Amplitude calculation failed -", e$message, "\n")
      })
      
      # Add results to the main dataframe - handle different result structures
      if (!is.null(spectro_results) && nrow(spectro_results) > 0) {
        for (col in spectro_cols) {
          if (col %in% names(spectro_results)) {
            # Extract the value properly - could be in different row depending on warbleR version
            val <- spectro_results[[col]][1]
            if (!is.null(val) && length(val) > 0) {
              birdnet_data[[col]][i] <- val
            }
          }
        }
      }
      
      if (!is.null(snr_results) && nrow(snr_results) > 0) {
        if ("SNR" %in% names(snr_results)) {
          val <- snr_results$SNR[1]
          if (!is.null(val) && length(val) > 0) {
            birdnet_data$SNR[i] <- val
          }
        }
      }
      
      # Additional debug for first row to verify assignment
      if (i == 1) {
        cat("  Verified: meanfreq =", birdnet_data$meanfreq[1], 
            "| rms_amplitude =", birdnet_data$rms_amplitude[1], "\n")
      }
      
      # Clean up temporary file immediately to free memory
      if (file.exists(temp_wav_path)) {
        file.remove(temp_wav_path)
      }
      
      # Force garbage collection every 500 rows to free memory
      if (i %% 500 == 0) {
        gc(verbose = FALSE)
      }
      
      # Check for NA values in new columns (only report at intervals)
      if (i <= 3 || i %% progress_interval == 0) {
        na_cols <- c(spectro_cols, snr_cols, amplitude_cols)[sapply(c(spectro_cols, snr_cols, amplitude_cols), 
                                                                    function(col) is.na(birdnet_data[[col]][i]))]
        if (length(na_cols) > 0) {
          cat(paste0("  WARNING: NA values in: ", paste(na_cols, collapse = ", "), "\n"))
        }
      }
      
    }, error = function(e) {
      
      # Log error
      error_entry <- tibble(
        Row = i,
        Begin.Path = birdnet_data$Begin.Path[i],
        File.Offset = birdnet_data$File.Offset..s.[i],
        Error.Type = class(e)[1],
        Error.Message = as.character(e$message),
        Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
      
      error_log <<- bind_rows(error_log, error_entry)
      
      # Only print errors at intervals or for first few rows
      if (i <= 3 || i %% progress_interval == 0) {
        cat(paste0("  ERROR: ", e$message, "\n"))
      }
      
      # Clean up temp file even on error
      temp_wav_path <- file.path(tempdir(), paste0("temp_", i, "_", basename(birdnet_data$Begin.Path[i])))
      if (file.exists(temp_wav_path)) {
        file.remove(temp_wav_path)
      }
      
    })
    
  }
  
  # Final progress update
  total_time <- difftime(Sys.time(), start_time, units = "mins")
  cat(sprintf("\nProcessing complete! Total time: %.1f minutes (%.2f rows/minute)\n", 
              as.numeric(total_time), nrow(birdnet_data)/as.numeric(total_time)))
  
  # Generate output filename
  original_filename <- basename(selection_table_path)
  output_filename <- paste0("Appended_", original_filename)
  output_path <- file.path(dirname(selection_table_path), output_filename)
  
  # Save the appended table
  cat("\nSaving results...\n")
  write.table(birdnet_data, output_path, sep = "\t", row.names = FALSE, quote = FALSE)
  cat("Results saved successfully!\n")
  
  # Save error log if there were errors
  if (nrow(error_log) > 0) {
    error_log_filename <- paste0("ErrorLog_", 
                                 format(Sys.time(), "%Y%m%d_%H%M%S"), "_",
                                 tools::file_path_sans_ext(original_filename), ".txt")
    error_log_path <- file.path(dirname(selection_table_path), error_log_filename)
    write.table(error_log, error_log_path, sep = "\t", row.names = FALSE, quote = FALSE)
    cat(paste0("\nError log saved to: ", error_log_path, "\n"))
  }
  
  # Generate summary report
  cat("\n========== PROCESSING COMPLETE ==========\n")
  cat(paste0("Output file: ", output_filename, "\n"))
  cat(paste0("Saved to: ", output_path, "\n\n"))
  
  cat("Newly added columns:\n")
  cat("Spectro-analysis measures:\n")
  for (col in spectro_cols) {
    cat(paste0("  - ", col, "\n"))
  }
  cat("\nSignal-to-noise ratio:\n")
  for (col in snr_cols) {
    cat(paste0("  - ", col, "\n"))
  }
  cat("\nAmplitude measures:\n")
  for (col in amplitude_cols) {
    cat(paste0("  - ", col, "\n"))
  }
  
  cat(paste0("\nTotal number of rows: ", nrow(birdnet_data), "\n"))
  
  # Count rows with NA values in new columns
  new_cols <- c(spectro_cols, snr_cols, amplitude_cols)
  na_count <- sum(apply(birdnet_data[, new_cols], 1, function(row) any(is.na(row))))
  cat(paste0("Rows with NA values in new columns: ", na_count, "\n"))
  
  # Detailed NA breakdown by column
  cat("\nNA count by column:\n")
  for (col in new_cols) {
    na_col_count <- sum(is.na(birdnet_data[[col]]))
    if (na_col_count > 0) {
      cat(paste0("  - ", col, ": ", na_col_count, "\n"))
    }
  }
  
  if (nrow(error_log) > 0) {
    cat(paste0("\nTotal errors encountered: ", nrow(error_log), "\n"))
  }
  
  cat("=========================================\n")
  
  return(list(
    output_path = output_path,
    spectro_columns = spectro_cols,
    snr_columns = snr_cols,
    amplitude_columns = amplitude_cols,
    total_rows = nrow(birdnet_data),
    rows_with_na = na_count,
    error_count = nrow(error_log)
  ))
}

# Example usage:
# results <- process_birdnet_table("path/to/BirdNET_SelectionTable_SPP-DD10_48c_20250716.txt")


######################################################
# copy birdnet selection tables to github raw folder #
######################################################
copy_birdnet_files <- function(root_path, dest_path) {
  # Find all files recursively
  all_files <- dir_ls(root_path, recurse = TRUE)
  
  # Filter for files containing "BirdNET_SelectionTable_" (case insensitive)
  files_to_copy <- all_files[str_detect(basename(all_files), regex("Appended_BirdNET_SelectionTable", ignore_case = TRUE))]
  
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

# # Apply to all subfolders
# walk(subfolders, process_folder)
# 
# message("✅ Finished processing all folders in ", root_path)


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

###############################
# pair rail detections for DD #
###############################
library(tidyverse)

process_dd_rails <- function(dd_rails) {
  
  # Step 1: Filter to only DD01 and DD02
  dd_filtered <- dd_rails %>%
    filter(site %in% c("DD01", "DD02")) %>%
    mutate(row_id = row_number())
  
  dd01_calls <- dd_filtered %>% filter(site == "DD01")
  dd02_calls <- dd_filtered %>% filter(site == "DD02")
  
  # Step 2: Find pairs within 3 seconds
  pairs_list <- vector("list", nrow(dd01_calls))
  
  for (i in seq_len(nrow(dd01_calls))) {
    dd01_time <- dd01_calls$utc[i]
    time_diffs <- abs(as.numeric(difftime(dd02_calls$utc, dd01_time, units = "secs")))
    matches_idx <- which(time_diffs < time_difference)
    
    if (length(matches_idx) > 0) {
      pairs_list[[i]] <- tibble(
        dd01_row_id = dd01_calls$row_id[i],
        dd02_row_id = dd02_calls$row_id[matches_idx],
        time_diff = time_diffs[matches_idx]
      )
    }
  }
  
  all_pairs <- bind_rows(pairs_list)
  
  # Step 3: Filter to exact 1:1 pairs
  if (nrow(all_pairs) > 0) {
    valid_pair_ids <- all_pairs %>%
      add_count(dd01_row_id, name = "n_dd01") %>%
      add_count(dd02_row_id, name = "n_dd02") %>%
      filter(n_dd01 == 1, n_dd02 == 1) %>%
      select(dd01_row_id, dd02_row_id)
    
    matched_dd01_ids <- valid_pair_ids$dd01_row_id
    matched_dd02_ids <- valid_pair_ids$dd02_row_id
  } else {
    valid_pair_ids <- tibble()
    matched_dd01_ids <- integer(0)
    matched_dd02_ids <- integer(0)
  }
  
  # Create NoMatch dataframe
  dd_rails_NoMatch <- dd_filtered %>%
    filter(!row_id %in% c(matched_dd01_ids, matched_dd02_ids)) %>%
    select(-row_id)
  
  # Step 4: Process valid pairs
  if (nrow(valid_pair_ids) > 0) {
    
    # Join paired data
    paired_data <- valid_pair_ids %>%
      left_join(dd01_calls, by = c("dd01_row_id" = "row_id")) %>%
      left_join(dd02_calls, by = c("dd02_row_id" = "row_id"), suffix = c(".dd01", ".dd02")) %>%
      mutate(pair_id = row_number()) %>%
      rowwise() %>%
      mutate(
        # Calculate comparisons for each criterion
        meanfreq_comp = if (meanfreq.dd01 == meanfreq.dd02) NA else meanfreq.dd01 > meanfreq.dd02,
        rms_amplitude_comp = if (rms_amplitude.dd01 == rms_amplitude.dd02) NA else rms_amplitude.dd01 > rms_amplitude.dd02,
        peak_amplitude_comp = if (peak_amplitude.dd01 == peak_amplitude.dd02) NA else peak_amplitude.dd01 > peak_amplitude.dd02,
        maxdom_comp = if (maxdom.dd01 == maxdom.dd02) NA else maxdom.dd01 > maxdom.dd02,
        dfrange_comp = if (dfrange.dd01 == dfrange.dd02) NA else dfrange.dd01 > dfrange.dd02,
        
        # Calculate source values

        dd01_source_value = {
          comps <- c(meanfreq_comp, rms_amplitude_comp, peak_amplitude_comp, maxdom_comp, dfrange_comp)
          n_non_na <- sum(!is.na(comps))
          if (n_non_na > 0) sum(comps, na.rm = TRUE) / n_non_na else NA_real_
        },
        dd02_source_value = {
          comps <- c(meanfreq_comp, rms_amplitude_comp, peak_amplitude_comp, maxdom_comp, dfrange_comp)
          n_non_na <- sum(!is.na(comps))
          if (n_non_na > 0) sum(!comps, na.rm = TRUE) / n_non_na else NA_real_
        },
        
        # Determine pair source
        pair_source = case_when(
          is.na(dd01_source_value) | is.na(dd02_source_value) ~ NA_character_,
          dd01_source_value > dd02_source_value ~ "DD01",
          dd02_source_value > dd01_source_value ~ "DD02",
          TRUE ~ NA_character_
        )
      ) %>%
      ungroup()
    
    original_cols <- setdiff(names(dd_filtered), "row_id")
    
    # Helper function to create output rows
    create_output <- function(data, suffix, source_val_col) {
      cols_to_select <- paste0(original_cols, suffix)
      data %>%
        select(all_of(cols_to_select), pair_id, pair_source, all_of(source_val_col)) %>%
        rename_with(~str_remove(., suffix)) %>%
        rename(source = pair_source, source_value = all_of(source_val_col))
    }
    
    # Create resolved and unresolved outputs
    resolved <- paired_data %>% filter(!is.na(pair_source))
    unresolved <- paired_data %>% filter(is.na(pair_source))
    
    dd_rails_resolved <- if (nrow(resolved) > 0) {
      bind_rows(
        create_output(resolved, ".dd01", "dd01_source_value"),
        create_output(resolved, ".dd02", "dd02_source_value")
      ) %>% arrange(pair_id, site)
    } else tibble()
    
    dd_rails_UnidSource <- if (nrow(unresolved) > 0) {
      bind_rows(
        create_output(unresolved, ".dd01", "dd01_source_value"),
        create_output(unresolved, ".dd02", "dd02_source_value")
      ) %>% arrange(pair_id, site)
    } else tibble()
    
    # Create dd_match_results for paired calls
    dd_match_results_paired <- bind_rows(
      paired_data %>%
        transmute(
          ID = ID.dd01,
          site = site.dd01, 
          begin_path = begin_path.dd01, 
          file_offset_s = file_offset_s.dd01, 
          utc = utc.dd01, 
          pair_id,
          pair_meanfreq = meanfreq_comp,
          pair_rms_amplitude = rms_amplitude_comp,
          pair_peak_amplitude = peak_amplitude_comp,
          pair_maxdom = maxdom_comp,
          pair_dfrange = dfrange_comp,
          pair_source_value = dd01_source_value,
          pair_source
        ),
      paired_data %>%
        transmute(
          ID = ID.dd02,
          site = site.dd02, 
          begin_path = begin_path.dd02,
          file_offset_s = file_offset_s.dd02, 
          utc = utc.dd02, 
          pair_id,
          pair_meanfreq = !meanfreq_comp,
          pair_rms_amplitude = !rms_amplitude_comp,
          pair_peak_amplitude = !peak_amplitude_comp,
          pair_maxdom = !maxdom_comp,
          pair_dfrange = !dfrange_comp,
          pair_source_value = dd02_source_value,
          pair_source
        )
    )
    
  } else {
    dd_rails_resolved <- tibble()
    dd_rails_UnidSource <- tibble()
    dd_match_results_paired <- tibble()
  }
  
  # Create dd_match_results for unpaired calls
  dd_match_results <- bind_rows(
    dd_match_results_paired,
    dd_rails_NoMatch %>%
      select(ID, site, begin_path, file_offset_s, utc) %>%
      mutate(pair_id = NA_integer_, 
             pair_meanfreq = NA, pair_rms_amplitude = NA, 
             pair_peak_amplitude = NA, pair_maxdom = NA, pair_dfrange = NA,
             pair_source_value = NA_real_, pair_source = NA_character_)
  )
  
  # Assign to parent environment explicitly
  assign("dd_rails_NoMatch", dd_rails_NoMatch, envir = parent.frame())
  assign("dd_rails_UnidSource", dd_rails_UnidSource, envir = parent.frame())
  assign("dd_rails_resolved", dd_rails_resolved, envir = parent.frame())
  assign("dd_match_results", dd_match_results, envir = parent.frame())
  
  # Return summary
  cat("Processing complete!\n",
      "Resolved pairs:", nrow(dd_rails_resolved), "rows\n",
      "Unidentified source:", nrow(dd_rails_UnidSource), "rows\n",
      "No match:", nrow(dd_rails_NoMatch), "rows\n",
      "Match results:", nrow(dd_match_results), "rows\n")
}


#############################################################
# plot variable distribution for Source vs Non-Source sites #
#############################################################
library(tidyverse)

plot_source_distribution <- function(data, variables) {
  
  # Filter to only valid pair_source (remove NA)
  data_filtered <- data %>%
    filter(!is.na(pair_source))
  
  # Create Source vs Non-Source classification
  data_classified <- data_filtered %>%
    mutate(
      source_type = if_else(site.x == pair_source, "Source", "Non-Source")
    )
  
  # Prepare data for plotting (pivot longer if multiple variables)
  data_long <- data_classified %>%
    select(source_type, all_of(variables)) %>%
    pivot_longer(
      cols = all_of(variables),
      names_to = "variable",
      values_to = "value"
    )
  
  # Create the plot
  p <- ggplot(data_long, aes(x = value, color = source_type, fill = source_type)) +
    geom_density(alpha = 0.3, linewidth = 1) +
    scale_color_manual(
      values = c("Source" = "#0072B2", "Non-Source" = "#D55E00"),
      name = "Classification"
    ) +
    scale_fill_manual(
      values = c("Source" = "#0072B2", "Non-Source" = "#D55E00"),
      name = "Classification"
    ) +
    labs(
      y = "Density",
      x = "Value"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 11, face = "bold")
    )
  
  # Add faceting if multiple variables
  if (length(variables) > 1) {
    p <- p + facet_wrap(~ variable, scales = "free", ncol = 2)
  } else {
    p <- p + labs(x = variables)
  }
  
  # Print summary statistics
  cat("Distribution comparison summary:\n")
  summary_stats <- data_classified %>%
    group_by(source_type) %>%
    summarise(n = n(), .groups = "drop")
  print(summary_stats)
  
  return(p)
}

# Usage example:
# plot_source_distribution(dd_merged, c("meanfreq", "rms_amplitude"))
# plot_source_distribution(dd_merged, "meanfreq")

##########################################
# Match Detections to Paired Detections  #
##########################################

match_detections_to_pairs <- function(dd_merged, time_difference = 3, closest_match_only = TRUE) {
  
  # Get reference detections (DD01 and DD02 pairs) with their times
  reference_detections <- dd_merged %>%
    filter(pair_source %in% c("DD01", "DD02")) %>%
    select(pair_id, pair_source, utc) %>%
    distinct()
  
  # Get rows to check (DD03-DD10) and track existing pair_ids
  rows_to_check <- dd_merged %>%
    filter(!site %in% c("DD01", "DD02")) %>%
    mutate(had_existing_pair = !is.na(pair_id),
           original_pair_id = pair_id)
  
  # Cross join to find all potential matches within time window
  potential_matches <- rows_to_check %>%
    select(ID, site, utc, had_existing_pair, original_pair_id) %>%
    crossing(reference_detections %>% 
               select(pair_id, pair_source, ref_utc = utc)) %>%
    mutate(time_diff = abs(as.numeric(difftime(utc, ref_utc, units = "secs")))) %>%
    filter(time_diff <= time_difference)
  
  # FIRST: Count unique pair_ids matched per detection (before filtering to closest)
  match_counts <- potential_matches %>%
    group_by(ID) %>%
    summarise(
      n_unique_pairs = n_distinct(pair_id),
      matched_pair_ids = paste(unique(pair_id), collapse = ", "),
      had_existing_pair = first(had_existing_pair),
      .groups = "drop"
    )
  
  # Identify multi-matches for errors dataframe (always, regardless of closest_match_only)
  multi_matches <- match_counts %>%
    filter(n_unique_pairs > 1)
  
  # Create errors dataframe
  dd_merged_errors <- multi_matches %>%
    select(ID, matched_pair_ids, n_matches = n_unique_pairs)
  
  # SECOND: Apply closest_match_only filter if requested
  matches_for_assignment <- potential_matches
  if (closest_match_only) {
    matches_for_assignment <- potential_matches %>%
      group_by(ID) %>%
      slice_min(time_diff, n = 1, with_ties = FALSE) %>%
      ungroup()
  } else {
    # If not using closest_match_only, exclude multi-matches from assignment
    matches_for_assignment <- potential_matches %>%
      filter(!ID %in% multi_matches$ID)
  }
  
  # Get single matches for assignment
  single_matches <- matches_for_assignment %>%
    group_by(ID) %>%
    summarise(
      pair_id = first(pair_id),
      pair_source = first(pair_source),
      had_existing_pair = first(had_existing_pair),
      .groups = "drop"
    )
  
  # Update dd_merged with single matches to create dd_merged_updated
  dd_merged_updated <- dd_merged %>%
    left_join(
      single_matches %>% 
        select(ID, new_pair_id = pair_id, new_pair_source = pair_source),
      by = "ID"
    ) %>%
    mutate(
      pair_id = if_else(!is.na(new_pair_id), new_pair_id, pair_id),
      pair_source = if_else(!is.na(new_pair_source), new_pair_source, pair_source)
    ) %>%
    select(-new_pair_id, -new_pair_source)
  
  # Calculate summary statistics
  n_multi_match <- nrow(multi_matches)
  n_matched <- nrow(single_matches)
  n_replaced <- sum(single_matches$had_existing_pair, na.rm = TRUE)
  n_no_match <- nrow(rows_to_check) - n_matched - n_multi_match
  
  # Multi-site check: find pair_ids with duplicate sites in dd_merged_updated
  multi_site_check <- dd_merged_updated %>%
    filter(!is.na(pair_id)) %>%
    group_by(pair_id, site) %>%
    summarise(n_detections = n(), .groups = "drop") %>%
    filter(n_detections > 1)
  
  n_multi_site <- nrow(multi_site_check)
  
  # Print summary
  cat("=== Detection Matching Summary ===\n")
  cat("Time difference threshold:", time_difference, "seconds\n")
  cat("Closest match only:", closest_match_only, "\n\n")
  cat("Rows that matched more than one unique pair_id:", n_multi_match, "\n")
  cat("Rows with no matches:", n_no_match, "\n")
  cat("Rows that matched exactly one pair_id:", n_matched, "\n")
  cat("Rows with existing pair_id that were replaced:", n_replaced, "\n")
  cat("Multi-site issues (same site multiple times in one pair_id):", n_multi_site, "\n")
  
  if (n_multi_match > 0) {
    cat("\nNote: When closest_match_only=TRUE, multi-match detections are flagged in errors\n")
    cat("      but assigned to their closest pair_id in the updated data.\n")
  }
  
  if (n_multi_site > 0) {
    cat("\nMulti-site details (top 20):\n")
    print(multi_site_check %>% head(20))
  }
  
  # Return results
  return(list(
    dd_merged = dd_merged_updated,
    dd_merged_errors = dd_merged_errors,
    summary = list(
      n_multi_match = n_multi_match,
      n_no_match = n_no_match,
      n_matched = n_matched,
      n_replaced = n_replaced,
      n_multi_site = n_multi_site
    ),
    multi_site_details = multi_site_check
  ))
}

# Example usage:
# results <- match_detections_to_pairs(dd_merged, time_difference = 3, closest_match_only = TRUE)
# dd_merged_updated <- results$dd_merged
# dd_merged_errors <- results$dd_merged_errors
# summary_stats <- results$summary



###########################################
# Process Birdnet Data (create Rds File)  #
###########################################
process_birdnet_data <- function(dataset, 
                                 projectID,
                                 config_type,
                                 raw_files_path = here("bigData/rails/rawBirdnetFiles"),
                                 output_path = here("data"),
                                 local_tz = "America/Los_Angeles") {
  
  # Load lat/long data (always from same location)
  latlong <- readRDS(here("data/deployments.rds")) %>%
    clean_names() %>%
    select(deployment_id, latitude, longitude)
  
  # Get list of files containing both the dataset and config_type strings
  all_files <- list.files(raw_files_path, full.names = TRUE)
  filtered_files <- all_files[grepl(dataset, basename(all_files)) & 
                                grepl(config_type, basename(all_files))]
  
  # Check if any files were found
  if (length(filtered_files) == 0) {
    stop(paste0("No files found containing both '", dataset, 
                "' and '", config_type, "' in ", raw_files_path))
  }
  
  # Create temporary directory and copy/link filtered files
  temp_dir <- file.path(tempdir(), paste0("birdnet_", dataset, "_", Sys.time() %>% as.numeric()))
  dir.create(temp_dir, recursive = TRUE)
  
  # Copy filtered files to temp directory
  file.copy(filtered_files, temp_dir)
  
  # Combine birdNET files from temp directory
  data <- birdnet_combine(temp_dir)
  
  # Clean up temp directory
  unlink(temp_dir, recursive = TRUE)
  
  # Modify birdNET data as tibble 
  birdnet_all <- birdnet_add_datetime(data, tz = "UTC") %>% 
    clean_names() %>%
    remove_empty(which = c("rows", "cols")) %>%
    rename(datetime_UTC = datetime) %>% 
    mutate(datetime_local = with_tz(datetime_UTC, local_tz)) %>%
    separate(col = deployment_id, 
             into = c("location-site", "config", "deploy_id_date"), 
             sep = "_", 
             remove = FALSE) %>%
    separate("location-site", 
             into = c("location", "site"), 
             sep = "-") %>%
    left_join(latlong, by = "deployment_id") %>%
    select(-date, -year, -month, -mday, -yday, -hour, -minute, -deploy_id_date) 
  
  # Add environmental data
  birdnet_env <- birdnet_all %>%
    rename(UTC = datetime_UTC, Latitude = latitude, Longitude = longitude)

  birdnet_all_env <- matchGFS(birdnet_env)

  birdnet_all <- birdnet_all_env %>%
    clean_names() %>%
    remove_empty(which = c("rows", "cols"))
  
  # Create filename and save
  birdnetFileName <- paste0("birdnet_", projectID, "_", dataset, ".rds")
  saveRDS(birdnet_all, file = file.path(output_path, birdnetFileName))
  
  # Return the data (optional, but useful)
  return(birdnet_all)
}

# result <- process_birdnet_data(
#   dataset = "site1", 
#   projectID = "MyProject",
#   config_type = "48c",
#   raw_files_path = here("data/rails/rawBirdnetFiles"),
#   output_path = here("data"),
#   local_tz = "America/Los_Angeles")
# )

#################################################
# Convert Birnet File to Raven Selection Table  #
#################################################
convert_birdnet_to_raven <- function(
    infile,
    outfile = NULL,
    low_freq = 2200,
    high_freq = 4600
) {
  
  # Read BirdNET table (tab-delimited)
  df <- read.delim(infile, stringsAsFactors = FALSE, check.names = FALSE)
  
  # Rename time columns if present
  if ("Begin.Time..s." %in% names(df)) {
    names(df)[names(df) == "Begin.Time..s."] <- "Begin Time (s)"
  }
  if ("End.Time..s." %in% names(df)) {
    names(df)[names(df) == "End.Time..s."] <- "End Time (s)"
  }
  
  # Sanity check
  required_time_cols <- c("Begin Time (s)", "End Time (s)")
  if (!all(required_time_cols %in% names(df))) {
    stop("Required time columns not found after renaming.")
  }
  
  n <- nrow(df)
  
  # Add required Raven columns
  df$Selection <- seq_len(n)
  df$View <- 1
  df$Channel <- 1
  df$`Low Freq (Hz)` <- low_freq
  df$`High Freq (Hz)` <- high_freq
  # df$`Low Freq (Hz)` <- 1
  # df$`High Freq (Hz)` <- 1000
  

  # Reorder columns to a conventional Raven layout
  raven_order <- c(
    "Selection",
    "View",
    "Channel",
    "Begin Time (s)",
    "End Time (s)",
    "Low Freq (Hz)",
    "High Freq (Hz)"
  )
  
  remaining_cols <- setdiff(names(df), raven_order)
  df <- df[, c(raven_order, remaining_cols)]
  
  # Default output filename: append "_raven" before extension
  if (is.null(outfile)) {
    outfile <- sub(
      "(\\.[^.]+)$",
      "_raven\\1",
      infile
    )
  }
  
  # Write tab-delimited file
  write.table(
    df,
    file = outfile,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE
  )
  
  message("Wrote Raven selection table to:\n", outfile)
}


##########################################
# xxxx  #
##########################################
