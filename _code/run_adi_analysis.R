#' Batch Acoustic Diversity Index (ADI) Analysis with Visualization
#'
#' This function processes multiple WAV files to calculate the Acoustic Diversity Index (ADI),
#' integrates with BirdNET selection tables, generates summary statistics, and creates
#' customizable visualizations. Designed for large-scale bioacoustic monitoring datasets.
#'
#' @param audio_path Character. Path to the top-level folder containing site folders with deployments
#' @param output_path Character. Path where all outputs will be saved
#' @param timezone Character. Timezone for datetime conversion (default: "America/Los_Angeles")
#' @param max_freq Numeric. Maximum frequency in Hz to analyze (default: 10000, recommended for bird studies)
#' @param db_threshold Numeric. Threshold in dB to consider a band "occupied" (default: -50)
#' @param freq_step Numeric. Size of frequency bands in Hz (default: 1000)
#' @param n_cores Numeric. Number of cores for parallel processing (default: NULL, uses n-1)
#' @param plot_types Character vector. Types of plots to generate. Options:
#'   - "daily_pattern": Time series showing ADI by hour of day
#'   - "temporal_trend": Time series showing ADI over dates
#'   - "heatmap_daily": Heatmap of hour x date
#'   - "heatmap_seasonal": Heatmap of hour x month
#'   - "boxplot_site": Boxplot comparing sites
#'   - "boxplot_period": Boxplot comparing time periods (dawn/day/dusk/night)
#'   - "ridge_month": Ridge plot showing ADI distribution by month
#'   - "species_correlation": Scatterplot of ADI vs species richness (requires BirdNET)
#'   - "distribution": Histogram of ADI values
#'   - "all": Generate all available plots
#' @param figure_width Numeric. Width of output figures in inches (default: 12)
#' @param figure_height Numeric. Height of output figures in inches (default: 8)
#' @param dpi Numeric. Resolution of output figures (default: 300)
#' @param integrate_birdnet Logical. Whether to integrate BirdNET selection tables (default: TRUE)
#'
#' @return List containing:
#'   - adi_results: Data frame with raw ADI results
#'   - summary_stats: List of summary data frames
#'   - log_file: Path to error/warning log file
#'   - output_files: List of all generated file paths
#'
#' @details
#' The Acoustic Diversity Index (ADI) was developed by Villanueva-Rivera et al. (2011) and 
#' is based on the Shannon diversity index. It measures the evenness of sound energy 
#' distribution across frequency bands. ADI values range from 0 to 1:
#' - Higher ADI (approaching 1) = more even distribution across frequencies = higher acoustic diversity
#' - Lower ADI (approaching 0) = sound concentrated in fewer frequency bands = lower acoustic diversity
#'
#' Best practices for bird studies (Villanueva-Rivera et al. 2011, Pieretti et al. 2011):
#' - max_freq: 10000 Hz (captures most bird vocalizations while excluding ultrasonic noise)
#' - db_threshold: -50 dB (standard threshold for considering a band "occupied")
#' - freq_step: 1000 Hz (1 kHz bands provide good resolution for bird diversity)
#'
#' The function expects a specific folder structure:
#' - Top Folder: 'Location-Site'
#' - Deployment Folder: 'Location-Site_config_YYYYMMDD'
#' - WAV files: 'Location-Site_config_YYYYMMDD_HHMMSS.WAV'
#' - BirdNET folder: Contains 'birdnetLocal' in name
#' - BirdNET files: 'Location-Site_config_YYYYMMDD_HHMMSS.BirdNET.selection.table.txt'
#'
#' Output files created in output_path:
#' - adi_raw_results.csv: Raw ADI values for each file
#' - adi_summary_by_hour.csv: Mean ADI by hour of day
#' - adi_summary_by_date.csv: Mean ADI by date
#' - adi_summary_by_site.csv: Mean ADI by site
#' - adi_summary_by_deployment.csv: Mean ADI by deployment
#' - adi_analysis_log.txt: Error and warning log
#' - [plot_type].png: Individual visualization files
#'
#' @references
#' Villanueva-Rivera, L. J., et al. (2011). A primer of acoustic analysis for landscape ecologists.
#'   Landscape Ecology, 26(9), 1233-1246.
#'
#' Pieretti, N., et al. (2011). A new methodology to infer the singing activity of an avian community:
#'   The Acoustic Complexity Index (ACI). Ecological Indicators, 11(3), 868-873.
#'
#' @examples
#' \dontrun{
#' # Basic usage with default parameters
#' results <- run_adi_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs"
#' )
#'
#' # Custom parameters with specific visualizations
#' results <- run_adi_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   timezone = "America/New_York",
#'   max_freq = 12000,
#'   db_threshold = -45,
#'   freq_step = 500,
#'   plot_types = c("daily_pattern", "heatmap_daily", "species_correlation"),
#'   n_cores = 4
#' )
#'
#' # Generate all available plots
#' results <- run_adi_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   plot_types = "all"
#' )
#' }
#'
#' @export

run_adi_analysis <- function(
    audio_path,
    output_path,
    timezone = "America/Los_Angeles",
    max_freq = 10000,
    db_threshold = -50,
    freq_step = 1000,
    n_cores = NULL,
    plot_types = c("daily_pattern", "temporal_trend", "heatmap_daily"),
    figure_width = 12,
    figure_height = 8,
    dpi = 300,
    integrate_birdnet = TRUE
) {
  
  # Load required packages
  required_packages <- c("tidyverse", "tuneR", "soundecology", "lubridate", 
                         "furrr", "future", "ggridges", "patchwork", "viridis")
  
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      stop(paste("Package", pkg, "is required but not installed. Install with: install.packages('", pkg, "')", sep = ""))
    }
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
    message("Created output directory: ", output_path)
  }
  
  # Initialize log file
  log_file <- file.path(output_path, "adi_analysis_log.txt")
  log_con <- file(log_file, open = "wt")
  writeLines(paste("Acoustic Diversity Index Analysis Log - Started:", Sys.time()), log_con)
  writeLines(paste("Audio path:", audio_path), log_con)
  writeLines(paste("Output path:", output_path), log_con)
  writeLines(paste("Timezone:", timezone), log_con)
  writeLines(paste("ADI parameters: max_freq =", max_freq, "Hz, db_threshold =", db_threshold, 
                   "dB, freq_step =", freq_step, "Hz"), log_con)
  writeLines("", log_con)
  
  # Validate inputs
  if (!dir.exists(audio_path)) {
    close(log_con)
    stop("audio_path does not exist: ", audio_path)
  }
  
  if (max_freq <= 0) {
    close(log_con)
    stop("max_freq must be positive")
  }
  
  if (freq_step <= 0 || freq_step > max_freq) {
    close(log_con)
    stop("freq_step must be positive and less than max_freq")
  }
  
  # Expand "all" plot types
  all_plot_types <- c("daily_pattern", "temporal_trend", "heatmap_daily", 
                      "heatmap_seasonal", "boxplot_site", "boxplot_period",
                      "ridge_month", "species_correlation", "distribution")
  
  if ("all" %in% plot_types) {
    plot_types <- all_plot_types
  }
  
  # Validate plot types
  invalid_types <- setdiff(plot_types, all_plot_types)
  if (length(invalid_types) > 0) {
    warning("Invalid plot types will be ignored: ", paste(invalid_types, collapse = ", "))
    plot_types <- intersect(plot_types, all_plot_types)
  }
  
  # Find all WAV files
  message("Scanning for WAV files...")
  audio_files <- list.files(
    path = audio_path,
    pattern = "\\.WAV$|\\.wav$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  if (length(audio_files) == 0) {
    close(log_con)
    stop("No WAV files found in ", audio_path)
  }
  
  message(paste("Found", length(audio_files), "WAV files"))
  writeLines(paste("Found", length(audio_files), "WAV files"), log_con)
  
  # Set up parallel processing
  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 1)
  }
  message(paste("Using", n_cores, "cores for parallel processing"))
  writeLines(paste("Using", n_cores, "cores"), log_con)
  
  plan(multisession, workers = n_cores)
  
  # Helper function to calculate ADI for a single file
  calculate_adi_single <- function(file_path, max_freq, db_threshold, freq_step, log_con) {
    tryCatch({
      # Read audio
      audio <- readWave(file_path)
      
      # Calculate Acoustic Diversity Index
      # Note: soundecology's acoustic_diversity returns a list with:
      # - adi_left (or left_channel): the ADI value (0-1)
      # - Possibly additional metrics
      adi_result <- acoustic_diversity(
        soundfile = audio,
        max_freq = max_freq,
        db_threshold = db_threshold,
        freq_step = freq_step
      )
      
      # Extract metadata from filepath
      file_name <- basename(file_path)
      dir_structure <- dirname(file_path)
      
      # Parse site, deployment, datetime
      # Expected format: Location-Site_config_YYYYMMDD_HHMMSS.WAV
      parsed <- str_match(file_name, "^(.+?)_(\\d{8})_(\\d{6})\\.WAV$")
      
      if (is.na(parsed[1])) {
        warning(paste("Could not parse filename:", file_name))
        site_deployment <- "Unknown"
        datetime_utc <- NA
      } else {
        site_deployment <- parsed[2]
        date_str <- parsed[3]
        time_str <- parsed[4]
        
        # Construct datetime (assuming UTC in filename)
        datetime_str <- paste0(date_str, " ", time_str)
        datetime_utc <- ymd_hms(datetime_str, tz = "UTC")
      }
      
      # Extract site and deployment info from folder structure
      path_parts <- str_split(dir_structure, .Platform$file.sep)[[1]]
      
      # Find deployment folder (contains date)
      deployment_folder <- path_parts[str_detect(path_parts, "\\d{8}")]
      if (length(deployment_folder) > 0) {
        deployment <- deployment_folder[length(deployment_folder)]
      } else {
        deployment <- "Unknown"
      }
      
      # Find site folder (parent of deployment)
      site_folder_idx <- which(str_detect(path_parts, "\\d{8}"))
      if (length(site_folder_idx) > 0 && site_folder_idx[1] > 1) {
        site <- path_parts[site_folder_idx[1] - 1]
      } else {
        site <- "Unknown"
      }
      
      # Extract ADI value (handle different possible output names)
      if (!is.null(adi_result$adi_left)) {
        adi_value <- adi_result$adi_left
      } else if (!is.null(adi_result$left_channel)) {
        adi_value <- adi_result$left_channel
      } else if (!is.null(adi_result$adi)) {
        adi_value <- adi_result$adi
      } else {
        # If none of the above, try to get the first numeric element
        adi_value <- as.numeric(adi_result[[1]])
      }
      
      # Return results with all available metrics
      result_df <- tibble(
        file = file_name,
        file_path = file_path,
        site = site,
        deployment = deployment,
        datetime_utc = datetime_utc,
        adi = adi_value
      )
      
      # Add any additional metrics from the ADI result
      # Common additional outputs might include shannon index components
      other_metrics <- names(adi_result)[!names(adi_result) %in% 
                                           c("adi_left", "left_channel", "adi", 
                                             "adi_right", "right_channel")]
      for (metric in other_metrics) {
        if (is.numeric(adi_result[[metric]]) && length(adi_result[[metric]]) == 1) {
          result_df[[metric]] <- adi_result[[metric]]
        }
      }
      
      return(result_df)
      
    }, error = function(e) {
      warning(paste("Error processing", basename(file_path), ":", e$message))
      return(tibble(
        file = basename(file_path),
        file_path = file_path,
        site = NA_character_,
        deployment = NA_character_,
        datetime_utc = as.POSIXct(NA),
        adi = NA_real_
      ))
    })
  }
  
  # Process all files in parallel with progress
  message("\nCalculating Acoustic Diversity Index for all files...")
  message("This may take a while for large datasets...")
  
  adi_results <- future_map_dfr(
    audio_files,
    ~calculate_adi_single(.x, max_freq, db_threshold, freq_step, log_con),
    .progress = TRUE,
    .options = furrr_options(seed = TRUE)
  )
  
  # Convert to local timezone
  message("\nConverting timestamps to local timezone...")
  adi_results <- adi_results %>%
    mutate(
      datetime_local = with_tz(datetime_utc, tzone = timezone),
      date = as_date(datetime_local),
      hour = hour(datetime_local),
      month = month(datetime_local, label = TRUE),
      year = year(datetime_local),
      time_period = case_when(
        hour >= 5 & hour < 9 ~ "Dawn",
        hour >= 9 & hour < 17 ~ "Day",
        hour >= 17 & hour < 21 ~ "Dusk",
        TRUE ~ "Night"
      ),
      time_period = factor(time_period, levels = c("Dawn", "Day", "Dusk", "Night"))
    )
  
  # Log any errors
  n_errors <- sum(is.na(adi_results$adi))
  if (n_errors > 0) {
    writeLines(paste("\nErrors encountered:", n_errors, "files failed to process"), log_con)
    writeLines("Failed files:", log_con)
    writeLines(adi_results$file[is.na(adi_results$adi)], log_con)
  }
  
  # Save raw results
  message("\nSaving raw ADI results...")
  raw_results_file <- file.path(output_path, "adi_raw_results.csv")
  write_csv(adi_results, raw_results_file)
  message("Saved: ", raw_results_file)
  
  # Generate summary statistics
  message("\nGenerating summary statistics...")
  
  # Summary by hour
  summary_hour <- adi_results %>%
    group_by(hour) %>%
    summarise(
      n_files = n(),
      mean_adi = mean(adi, na.rm = TRUE),
      sd_adi = sd(adi, na.rm = TRUE),
      median_adi = median(adi, na.rm = TRUE),
      min_adi = min(adi, na.rm = TRUE),
      max_adi = max(adi, na.rm = TRUE),
      .groups = "drop"
    )
  write_csv(summary_hour, file.path(output_path, "adi_summary_by_hour.csv"))
  
  # Summary by date
  summary_date <- adi_results %>%
    group_by(date) %>%
    summarise(
      n_files = n(),
      mean_adi = mean(adi, na.rm = TRUE),
      sd_adi = sd(adi, na.rm = TRUE),
      median_adi = median(adi, na.rm = TRUE),
      min_adi = min(adi, na.rm = TRUE),
      max_adi = max(adi, na.rm = TRUE),
      .groups = "drop"
    )
  write_csv(summary_date, file.path(output_path, "adi_summary_by_date.csv"))
  
  # Summary by site
  summary_site <- adi_results %>%
    group_by(site) %>%
    summarise(
      n_files = n(),
      mean_adi = mean(adi, na.rm = TRUE),
      sd_adi = sd(adi, na.rm = TRUE),
      median_adi = median(adi, na.rm = TRUE),
      min_adi = min(adi, na.rm = TRUE),
      max_adi = max(adi, na.rm = TRUE),
      .groups = "drop"
    )
  write_csv(summary_site, file.path(output_path, "adi_summary_by_site.csv"))
  
  # Summary by deployment
  summary_deployment <- adi_results %>%
    group_by(deployment) %>%
    summarise(
      n_files = n(),
      mean_adi = mean(adi, na.rm = TRUE),
      sd_adi = sd(adi, na.rm = TRUE),
      median_adi = median(adi, na.rm = TRUE),
      min_adi = min(adi, na.rm = TRUE),
      max_adi = max(adi, na.rm = TRUE),
      .groups = "drop"
    )
  write_csv(summary_deployment, file.path(output_path, "adi_summary_by_deployment.csv"))
  
  message("Saved summary statistics")
  
  # Integrate BirdNET data if requested
  birdnet_integrated <- FALSE
  if (integrate_birdnet) {
    message("\nIntegrating BirdNET selection tables...")
    
    tryCatch({
      # Find all BirdNET selection tables
      birdnet_files <- list.files(
        path = audio_path,
        pattern = "\\.BirdNET\\.selection\\.table\\.txt$",
        full.names = TRUE,
        recursive = TRUE
      )
      
      if (length(birdnet_files) > 0) {
        message(paste("Found", length(birdnet_files), "BirdNET selection tables"))
        
        # Read and combine all BirdNET files
        birdnet_data <- map_dfr(birdnet_files, ~{
          tryCatch({
            df <- read_tsv(.x, show_col_types = FALSE)
            # Extract corresponding WAV filename
            base_name <- str_replace(basename(.x), "\\.BirdNET\\.selection\\.table\\.txt$", ".WAV")
            df$file <- base_name
            return(df)
          }, error = function(e) {
            warning(paste("Could not read BirdNET file:", basename(.x)))
            return(NULL)
          })
        })
        
        if (nrow(birdnet_data) > 0) {
          # Summarize by file
          birdnet_summary <- birdnet_data %>%
            group_by(file) %>%
            summarise(
              n_species = n_distinct(`Common Name`),
              n_detections = n(),
              .groups = "drop"
            )
          
          # Join with ADI results
          adi_results <- adi_results %>%
            left_join(birdnet_summary, by = "file")
          
          birdnet_integrated <- TRUE
          message("BirdNET data integrated successfully")
          writeLines("BirdNET data integrated successfully", log_con)
          
          # Save updated results with BirdNET
          write_csv(adi_results, raw_results_file)
        } else {
          message("No valid BirdNET data found")
        }
      } else {
        message("No BirdNET selection tables found")
      }
    }, error = function(e) {
      warning("Error integrating BirdNET data: ", e$message)
      writeLines(paste("Error integrating BirdNET data:", e$message), log_con)
    })
  }
  
  # Generate visualizations
  message("\nGenerating visualizations...")
  output_files <- list()
  
  # Helper function to save plot
  save_plot_safe <- function(plot_obj, filename, width, height, dpi) {
    tryCatch({
      filepath <- file.path(output_path, filename)
      ggsave(filepath, plot = plot_obj, width = width, height = height, dpi = dpi, bg = "white")
      message(paste("Saved:", filename))
      return(filepath)
    }, error = function(e) {
      warning(paste("Could not save plot", filename, ":", e$message))
      writeLines(paste("Error saving plot", filename, ":", e$message), log_con)
      return(NULL)
    })
  }
  
  # 1. Daily Pattern
  if ("daily_pattern" %in% plot_types) {
    p <- adi_results %>%
      ggplot(aes(x = hour, y = adi)) +
      geom_point(alpha = 0.3, color = "steelblue", size = 1.5) +
      geom_smooth(method = "loess", color = "darkred", size = 1.2, se = TRUE) +
      scale_x_continuous(breaks = seq(0, 23, 2)) +
      labs(
        title = "Acoustic Diversity Index by Hour of Day",
        x = "Hour of Day",
        y = "ADI Value (0-1)",
        caption = paste("Higher values indicate more even sound distribution | n =", 
                        sum(!is.na(adi_results$adi)), "files")
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        panel.grid.minor = element_blank()
      )
    
    output_files$daily_pattern <- save_plot_safe(p, "adi_daily_pattern.png", 
                                                 figure_width, figure_height, dpi)
  }
  
  # 2. Temporal Trend
  if ("temporal_trend" %in% plot_types) {
    p <- adi_results %>%
      ggplot(aes(x = datetime_local, y = adi)) +
      geom_line(alpha = 0.4, color = "forestgreen") +
      geom_smooth(method = "loess", color = "darkred", size = 1.2) +
      labs(
        title = "Acoustic Diversity Index Over Time",
        x = "Date",
        y = "ADI Value (0-1)",
        caption = paste("Timezone:", timezone)
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        panel.grid.minor = element_blank()
      )
    
    output_files$temporal_trend <- save_plot_safe(p, "adi_temporal_trend.png", 
                                                  figure_width, figure_height, dpi)
  }
  
  # 3. Heatmap - Daily (Hour x Date)
  if ("heatmap_daily" %in% plot_types) {
    adi_heatmap_data <- adi_results %>%
      group_by(date, hour) %>%
      summarise(mean_adi = mean(adi, na.rm = TRUE), .groups = "drop")
    
    p <- adi_heatmap_data %>%
      ggplot(aes(x = hour, y = date, fill = mean_adi)) +
      geom_tile() +
      scale_fill_viridis_c(option = "magma", name = "Mean ADI") +
      scale_x_continuous(breaks = seq(0, 23, 2)) +
      labs(
        title = "Acoustic Diversity Index Heatmap",
        subtitle = "Hour of Day × Date",
        x = "Hour of Day",
        y = "Date"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "right"
      )
    
    output_files$heatmap_daily <- save_plot_safe(p, "adi_heatmap_daily.png", 
                                                 figure_width, figure_height, dpi)
  }
  
  # 4. Heatmap - Seasonal (Hour x Month)
  if ("heatmap_seasonal" %in% plot_types) {
    adi_seasonal_data <- adi_results %>%
      group_by(month, hour) %>%
      summarise(mean_adi = mean(adi, na.rm = TRUE), .groups = "drop")
    
    p <- adi_seasonal_data %>%
      ggplot(aes(x = hour, y = month, fill = mean_adi)) +
      geom_tile() +
      scale_fill_viridis_c(option = "magma", name = "Mean ADI") +
      scale_x_continuous(breaks = seq(0, 23, 2)) +
      labs(
        title = "Seasonal Acoustic Diversity Patterns",
        subtitle = "Hour of Day × Month",
        x = "Hour of Day",
        y = "Month"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "right"
      )
    
    output_files$heatmap_seasonal <- save_plot_safe(p, "adi_heatmap_seasonal.png", 
                                                    figure_width, figure_height, dpi)
  }
  
  # 5. Boxplot - By Site
  if ("boxplot_site" %in% plot_types) {
    n_sites <- n_distinct(adi_results$site)
    
    if (n_sites > 1) {
      p <- adi_results %>%
        filter(!is.na(site)) %>%
        ggplot(aes(x = reorder(site, adi, FUN = median, na.rm = TRUE), 
                   y = adi, fill = site)) +
        geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
        scale_fill_viridis_d(option = "turbo") +
        labs(
          title = "Acoustic Diversity Index by Recording Site",
          x = "Site",
          y = "ADI Value (0-1)"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 16),
          legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      output_files$boxplot_site <- save_plot_safe(p, "adi_boxplot_site.png", 
                                                  figure_width, figure_height, dpi)
    } else {
      message("Skipping boxplot_site: Only one site detected")
    }
  }
  
  # 6. Boxplot - By Time Period
  if ("boxplot_period" %in% plot_types) {
    p <- adi_results %>%
      ggplot(aes(x = time_period, y = adi, fill = time_period)) +
      geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
      scale_fill_manual(values = c("Dawn" = "#FF9999", "Day" = "#FFD700", 
                                   "Dusk" = "#9999FF", "Night" = "#333333")) +
      labs(
        title = "Acoustic Diversity Index by Time Period",
        x = "Time Period",
        y = "ADI Value (0-1)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "none"
      )
    
    output_files$boxplot_period <- save_plot_safe(p, "adi_boxplot_period.png", 
                                                  figure_width, figure_height, dpi)
  }
  
  # 7. Ridge Plot - By Month
  if ("ridge_month" %in% plot_types) {
    if (requireNamespace("ggridges", quietly = TRUE)) {
      p <- adi_results %>%
        filter(!is.na(month)) %>%
        ggplot(aes(x = adi, y = month, fill = stat(x))) +
        ggridges::geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
        scale_fill_viridis_c(option = "plasma", name = "ADI") +
        labs(
          title = "Distribution of ADI Values by Month",
          x = "ADI Value (0-1)",
          y = "Month"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 16)
        )
      
      output_files$ridge_month <- save_plot_safe(p, "adi_ridge_month.png", 
                                                 figure_width, figure_height, dpi)
    } else {
      warning("Package 'ggridges' not available. Skipping ridge plot.")
    }
  }
  
  # 8. Species Correlation
  if ("species_correlation" %in% plot_types && birdnet_integrated) {
    corr_data <- adi_results %>%
      filter(!is.na(adi), !is.na(n_species))
    
    if (nrow(corr_data) > 0) {
      corr_value <- cor(corr_data$adi, corr_data$n_species, use = "complete.obs")
      
      p <- corr_data %>%
        ggplot(aes(x = adi, y = n_species)) +
        geom_point(alpha = 0.5, color = "steelblue", size = 2) +
        geom_smooth(method = "lm", color = "darkred", size = 1.2) +
        labs(
          title = "Relationship Between ADI and Species Richness",
          x = "Acoustic Diversity Index (ADI)",
          y = "Number of Species Detected (BirdNET)",
          caption = paste("Pearson correlation:", round(corr_value, 3), 
                          "| n =", nrow(corr_data), "recordings")
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 16)
        )
      
      output_files$species_correlation <- save_plot_safe(p, "adi_species_correlation.png", 
                                                         figure_width, figure_height, dpi)
    } else {
      message("Skipping species_correlation: No overlapping data")
    }
  } else if ("species_correlation" %in% plot_types && !birdnet_integrated) {
    message("Skipping species_correlation: BirdNET data not integrated")
  }
  
  # 9. Distribution
  if ("distribution" %in% plot_types) {
    p <- adi_results %>%
      ggplot(aes(x = adi)) +
      geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7, color = "white") +
      geom_vline(aes(xintercept = mean(adi, na.rm = TRUE)), 
                 color = "darkred", linetype = "dashed", size = 1.2) +
      geom_vline(aes(xintercept = median(adi, na.rm = TRUE)), 
                 color = "darkorange", linetype = "dashed", size = 1.2) +
      annotate("text", x = mean(adi_results$adi, na.rm = TRUE), 
               y = Inf, label = "Mean", vjust = 1.5, color = "darkred", fontface = "bold") +
      annotate("text", x = median(adi_results$adi, na.rm = TRUE), 
               y = Inf, label = "Median", vjust = 3, color = "darkorange", fontface = "bold") +
      labs(
        title = "Distribution of ADI Values",
        x = "ADI Value (0-1)",
        y = "Count",
        caption = paste("Mean:", round(mean(adi_results$adi, na.rm = TRUE), 3),
                        "| Median:", round(median(adi_results$adi, na.rm = TRUE), 3))
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16)
      )
    
    output_files$distribution <- save_plot_safe(p, "adi_distribution.png", 
                                                figure_width, figure_height, dpi)
  }
  
  # Close log file
  writeLines(paste("\nAnalysis completed:", Sys.time()), log_con)
  close(log_con)
  
  # Reset future plan
  plan(sequential)
  
  # Summary message
  message("\n========================================")
  message("Acoustic Diversity Index Analysis Complete!")
  message("========================================")
  message(paste("Processed:", length(audio_files), "files"))
  message(paste("Successful:", sum(!is.na(adi_results$adi)), "files"))
  message(paste("Failed:", n_errors, "files"))
  message(paste("\nOutputs saved to:", output_path))
  message("\nGenerated files:")
  message("  - adi_raw_results.csv")
  message("  - adi_summary_by_hour.csv")
  message("  - adi_summary_by_date.csv")
  message("  - adi_summary_by_site.csv")
  message("  - adi_summary_by_deployment.csv")
  message("  - adi_analysis_log.txt")
  message(paste("  -", length(output_files), "visualization(s)"))
  message("========================================\n")
  
  # Return results
  return(invisible(list(
    adi_results = adi_results,
    summary_stats = list(
      by_hour = summary_hour,
      by_date = summary_date,
      by_site = summary_site,
      by_deployment = summary_deployment
    ),
    log_file = log_file,
    output_files = output_files,
    n_files_processed = length(audio_files),
    n_files_successful = sum(!is.na(adi_results$adi)),
    n_files_failed = n_errors,
    birdnet_integrated = birdnet_integrated
  )))
}


#' ============================================================================
#' USAGE GUIDE
#' ============================================================================
#'
#' BASIC USAGE:
#' ------------
#' results <- run_adi_analysis(
#'   audio_path = "/path/to/your/recordings",
#'   output_path = "/path/to/save/outputs"
#' )
#'
#' CUSTOMIZING ADI PARAMETERS:
#' ---------------------------
#' results <- run_adi_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   max_freq = 12000,        # Maximum frequency in Hz (default: 10000)
#'   db_threshold = -45,      # dB threshold (default: -50)
#'   freq_step = 500          # Frequency band size in Hz (default: 1000)
#' )
#'
#' SELECTING SPECIFIC VISUALIZATIONS:
#' -----------------------------------
#' Available plot types:
#'   - "daily_pattern"        : ADI by hour of day with smoothed trend
#'   - "temporal_trend"       : ADI over time (dates)
#'   - "heatmap_daily"        : Hour × Date heatmap
#'   - "heatmap_seasonal"     : Hour × Month heatmap
#'   - "boxplot_site"         : Compare ADI across sites
#'   - "boxplot_period"       : Compare ADI across time periods (Dawn/Day/Dusk/Night)
#'   - "ridge_month"          : Distribution of ADI by month (ridge plot)
#'   - "species_correlation"  : ADI vs Species Richness (requires BirdNET)
#'   - "distribution"         : Histogram of ADI values
#'   - "all"                  : Generate all available plots
#'
#' Example - Select specific plots:
#' results <- run_adi_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   plot_types = c("daily_pattern", "heatmap_daily", "species_correlation")
#' )
#'
#' Example - Generate all plots:
#' results <- run_adi_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   plot_types = "all"
#' )
#'
#' TIMEZONE CUSTOMIZATION:
#' -----------------------
#' results <- run_adi_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   timezone = "America/New_York"    # Default: "America/Los_Angeles"
#' )
#'
#' PARALLEL PROCESSING:
#' --------------------
#' results <- run_adi_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   n_cores = 6              # Default: uses (total cores - 1)
#' )
#'
#' FIGURE CUSTOMIZATION:
#' ---------------------
#' results <- run_adi_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   figure_width = 16,       # Width in inches (default: 12)
#'   figure_height = 10,      # Height in inches (default: 8)
#'   dpi = 600                # Resolution (default: 300)
#' )
#'
#' DISABLE BIRDNET INTEGRATION:
#' ----------------------------
#' results <- run_adi_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   integrate_birdnet = FALSE
#' )
#'
#' ACCESSING RESULTS:
#' ------------------
#' After running the function, you can access:
#'
#' # Raw ADI data
#' adi_data <- results$adi_results
#'
#' # Summary statistics
#' hourly_summary <- results$summary_stats$by_hour
#' daily_summary <- results$summary_stats$by_date
#' site_summary <- results$summary_stats$by_site
#' deployment_summary <- results$summary_stats$by_deployment
#'
#' # Processing statistics
#' cat("Files processed:", results$n_files_processed, "\n")
#' cat("Files successful:", results$n_files_successful, "\n")
#' cat("Files failed:", results$n_files_failed, "\n")
#' cat("BirdNET integrated:", results$birdnet_integrated, "\n")
#'
#' # View log file
#' file.show(results$log_file)
#'
#' COMPLETE EXAMPLE:
#' -----------------
#' results <- run_adi_analysis(
#'   audio_path = "/data/bioacoustics/recordings",
#'   output_path = "/data/bioacoustics/adi_analysis_2024",
#'   timezone = "America/Los_Angeles",
#'   max_freq = 10000,
#'   db_threshold = -50,
#'   freq_step = 1000,
#'   n_cores = 8,
#'   plot_types = c("daily_pattern", "heatmap_daily", "boxplot_site", 
#'                  "species_correlation", "distribution"),
#'   figure_width = 14,
#'   figure_height = 10,
#'   dpi = 300,
#'   integrate_birdnet = TRUE
#' )
#'
#' # View summary
#' print(results$summary_stats$by_hour)
#'
#' # Further analysis with tidyverse
#' library(tidyverse)
#'
#' # Plot custom visualization
#' results$adi_results %>%
#'   filter(site == "MyFavoriteSite") %>%
#'   ggplot(aes(x = datetime_local, y = adi)) +
#'   geom_line() +
#'   theme_minimal()
#'
#' ============================================================================
#'
#' UNDERSTANDING ACOUSTIC DIVERSITY INDEX:
#' ========================================
#'
#' The Acoustic Diversity Index (ADI) is based on the Shannon diversity index
#' and measures how evenly sound energy is distributed across frequency bands.
#'
#' ADI Calculation:
#' 1. Divide spectrum into frequency bands (freq_step width)
#' 2. Calculate proportion of energy in each band
#' 3. Apply Shannon index formula: H = -Σ(pi * ln(pi))
#' 4. Normalize to 0-1 scale
#'
#' INTERPRETATION:
#' - ADI = 1.0: Sound energy perfectly evenly distributed (maximum diversity)
#' - ADI = 0.0: All sound energy in one frequency band (minimum diversity)
#' - Higher ADI typically indicates:
#'   * More species vocalizing
#'   * Greater variety of sound sources
#'   * More complex acoustic environment
#'
#' PARAMETER RECOMMENDATIONS FOR BIRD STUDIES:
#' - max_freq: 10000 Hz
#'   * Captures most bird vocalizations (200-10000 Hz)
#'   * Excludes ultrasonic noise
#'   * Can increase to 12000 Hz for high-frequency species
#'
#' - db_threshold: -50 dB
#'   * Standard threshold for "occupied" band
#'   * Bands above this threshold are included in calculation
#'   * Lower values (e.g., -55) include more quiet sounds
#'   * Higher values (e.g., -45) focus on louder sounds
#'
#' - freq_step: 1000 Hz
#'   * 1 kHz bands provide good resolution for birds
#'   * Smaller steps (500 Hz) = finer resolution, more bands
#'   * Larger steps (2000 Hz) = coarser resolution, fewer bands
#'
#' BEST PRACTICES:
#' - Use consistent parameters across all recordings for comparison
#' - Higher ADI often correlates with species richness
#' - Consider temporal patterns (dawn chorus typically shows high ADI)
#' - Validate with traditional surveys
#' - ADI can be affected by:
#'   * Background noise (can inflate values)
#'   * Recording quality
#'   * Weather conditions
#'   * Dominant vocalizers
#'
#' TYPICAL VALUES:
#' - Quiet forest: ADI = 0.3-0.5
#' - Active dawn chorus: ADI = 0.6-0.8
#' - Very diverse soundscape: ADI > 0.8
#' - Dominated by single species: ADI < 0.3
#'
#' REFERENCES:
#' Villanueva-Rivera et al. (2011) - Original ADI paper and methodology
#' Pieretti et al. (2011) - Comparison of acoustic indices
#' Sueur et al. (2014) - soundecology R package documentation
#'
#' ============================================================================