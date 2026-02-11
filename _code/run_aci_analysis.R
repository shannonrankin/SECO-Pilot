#' Batch Acoustic Complexity Index (ACI) Analysis with Visualization
#'
#' This function processes multiple WAV files to calculate the Acoustic Complexity Index (ACI),
#' integrates with BirdNET selection tables, generates summary statistics, and creates
#' customizable visualizations. Designed for large-scale bioacoustic monitoring datasets.
#'
#' @param audio_path Character. Path to the top-level folder containing site folders with deployments
#' @param output_path Character. Path where all outputs will be saved
#' @param timezone Character. Timezone for datetime conversion (default: "America/Los_Angeles")
#' @param wl Numeric. Window length for FFT (default: 512)
#' @param ovlp Numeric. Overlap between successive windows in percent (default: 50, range: 0-99)
#' @param flim Numeric vector. Frequency limits in kHz (default: c(0, 12))
#' @param nbwindows Numeric. Number of time windows to divide recording (default: 1)
#' @param n_cores Numeric. Number of cores for parallel processing (default: NULL, uses n-1)
#' @param plot_types Character vector. Types of plots to generate. Options:
#'   - "daily_pattern": Time series showing ACI by hour of day
#'   - "temporal_trend": Time series showing ACI over dates
#'   - "heatmap_daily": Heatmap of hour x date
#'   - "heatmap_seasonal": Heatmap of hour x month
#'   - "boxplot_site": Boxplot comparing sites
#'   - "boxplot_period": Boxplot comparing time periods (dawn/day/dusk/night)
#'   - "ridge_month": Ridge plot showing ACI distribution by month
#'   - "species_correlation": Scatterplot of ACI vs species richness (requires BirdNET)
#'   - "distribution": Histogram of ACI values
#'   - "all": Generate all available plots
#' @param figure_width Numeric. Width of output figures in inches (default: 12)
#' @param figure_height Numeric. Height of output figures in inches (default: 8)
#' @param dpi Numeric. Resolution of output figures (default: 300)
#' @param integrate_birdnet Logical. Whether to integrate BirdNET selection tables (default: TRUE)
#'
#' @return List containing:
#'   - aci_results: Data frame with raw ACI results
#'   - summary_stats: List of summary data frames
#'   - log_file: Path to error/warning log file
#'   - output_files: List of all generated file paths
#'
#' @details
#' The function expects a specific folder structure:
#' - Top Folder: 'Location-Site'
#' - Deployment Folder: 'Location-Site_config_YYYYMMDD'
#' - WAV files: 'Location-Site_config_YYYYMMDD_HHMMSS.WAV'
#' - BirdNET folder: Contains 'birdnetLocal' in name
#' - BirdNET files: 'Location-Site_config_YYYYMMDD_HHMMSS.BirdNET.selection.table.txt'
#'
#' Output files created in output_path:
#' - aci_raw_results.csv: Raw ACI values for each file
#' - aci_summary_by_hour.csv: Mean ACI by hour of day
#' - aci_summary_by_date.csv: Mean ACI by date
#' - aci_summary_by_site.csv: Mean ACI by site
#' - aci_summary_by_deployment.csv: Mean ACI by deployment
#' - aci_analysis_log.txt: Error and warning log
#' - [plot_type].png: Individual visualization files
#'
#' @examples
#' \dontrun{
#' # Basic usage with default parameters
#' results <- run_aci_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs"
#' )
#'
#' # Custom parameters with specific visualizations
#' results <- run_aci_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   timezone = "America/New_York",
#'   wl = 1024,
#'   flim = c(2, 10),
#'   plot_types = c("daily_pattern", "heatmap_daily", "species_correlation"),
#'   n_cores = 4
#' )
#'
#' # Generate all available plots
#' results <- run_aci_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   plot_types = "all"
#' )
#' }
#'
#' @export

run_aci_analysis <- function(
    audio_path,
    output_path,
    timezone = "America/Los_Angeles",
    wl = 512,
    ovlp = 50,
    flim = c(0, 12),
    nbwindows = 1,
    n_cores = NULL,
    plot_types = c("daily_pattern", "temporal_trend", "heatmap_daily"),
    figure_width = 12,
    figure_height = 8,
    dpi = 300,
    integrate_birdnet = TRUE
) {
  
  # Load required packages
  required_packages <- c("tidyverse", "tuneR", "seewave", "lubridate", 
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
  log_file <- file.path(output_path, "aci_analysis_log.txt")
  log_con <- file(log_file, open = "wt")
  writeLines(paste("ACI Analysis Log - Started:", Sys.time()), log_con)
  writeLines(paste("Audio path:", audio_path), log_con)
  writeLines(paste("Output path:", output_path), log_con)
  writeLines(paste("Timezone:", timezone), log_con)
  writeLines(paste("ACI parameters: wl =", wl, ", ovlp =", ovlp, 
                   ", flim = c(", paste(flim, collapse = ", "), 
                   "), nbwindows =", nbwindows), log_con)
  writeLines("", log_con)
  
  # Validate inputs
  if (!dir.exists(audio_path)) {
    close(log_con)
    stop("audio_path does not exist: ", audio_path)
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
  
  # Helper function to calculate ACI for a single file
  calculate_aci_single <- function(file_path, wl, ovlp, flim, nbwindows, log_con) {
    tryCatch({
      # Read audio
      audio <- readWave(file_path)
      
      # Calculate ACI
      aci_result <- ACI(audio, wl = wl, ovlp = ovlp, flim = flim, nbwindows = nbwindows)
      
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
      
      # Return results
      if (nbwindows == 1) {
        return(tibble(
          file = file_name,
          file_path = file_path,
          site = site,
          deployment = deployment,
          datetime_utc = datetime_utc,
          aci = aci_result
        ))
      } else {
        return(tibble(
          file = file_name,
          file_path = file_path,
          site = site,
          deployment = deployment,
          datetime_utc = datetime_utc,
          aci_mean = mean(aci_result$AciTotAll_left, na.rm = TRUE),
          aci_sd = sd(aci_result$AciTotAll_left, na.rm = TRUE),
          aci_min = min(aci_result$AciTotAll_left, na.rm = TRUE),
          aci_max = max(aci_result$AciTotAll_left, na.rm = TRUE)
        ))
      }
      
    }, error = function(e) {
      warning(paste("Error processing", basename(file_path), ":", e$message))
      return(tibble(
        file = basename(file_path),
        file_path = file_path,
        site = NA_character_,
        deployment = NA_character_,
        datetime_utc = as.POSIXct(NA),
        aci = NA_real_
      ))
    })
  }
  
  # Process all files in parallel with progress
  message("\nCalculating ACI for all files...")
  message("This may take a while for large datasets...")
  
  aci_results <- future_map_dfr(
    audio_files,
    ~calculate_aci_single(.x, wl, ovlp, flim, nbwindows, log_con),
    .progress = TRUE,
    .options = furrr_options(seed = TRUE)
  )
  
  # Convert to local timezone
  message("\nConverting timestamps to local timezone...")
  aci_results <- aci_results %>%
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
  n_errors <- sum(is.na(aci_results$aci))
  if (n_errors > 0) {
    writeLines(paste("\nErrors encountered:", n_errors, "files failed to process"), log_con)
    writeLines("Failed files:", log_con)
    writeLines(aci_results$file[is.na(aci_results$aci)], log_con)
  }
  
  # Save raw results
  message("\nSaving raw ACI results...")
  raw_results_file <- file.path(output_path, "aci_raw_results.csv")
  write_csv(aci_results, raw_results_file)
  message("Saved: ", raw_results_file)
  
  # Generate summary statistics
  message("\nGenerating summary statistics...")
  
  # Summary by hour
  summary_hour <- aci_results %>%
    group_by(hour) %>%
    summarise(
      n_files = n(),
      mean_aci = mean(aci, na.rm = TRUE),
      sd_aci = sd(aci, na.rm = TRUE),
      median_aci = median(aci, na.rm = TRUE),
      min_aci = min(aci, na.rm = TRUE),
      max_aci = max(aci, na.rm = TRUE),
      .groups = "drop"
    )
  write_csv(summary_hour, file.path(output_path, "aci_summary_by_hour.csv"))
  
  # Summary by date
  summary_date <- aci_results %>%
    group_by(date) %>%
    summarise(
      n_files = n(),
      mean_aci = mean(aci, na.rm = TRUE),
      sd_aci = sd(aci, na.rm = TRUE),
      median_aci = median(aci, na.rm = TRUE),
      min_aci = min(aci, na.rm = TRUE),
      max_aci = max(aci, na.rm = TRUE),
      .groups = "drop"
    )
  write_csv(summary_date, file.path(output_path, "aci_summary_by_date.csv"))
  
  # Summary by site
  summary_site <- aci_results %>%
    group_by(site) %>%
    summarise(
      n_files = n(),
      mean_aci = mean(aci, na.rm = TRUE),
      sd_aci = sd(aci, na.rm = TRUE),
      median_aci = median(aci, na.rm = TRUE),
      min_aci = min(aci, na.rm = TRUE),
      max_aci = max(aci, na.rm = TRUE),
      .groups = "drop"
    )
  write_csv(summary_site, file.path(output_path, "aci_summary_by_site.csv"))
  
  # Summary by deployment
  summary_deployment <- aci_results %>%
    group_by(deployment) %>%
    summarise(
      n_files = n(),
      mean_aci = mean(aci, na.rm = TRUE),
      sd_aci = sd(aci, na.rm = TRUE),
      median_aci = median(aci, na.rm = TRUE),
      min_aci = min(aci, na.rm = TRUE),
      max_aci = max(aci, na.rm = TRUE),
      .groups = "drop"
    )
  write_csv(summary_deployment, file.path(output_path, "aci_summary_by_deployment.csv"))
  
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
          
          # Join with ACI results
          aci_results <- aci_results %>%
            left_join(birdnet_summary, by = "file")
          
          birdnet_integrated <- TRUE
          message("BirdNET data integrated successfully")
          writeLines("BirdNET data integrated successfully", log_con)
          
          # Save updated results with BirdNET
          write_csv(aci_results, raw_results_file)
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
    p <- aci_results %>%
      ggplot(aes(x = hour, y = aci)) +
      geom_point(alpha = 0.3, color = "steelblue", size = 1.5) +
      geom_smooth(method = "loess", color = "darkred", size = 1.2, se = TRUE) +
      scale_x_continuous(breaks = seq(0, 23, 2)) +
      labs(
        title = "Acoustic Complexity Index by Hour of Day",
        x = "Hour of Day",
        y = "ACI Value",
        caption = paste("Higher values indicate greater acoustic complexity | n =", 
                        sum(!is.na(aci_results$aci)), "files")
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        panel.grid.minor = element_blank()
      )
    
    output_files$daily_pattern <- save_plot_safe(p, "aci_daily_pattern.png", 
                                                 figure_width, figure_height, dpi)
  }
  
  # 2. Temporal Trend
  if ("temporal_trend" %in% plot_types) {
    p <- aci_results %>%
      ggplot(aes(x = datetime_local, y = aci)) +
      geom_line(alpha = 0.4, color = "forestgreen") +
      geom_smooth(method = "loess", color = "darkred", size = 1.2) +
      labs(
        title = "Acoustic Complexity Index Over Time",
        x = "Date",
        y = "ACI Value",
        caption = paste("Timezone:", timezone)
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        panel.grid.minor = element_blank()
      )
    
    output_files$temporal_trend <- save_plot_safe(p, "aci_temporal_trend.png", 
                                                  figure_width, figure_height, dpi)
  }
  
  # 3. Heatmap - Daily (Hour x Date)
  if ("heatmap_daily" %in% plot_types) {
    aci_heatmap_data <- aci_results %>%
      group_by(date, hour) %>%
      summarise(mean_aci = mean(aci, na.rm = TRUE), .groups = "drop")
    
    p <- aci_heatmap_data %>%
      ggplot(aes(x = hour, y = date, fill = mean_aci)) +
      geom_tile() +
      scale_fill_viridis_c(option = "magma", name = "Mean ACI") +
      scale_x_continuous(breaks = seq(0, 23, 2)) +
      labs(
        title = "Acoustic Complexity Index Heatmap",
        subtitle = "Hour of Day × Date",
        x = "Hour of Day",
        y = "Date"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "right"
      )
    
    output_files$heatmap_daily <- save_plot_safe(p, "aci_heatmap_daily.png", 
                                                 figure_width, figure_height, dpi)
  }
  
  # 4. Heatmap - Seasonal (Hour x Month)
  if ("heatmap_seasonal" %in% plot_types) {
    aci_seasonal_data <- aci_results %>%
      group_by(month, hour) %>%
      summarise(mean_aci = mean(aci, na.rm = TRUE), .groups = "drop")
    
    p <- aci_seasonal_data %>%
      ggplot(aes(x = hour, y = month, fill = mean_aci)) +
      geom_tile() +
      scale_fill_viridis_c(option = "magma", name = "Mean ACI") +
      scale_x_continuous(breaks = seq(0, 23, 2)) +
      labs(
        title = "Seasonal Acoustic Complexity Patterns",
        subtitle = "Hour of Day × Month",
        x = "Hour of Day",
        y = "Month"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "right"
      )
    
    output_files$heatmap_seasonal <- save_plot_safe(p, "aci_heatmap_seasonal.png", 
                                                    figure_width, figure_height, dpi)
  }
  
  # 5. Boxplot - By Site
  if ("boxplot_site" %in% plot_types) {
    n_sites <- n_distinct(aci_results$site)
    
    if (n_sites > 1) {
      p <- aci_results %>%
        filter(!is.na(site)) %>%
        ggplot(aes(x = reorder(site, aci, FUN = median, na.rm = TRUE), 
                   y = aci, fill = site)) +
        geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
        scale_fill_viridis_d(option = "turbo") +
        labs(
          title = "Acoustic Complexity Index by Recording Site",
          x = "Site",
          y = "ACI Value"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 16),
          legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      output_files$boxplot_site <- save_plot_safe(p, "aci_boxplot_site.png", 
                                                  figure_width, figure_height, dpi)
    } else {
      message("Skipping boxplot_site: Only one site detected")
    }
  }
  
  # 6. Boxplot - By Time Period
  if ("boxplot_period" %in% plot_types) {
    p <- aci_results %>%
      ggplot(aes(x = time_period, y = aci, fill = time_period)) +
      geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
      scale_fill_manual(values = c("Dawn" = "#FF9999", "Day" = "#FFD700", 
                                   "Dusk" = "#9999FF", "Night" = "#333333")) +
      labs(
        title = "Acoustic Complexity Index by Time Period",
        x = "Time Period",
        y = "ACI Value"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "none"
      )
    
    output_files$boxplot_period <- save_plot_safe(p, "aci_boxplot_period.png", 
                                                  figure_width, figure_height, dpi)
  }
  
  # 7. Ridge Plot - By Month
  if ("ridge_month" %in% plot_types) {
    if (requireNamespace("ggridges", quietly = TRUE)) {
      p <- aci_results %>%
        filter(!is.na(month)) %>%
        ggplot(aes(x = aci, y = month, fill = stat(x))) +
        ggridges::geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
        scale_fill_viridis_c(option = "plasma", name = "ACI") +
        labs(
          title = "Distribution of ACI Values by Month",
          x = "ACI Value",
          y = "Month"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 16)
        )
      
      output_files$ridge_month <- save_plot_safe(p, "aci_ridge_month.png", 
                                                 figure_width, figure_height, dpi)
    } else {
      warning("Package 'ggridges' not available. Skipping ridge plot.")
    }
  }
  
  # 8. Species Correlation
  if ("species_correlation" %in% plot_types && birdnet_integrated) {
    corr_data <- aci_results %>%
      filter(!is.na(aci), !is.na(n_species))
    
    if (nrow(corr_data) > 0) {
      corr_value <- cor(corr_data$aci, corr_data$n_species, use = "complete.obs")
      
      p <- corr_data %>%
        ggplot(aes(x = aci, y = n_species)) +
        geom_point(alpha = 0.5, color = "steelblue", size = 2) +
        geom_smooth(method = "lm", color = "darkred", size = 1.2) +
        labs(
          title = "Relationship Between ACI and Species Richness",
          x = "Acoustic Complexity Index (ACI)",
          y = "Number of Species Detected (BirdNET)",
          caption = paste("Pearson correlation:", round(corr_value, 3), 
                          "| n =", nrow(corr_data), "recordings")
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 16)
        )
      
      output_files$species_correlation <- save_plot_safe(p, "aci_species_correlation.png", 
                                                         figure_width, figure_height, dpi)
    } else {
      message("Skipping species_correlation: No overlapping data")
    }
  } else if ("species_correlation" %in% plot_types && !birdnet_integrated) {
    message("Skipping species_correlation: BirdNET data not integrated")
  }
  
  # 9. Distribution
  if ("distribution" %in% plot_types) {
    p <- aci_results %>%
      ggplot(aes(x = aci)) +
      geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7, color = "white") +
      geom_vline(aes(xintercept = mean(aci, na.rm = TRUE)), 
                 color = "darkred", linetype = "dashed", size = 1.2) +
      geom_vline(aes(xintercept = median(aci, na.rm = TRUE)), 
                 color = "darkorange", linetype = "dashed", size = 1.2) +
      annotate("text", x = mean(aci_results$aci, na.rm = TRUE), 
               y = Inf, label = "Mean", vjust = 1.5, color = "darkred", fontface = "bold") +
      annotate("text", x = median(aci_results$aci, na.rm = TRUE), 
               y = Inf, label = "Median", vjust = 3, color = "darkorange", fontface = "bold") +
      labs(
        title = "Distribution of ACI Values",
        x = "ACI Value",
        y = "Count",
        caption = paste("Mean:", round(mean(aci_results$aci, na.rm = TRUE), 2),
                        "| Median:", round(median(aci_results$aci, na.rm = TRUE), 2))
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16)
      )
    
    output_files$distribution <- save_plot_safe(p, "aci_distribution.png", 
                                                figure_width, figure_height, dpi)
  }
  
  # Close log file
  writeLines(paste("\nAnalysis completed:", Sys.time()), log_con)
  close(log_con)
  
  # Reset future plan
  plan(sequential)
  
  # Summary message
  message("\n========================================")
  message("ACI Analysis Complete!")
  message("========================================")
  message(paste("Processed:", length(audio_files), "files"))
  message(paste("Successful:", sum(!is.na(aci_results$aci)), "files"))
  message(paste("Failed:", n_errors, "files"))
  message(paste("\nOutputs saved to:", output_path))
  message("\nGenerated files:")
  message("  - aci_raw_results.csv")
  message("  - aci_summary_by_hour.csv")
  message("  - aci_summary_by_date.csv")
  message("  - aci_summary_by_site.csv")
  message("  - aci_summary_by_deployment.csv")
  message("  - aci_analysis_log.txt")
  message(paste("  -", length(output_files), "visualization(s)"))
  message("========================================\n")
  
  # Return results
  return(invisible(list(
    aci_results = aci_results,
    summary_stats = list(
      by_hour = summary_hour,
      by_date = summary_date,
      by_site = summary_site,
      by_deployment = summary_deployment
    ),
    log_file = log_file,
    output_files = output_files,
    n_files_processed = length(audio_files),
    n_files_successful = sum(!is.na(aci_results$aci)),
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
#' results <- run_aci_analysis(
#'   audio_path = "/path/to/your/recordings",
#'   output_path = "/path/to/save/outputs"
#' )
#'
#' CUSTOMIZING ACI PARAMETERS:
#' ---------------------------
#' results <- run_aci_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   wl = 1024,              # Window length (default: 512)
#'   ovlp = 75,              # Overlap percentage (default: 50)
#'   flim = c(2, 10),        # Frequency limits in kHz (default: c(0, 12))
#'   nbwindows = 10          # Number of time windows (default: 1)
#' )
#'
#' SELECTING SPECIFIC VISUALIZATIONS:
#' -----------------------------------
#' Available plot types:
#'   - "daily_pattern"        : ACI by hour of day with smoothed trend
#'   - "temporal_trend"       : ACI over time (dates)
#'   - "heatmap_daily"        : Hour × Date heatmap
#'   - "heatmap_seasonal"     : Hour × Month heatmap
#'   - "boxplot_site"         : Compare ACI across sites
#'   - "boxplot_period"       : Compare ACI across time periods (Dawn/Day/Dusk/Night)
#'   - "ridge_month"          : Distribution of ACI by month (ridge plot)
#'   - "species_correlation"  : ACI vs Species Richness (requires BirdNET)
#'   - "distribution"         : Histogram of ACI values
#'   - "all"                  : Generate all available plots
#'
#' Example - Select specific plots:
#' results <- run_aci_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   plot_types = c("daily_pattern", "heatmap_daily", "species_correlation")
#' )
#'
#' Example - Generate all plots:
#' results <- run_aci_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   plot_types = "all"
#' )
#'
#' TIMEZONE CUSTOMIZATION:
#' -----------------------
#' results <- run_aci_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   timezone = "America/New_York"    # Default: "America/Los_Angeles"
#' )
#'
#' Common timezone options:
#'   - "America/Los_Angeles" (Pacific)
#'   - "America/Denver" (Mountain)
#'   - "America/Chicago" (Central)
#'   - "America/New_York" (Eastern)
#'   - "UTC" (Coordinated Universal Time)
#'
#' PARALLEL PROCESSING:
#' --------------------
#' results <- run_aci_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   n_cores = 6              # Default: uses (total cores - 1)
#' )
#'
#' FIGURE CUSTOMIZATION:
#' ---------------------
#' results <- run_aci_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   figure_width = 16,       # Width in inches (default: 12)
#'   figure_height = 10,      # Height in inches (default: 8)
#'   dpi = 600                # Resolution (default: 300)
#' )
#'
#' DISABLE BIRDNET INTEGRATION:
#' ----------------------------
#' results <- run_aci_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   integrate_birdnet = FALSE
#' )
#'
#' ACCESSING RESULTS:
#' ------------------
#' After running the function, you can access:
#'
#' # Raw ACI data
#' aci_data <- results$aci_results
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
#' results <- run_aci_analysis(
#'   audio_path = "/data/bioacoustics/recordings",
#'   output_path = "/data/bioacoustics/aci_analysis_2024",
#'   timezone = "America/Los_Angeles",
#'   wl = 512,
#'   ovlp = 50,
#'   flim = c(0, 12),
#'   nbwindows = 1,
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
#' results$aci_results %>%
#'   filter(site == "MyFavoriteSite") %>%
#'   ggplot(aes(x = datetime_local, y = aci)) +
#'   geom_line() +
#'   theme_minimal()
#'
#' ============================================================================