#' Batch Acoustic Entropy Index (H) Analysis with Visualization
#'
#' This function processes multiple WAV files to calculate the Acoustic Entropy Index (H),
#' including temporal entropy (Ht), spectral entropy (Hf), and total entropy (H),
#' integrates with BirdNET selection tables, generates summary statistics, and creates
#' customizable visualizations. Designed for large-scale bioacoustic monitoring datasets.
#'
#' @param audio_path Character. Path to the top-level folder containing site folders with deployments
#' @param output_path Character. Path where all outputs will be saved
#' @param timezone Character. Timezone for datetime conversion (default: "America/Los_Angeles")
#' @param wl Numeric. Window length for FFT (default: 512, recommended for bird studies)
#' @param envt Character. Envelope type for temporal entropy: "hil" (Hilbert) or "abs" (absolute) (default: "hil")
#' @param msmooth Numeric vector. Mean smoothing for temporal entropy (default: c(512, 90))
#' @param ksmooth Numeric vector. Kernel smoothing for spectral entropy (default: c(3, 3))
#' @param n_cores Numeric. Number of cores for parallel processing (default: NULL, uses n-1)
#' @param plot_types Character vector. Types of plots to generate. Options:
#'   - "daily_pattern": Time series showing H by hour of day
#'   - "temporal_trend": Time series showing H over dates
#'   - "heatmap_daily": Heatmap of hour x date
#'   - "heatmap_seasonal": Heatmap of hour x month
#'   - "boxplot_site": Boxplot comparing sites
#'   - "boxplot_period": Boxplot comparing time periods (dawn/day/dusk/night)
#'   - "ridge_month": Ridge plot showing H distribution by month
#'   - "species_correlation": Scatterplot of H vs species richness (requires BirdNET)
#'   - "distribution": Histogram of H values
#'   - "entropy_components": Scatterplot of Ht vs Hf with H as color
#'   - "all": Generate all available plots
#' @param figure_width Numeric. Width of output figures in inches (default: 12)
#' @param figure_height Numeric. Height of output figures in inches (default: 8)
#' @param dpi Numeric. Resolution of output figures (default: 300)
#' @param integrate_birdnet Logical. Whether to integrate BirdNET selection tables (default: TRUE)
#'
#' @return List containing:
#'   - h_results: Data frame with raw H results (including Ht, Hf, and H)
#'   - summary_stats: List of summary data frames
#'   - log_file: Path to error/warning log file
#'   - output_files: List of all generated file paths
#'
#' @details
#' The Acoustic Entropy Index (H) was developed by Sueur et al. (2008) based on 
#' information theory. It measures the complexity of a soundscape through two components:
#' 
#' - Temporal Entropy (Ht): Evenness of the amplitude envelope over time (0-1)
#'   * Ht = 1: Uniform amplitude over time
#'   * Ht = 0: Concentrated energy in brief periods
#' 
#' - Spectral Entropy (Hf): Evenness of the frequency spectrum (0-1)
#'   * Hf = 1: Energy evenly distributed across frequencies (white noise-like)
#'   * Hf = 0: Energy concentrated in narrow frequency bands (pure tone-like)
#' 
#' - Total Entropy (H): Product of Ht and Hf (0-1)
#'   * H = Ht × Hf
#'   * Higher H indicates more complex, diverse soundscape
#'   * Lower H indicates simpler, more stereotyped soundscape
#'
#' Best practices for bird studies (Sueur et al. 2008, 2014):
#' - wl: 512 (standard for 48 kHz recordings, provides good time-frequency resolution)
#' - envt: "hil" (Hilbert envelope, preferred for biological sounds)
#' - msmooth: c(512, 90) (temporal smoothing parameters)
#' - ksmooth: c(3, 3) (spectral smoothing parameters)
#'
#' The function expects a specific folder structure:
#' - Top Folder: 'Location-Site'
#' - Deployment Folder: 'Location-Site_config_YYYYMMDD'
#' - WAV files: 'Location-Site_config_YYYYMMDD_HHMMSS.WAV'
#' - BirdNET folder: Contains 'birdnetLocal' in name
#' - BirdNET files: 'Location-Site_config_YYYYMMDD_HHMMSS.BirdNET.selection.table.txt'
#'
#' Output files created in output_path:
#' - h_raw_results.csv: Raw H, Ht, and Hf values for each file
#' - h_summary_by_hour.csv: Mean H by hour of day
#' - h_summary_by_date.csv: Mean H by date
#' - h_summary_by_site.csv: Mean H by site
#' - h_summary_by_deployment.csv: Mean H by deployment
#' - h_analysis_log.txt: Error and warning log
#' - [plot_type].png: Individual visualization files
#'
#' @references
#' Sueur, J., et al. (2008). Rapid acoustic survey for biodiversity appraisal.
#'   PLoS ONE, 3(12), e4065.
#'
#' Sueur, J., et al. (2014). Acoustic indices for biodiversity assessment and 
#'   landscape investigation. Acta Acustica united with Acustica, 100(4), 772-781.
#'
#' @examples
#' \dontrun{
#' # Basic usage with default parameters
#' results <- run_h_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs"
#' )
#'
#' # Custom parameters with specific visualizations
#' results <- run_h_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   timezone = "America/New_York",
#'   wl = 1024,
#'   plot_types = c("daily_pattern", "heatmap_daily", "entropy_components"),
#'   n_cores = 4
#' )
#'
#' # Generate all available plots
#' results <- run_h_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   plot_types = "all"
#' )
#' }
#'
#' @export

run_h_analysis <- function(
    audio_path,
    output_path,
    timezone = "America/Los_Angeles",
    wl = 512,
    envt = "hil",
    msmooth = c(512, 90),
    ksmooth = c(3, 3),
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
  log_file <- file.path(output_path, "h_analysis_log.txt")
  log_con <- file(log_file, open = "wt")
  writeLines(paste("Acoustic Entropy Index Analysis Log - Started:", Sys.time()), log_con)
  writeLines(paste("Audio path:", audio_path), log_con)
  writeLines(paste("Output path:", output_path), log_con)
  writeLines(paste("Timezone:", timezone), log_con)
  writeLines(paste("H parameters: wl =", wl, ", envt =", envt, 
                   ", msmooth = c(", paste(msmooth, collapse = ", "), ")",
                   ", ksmooth = c(", paste(ksmooth, collapse = ", "), ")"), log_con)
  writeLines("", log_con)
  
  # Validate inputs
  if (!dir.exists(audio_path)) {
    close(log_con)
    stop("audio_path does not exist: ", audio_path)
  }
  
  if (!envt %in% c("hil", "abs")) {
    close(log_con)
    stop("envt must be either 'hil' (Hilbert) or 'abs' (absolute)")
  }
  
  # Expand "all" plot types
  all_plot_types <- c("daily_pattern", "temporal_trend", "heatmap_daily", 
                      "heatmap_seasonal", "boxplot_site", "boxplot_period",
                      "ridge_month", "species_correlation", "distribution",
                      "entropy_components")
  
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
  
  # Helper function to calculate H for a single file
  calculate_h_single <- function(file_path, wl, envt, msmooth, ksmooth, log_con) {
    tryCatch({
      # Read audio
      audio <- readWave(file_path)
      
      # Calculate Acoustic Entropy Index
      # seewave's H() returns a list with:
      # - Ht: temporal entropy (0-1)
      # - Hf: spectral entropy (0-1)
      # - H: total entropy = Ht * Hf (0-1)
      h_result <- H(
        wave = audio,
        wl = wl,
        envt = envt,
        msmooth = msmooth,
        ksmooth = ksmooth
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
      
      # Extract all entropy values
      ht_value <- as.numeric(h_result[1])  # Temporal entropy
      hf_value <- as.numeric(h_result[2])  # Spectral entropy
      h_value <- ht_value * hf_value       # Total entropy (product)
      
      # Return results with all entropy components
      result_df <- tibble(
        file = file_name,
        file_path = file_path,
        site = site,
        deployment = deployment,
        datetime_utc = datetime_utc,
        ht = ht_value,      # Temporal entropy
        hf = hf_value,      # Spectral entropy
        h = h_value         # Total entropy
      )
      
      return(result_df)
      
    }, error = function(e) {
      warning(paste("Error processing", basename(file_path), ":", e$message))
      return(tibble(
        file = basename(file_path),
        file_path = file_path,
        site = NA_character_,
        deployment = NA_character_,
        datetime_utc = as.POSIXct(NA),
        ht = NA_real_,
        hf = NA_real_,
        h = NA_real_
      ))
    })
  }
  
  # Process all files in parallel with progress
  message("\nCalculating Acoustic Entropy Index for all files...")
  message("This may take a while for large datasets...")
  
  h_results <- future_map_dfr(
    audio_files,
    ~calculate_h_single(.x, wl, envt, msmooth, ksmooth, log_con),
    .progress = TRUE,
    .options = furrr_options(seed = TRUE)
  )
  
  # Convert to local timezone
  message("\nConverting timestamps to local timezone...")
  h_results <- h_results %>%
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
  n_errors <- sum(is.na(h_results$h))
  if (n_errors > 0) {
    writeLines(paste("\nErrors encountered:", n_errors, "files failed to process"), log_con)
    writeLines("Failed files:", log_con)
    writeLines(h_results$file[is.na(h_results$h)], log_con)
  }
  
  # Save raw results
  message("\nSaving raw H results...")
  raw_results_file <- file.path(output_path, "h_raw_results.csv")
  write_csv(h_results, raw_results_file)
  message("Saved: ", raw_results_file)
  
  # Generate summary statistics
  message("\nGenerating summary statistics...")
  
  # Summary by hour
  summary_hour <- h_results %>%
    group_by(hour) %>%
    summarise(
      n_files = n(),
      mean_h = mean(h, na.rm = TRUE),
      sd_h = sd(h, na.rm = TRUE),
      median_h = median(h, na.rm = TRUE),
      min_h = min(h, na.rm = TRUE),
      max_h = max(h, na.rm = TRUE),
      mean_ht = mean(ht, na.rm = TRUE),
      mean_hf = mean(hf, na.rm = TRUE),
      .groups = "drop"
    )
  write_csv(summary_hour, file.path(output_path, "h_summary_by_hour.csv"))
  
  # Summary by date
  summary_date <- h_results %>%
    group_by(date) %>%
    summarise(
      n_files = n(),
      mean_h = mean(h, na.rm = TRUE),
      sd_h = sd(h, na.rm = TRUE),
      median_h = median(h, na.rm = TRUE),
      min_h = min(h, na.rm = TRUE),
      max_h = max(h, na.rm = TRUE),
      mean_ht = mean(ht, na.rm = TRUE),
      mean_hf = mean(hf, na.rm = TRUE),
      .groups = "drop"
    )
  write_csv(summary_date, file.path(output_path, "h_summary_by_date.csv"))
  
  # Summary by site
  summary_site <- h_results %>%
    group_by(site) %>%
    summarise(
      n_files = n(),
      mean_h = mean(h, na.rm = TRUE),
      sd_h = sd(h, na.rm = TRUE),
      median_h = median(h, na.rm = TRUE),
      min_h = min(h, na.rm = TRUE),
      max_h = max(h, na.rm = TRUE),
      mean_ht = mean(ht, na.rm = TRUE),
      mean_hf = mean(hf, na.rm = TRUE),
      .groups = "drop"
    )
  write_csv(summary_site, file.path(output_path, "h_summary_by_site.csv"))
  
  # Summary by deployment
  summary_deployment <- h_results %>%
    group_by(deployment) %>%
    summarise(
      n_files = n(),
      mean_h = mean(h, na.rm = TRUE),
      sd_h = sd(h, na.rm = TRUE),
      median_h = median(h, na.rm = TRUE),
      min_h = min(h, na.rm = TRUE),
      max_h = max(h, na.rm = TRUE),
      mean_ht = mean(ht, na.rm = TRUE),
      mean_hf = mean(hf, na.rm = TRUE),
      .groups = "drop"
    )
  write_csv(summary_deployment, file.path(output_path, "h_summary_by_deployment.csv"))
  
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
          
          # Join with H results
          h_results <- h_results %>%
            left_join(birdnet_summary, by = "file")
          
          birdnet_integrated <- TRUE
          message("BirdNET data integrated successfully")
          writeLines("BirdNET data integrated successfully", log_con)
          
          # Save updated results with BirdNET
          write_csv(h_results, raw_results_file)
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
    p <- h_results %>%
      ggplot(aes(x = hour, y = h)) +
      geom_point(alpha = 0.3, color = "steelblue", size = 1.5) +
      geom_smooth(method = "loess", color = "darkred", size = 1.2, se = TRUE) +
      scale_x_continuous(breaks = seq(0, 23, 2)) +
      labs(
        title = "Acoustic Entropy Index by Hour of Day",
        x = "Hour of Day",
        y = "H Value (0-1)",
        caption = paste("Higher values indicate more complex soundscape | n =", 
                        sum(!is.na(h_results$h)), "files")
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        panel.grid.minor = element_blank()
      )
    
    output_files$daily_pattern <- save_plot_safe(p, "h_daily_pattern.png", 
                                                 figure_width, figure_height, dpi)
  }
  
  # 2. Temporal Trend
  if ("temporal_trend" %in% plot_types) {
    p <- h_results %>%
      ggplot(aes(x = datetime_local, y = h)) +
      geom_line(alpha = 0.4, color = "forestgreen") +
      geom_smooth(method = "loess", color = "darkred", size = 1.2) +
      labs(
        title = "Acoustic Entropy Index Over Time",
        x = "Date",
        y = "H Value (0-1)",
        caption = paste("Timezone:", timezone)
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        panel.grid.minor = element_blank()
      )
    
    output_files$temporal_trend <- save_plot_safe(p, "h_temporal_trend.png", 
                                                  figure_width, figure_height, dpi)
  }
  
  # 3. Heatmap - Daily (Hour x Date)
  if ("heatmap_daily" %in% plot_types) {
    h_heatmap_data <- h_results %>%
      group_by(date, hour) %>%
      summarise(mean_h = mean(h, na.rm = TRUE), .groups = "drop")
    
    p <- h_heatmap_data %>%
      ggplot(aes(x = hour, y = date, fill = mean_h)) +
      geom_tile() +
      scale_fill_viridis_c(option = "magma", name = "Mean H") +
      scale_x_continuous(breaks = seq(0, 23, 2)) +
      labs(
        title = "Acoustic Entropy Index Heatmap",
        subtitle = "Hour of Day × Date",
        x = "Hour of Day",
        y = "Date"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "right"
      )
    
    output_files$heatmap_daily <- save_plot_safe(p, "h_heatmap_daily.png", 
                                                 figure_width, figure_height, dpi)
  }
  
  # 4. Heatmap - Seasonal (Hour x Month)
  if ("heatmap_seasonal" %in% plot_types) {
    h_seasonal_data <- h_results %>%
      group_by(month, hour) %>%
      summarise(mean_h = mean(h, na.rm = TRUE), .groups = "drop")
    
    p <- h_seasonal_data %>%
      ggplot(aes(x = hour, y = month, fill = mean_h)) +
      geom_tile() +
      scale_fill_viridis_c(option = "magma", name = "Mean H") +
      scale_x_continuous(breaks = seq(0, 23, 2)) +
      labs(
        title = "Seasonal Acoustic Entropy Patterns",
        subtitle = "Hour of Day × Month",
        x = "Hour of Day",
        y = "Month"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "right"
      )
    
    output_files$heatmap_seasonal <- save_plot_safe(p, "h_heatmap_seasonal.png", 
                                                    figure_width, figure_height, dpi)
  }
  
  # 5. Boxplot - By Site
  if ("boxplot_site" %in% plot_types) {
    n_sites <- n_distinct(h_results$site)
    
    if (n_sites > 1) {
      p <- h_results %>%
        filter(!is.na(site)) %>%
        ggplot(aes(x = reorder(site, h, FUN = median, na.rm = TRUE), 
                   y = h, fill = site)) +
        geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
        scale_fill_viridis_d(option = "turbo") +
        labs(
          title = "Acoustic Entropy Index by Recording Site",
          x = "Site",
          y = "H Value (0-1)"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 16),
          legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      output_files$boxplot_site <- save_plot_safe(p, "h_boxplot_site.png", 
                                                  figure_width, figure_height, dpi)
    } else {
      message("Skipping boxplot_site: Only one site detected")
    }
  }
  
  # 6. Boxplot - By Time Period
  if ("boxplot_period" %in% plot_types) {
    p <- h_results %>%
      ggplot(aes(x = time_period, y = h, fill = time_period)) +
      geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
      scale_fill_manual(values = c("Dawn" = "#FF9999", "Day" = "#FFD700", 
                                   "Dusk" = "#9999FF", "Night" = "#333333")) +
      labs(
        title = "Acoustic Entropy Index by Time Period",
        x = "Time Period",
        y = "H Value (0-1)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "none"
      )
    
    output_files$boxplot_period <- save_plot_safe(p, "h_boxplot_period.png", 
                                                  figure_width, figure_height, dpi)
  }
  
  # 7. Ridge Plot - By Month
  if ("ridge_month" %in% plot_types) {
    if (requireNamespace("ggridges", quietly = TRUE)) {
      p <- h_results %>%
        filter(!is.na(month)) %>%
        ggplot(aes(x = h, y = month, fill = stat(x))) +
        ggridges::geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
        scale_fill_viridis_c(option = "plasma", name = "H") +
        labs(
          title = "Distribution of H Values by Month",
          x = "H Value (0-1)",
          y = "Month"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 16)
        )
      
      output_files$ridge_month <- save_plot_safe(p, "h_ridge_month.png", 
                                                 figure_width, figure_height, dpi)
    } else {
      warning("Package 'ggridges' not available. Skipping ridge plot.")
    }
  }
  
  # 8. Species Correlation
  if ("species_correlation" %in% plot_types && birdnet_integrated) {
    corr_data <- h_results %>%
      filter(!is.na(h), !is.na(n_species))
    
    if (nrow(corr_data) > 0) {
      corr_value <- cor(corr_data$h, corr_data$n_species, use = "complete.obs")
      
      p <- corr_data %>%
        ggplot(aes(x = h, y = n_species)) +
        geom_point(alpha = 0.5, color = "steelblue", size = 2) +
        geom_smooth(method = "lm", color = "darkred", size = 1.2) +
        labs(
          title = "Relationship Between H and Species Richness",
          x = "Acoustic Entropy Index (H)",
          y = "Number of Species Detected (BirdNET)",
          caption = paste("Pearson correlation:", round(corr_value, 3), 
                          "| n =", nrow(corr_data), "recordings")
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 16)
        )
      
      output_files$species_correlation <- save_plot_safe(p, "h_species_correlation.png", 
                                                         figure_width, figure_height, dpi)
    } else {
      message("Skipping species_correlation: No overlapping data")
    }
  } else if ("species_correlation" %in% plot_types && !birdnet_integrated) {
    message("Skipping species_correlation: BirdNET data not integrated")
  }
  
  # 9. Distribution
  if ("distribution" %in% plot_types) {
    p <- h_results %>%
      ggplot(aes(x = h)) +
      geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7, color = "white") +
      geom_vline(aes(xintercept = mean(h, na.rm = TRUE)), 
                 color = "darkred", linetype = "dashed", size = 1.2) +
      geom_vline(aes(xintercept = median(h, na.rm = TRUE)), 
                 color = "darkorange", linetype = "dashed", size = 1.2) +
      annotate("text", x = mean(h_results$h, na.rm = TRUE), 
               y = Inf, label = "Mean", vjust = 1.5, color = "darkred", fontface = "bold") +
      annotate("text", x = median(h_results$h, na.rm = TRUE), 
               y = Inf, label = "Median", vjust = 3, color = "darkorange", fontface = "bold") +
      labs(
        title = "Distribution of H Values",
        x = "H Value (0-1)",
        y = "Count",
        caption = paste("Mean:", round(mean(h_results$h, na.rm = TRUE), 3),
                        "| Median:", round(median(h_results$h, na.rm = TRUE), 3))
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16)
      )
    
    output_files$distribution <- save_plot_safe(p, "h_distribution.png", 
                                                figure_width, figure_height, dpi)
  }
  
  # 10. Entropy Components (Ht vs Hf)
  if ("entropy_components" %in% plot_types) {
    p <- h_results %>%
      filter(!is.na(ht), !is.na(hf)) %>%
      ggplot(aes(x = ht, y = hf, color = h)) +
      geom_point(alpha = 0.5, size = 2) +
      scale_color_viridis_c(option = "plasma", name = "H\n(Total)") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
      labs(
        title = "Entropy Components: Temporal vs Spectral",
        x = "Temporal Entropy (Ht)",
        y = "Spectral Entropy (Hf)",
        caption = "Diagonal line indicates Ht = Hf"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16)
      )
    
    output_files$entropy_components <- save_plot_safe(p, "h_entropy_components.png", 
                                                      figure_width, figure_height, dpi)
  }
  
  # Close log file
  writeLines(paste("\nAnalysis completed:", Sys.time()), log_con)
  close(log_con)
  
  # Reset future plan
  plan(sequential)
  
  # Summary message
  message("\n========================================")
  message("Acoustic Entropy Index Analysis Complete!")
  message("========================================")
  message(paste("Processed:", length(audio_files), "files"))
  message(paste("Successful:", sum(!is.na(h_results$h)), "files"))
  message(paste("Failed:", n_errors, "files"))
  message(paste("\nOutputs saved to:", output_path))
  message("\nGenerated files:")
  message("  - h_raw_results.csv (includes Ht, Hf, and H)")
  message("  - h_summary_by_hour.csv")
  message("  - h_summary_by_date.csv")
  message("  - h_summary_by_site.csv")
  message("  - h_summary_by_deployment.csv")
  message("  - h_analysis_log.txt")
  message(paste("  -", length(output_files), "visualization(s)"))
  message("========================================\n")
  
  # Return results
  return(invisible(list(
    h_results = h_results,
    summary_stats = list(
      by_hour = summary_hour,
      by_date = summary_date,
      by_site = summary_site,
      by_deployment = summary_deployment
    ),
    log_file = log_file,
    output_files = output_files,
    n_files_processed = length(audio_files),
    n_files_successful = sum(!is.na(h_results$h)),
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
#' results <- run_h_analysis(
#'   audio_path = "/path/to/your/recordings",
#'   output_path = "/path/to/save/outputs"
#' )
#'
#' CUSTOMIZING H PARAMETERS:
#' ---------------------------
#' results <- run_h_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   wl = 1024,               # Window length for FFT (default: 512)
#'   envt = "abs",            # Envelope type: "hil" or "abs" (default: "hil")
#'   msmooth = c(1024, 95),   # Mean smoothing (default: c(512, 90))
#'   ksmooth = c(5, 5)        # Kernel smoothing (default: c(3, 3))
#' )
#'
#' SELECTING SPECIFIC VISUALIZATIONS:
#' -----------------------------------
#' Available plot types:
#'   - "daily_pattern"        : H by hour of day with smoothed trend
#'   - "temporal_trend"       : H over time (dates)
#'   - "heatmap_daily"        : Hour × Date heatmap
#'   - "heatmap_seasonal"     : Hour × Month heatmap
#'   - "boxplot_site"         : Compare H across sites
#'   - "boxplot_period"       : Compare H across time periods (Dawn/Day/Dusk/Night)
#'   - "ridge_month"          : Distribution of H by month (ridge plot)
#'   - "species_correlation"  : H vs Species Richness (requires BirdNET)
#'   - "distribution"         : Histogram of H values
#'   - "entropy_components"   : Scatterplot of Ht vs Hf (colored by H)
#'   - "all"                  : Generate all available plots
#'
#' Example - Select specific plots:
#' results <- run_h_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   plot_types = c("daily_pattern", "entropy_components", "species_correlation")
#' )
#'
#' Example - Generate all plots:
#' results <- run_h_analysis(
#'   audio_path = "/path/to/recordings",
#'   output_path = "/path/to/outputs",
#'   plot_types = "all"
#' )
#'
#' ACCESSING RESULTS:
#' ------------------
#' After running the function, you can access:
#'
#' # Raw H data (includes Ht, Hf, and H)
#' h_data <- results$h_results
#' head(h_data)
#'
#' # Summary statistics
#' hourly_summary <- results$summary_stats$by_hour
#' daily_summary <- results$summary_stats$by_date
#'
#' # Examine entropy components
#' h_data %>%
#'   select(file, ht, hf, h) %>%
#'   head()
#'
#' COMPLETE EXAMPLE:
#' -----------------
#' results <- run_h_analysis(
#'   audio_path = "/data/bioacoustics/recordings",
#'   output_path = "/data/bioacoustics/h_analysis_2024",
#'   timezone = "America/Los_Angeles",
#'   wl = 512,
#'   envt = "hil",
#'   n_cores = 8,
#'   plot_types = c("daily_pattern", "heatmap_daily", "entropy_components", 
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
#' # Examine entropy components
#' results$h_results %>%
#'   ggplot(aes(x = ht, y = hf)) +
#'   geom_point(aes(color = h), alpha = 0.6) +
#'   scale_color_viridis_c() +
#'   labs(title = "Temporal vs Spectral Entropy") +
#'   theme_minimal()
#'
#' ============================================================================