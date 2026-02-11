#' Compare Acoustic Indices Across Analyses
#'
#' This function reads results from ACI, BI, ADI, and H analyses, merges them,
#' performs comparative statistical analyses, and generates visualizations and
#' publication-ready tables comparing the indices.
#'
#' @param aci_path Character. Path to ACI analysis output directory (optional, set to NULL if not run)
#' @param bi_path Character. Path to BI analysis output directory (optional, set to NULL if not run)
#' @param adi_path Character. Path to ADI analysis output directory (optional, set to NULL if not run)
#' @param h_path Character. Path to H analysis output directory (optional, set to NULL if not run)
#' @param output_path Character. Path where comparison outputs will be saved
#' @param plot_types Character vector. Types of plots to generate. Options:
#'   - "correlation_matrix": Heatmap of correlations between all indices
#'   - "pairwise_scatter": Pairwise scatterplots between indices
#'   - "temporal_overlay": All indices overlaid by hour of day
#'   - "temporal_trends": All indices over time (dates)
#'   - "temporal_heatmaps": Side-by-side heatmaps for each index
#'   - "site_comparison": Boxplots for each index by site
#'   - "site_radar": Radar/spider plots comparing sites across indices
#'   - "species_correlation": All indices vs species richness
#'   - "pca_biplot": PCA biplot of indices
#'   - "index_distributions": Distribution comparison across indices
#'   - "all": Generate all available plots
#' @param filter_date_range Date vector of length 2. Filter data to date range c(start_date, end_date) (optional)
#' @param filter_sites Character vector. Filter to specific sites (optional)
#' @param filter_deployments Character vector. Filter to specific deployments (optional)
#' @param normalize Logical. Whether to normalize/scale indices to 0-1 range before comparison (default: FALSE)
#' @param figure_width Numeric. Width of output figures in inches (default: 14)
#' @param figure_height Numeric. Height of output figures in inches (default: 10)
#' @param dpi Numeric. Resolution of output figures (default: 300)
#' @param create_combined_figures Logical. Create multi-panel combined figures (default: TRUE)
#' @param create_separate_figures Logical. Create separate PNG files for each plot (default: TRUE)
#'
#' @return List containing:
#'   - combined_data: Merged data frame with all indices
#'   - correlation_matrix: Correlation matrix between indices
#'   - summary_stats: Summary statistics for each index
#'   - missing_files: Information about file mismatches
#'   - output_files: List of all generated file paths
#'
#' @details
#' This function integrates results from multiple acoustic index analyses and provides
#' comprehensive comparisons through visualizations and statistical summaries.
#'
#' At least two index paths must be provided (non-NULL). The function will:
#' 1. Read raw results from each analysis
#' 2. Merge data by filename
#' 3. Apply filters if specified
#' 4. Optionally normalize indices
#' 5. Generate correlation matrices and summary statistics
#' 6. Create comparison visualizations
#' 7. Export publication-ready tables
#'
#' Output files created in output_path:
#' - combined_indices.csv: Merged data with all indices
#' - correlation_matrix.csv: Correlation coefficients between indices
#' - summary_statistics.csv: Descriptive stats for each index
#' - missing_files_log.txt: Log of file mismatches (if > 5 mismatches)
#' - [plot_type].png: Individual visualization files
#' - combined_figure_[type].png: Multi-panel combined figures
#'
#' @examples
#' \dontrun{
#' # Basic comparison with all four indices
#' results <- compare_acoustic_indices(
#'   aci_path = "/path/to/aci_analysis",
#'   bi_path = "/path/to/bi_analysis",
#'   adi_path = "/path/to/adi_analysis",
#'   h_path = "/path/to/h_analysis",
#'   output_path = "/path/to/comparison_outputs",
#'   plot_types = "all"
#' )
#'
#' # Comparison with only ACI and BI
#' results <- compare_acoustic_indices(
#'   aci_path = "/path/to/aci_analysis",
#'   bi_path = "/path/to/bi_analysis",
#'   adi_path = NULL,
#'   h_path = NULL,
#'   output_path = "/path/to/comparison_outputs",
#'   plot_types = c("correlation_matrix", "pairwise_scatter", "temporal_overlay")
#' )
#'
#' # Filtered comparison with normalization
#' results <- compare_acoustic_indices(
#'   aci_path = "/path/to/aci_analysis",
#'   bi_path = "/path/to/bi_analysis",
#'   adi_path = "/path/to/adi_analysis",
#'   h_path = "/path/to/h_analysis",
#'   output_path = "/path/to/comparison_outputs",
#'   filter_date_range = c(as.Date("2024-05-01"), as.Date("2024-08-31")),
#'   filter_sites = c("Site-A", "Site-B"),
#'   normalize = TRUE,
#'   plot_types = "all"
#' )
#' }
#'
#' @export

compare_acoustic_indices <- function(
    aci_path = NULL,
    bi_path = NULL,
    adi_path = NULL,
    h_path = NULL,
    output_path,
    plot_types = c("correlation_matrix", "pairwise_scatter", "temporal_overlay"),
    filter_date_range = NULL,
    filter_sites = NULL,
    filter_deployments = NULL,
    normalize = FALSE,
    figure_width = 14,
    figure_height = 10,
    dpi = 300,
    create_combined_figures = TRUE,
    create_separate_figures = TRUE
) {
  
  # Load required packages
  required_packages <- c("tidyverse", "corrplot", "GGally", "patchwork", 
                         "viridis", "lubridate", "scales", "ggrepel")
  
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      stop(paste("Package", pkg, "is required but not installed. Install with: install.packages('", pkg, "')", sep = ""))
    }
  }
  
  # Create output directory
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
    message("Created output directory: ", output_path)
  }
  
  # Validate that at least two indices are provided
  indices_provided <- c(
    aci = !is.null(aci_path),
    bi = !is.null(bi_path),
    adi = !is.null(adi_path),
    h = !is.null(h_path)
  )
  
  if (sum(indices_provided) < 2) {
    stop("At least two index paths must be provided for comparison")
  }
  
  message("\n========================================")
  message("Acoustic Indices Comparison")
  message("========================================")
  message("Indices to compare:")
  if (indices_provided["aci"]) message("  ✓ ACI")
  if (indices_provided["bi"]) message("  ✓ BI")
  if (indices_provided["adi"]) message("  ✓ ADI")
  if (indices_provided["h"]) message("  ✓ H")
  message("========================================\n")
  
  # Read data from each analysis
  message("Reading analysis results...")
  
  index_data_list <- list()
  
  if (indices_provided["aci"]) {
    aci_file <- file.path(aci_path, "aci_raw_results.csv")
    if (!file.exists(aci_file)) {
      stop("ACI results file not found: ", aci_file)
    }
    index_data_list$aci <- read_csv(aci_file, show_col_types = FALSE) %>%
      select(file, site, deployment, datetime_local, date, hour, month, 
             year, time_period, aci, contains("n_species"), contains("n_detections"))
    message("  ✓ Read ACI data: ", nrow(index_data_list$aci), " files")
  }
  
  if (indices_provided["bi"]) {
    bi_file <- file.path(bi_path, "bi_raw_results.csv")
    if (!file.exists(bi_file)) {
      stop("BI results file not found: ", bi_file)
    }
    index_data_list$bi <- read_csv(bi_file, show_col_types = FALSE) %>%
      select(file, bi)
    message("  ✓ Read BI data: ", nrow(index_data_list$bi), " files")
  }
  
  if (indices_provided["adi"]) {
    adi_file <- file.path(adi_path, "adi_raw_results.csv")
    if (!file.exists(adi_file)) {
      stop("ADI results file not found: ", adi_file)
    }
    index_data_list$adi <- read_csv(adi_file, show_col_types = FALSE) %>%
      select(file, adi)
    message("  ✓ Read ADI data: ", nrow(index_data_list$adi), " files")
  }
  
  if (indices_provided["h"]) {
    h_file <- file.path(h_path, "h_raw_results.csv")
    if (!file.exists(h_file)) {
      stop("H results file not found: ", h_file)
    }
    index_data_list$h <- read_csv(h_file, show_col_types = FALSE) %>%
      select(file, h, ht, hf)
    message("  ✓ Read H data: ", nrow(index_data_list$h), " files")
  }
  
  # Start with the first available index as base
  base_index <- names(index_data_list)[1]
  combined_data <- index_data_list[[base_index]]
  
  # Merge all other indices
  missing_files <- list()
  n_mismatches <- 0
  
  for (idx_name in names(index_data_list)[-1]) {
    before_merge <- nrow(combined_data)
    
    combined_data <- combined_data %>%
      full_join(index_data_list[[idx_name]], by = "file")
    
    # Check for mismatches
    new_files_added <- nrow(combined_data) - before_merge
    if (new_files_added > 0) {
      n_mismatches <- n_mismatches + new_files_added
      missing_files[[idx_name]] <- combined_data %>%
        filter(is.na(!!sym(base_index))) %>%
        pull(file)
    }
  }
  
  # Handle missing files
  if (n_mismatches > 0) {
    warning(paste("File mismatch detected:", n_mismatches, "files present in some but not all analyses"))
    
    if (n_mismatches > 5) {
      # Create error log
      log_file <- file.path(output_path, "missing_files_log.txt")
      log_con <- file(log_file, open = "wt")
      writeLines(paste("Missing Files Log - Created:", Sys.time()), log_con)
      writeLines(paste("Total mismatches:", n_mismatches), log_con)
      writeLines("", log_con)
      
      for (idx_name in names(missing_files)) {
        if (length(missing_files[[idx_name]]) > 0) {
          writeLines(paste("\nFiles in", idx_name, "but not in", base_index, ":"), log_con)
          writeLines(missing_files[[idx_name]], log_con)
        }
      }
      
      close(log_con)
      message("  ⚠ Created missing files log: ", log_file)
    }
  }
  
  message("\nMerged data: ", nrow(combined_data), " total files")
  
  # Apply filters
  if (!is.null(filter_date_range)) {
    if (length(filter_date_range) != 2) {
      stop("filter_date_range must be a vector of length 2: c(start_date, end_date)")
    }
    before_filter <- nrow(combined_data)
    combined_data <- combined_data %>%
      filter(date >= filter_date_range[1], date <= filter_date_range[2])
    message("Date filter applied: ", before_filter, " → ", nrow(combined_data), " files")
  }
  
  if (!is.null(filter_sites)) {
    before_filter <- nrow(combined_data)
    combined_data <- combined_data %>%
      filter(site %in% filter_sites)
    message("Site filter applied: ", before_filter, " → ", nrow(combined_data), " files")
  }
  
  if (!is.null(filter_deployments)) {
    before_filter <- nrow(combined_data)
    combined_data <- combined_data %>%
      filter(deployment %in% filter_deployments)
    message("Deployment filter applied: ", before_filter, " → ", nrow(combined_data), " files")
  }
  
  # Normalize if requested
  index_cols <- c("aci", "bi", "adi", "h", "ht", "hf")[c("aci", "bi", "adi", "h", "ht", "hf") %in% names(combined_data)]
  
  if (normalize) {
    message("\nNormalizing indices to 0-1 range...")
    for (idx in index_cols) {
      if (idx %in% names(combined_data)) {
        min_val <- min(combined_data[[idx]], na.rm = TRUE)
        max_val <- max(combined_data[[idx]], na.rm = TRUE)
        combined_data[[paste0(idx, "_norm")]] <- (combined_data[[idx]] - min_val) / (max_val - min_val)
      }
    }
    # Use normalized columns for plotting
    plot_index_cols <- paste0(index_cols, "_norm")
  } else {
    plot_index_cols <- index_cols
  }
  
  # Save combined data
  message("\nSaving combined data...")
  combined_file <- file.path(output_path, "combined_indices.csv")
  write_csv(combined_data, combined_file)
  message("  ✓ Saved: combined_indices.csv")
  
  # Calculate correlation matrix
  message("\nCalculating correlation matrix...")
  cor_data <- combined_data %>%
    select(all_of(index_cols)) %>%
    drop_na()
  
  if (nrow(cor_data) < 10) {
    warning("Very few complete cases for correlation analysis (n = ", nrow(cor_data), ")")
  }
  
  cor_matrix <- cor(cor_data, use = "complete.obs", method = "pearson")
  
  # Save correlation matrix
  cor_file <- file.path(output_path, "correlation_matrix.csv")
  write.csv(cor_matrix, cor_file)
  message("  ✓ Saved: correlation_matrix.csv")
  
  # Calculate summary statistics
  message("\nCalculating summary statistics...")
  summary_stats <- combined_data %>%
    select(all_of(index_cols)) %>%
    pivot_longer(everything(), names_to = "index", values_to = "value") %>%
    group_by(index) %>%
    summarise(
      n = sum(!is.na(value)),
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      q25 = quantile(value, 0.25, na.rm = TRUE),
      q75 = quantile(value, 0.75, na.rm = TRUE),
      .groups = "drop"
    )
  
  summary_file <- file.path(output_path, "summary_statistics.csv")
  write_csv(summary_stats, summary_file)
  message("  ✓ Saved: summary_statistics.csv")
  
  # Expand "all" plot types
  all_plot_types <- c("correlation_matrix", "pairwise_scatter", "temporal_overlay",
                      "temporal_trends", "temporal_heatmaps", "site_comparison",
                      "site_radar", "species_correlation", "pca_biplot",
                      "index_distributions")
  
  if ("all" %in% plot_types) {
    plot_types <- all_plot_types
  }
  
  # Validate plot types
  invalid_types <- setdiff(plot_types, all_plot_types)
  if (length(invalid_types) > 0) {
    warning("Invalid plot types will be ignored: ", paste(invalid_types, collapse = ", "))
    plot_types <- intersect(plot_types, all_plot_types)
  }
  
  # Generate visualizations
  message("\n========================================")
  message("Generating Visualizations")
  message("========================================\n")
  
  output_files <- list()
  
  # Helper function to save plots
  save_plot_safe <- function(plot_obj, filename, width, height, dpi, type = "separate") {
    tryCatch({
      if (type == "separate" && !create_separate_figures) return(NULL)
      if (type == "combined" && !create_combined_figures) return(NULL)
      
      filepath <- file.path(output_path, filename)
      ggsave(filepath, plot = plot_obj, width = width, height = height, dpi = dpi, bg = "white")
      message(paste("  ✓ Saved:", filename))
      return(filepath)
    }, error = function(e) {
      warning(paste("Could not save plot", filename, ":", e$message))
      return(NULL)
    })
  }
  
  # 1. Correlation Matrix Heatmap
  if ("correlation_matrix" %in% plot_types) {
    message("Creating correlation matrix heatmap...")
    
    # Create ggplot version
    cor_df <- as.data.frame(cor_matrix) %>%
      rownames_to_column("index1") %>%
      pivot_longer(-index1, names_to = "index2", values_to = "correlation")
    
    p <- ggplot(cor_df, aes(x = index1, y = index2, fill = correlation)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%.2f", correlation)), 
                color = "white", size = 5, fontface = "bold") +
      scale_fill_gradient2(low = "#3B4CC0", mid = "white", high = "#B40426",
                          midpoint = 0, limit = c(-1, 1), name = "Correlation") +
      labs(title = "Correlation Matrix of Acoustic Indices",
           x = NULL, y = NULL) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank()
      ) +
      coord_equal()
    
    output_files$correlation_matrix <- save_plot_safe(p, "comparison_correlation_matrix.png",
                                                       10, 8, dpi, "separate")
  }
  
  # 2. Pairwise Scatter Plots
  if ("pairwise_scatter" %in% plot_types) {
    message("Creating pairwise scatterplots...")
    
    p <- combined_data %>%
      select(all_of(plot_index_cols)) %>%
      drop_na() %>%
      ggpairs(
        lower = list(continuous = wrap("points", alpha = 0.3, size = 0.5, color = "steelblue")),
        diag = list(continuous = wrap("barDiag", fill = "steelblue", alpha = 0.7)),
        upper = list(continuous = wrap("cor", size = 4))
      ) +
      theme_minimal() +
      labs(title = "Pairwise Relationships Between Acoustic Indices") +
      theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
    
    output_files$pairwise_scatter <- save_plot_safe(p, "comparison_pairwise_scatter.png",
                                                     figure_width, figure_height, dpi, "separate")
  }
  
  # 3. Temporal Overlay (by hour)
  if ("temporal_overlay" %in% plot_types) {
    message("Creating temporal overlay plot...")
    
    temporal_data <- combined_data %>%
      select(hour, all_of(plot_index_cols)) %>%
      group_by(hour) %>%
      summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
      pivot_longer(-hour, names_to = "index", values_to = "value")
    
    p <- ggplot(temporal_data, aes(x = hour, y = value, color = index)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = seq(0, 23, 2)) +
      scale_color_viridis_d(option = "turbo", name = "Index",
                           labels = toupper(gsub("_norm", "", unique(temporal_data$index)))) +
      labs(
        title = "Acoustic Indices by Hour of Day",
        x = "Hour of Day",
        y = ifelse(normalize, "Normalized Value", "Index Value")
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "right"
      )
    
    output_files$temporal_overlay <- save_plot_safe(p, "comparison_temporal_overlay.png",
                                                     figure_width, figure_height * 0.7, dpi, "separate")
  }
  
  # 4. Temporal Trends (by date)
  if ("temporal_trends" %in% plot_types) {
    message("Creating temporal trends plot...")
    
    trends_data <- combined_data %>%
      select(date, all_of(plot_index_cols)) %>%
      group_by(date) %>%
      summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
      pivot_longer(-date, names_to = "index", values_to = "value")
    
    p <- ggplot(trends_data, aes(x = date, y = value, color = index)) +
      geom_line(alpha = 0.6, size = 0.8) +
      geom_smooth(method = "loess", se = FALSE, size = 1.2) +
      scale_color_viridis_d(option = "turbo", name = "Index",
                           labels = toupper(gsub("_norm", "", unique(trends_data$index)))) +
      labs(
        title = "Acoustic Indices Over Time",
        x = "Date",
        y = ifelse(normalize, "Normalized Value", "Index Value")
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "right"
      )
    
    output_files$temporal_trends <- save_plot_safe(p, "comparison_temporal_trends.png",
                                                    figure_width, figure_height * 0.7, dpi, "separate")
  }
  
  # 5. Temporal Heatmaps (side by side)
  if ("temporal_heatmaps" %in% plot_types && create_combined_figures) {
    message("Creating temporal heatmaps...")
    
    plot_list <- list()
    
    for (idx in plot_index_cols) {
      heatmap_data <- combined_data %>%
        select(date, hour, !!sym(idx)) %>%
        group_by(date, hour) %>%
        summarise(mean_value = mean(!!sym(idx), na.rm = TRUE), .groups = "drop")
      
      p <- ggplot(heatmap_data, aes(x = hour, y = date, fill = mean_value)) +
        geom_tile() +
        scale_fill_viridis_c(option = "magma", name = gsub("_norm", "", toupper(idx))) +
        scale_x_continuous(breaks = seq(0, 23, 4)) +
        labs(
          title = gsub("_norm", "", toupper(idx)),
          x = "Hour",
          y = "Date"
        ) +
        theme_minimal(base_size = 10) +
        theme(
          plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
          axis.text.y = element_text(size = 8),
          legend.position = "right"
        )
      
      plot_list[[idx]] <- p
    }
    
    combined_plot <- wrap_plots(plot_list, ncol = 2) +
      plot_annotation(
        title = "Temporal Heatmaps of Acoustic Indices",
        theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
      )
    
    output_files$temporal_heatmaps <- save_plot_safe(combined_plot, "comparison_temporal_heatmaps.png",
                                                      figure_width, figure_height * 1.2, dpi, "combined")
  }
  
  # 6. Site Comparison
  if ("site_comparison" %in% plot_types && "site" %in% names(combined_data)) {
    message("Creating site comparison plot...")
    
    n_sites <- n_distinct(combined_data$site, na.rm = TRUE)
    
    if (n_sites > 1) {
      site_data <- combined_data %>%
        select(site, all_of(plot_index_cols)) %>%
        pivot_longer(-site, names_to = "index", values_to = "value")
      
      p <- ggplot(site_data, aes(x = site, y = value, fill = index)) +
        geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
        scale_fill_viridis_d(option = "turbo", name = "Index",
                            labels = toupper(gsub("_norm", "", unique(site_data$index)))) +
        labs(
          title = "Acoustic Indices by Site",
          x = "Site",
          y = ifelse(normalize, "Normalized Value", "Index Value")
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 16),
          axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        facet_wrap(~index, scales = "free_y", ncol = 2,
                   labeller = labeller(index = ~toupper(gsub("_norm", "", .x))))
      
      output_files$site_comparison <- save_plot_safe(p, "comparison_site_boxplots.png",
                                                      figure_width, figure_height, dpi, "separate")
    } else {
      message("  ⚠ Skipping site comparison: Only one site detected")
    }
  }
  
  # 7. Site Radar Plot
  if ("site_radar" %in% plot_types && "site" %in% names(combined_data)) {
    message("Creating site radar plot...")
    
    n_sites <- n_distinct(combined_data$site, na.rm = TRUE)
    
    if (n_sites > 1 && n_sites <= 6) {  # Reasonable number for radar plot
      
      # Calculate mean values by site
      radar_data <- combined_data %>%
        select(site, all_of(plot_index_cols)) %>%
        group_by(site) %>%
        summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
        pivot_longer(-site, names_to = "index", values_to = "value") %>%
        mutate(index = toupper(gsub("_norm", "", index)))
      
      # Normalize for radar if not already normalized
      if (!normalize) {
        radar_data <- radar_data %>%
          group_by(index) %>%
          mutate(value = (value - min(value, na.rm = TRUE)) / 
                        (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))) %>%
          ungroup()
      }
      
      p <- ggplot(radar_data, aes(x = index, y = value, group = site, color = site)) +
        geom_polygon(aes(fill = site), alpha = 0.2, size = 1) +
        geom_point(size = 3) +
        coord_polar() +
        scale_y_continuous(limits = c(0, 1)) +
        scale_color_viridis_d(option = "turbo", name = "Site") +
        scale_fill_viridis_d(option = "turbo", name = "Site") +
        labs(
          title = "Site Comparison Across Acoustic Indices",
          x = NULL,
          y = "Normalized Value"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
          axis.text.y = element_text(size = 8),
          legend.position = "right"
        )
      
      output_files$site_radar <- save_plot_safe(p, "comparison_site_radar.png",
                                                 12, 10, dpi, "separate")
    } else if (n_sites > 6) {
      message("  ⚠ Skipping site radar: Too many sites (", n_sites, ") for clear visualization")
    } else {
      message("  ⚠ Skipping site radar: Only one site detected")
    }
  }
  
  # 8. Species Correlation
  if ("species_correlation" %in% plot_types && "n_species" %in% names(combined_data)) {
    message("Creating species correlation plot...")
    
    species_data <- combined_data %>%
      select(n_species, all_of(plot_index_cols)) %>%
      drop_na(n_species) %>%
      pivot_longer(-n_species, names_to = "index", values_to = "value")
    
    if (nrow(species_data) > 0) {
      # Calculate correlations
      cors <- species_data %>%
        group_by(index) %>%
        summarise(
          cor = cor(n_species, value, use = "complete.obs"),
          .groups = "drop"
        ) %>%
        mutate(label = paste0("r = ", round(cor, 2)))
      
      species_data <- species_data %>%
        left_join(cors, by = "index")
      
      p <- ggplot(species_data, aes(x = value, y = n_species)) +
        geom_point(alpha = 0.3, color = "steelblue", size = 1.5) +
        geom_smooth(method = "lm", color = "darkred", size = 1) +
        geom_text(data = cors, aes(x = Inf, y = Inf, label = label),
                 hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold") +
        facet_wrap(~index, scales = "free_x", ncol = 2,
                   labeller = labeller(index = ~toupper(gsub("_norm", "", .x)))) +
        labs(
          title = "Acoustic Indices vs Species Richness",
          x = ifelse(normalize, "Normalized Index Value", "Index Value"),
          y = "Number of Species (BirdNET)"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 16)
        )
      
      output_files$species_correlation <- save_plot_safe(p, "comparison_species_correlation.png",
                                                          figure_width, figure_height, dpi, "separate")
    } else {
      message("  ⚠ Skipping species correlation: No species data available")
    }
  }
  
  # 9. PCA Biplot
  if ("pca_biplot" %in% plot_types) {
    message("Creating PCA biplot...")
    
    pca_data <- combined_data %>%
      select(all_of(plot_index_cols)) %>%
      drop_na()
    
    if (nrow(pca_data) >= 10 && ncol(pca_data) >= 2) {
      pca_result <- prcomp(pca_data, scale. = TRUE)
      
      # Variance explained
      var_explained <- summary(pca_result)$importance[2, 1:2] * 100
      
      # PCA scores
      pca_scores <- as_tibble(pca_result$x[, 1:2]) %>%
        bind_cols(combined_data %>% select(site) %>% slice(1:nrow(pca_data)))
      
      # Variable loadings
      loadings <- as_tibble(pca_result$rotation[, 1:2]) %>%
        mutate(variable = toupper(gsub("_norm", "", rownames(pca_result$rotation))))
      
      p <- ggplot() +
        geom_point(data = pca_scores, aes(x = PC1, y = PC2, color = site),
                  alpha = 0.5, size = 2) +
        geom_segment(data = loadings, 
                    aes(x = 0, y = 0, xend = PC1 * 3, yend = PC2 * 3),
                    arrow = arrow(length = unit(0.3, "cm")), 
                    color = "darkred", size = 1) +
        geom_text_repel(data = loadings,
                       aes(x = PC1 * 3, y = PC2 * 3, label = variable),
                       color = "darkred", fontface = "bold", size = 5) +
        scale_color_viridis_d(option = "turbo", name = "Site") +
        labs(
          title = "PCA Biplot of Acoustic Indices",
          x = paste0("PC1 (", round(var_explained[1], 1), "%)"),
          y = paste0("PC2 (", round(var_explained[2], 1), "%)")
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 16)
        )
      
      output_files$pca_biplot <- save_plot_safe(p, "comparison_pca_biplot.png",
                                                 12, 10, dpi, "separate")
      
      # Save PCA results
      pca_summary <- tibble(
        Component = paste0("PC", 1:length(pca_result$sdev)),
        Variance = pca_result$sdev^2,
        Proportion = summary(pca_result)$importance[2, ],
        Cumulative = summary(pca_result)$importance[3, ]
      )
      write_csv(pca_summary, file.path(output_path, "pca_summary.csv"))
      
    } else {
      message("  ⚠ Skipping PCA: Insufficient complete cases (n = ", nrow(pca_data), ")")
    }
  }
  
  # 10. Index Distributions
  if ("index_distributions" %in% plot_types) {
    message("Creating index distributions plot...")
    
    dist_data <- combined_data %>%
      select(all_of(plot_index_cols)) %>%
      pivot_longer(everything(), names_to = "index", values_to = "value") %>%
      mutate(index = toupper(gsub("_norm", "", index)))
    
    p <- ggplot(dist_data, aes(x = value, fill = index)) +
      geom_histogram(bins = 50, alpha = 0.7, color = "white") +
      geom_vline(data = dist_data %>% group_by(index) %>% 
                      summarise(mean = mean(value, na.rm = TRUE)),
                aes(xintercept = mean, color = index), 
                linetype = "dashed", size = 1) +
      facet_wrap(~index, scales = "free", ncol = 2) +
      scale_fill_viridis_d(option = "turbo") +
      scale_color_viridis_d(option = "turbo") +
      labs(
        title = "Distribution of Acoustic Indices",
        x = ifelse(normalize, "Normalized Value", "Index Value"),
        y = "Count"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "none"
      )
    
    output_files$index_distributions <- save_plot_safe(p, "comparison_index_distributions.png",
                                                        figure_width, figure_height, dpi, "separate")
  }
  
  # Summary message
  message("\n========================================")
  message("Comparison Analysis Complete!")
  message("========================================")
  message(paste("Output directory:", output_path))
  message("\nGenerated files:")
  message("  - combined_indices.csv")
  message("  - correlation_matrix.csv")
  message("  - summary_statistics.csv")
  if (n_mismatches > 5) message("  - missing_files_log.txt")
  message(paste("  -", length(output_files), "visualization(s)"))
  message("========================================\n")
  
  # Return results
  return(invisible(list(
    combined_data = combined_data,
    correlation_matrix = cor_matrix,
    summary_stats = summary_stats,
    missing_files = missing_files,
    n_mismatches = n_mismatches,
    output_files = output_files
  )))
}


#' ============================================================================
#' USAGE GUIDE
#' ============================================================================
#'
#' BASIC USAGE:
#' ------------
#' results <- compare_acoustic_indices(
#'   aci_path = "/path/to/aci_analysis",
#'   bi_path = "/path/to/bi_analysis",
#'   adi_path = "/path/to/adi_analysis",
#'   h_path = "/path/to/h_analysis",
#'   output_path = "/path/to/comparison_outputs"
#' )
#'
#' COMPARING SUBSET OF INDICES:
#' ----------------------------
#' # Only ACI and BI (set others to NULL)
#' results <- compare_acoustic_indices(
#'   aci_path = "/path/to/aci_analysis",
#'   bi_path = "/path/to/bi_analysis",
#'   adi_path = NULL,
#'   h_path = NULL,
#'   output_path = "/path/to/comparison_outputs"
#' )
#'
#' SELECTING SPECIFIC VISUALIZATIONS:
#' -----------------------------------
#' Available plot types:
#'   - "correlation_matrix"   : Heatmap of correlations
#'   - "pairwise_scatter"     : Pairwise scatterplots (GGally)
#'   - "temporal_overlay"     : All indices by hour (overlay)
#'   - "temporal_trends"      : All indices over time
#'   - "temporal_heatmaps"    : Side-by-side heatmaps
#'   - "site_comparison"      : Boxplots by site
#'   - "site_radar"           : Radar/spider plot by site
#'   - "species_correlation"  : Indices vs species richness
#'   - "pca_biplot"           : PCA with variable loadings
#'   - "index_distributions"  : Histograms of each index
#'   - "all"                  : Generate all plots
#'
#' Example:
#' results <- compare_acoustic_indices(
#'   aci_path = "/path/to/aci",
#'   bi_path = "/path/to/bi",
#'   adi_path = "/path/to/adi",
#'   h_path = "/path/to/h",
#'   output_path = "/path/to/outputs",
#'   plot_types = c("correlation_matrix", "pairwise_scatter", "species_correlation")
#' )
#'
#' FILTERING DATA:
#' ---------------
#' # Filter by date range
#' results <- compare_acoustic_indices(
#'   aci_path = "/path/to/aci",
#'   bi_path = "/path/to/bi",
#'   adi_path = "/path/to/adi",
#'   h_path = "/path/to/h",
#'   output_path = "/path/to/outputs",
#'   filter_date_range = c(as.Date("2024-05-01"), as.Date("2024-08-31"))
#' )
#'
#' # Filter by sites
#' results <- compare_acoustic_indices(
#'   aci_path = "/path/to/aci",
#'   bi_path = "/path/to/bi",
#'   adi_path = "/path/to/adi",
#'   h_path = "/path/to/h",
#'   output_path = "/path/to/outputs",
#'   filter_sites = c("Site-A", "Site-B", "Site-C")
#' )
#'
#' # Multiple filters
#' results <- compare_acoustic_indices(
#'   aci_path = "/path/to/aci",
#'   bi_path = "/path/to/bi",
#'   adi_path = "/path/to/adi",
#'   h_path = "/path/to/h",
#'   output_path = "/path/to/outputs",
#'   filter_date_range = c(as.Date("2024-06-01"), as.Date("2024-06-30")),
#'   filter_sites = c("Site-A"),
#'   filter_deployments = c("Site-A_config_20240601")
#' )
#'
#' NORMALIZATION:
#' --------------
#' # Normalize all indices to 0-1 scale before comparison
#' results <- compare_acoustic_indices(
#'   aci_path = "/path/to/aci",
#'   bi_path = "/path/to/bi",
#'   adi_path = "/path/to/adi",
#'   h_path = "/path/to/h",
#'   output_path = "/path/to/outputs",
#'   normalize = TRUE  # Recommended for comparing indices with different scales
#' )
#'
#' OUTPUT OPTIONS:
#' ---------------
#' # Only combined figures
#' results <- compare_acoustic_indices(
#'   aci_path = "/path/to/aci",
#'   bi_path = "/path/to/bi",
#'   adi_path = "/path/to/adi",
#'   h_path = "/path/to/h",
#'   output_path = "/path/to/outputs",
#'   create_separate_figures = FALSE,
#'   create_combined_figures = TRUE
#' )
#'
#' # Both separate and combined
#' results <- compare_acoustic_indices(
#'   aci_path = "/path/to/aci",
#'   bi_path = "/path/to/bi",
#'   adi_path = "/path/to/adi",
#'   h_path = "/path/to/h",
#'   output_path = "/path/to/outputs",
#'   create_separate_figures = TRUE,
#'   create_combined_figures = TRUE
#' )
#'
#' ACCESSING RESULTS:
#' ------------------
#' # Combined data frame
#' all_data <- results$combined_data
#' head(all_data)
#'
#' # Correlation matrix
#' print(results$correlation_matrix)
#'
#' # Summary statistics
#' print(results$summary_stats)
#'
#' # Check for missing files
#' if (results$n_mismatches > 0) {
#'   print(results$missing_files)
#' }
#'
#' COMPLETE EXAMPLE:
#' -----------------
#' results <- compare_acoustic_indices(
#'   aci_path = "/data/analysis/aci_2024",
#'   bi_path = "/data/analysis/bi_2024",
#'   adi_path = "/data/analysis/adi_2024",
#'   h_path = "/data/analysis/h_2024",
#'   output_path = "/data/analysis/comparison_2024",
#'   plot_types = "all",
#'   filter_date_range = c(as.Date("2024-05-01"), as.Date("2024-08-31")),
#'   normalize = TRUE,
#'   figure_width = 16,
#'   figure_height = 12,
#'   dpi = 300,
#'   create_separate_figures = TRUE,
#'   create_combined_figures = TRUE
#' )
#'
#' # View correlation matrix
#' print(results$correlation_matrix)
#'
#' # Which index best correlates with species richness?
#' if ("n_species" %in% names(results$combined_data)) {
#'   species_cors <- results$combined_data %>%
#'     select(n_species, aci, bi, adi, h) %>%
#'     cor(use = "complete.obs")
#'   
#'   print(species_cors["n_species", ])
#' }
#'
#' # Further custom analysis
#' library(tidyverse)
#'
#' # Which index has highest values during dawn chorus?
#' results$combined_data %>%
#'   filter(time_period == "Dawn") %>%
#'   summarise(
#'     mean_aci = mean(aci, na.rm = TRUE),
#'     mean_bi = mean(bi, na.rm = TRUE),
#'     mean_adi = mean(adi, na.rm = TRUE),
#'     mean_h = mean(h, na.rm = TRUE)
#'   )
#'
#' ============================================================================
