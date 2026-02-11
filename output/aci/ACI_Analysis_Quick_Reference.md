# ACI Analysis Function - Quick Reference Guide

## Installation Requirements

Before using the function, install required packages:

```r
install.packages(c("tidyverse", "tuneR", "seewave", "lubridate", 
                   "furrr", "future", "ggridges", "patchwork", "viridis"))
```

## Basic Usage

```r
# Source the function
source("run_aci_analysis.R")

# Run with default settings
results <- run_aci_analysis(
  audio_path = "/path/to/your/recordings",
  output_path = "/path/to/save/outputs"
)
```

## Visualization Options

You can select from 9 different visualization types:

| Plot Type | Description | Best For |
|-----------|-------------|----------|
| `daily_pattern` | ACI by hour with trend line | Understanding daily cycles |
| `temporal_trend` | ACI over dates | Long-term trends |
| `heatmap_daily` | Hour × Date heatmap | Identifying patterns over time |
| `heatmap_seasonal` | Hour × Month heatmap | Seasonal patterns |
| `boxplot_site` | Compare across sites | Multi-site comparisons |
| `boxplot_period` | Dawn/Day/Dusk/Night | Diel period comparisons |
| `ridge_month` | Distribution by month | Monthly variation |
| `species_correlation` | ACI vs Species Richness | Validating ACI with biodiversity |
| `distribution` | Histogram of ACI values | Overall data distribution |

### Examples:

```r
# Generate all plots
results <- run_aci_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  plot_types = "all"
)

# Select specific plots
results <- run_aci_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  plot_types = c("daily_pattern", "heatmap_daily", "species_correlation")
)

# Just one plot
results <- run_aci_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  plot_types = "daily_pattern"
)
```

## Common Parameter Adjustments

### For Higher Frequency Resolution
```r
results <- run_aci_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  wl = 1024,  # Increase from 512
  flim = c(2, 10)  # Focus on typical bird range
)
```

### For Different Timezone
```r
results <- run_aci_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  timezone = "America/New_York"
)
```

### For Faster Processing (More Cores)
```r
results <- run_aci_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  n_cores = 10  # Use more cores if available
)
```

### For Publication-Quality Figures
```r
results <- run_aci_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  figure_width = 16,
  figure_height = 12,
  dpi = 600
)
```

## Output Files

The function creates these files in your output_path:

### Data Files:
- `aci_raw_results.csv` - One row per WAV file with ACI value
- `aci_summary_by_hour.csv` - Mean/SD/median ACI for each hour
- `aci_summary_by_date.csv` - Mean/SD/median ACI for each date
- `aci_summary_by_site.csv` - Mean/SD/median ACI for each site
- `aci_summary_by_deployment.csv` - Mean/SD/median ACI for each deployment
- `aci_analysis_log.txt` - Log of processing errors/warnings

### Visualization Files:
Each selected plot type generates a PNG file:
- `aci_daily_pattern.png`
- `aci_temporal_trend.png`
- `aci_heatmap_daily.png`
- `aci_heatmap_seasonal.png`
- `aci_boxplot_site.png`
- `aci_boxplot_period.png`
- `aci_ridge_month.png`
- `aci_species_correlation.png`
- `aci_distribution.png`

## Accessing Results in R

```r
# Run analysis
results <- run_aci_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs"
)

# Access raw data
aci_data <- results$aci_results
head(aci_data)

# Access summaries
hourly_stats <- results$summary_stats$by_hour
daily_stats <- results$summary_stats$by_date
site_stats <- results$summary_stats$by_site

# Check processing success
cat("Files processed:", results$n_files_processed, "\n")
cat("Successful:", results$n_files_successful, "\n")
cat("Failed:", results$n_files_failed, "\n")

# View log
file.show(results$log_file)
```

## Custom Analysis After Running

```r
library(tidyverse)

# Filter to specific site
site_data <- results$aci_results %>%
  filter(site == "MySite")

# Calculate custom statistics
custom_summary <- results$aci_results %>%
  group_by(site, month) %>%
  summarise(
    mean_aci = mean(aci, na.rm = TRUE),
    n = n()
  )

# Create custom plot
results$aci_results %>%
  filter(hour >= 5, hour <= 9) %>%  # Dawn chorus
  ggplot(aes(x = date, y = aci, color = site)) +
  geom_line() +
  labs(title = "Dawn Chorus ACI Trends by Site") +
  theme_minimal()
```

## Troubleshooting

### "Package X is required but not installed"
```r
install.packages("X")  # Replace X with package name
```

### "No WAV files found"
- Check that audio_path is correct
- Verify files have .WAV or .wav extension
- Check folder permissions

### Too many files failing
- Check the log file: `file.show(results$log_file)`
- Verify WAV files are not corrupted
- Check sample rate (should be 48kHz for your data)

### Memory issues with large datasets
```r
# Reduce number of cores
results <- run_aci_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  n_cores = 2  # Use fewer cores
)
```

### BirdNET integration not working
- Verify BirdNET files end with `.BirdNET.selection.table.txt`
- Check that BirdNET folder contains "birdnetLocal" in name
- Set `integrate_birdnet = FALSE` if not needed

## Complete Working Example

```r
# Source function
source("run_aci_analysis.R")

# Run comprehensive analysis
results <- run_aci_analysis(
  audio_path = "/data/recordings/Site-A",
  output_path = "/data/analysis/aci_site_a_2024",
  timezone = "America/Los_Angeles",
  wl = 512,
  ovlp = 50,
  flim = c(0, 12),
  n_cores = 8,
  plot_types = c("daily_pattern", "heatmap_daily", "boxplot_period", 
                 "species_correlation", "distribution"),
  figure_width = 14,
  figure_height = 10,
  dpi = 300,
  integrate_birdnet = TRUE
)

# View hourly patterns
print(results$summary_stats$by_hour)

# Create custom analysis
library(tidyverse)

# Examine dawn chorus specifically
dawn_data <- results$aci_results %>%
  filter(time_period == "Dawn") %>%
  group_by(date) %>%
  summarise(mean_dawn_aci = mean(aci, na.rm = TRUE))

# Plot
ggplot(dawn_data, aes(x = date, y = mean_dawn_aci)) +
  geom_line(color = "coral", size = 1) +
  geom_smooth(method = "loess", color = "darkred") +
  labs(
    title = "Dawn Chorus Acoustic Complexity Over Time",
    x = "Date",
    y = "Mean ACI (Dawn Period)"
  ) +
  theme_minimal()
```

## Processing Multiple Sites

If you have multiple sites, process them separately:

```r
sites <- c("Site-A", "Site-B", "Site-C")

all_results <- list()

for (site in sites) {
  cat("Processing", site, "...\n")
  
  all_results[[site]] <- run_aci_analysis(
    audio_path = paste0("/data/recordings/", site),
    output_path = paste0("/data/analysis/aci_", tolower(site)),
    plot_types = c("daily_pattern", "temporal_trend", "distribution")
  )
}

# Combine results
combined_aci <- map_dfr(all_results, ~.x$aci_results)

# Save combined
write_csv(combined_aci, "/data/analysis/aci_all_sites_combined.csv")

# Cross-site comparison
combined_aci %>%
  group_by(site, hour) %>%
  summarise(mean_aci = mean(aci, na.rm = TRUE)) %>%
  ggplot(aes(x = hour, y = mean_aci, color = site)) +
  geom_line(size = 1.2) +
  theme_minimal()
```
