# Bioacoustic Index Analysis Function - Quick Reference Guide

## Installation Requirements

Before using the function, install required packages:

```r
install.packages(c("tidyverse", "tuneR", "soundecology", "lubridate", 
                   "furrr", "future", "ggridges", "patchwork", "viridis"))
```

## What is the Bioacoustic Index?

The **Bioacoustic Index (BI)** measures the area under the curve of the mean frequency spectrum between specified frequency limits. It was developed to:
- Detect bird vocalizations in field recordings
- Distinguish biological sounds from anthropogenic noise  
- Provide a proxy for biodiversity and ecosystem health

**Higher BI = More biological activity/vocalizations**

## Default Parameters (Optimized for Bird Studies)

Based on best practices from the literature:

| Parameter | Default | Reason |
|-----------|---------|--------|
| `min_freq` | 2000 Hz | Excludes low-frequency noise (traffic, machinery) |
| `max_freq` | 8000 Hz | Captures most passerine bird vocalizations (2-8 kHz) |
| `fft_w` | 512 | Standard for 48 kHz recordings, balances frequency/time resolution |

## Basic Usage

```r
# Source the function
source("run_bi_analysis.R")

# Run with default settings
results <- run_bi_analysis(
  audio_path = "/path/to/your/recordings",
  output_path = "/path/to/save/outputs"
)
```

## Visualization Options

Same 9 visualization types as ACI analysis:

| Plot Type | Description | Best For |
|-----------|-------------|----------|
| `daily_pattern` | BI by hour with trend line | Understanding daily cycles |
| `temporal_trend` | BI over dates | Long-term trends |
| `heatmap_daily` | Hour × Date heatmap | Identifying patterns over time |
| `heatmap_seasonal` | Hour × Month heatmap | Seasonal patterns |
| `boxplot_site` | Compare across sites | Multi-site comparisons |
| `boxplot_period` | Dawn/Day/Dusk/Night | Diel period comparisons |
| `ridge_month` | Distribution by month | Monthly variation |
| `species_correlation` | BI vs Species Richness | Validating BI with biodiversity |
| `distribution` | Histogram of BI values | Overall data distribution |

### Examples:

```r
# Generate all plots
results <- run_bi_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  plot_types = "all"
)

# Select specific plots
results <- run_bi_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  plot_types = c("daily_pattern", "heatmap_daily", "species_correlation")
)
```

## Common Parameter Adjustments

### For High-Frequency Birds (warblers, etc.)
```r
results <- run_bi_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  min_freq = 2000,
  max_freq = 11000  # Extended upper limit
)
```

### For Different Taxa (adjust frequency range)
```r
# For amphibians (lower frequencies)
results <- run_bi_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  min_freq = 200,
  max_freq = 4000
)

# For bats (ultrasonic)
results <- run_bi_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  min_freq = 15000,
  max_freq = 24000
)
```

### For Higher Frequency Resolution
```r
results <- run_bi_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  fft_w = 1024  # Increase from 512
)
```

## Output Files

The function creates these files in your output_path:

### Data Files:
- `bi_raw_results.csv` - One row per WAV file with BI value
- `bi_summary_by_hour.csv` - Mean/SD/median BI for each hour
- `bi_summary_by_date.csv` - Mean/SD/median BI for each date
- `bi_summary_by_site.csv` - Mean/SD/median BI for each site
- `bi_summary_by_deployment.csv` - Mean/SD/median BI for each deployment
- `bi_analysis_log.txt` - Log of processing errors/warnings

### Visualization Files:
Each selected plot type generates a PNG file:
- `bi_daily_pattern.png`
- `bi_temporal_trend.png`
- `bi_heatmap_daily.png`
- `bi_heatmap_seasonal.png`
- `bi_boxplot_site.png`
- `bi_boxplot_period.png`
- `bi_ridge_month.png`
- `bi_species_correlation.png`
- `bi_distribution.png`

## Accessing Results in R

```r
# Run analysis
results <- run_bi_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs"
)

# Access raw data
bi_data <- results$bi_results
head(bi_data)

# Access summaries
hourly_stats <- results$summary_stats$by_hour
daily_stats <- results$summary_stats$by_date
site_stats <- results$summary_stats$by_site

# Check processing success
cat("Files processed:", results$n_files_processed, "\n")
cat("Successful:", results$n_files_successful, "\n")
cat("Failed:", results$n_files_failed, "\n")
```

## Custom Analysis After Running

```r
library(tidyverse)

# Examine dawn chorus
dawn_data <- results$bi_results %>%
  filter(time_period == "Dawn") %>%
  group_by(date) %>%
  summarise(mean_dawn_bi = mean(bi, na.rm = TRUE))

# Plot
ggplot(dawn_data, aes(x = date, y = mean_dawn_bi)) +
  geom_line(color = "coral", size = 1) +
  geom_smooth(method = "loess", color = "darkred") +
  labs(title = "Dawn Chorus Bioacoustic Index Over Time") +
  theme_minimal()
```

## Comparing BI with ACI

If you've run both analyses on the same dataset:

```r
# Load both result sets
aci_data <- read_csv("path/to/aci_raw_results.csv")
bi_data <- read_csv("path/to/bi_raw_results.csv")

# Merge by file
comparison <- aci_data %>%
  select(file, site, datetime_local, aci) %>%
  left_join(
    bi_data %>% select(file, bi),
    by = "file"
  )

# Correlation
cor(comparison$aci, comparison$bi, use = "complete.obs")

# Scatterplot
ggplot(comparison, aes(x = aci, y = bi)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred") +
  labs(
    title = "Relationship Between ACI and BI",
    x = "Acoustic Complexity Index (ACI)",
    y = "Bioacoustic Index (BI)"
  ) +
  theme_minimal()

# Time series comparison
comparison %>%
  pivot_longer(cols = c(aci, bi), names_to = "index", values_to = "value") %>%
  ggplot(aes(x = datetime_local, y = value, color = index)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~index, scales = "free_y", ncol = 1) +
  theme_minimal()
```

## Complete Working Example

```r
# Source function
source("run_bi_analysis.R")

# Run comprehensive analysis
results <- run_bi_analysis(
  audio_path = "/data/recordings/Site-A",
  output_path = "/data/analysis/bi_site_a_2024",
  timezone = "America/Los_Angeles",
  min_freq = 2000,
  max_freq = 8000,
  fft_w = 512,
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

# Compare with species richness
if (results$birdnet_integrated) {
  results$bi_results %>%
    ggplot(aes(x = bi, y = n_species)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm") +
    labs(
      title = "BI vs Species Richness",
      x = "Bioacoustic Index",
      y = "Number of Species (BirdNET)"
    ) +
    theme_minimal()
}
```

## Processing Multiple Sites

```r
sites <- c("Site-A", "Site-B", "Site-C")

all_results <- list()

for (site in sites) {
  cat("Processing", site, "...\n")
  
  all_results[[site]] <- run_bi_analysis(
    audio_path = paste0("/data/recordings/", site),
    output_path = paste0("/data/analysis/bi_", tolower(site)),
    plot_types = c("daily_pattern", "temporal_trend", "distribution")
  )
}

# Combine results
combined_bi <- map_dfr(all_results, ~.x$bi_results)

# Save combined
write_csv(combined_bi, "/data/analysis/bi_all_sites_combined.csv")

# Cross-site comparison
combined_bi %>%
  group_by(site, hour) %>%
  summarise(mean_bi = mean(bi, na.rm = TRUE)) %>%
  ggplot(aes(x = hour, y = mean_bi, color = site)) +
  geom_line(size = 1.2) +
  labs(title = "Bioacoustic Index by Site and Hour") +
  theme_minimal()
```

## Troubleshooting

### "Package 'soundecology' is required but not installed"
```r
install.packages("soundecology")
```

### BI values seem unusually high or low
- Check frequency range (min_freq and max_freq)
- Verify sample rate of recordings (should be 48 kHz)
- Review raw waveforms to check for clipping or corruption

### Memory issues
```r
# Reduce number of cores
results <- run_bi_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  n_cores = 2
)
```

## Interpretation Guidelines

### Expected BI Patterns:

**Daily Patterns:**
- **Dawn Chorus**: Highest BI (5-9 AM) - peak bird activity
- **Midday**: Moderate BI - reduced activity
- **Dusk**: Moderate-high BI - evening activity
- **Night**: Low BI (unless nocturnal species present)

**Seasonal Patterns:**
- **Breeding Season**: Higher BI - increased vocalizations
- **Migration**: Variable BI - transient species
- **Winter**: Lower BI - reduced activity

**Site Comparisons:**
- Intact habitats typically show higher BI than degraded sites
- More diverse sites generally have higher BI
- Urban sites often have lower BI due to noise interference

### Validation:

Always validate BI results with:
1. Traditional bird surveys (point counts)
2. Species richness data (e.g., from BirdNET)
3. Environmental covariates (habitat quality, weather)
4. Visual/manual inspection of spectrograms

### Caveats:

- BI is sensitive to recording gain settings
- Background noise can inflate or deflate BI
- Different species contribute differently to BI
- Loud individuals can dominate the index

## Key References

**Original Paper:**
- Boelman, N. T., et al. (2007). Multi-trophic invasion resistance in Hawaii: bioacoustics, field surveys, and airborne remote sensing. *Ecological Applications*, 17(8), 2137-2144.

**Validation Studies:**
- Mammides, C., et al. (2017). Do acoustic indices correlate with bird diversity? *Ecological Indicators*, 82, 470-477.

**Review:**
- Buxton, R. T., et al. (2018). Acoustic indices as rapid indicators of avian diversity in different land-use types in an Indian biodiversity hotspot. *Journal of Ecoacoustics*, 2, #GWPZVD.

## Tips for Publication

When presenting BI results:

1. **Always report parameters used**: min_freq, max_freq, fft_w
2. **Include validation**: Correlation with traditional surveys
3. **Show temporal patterns**: Heatmaps are particularly effective
4. **Compare with other indices**: ACI, ADI, H, etc.
5. **Account for confounds**: Weather, recording quality, gain settings
6. **Use consistent methods**: Same parameters across all recordings
