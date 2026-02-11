# Acoustic Diversity Index Analysis Function - Quick Reference Guide

## Installation Requirements

Before using the function, install required packages:

```r
install.packages(c("tidyverse", "tuneR", "soundecology", "lubridate", 
                   "furrr", "future", "ggridges", "patchwork", "viridis"))
```

## What is the Acoustic Diversity Index?

The **Acoustic Diversity Index (ADI)** is based on the Shannon diversity index and measures how evenly sound energy is distributed across frequency bands.

**ADI ranges from 0 to 1:**
- **ADI = 1.0**: Sound energy perfectly evenly distributed (maximum diversity)
- **ADI = 0.0**: All sound in one frequency band (minimum diversity)
- **Higher ADI = More diverse acoustic environment**

## How ADI is Calculated

1. Divide frequency spectrum into bands (based on `freq_step`)
2. Calculate proportion of energy in each band
3. Apply Shannon index: H = -Σ(pi × ln(pi))
4. Normalize to 0-1 scale

Only bands above `db_threshold` are considered "occupied" and included.

## Default Parameters (Optimized for Bird Studies)

Based on best practices from Villanueva-Rivera et al. (2011):

| Parameter | Default | Reason |
|-----------|---------|--------|
| `max_freq` | 10000 Hz | Captures most bird vocalizations, excludes ultrasonic noise |
| `db_threshold` | -50 dB | Standard threshold for "occupied" frequency bands |
| `freq_step` | 1000 Hz | 1 kHz bands provide good resolution for bird diversity |

## Basic Usage

```r
# Source the function
source("run_adi_analysis.R")

# Run with default settings
results <- run_adi_analysis(
  audio_path = "/path/to/your/recordings",
  output_path = "/path/to/save/outputs"
)
```

## Visualization Options

Same 9 visualization types as ACI and BI analyses:

| Plot Type | Description | Best For |
|-----------|-------------|----------|
| `daily_pattern` | ADI by hour with trend line | Understanding daily cycles |
| `temporal_trend` | ADI over dates | Long-term trends |
| `heatmap_daily` | Hour × Date heatmap | Identifying patterns over time |
| `heatmap_seasonal` | Hour × Month heatmap | Seasonal patterns |
| `boxplot_site` | Compare across sites | Multi-site comparisons |
| `boxplot_period` | Dawn/Day/Dusk/Night | Diel period comparisons |
| `ridge_month` | Distribution by month | Monthly variation |
| `species_correlation` | ADI vs Species Richness | Validating ADI with biodiversity |
| `distribution` | Histogram of ADI values | Overall data distribution |

### Examples:

```r
# Generate all plots
results <- run_adi_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  plot_types = "all"
)

# Select specific plots
results <- run_adi_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  plot_types = c("daily_pattern", "heatmap_daily", "species_correlation")
)
```

## Common Parameter Adjustments

### For Finer Frequency Resolution
```r
results <- run_adi_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  freq_step = 500  # Smaller bands = finer resolution
)
```

### For High-Frequency Species
```r
results <- run_adi_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  max_freq = 12000  # Extended upper limit
)
```

### For Quieter Soundscapes (include more subtle sounds)
```r
results <- run_adi_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  db_threshold = -55  # Lower threshold includes quieter sounds
)
```

### For Louder Environments (focus on prominent sounds)
```r
results <- run_adi_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  db_threshold = -45  # Higher threshold focuses on louder sounds
)
```

## Output Files

The function creates these files in your output_path:

### Data Files:
- `adi_raw_results.csv` - One row per WAV file with ADI value (0-1)
- `adi_summary_by_hour.csv` - Mean/SD/median ADI for each hour
- `adi_summary_by_date.csv` - Mean/SD/median ADI for each date
- `adi_summary_by_site.csv` - Mean/SD/median ADI for each site
- `adi_summary_by_deployment.csv` - Mean/SD/median ADI for each deployment
- `adi_analysis_log.txt` - Log of processing errors/warnings

### Visualization Files:
Each selected plot type generates a PNG file:
- `adi_daily_pattern.png`
- `adi_temporal_trend.png`
- `adi_heatmap_daily.png`
- `adi_heatmap_seasonal.png`
- `adi_boxplot_site.png`
- `adi_boxplot_period.png`
- `adi_ridge_month.png`
- `adi_species_correlation.png`
- `adi_distribution.png`

## Accessing Results in R

```r
# Run analysis
results <- run_adi_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs"
)

# Access raw data
adi_data <- results$adi_results
head(adi_data)

# Access summaries
hourly_stats <- results$summary_stats$by_hour
daily_stats <- results$summary_stats$by_date
site_stats <- results$summary_stats$by_site

# Check processing success
cat("Files processed:", results$n_files_processed, "\n")
cat("Successful:", results$n_files_successful, "\n")
cat("Failed:", results$n_files_failed, "\n")
```

## Interpretation Guidelines

### Typical ADI Values:

| Soundscape Type | Expected ADI Range | Interpretation |
|----------------|-------------------|----------------|
| Single species dominant | 0.1 - 0.3 | Low diversity, sound concentrated |
| Quiet forest | 0.3 - 0.5 | Moderate diversity |
| Active chorus (few species) | 0.4 - 0.6 | Moderate-high diversity |
| Diverse dawn chorus | 0.6 - 0.8 | High diversity, many species |
| Very complex soundscape | 0.8 - 1.0 | Very high diversity, even distribution |

### Expected Daily Patterns:

- **Dawn Chorus (5-9 AM)**: Highest ADI - many species vocalizing
- **Midday (9 AM-5 PM)**: Lower ADI - reduced vocal activity
- **Dusk (5-9 PM)**: Moderate ADI - some evening activity
- **Night**: Variable ADI depending on nocturnal species

### What Affects ADI:

**Increases ADI:**
- Multiple species vocalizing across different frequencies
- Balanced contributions from different sound sources
- Complex acoustic environments

**Decreases ADI:**
- Dominance by single loud species
- Sounds concentrated in narrow frequency range
- Quiet periods with minimal activity

**Can Inflate ADI:**
- Broadband background noise (wind, rain, traffic)
- Recording artifacts across frequencies
- Gain settings that amplify quiet bands

## Comparing ADI with Other Indices

If you've run ACI, BI, and ADI on the same dataset:

```r
# Load all result sets
aci_data <- read_csv("path/to/aci_raw_results.csv")
bi_data <- read_csv("path/to/bi_raw_results.csv")
adi_data <- read_csv("path/to/adi_raw_results.csv")

# Merge all indices
all_indices <- aci_data %>%
  select(file, site, datetime_local, aci) %>%
  left_join(bi_data %>% select(file, bi), by = "file") %>%
  left_join(adi_data %>% select(file, adi), by = "file")

# Correlation matrix
library(corrr)
all_indices %>%
  select(aci, bi, adi) %>%
  correlate() %>%
  shave() %>%
  fashion()

# Pairwise scatterplots
library(GGally)
all_indices %>%
  select(aci, bi, adi) %>%
  ggpairs() +
  theme_minimal()

# Compare temporal patterns
all_indices %>%
  mutate(hour = hour(datetime_local)) %>%
  group_by(hour) %>%
  summarise(
    mean_aci = mean(aci, na.rm = TRUE),
    mean_bi = mean(bi, na.rm = TRUE),
    mean_adi = mean(adi, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with("mean_"), names_to = "index", values_to = "value") %>%
  ggplot(aes(x = hour, y = value, color = index)) +
  geom_line(size = 1.2) +
  facet_wrap(~index, scales = "free_y", ncol = 1) +
  labs(title = "Comparison of Acoustic Indices by Hour") +
  theme_minimal()
```

## Complete Working Example

```r
# Source function
source("run_adi_analysis.R")

# Run comprehensive analysis
results <- run_adi_analysis(
  audio_path = "/data/recordings/Site-A",
  output_path = "/data/analysis/adi_site_a_2024",
  timezone = "America/Los_Angeles",
  max_freq = 10000,
  db_threshold = -50,
  freq_step = 1000,
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

# Examine dawn chorus diversity
dawn_data <- results$adi_results %>%
  filter(time_period == "Dawn") %>%
  group_by(date) %>%
  summarise(mean_dawn_adi = mean(adi, na.rm = TRUE))

# Plot
ggplot(dawn_data, aes(x = date, y = mean_dawn_adi)) +
  geom_line(color = "coral", size = 1) +
  geom_smooth(method = "loess", color = "darkred") +
  labs(
    title = "Dawn Chorus Acoustic Diversity Over Time",
    x = "Date",
    y = "Mean ADI (Dawn Period)"
  ) +
  theme_minimal()
```

## Validation with Species Richness

ADI should correlate with species richness if it's capturing biological diversity:

```r
# Compare ADI with BirdNET species counts
if (results$birdnet_integrated) {
  
  # Scatterplot
  results$adi_results %>%
    filter(!is.na(adi), !is.na(n_species)) %>%
    ggplot(aes(x = adi, y = n_species)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm") +
    labs(
      title = "ADI vs Species Richness",
      x = "Acoustic Diversity Index",
      y = "Number of Species (BirdNET)"
    ) +
    theme_minimal()
  
  # Calculate correlation
  cor_test <- cor.test(
    results$adi_results$adi, 
    results$adi_results$n_species,
    use = "complete.obs"
  )
  
  print(cor_test)
}
```

## Troubleshooting

### ADI values all very similar (low variance)
- Check `db_threshold` - may need adjustment
- Verify `freq_step` is appropriate
- Check for consistent background noise affecting all recordings

### ADI values unexpectedly high
- May be picking up broadband noise (wind, rain, traffic)
- Consider increasing `db_threshold` to focus on louder sounds
- Check recording quality and gain settings

### ADI values unexpectedly low
- Single species may be dominating
- Check `db_threshold` - may be too high
- Verify frequency range captures target species

### Memory issues
```r
# Reduce number of cores
results <- run_adi_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/outputs",
  n_cores = 2
)
```

## When to Use ADI vs Other Indices

**Use ADI when:**
- Interested in evenness of sound distribution
- Want to detect changes in acoustic diversity
- Comparing soundscapes with different dominant species
- Assessing habitat quality based on sound diversity

**ADI complements other indices:**
- **ACI**: Measures temporal variability (good for detecting animal sounds)
- **BI**: Measures total bioacoustic activity (good for abundance)
- **ADI**: Measures frequency diversity (good for species richness)

**Use all three together** for comprehensive acoustic assessment!

## Key References

**Original Paper:**
- Villanueva-Rivera, L. J., et al. (2011). A primer of acoustic analysis for landscape ecologists. *Landscape Ecology*, 26(9), 1233-1246.

**Methodology:**
- Pieretti, N., et al. (2011). A new methodology to infer the singing activity of an avian community. *Ecological Indicators*, 11(3), 868-873.

**R Package:**
- Sueur, J., et al. (2014). Acoustic indices for biodiversity assessment and landscape investigation. *Acta Acustica united with Acustica*, 100(4), 772-781.

**Validation:**
- Fuller, S., et al. (2015). Connecting soundscape to landscape: Which acoustic index best describes landscape configuration? *Ecological Indicators*, 58, 207-215.

## Tips for Publication

When presenting ADI results:

1. **Report all parameters**: max_freq, db_threshold, freq_step
2. **Show frequency band analysis**: Number of active bands over time
3. **Validate with species data**: Correlation with field surveys or BirdNET
4. **Compare across habitat types**: ADI differences between sites
5. **Consider confounding factors**: Noise, weather, recording quality
6. **Use with other indices**: ADI alone is not sufficient - combine with ACI, BI
7. **Show temporal patterns**: Dawn chorus should show high ADI if functioning correctly

## Advanced: Understanding Frequency Bands

```r
# Example: 10000 Hz max_freq with 1000 Hz freq_step creates 10 bands:
# Band 1: 0-1000 Hz
# Band 2: 1000-2000 Hz
# Band 3: 2000-3000 Hz
# ...
# Band 10: 9000-10000 Hz

# ADI calculates Shannon index across these bands
# If energy is evenly distributed (each band has 10% of total), ADI ≈ 1.0
# If all energy in one band (one has 100%, others 0%), ADI ≈ 0.0
```
