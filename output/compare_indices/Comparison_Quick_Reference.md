# Acoustic Indices Comparison Function - Quick Reference Guide

## Installation Requirements

```r
install.packages(c("tidyverse", "corrplot", "GGally", "patchwork", 
                   "viridis", "lubridate", "scales", "ggrepel"))
```

## What Does This Function Do?

The `compare_acoustic_indices()` function:
- Reads results from your ACI, BI, ADI, and H analyses
- Merges all indices into one dataset
- Calculates correlations and statistics
- Creates comprehensive comparison visualizations
- Exports publication-ready tables

## Basic Usage

```r
# Source the function
source("compare_acoustic_indices.R")

# Compare all four indices
results <- compare_acoustic_indices(
  aci_path = "/path/to/aci_analysis",
  bi_path = "/path/to/bi_analysis",
  adi_path = "/path/to/adi_analysis",
  h_path = "/path/to/h_analysis",
  output_path = "/path/to/comparison_outputs"
)
```

## Input Requirements

**You need:**
- Paths to the output directories from each index analysis
- At least **2 indices** (can run with any combination)

**Expected directory structure for each index:**
```
/path/to/aci_analysis/
  └── aci_raw_results.csv  ← This file is required

/path/to/bi_analysis/
  └── bi_raw_results.csv   ← This file is required

# etc.
```

## Comparing Subset of Indices

You don't need all four - set unused ones to `NULL`:

```r
# Only ACI and BI
results <- compare_acoustic_indices(
  aci_path = "/path/to/aci_analysis",
  bi_path = "/path/to/bi_analysis",
  adi_path = NULL,  # Not run
  h_path = NULL,    # Not run
  output_path = "/path/to/comparison_outputs"
)

# ACI, ADI, and H (no BI)
results <- compare_acoustic_indices(
  aci_path = "/path/to/aci_analysis",
  bi_path = NULL,
  adi_path = "/path/to/adi_analysis",
  h_path = "/path/to/h_analysis",
  output_path = "/path/to/comparison_outputs"
)
```

## Visualization Options

10 different comparison plot types available:

| Plot Type | What It Shows | Use When |
|-----------|---------------|----------|
| `correlation_matrix` | Heatmap of correlations | Understanding index relationships |
| `pairwise_scatter` | Scatterplots between all pairs | Detailed pair relationships |
| `temporal_overlay` | All indices by hour (overlaid) | Comparing daily patterns |
| `temporal_trends` | All indices over dates | Long-term trends |
| `temporal_heatmaps` | Side-by-side heatmaps | Visual comparison of patterns |
| `site_comparison` | Boxplots by site for each index | Multi-site studies |
| `site_radar` | Radar plot across indices | Site characterization |
| `species_correlation` | Indices vs species richness | Validation with biodiversity |
| `pca_biplot` | PCA with loadings | Dimensionality reduction |
| `index_distributions` | Histograms of each index | Understanding data distribution |

### Examples:

```r
# Generate all plots
results <- compare_acoustic_indices(
  aci_path = "/path/to/aci",
  bi_path = "/path/to/bi",
  adi_path = "/path/to/adi",
  h_path = "/path/to/h",
  output_path = "/path/to/outputs",
  plot_types = "all"
)

# Select specific plots
results <- compare_acoustic_indices(
  aci_path = "/path/to/aci",
  bi_path = "/path/to/bi",
  adi_path = "/path/to/adi",
  h_path = "/path/to/h",
  output_path = "/path/to/outputs",
  plot_types = c("correlation_matrix", "temporal_overlay", "species_correlation")
)
```

## Filtering Data

### By Date Range

```r
results <- compare_acoustic_indices(
  aci_path = "/path/to/aci",
  bi_path = "/path/to/bi",
  adi_path = "/path/to/adi",
  h_path = "/path/to/h",
  output_path = "/path/to/outputs",
  filter_date_range = c(as.Date("2024-05-01"), as.Date("2024-08-31"))
)
```

### By Sites

```r
results <- compare_acoustic_indices(
  aci_path = "/path/to/aci",
  bi_path = "/path/to/bi",
  adi_path = "/path/to/adi",
  h_path = "/path/to/h",
  output_path = "/path/to/outputs",
  filter_sites = c("Site-A", "Site-B")
)
```

### By Deployments

```r
results <- compare_acoustic_indices(
  aci_path = "/path/to/aci",
  bi_path = "/path/to/bi",
  adi_path = "/path/to/adi",
  h_path = "/path/to/h",
  output_path = "/path/to/outputs",
  filter_deployments = c("Site-A_config_20240601", "Site-B_config_20240615")
)
```

### Multiple Filters

```r
results <- compare_acoustic_indices(
  aci_path = "/path/to/aci",
  bi_path = "/path/to/bi",
  adi_path = "/path/to/adi",
  h_path = "/path/to/h",
  output_path = "/path/to/outputs",
  filter_date_range = c(as.Date("2024-06-01"), as.Date("2024-06-30")),
  filter_sites = c("Site-A"),
  plot_types = "all"
)
```

## Normalization

**When to normalize:**
- Comparing indices with very different scales
- Creating radar plots
- PCA analysis
- Multi-index composite scores

```r
results <- compare_acoustic_indices(
  aci_path = "/path/to/aci",
  bi_path = "/path/to/bi",
  adi_path = "/path/to/adi",
  h_path = "/path/to/h",
  output_path = "/path/to/outputs",
  normalize = TRUE  # Scales all indices to 0-1
)
```

**How it works:**
- Each index is scaled to 0-1 range: `(value - min) / (max - min)`
- Original values preserved in output CSV
- Normalized values used for plotting

## Output Files

The function creates these files in your `output_path`:

### Data Files:
- **`combined_indices.csv`** - Merged dataset with all indices
- **`correlation_matrix.csv`** - Correlation coefficients between indices
- **`summary_statistics.csv`** - Descriptive stats for each index
- **`pca_summary.csv`** - PCA variance explained (if PCA plot created)
- **`missing_files_log.txt`** - Log of file mismatches (if > 5 mismatches)

### Visualization Files:
Each requested plot type creates a PNG file:
- `comparison_correlation_matrix.png`
- `comparison_pairwise_scatter.png`
- `comparison_temporal_overlay.png`
- `comparison_temporal_trends.png`
- `comparison_temporal_heatmaps.png` (combined figure)
- `comparison_site_boxplots.png`
- `comparison_site_radar.png`
- `comparison_species_correlation.png`
- `comparison_pca_biplot.png`
- `comparison_index_distributions.png`

## Accessing Results in R

```r
# Run comparison
results <- compare_acoustic_indices(
  aci_path = "/path/to/aci",
  bi_path = "/path/to/bi",
  adi_path = "/path/to/adi",
  h_path = "/path/to/h",
  output_path = "/path/to/outputs"
)

# View combined data
head(results$combined_data)

# View correlation matrix
print(results$correlation_matrix)

# View summary statistics
print(results$summary_stats)

# Check for file mismatches
cat("Number of mismatched files:", results$n_mismatches, "\n")
if (results$n_mismatches > 0) {
  print(results$missing_files)
}
```

## Interpreting the Correlation Matrix

```r
# Which indices are most correlated?
print(results$correlation_matrix)

# Example output:
#       aci    bi   adi     h
# aci  1.00  0.65  0.78  0.82
# bi   0.65  1.00  0.55  0.71
# adi  0.78  0.55  1.00  0.85
# h    0.82  0.71  0.85  1.00

# Interpretation:
# - Strong correlation (0.7-1.0): Indices measure similar aspects
# - Moderate correlation (0.4-0.7): Related but distinct information
# - Weak correlation (0-0.4): Capture different aspects of soundscape
```

## Which Index is "Best"?

There's no single "best" index - they measure different things:

| Index | What It Measures | Best For |
|-------|------------------|----------|
| **ACI** | Temporal variability | Detecting animal sounds vs constant noise |
| **BI** | Biological activity | Overall bioacoustic activity/abundance |
| **ADI** | Frequency diversity | Species richness proxy |
| **H** | Overall complexity | Soundscape complexity (temporal × spectral) |

### Validation with Species Richness

```r
# Which index best predicts species richness?
species_cors <- results$combined_data %>%
  select(n_species, aci, bi, adi, h) %>%
  cor(use = "complete.obs")

print(species_cors["n_species", ])

# Example output:
#  n_species       aci        bi       adi         h
#      1.000     0.456     0.523     0.687     0.598

# In this example, ADI has the strongest correlation with species richness
```

## Custom Analysis Examples

### Which index shows strongest diel pattern?

```r
library(tidyverse)

results$combined_data %>%
  group_by(hour) %>%
  summarise(
    aci_cv = sd(aci, na.rm = TRUE) / mean(aci, na.rm = TRUE),
    bi_cv = sd(bi, na.rm = TRUE) / mean(bi, na.rm = TRUE),
    adi_cv = sd(adi, na.rm = TRUE) / mean(adi, na.rm = TRUE),
    h_cv = sd(h, na.rm = TRUE) / mean(h, na.rm = TRUE)
  ) %>%
  summarise(across(ends_with("_cv"), mean)) %>%
  pivot_longer(everything()) %>%
  arrange(desc(value))

# Higher coefficient of variation = stronger temporal pattern
```

### Site characterization by indices

```r
results$combined_data %>%
  group_by(site) %>%
  summarise(
    mean_aci = mean(aci, na.rm = TRUE),
    mean_bi = mean(bi, na.rm = TRUE),
    mean_adi = mean(adi, na.rm = TRUE),
    mean_h = mean(h, na.rm = TRUE),
    n_files = n()
  ) %>%
  arrange(desc(mean_h))

# Rank sites by acoustic complexity
```

### Composite index

```r
# Create a composite score (normalized indices required)
results$combined_data %>%
  mutate(
    composite_score = (aci_norm + bi_norm + adi_norm + h_norm) / 4
  ) %>%
  select(file, site, datetime_local, composite_score, everything()) %>%
  arrange(desc(composite_score))
```

## Complete Working Example

```r
# Source function
source("compare_acoustic_indices.R")

# Run comprehensive comparison
results <- compare_acoustic_indices(
  aci_path = "/data/analysis/aci_2024",
  bi_path = "/data/analysis/bi_2024",
  adi_path = "/data/analysis/adi_2024",
  h_path = "/data/analysis/h_2024",
  output_path = "/data/analysis/comparison_2024",
  plot_types = "all",
  filter_date_range = c(as.Date("2024-05-01"), as.Date("2024-08-31")),
  normalize = TRUE,
  figure_width = 16,
  figure_height = 12,
  dpi = 300
)

# Examine correlation matrix
print(results$correlation_matrix)

# Which index best predicts species richness?
species_cors <- results$combined_data %>%
  select(n_species, aci_norm, bi_norm, adi_norm, h_norm) %>%
  drop_na() %>%
  cor()

print(species_cors["n_species", ])

# Dawn chorus analysis
dawn_comparison <- results$combined_data %>%
  filter(time_period == "Dawn") %>%
  select(aci_norm, bi_norm, adi_norm, h_norm) %>%
  pivot_longer(everything(), names_to = "index", values_to = "value") %>%
  group_by(index) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  ) %>%
  arrange(desc(mean))

print(dawn_comparison)

# Site ranking by acoustic quality
site_ranks <- results$combined_data %>%
  group_by(site) %>%
  summarise(
    mean_aci = mean(aci_norm, na.rm = TRUE),
    mean_bi = mean(bi_norm, na.rm = TRUE),
    mean_adi = mean(adi_norm, na.rm = TRUE),
    mean_h = mean(h_norm, na.rm = TRUE),
    composite = (mean_aci + mean_bi + mean_adi + mean_h) / 4
  ) %>%
  arrange(desc(composite))

print(site_ranks)
```

## Troubleshooting

### "File mismatch detected"
- **What it means**: Not all WAV files have results in all index analyses
- **Solutions**:
  - Check that you ran all analyses on the same audio files
  - Review the `missing_files_log.txt` to see which files are missing where
  - This is a warning, not an error - analysis will continue

### "At least two index paths must be provided"
- **What it means**: You set fewer than 2 indices to non-NULL values
- **Solution**: Provide at least 2 index paths

### "Results file not found"
- **What it means**: The `*_raw_results.csv` file doesn't exist in the specified path
- **Solutions**:
  - Check the path is correct
  - Make sure the index analysis completed successfully
  - Verify the CSV file exists in the directory

### PCA plot skipped
- **What it means**: Not enough complete cases for PCA (needs at least 10)
- **Solutions**:
  - Check for missing data in your indices
  - Try without filters to get more data
  - This is normal if you have very few overlapping files

### Radar plot skipped
- **What it means**: Either only 1 site, or more than 6 sites
- **Solutions**:
  - For 1 site: Radar plot not applicable
  - For >6 sites: Use site_comparison boxplots instead
  - Filter to fewer sites if you want the radar plot

## Tips for Publication

### Best practices:

1. **Use normalization** when creating composite scores or radar plots
2. **Report all correlations** between indices in your methods
3. **Show temporal patterns** for all indices (temporal_overlay is great for this)
4. **Validate with species data** - correlation with BirdNET richness
5. **Use PCA** to understand which indices capture unique information
6. **Include summary statistics table** in supplementary materials

### Publication-ready figures:

```r
# High-resolution figures for publication
results <- compare_acoustic_indices(
  aci_path = "/path/to/aci",
  bi_path = "/path/to/bi",
  adi_path = "/path/to/adi",
  h_path = "/path/to/h",
  output_path = "/path/to/outputs",
  plot_types = c("correlation_matrix", "pca_biplot", "species_correlation"),
  normalize = TRUE,
  figure_width = 10,
  figure_height = 8,
  dpi = 600  # High resolution
)
```

### Sample results paragraph:

> "We calculated four acoustic indices (ACI, BI, ADI, and H) for all recordings (n = 1,247). 
> Indices showed moderate to strong correlations (r = 0.55-0.85), with H and ADI most strongly 
> correlated (r = 0.85). ADI showed the strongest relationship with BirdNET-derived species 
> richness (r = 0.69, p < 0.001), followed by H (r = 0.60). PCA revealed that the first two 
> components explained 78% of variance, with PC1 primarily loading on H and ADI, and PC2 on 
> ACI, suggesting these indices capture complementary aspects of acoustic diversity."

## Summary

You now have a complete workflow:

1. ✅ Run individual index analyses (`run_aci_analysis`, `run_bi_analysis`, etc.)
2. ✅ Compare all indices together (`compare_acoustic_indices`)
3. ✅ Export publication-ready tables and figures
4. ✅ Validate indices against species richness
5. ✅ Understand relationships between indices

**The complete acoustic analysis pipeline! 🎉**
