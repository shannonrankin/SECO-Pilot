# Complete Acoustic Indices Analysis Workflow
## Master Guide for Bioacoustic Data Analysis

This guide provides a complete workflow for analyzing bioacoustic data using four acoustic indices: ACI, BI, ADI, and H.

---

## 📋 Table of Contents

1. [Overview](#overview)
2. [Installation](#installation)
3. [Data Requirements](#data-requirements)
4. [Individual Index Analyses](#individual-index-analyses)
5. [Comparison Analysis](#comparison-analysis)
6. [Complete Workflow Example](#complete-workflow-example)
7. [Files Included](#files-included)

---

## Overview

### What You Can Do

This analysis suite allows you to:
- Calculate **four major acoustic biodiversity indices** on thousands of audio files in parallel
- Integrate results with **BirdNET species detections** automatically
- Generate **publication-quality visualizations** for each index
- **Compare all indices** with comprehensive statistical analyses
- Export **publication-ready tables** and figures

### The Four Indices

| Index | Full Name | What It Measures | R Package |
|-------|-----------|------------------|-----------|
| **ACI** | Acoustic Complexity Index | Temporal variability in sound | seewave |
| **BI** | Bioacoustic Index | Biological activity/abundance | soundecology |
| **ADI** | Acoustic Diversity Index | Frequency diversity/evenness | soundecology |
| **H** | Acoustic Entropy Index | Overall complexity (temporal × spectral) | seewave |

---

## Installation

### Required R Packages

```r
# Install all required packages
install.packages(c(
  # Core packages
  "tidyverse", "lubridate",
  
  # Audio analysis
  "tuneR", "seewave", "soundecology",
  
  # Parallel processing
  "furrr", "future",
  
  # Visualization
  "ggridges", "patchwork", "viridis", "GGally", 
  "corrplot", "scales", "ggrepel"
))
```

---

## Data Requirements

### Folder Structure

Your recordings should be organized as:

```
Top-Level-Location/
├── Location-Site_config_20240601/          # Deployment folder
│   ├── Location-Site_config_20240601_080000.WAV
│   ├── Location-Site_config_20240601_080300.WAV
│   ├── ...
│   └── birdnetLocal_outputs/               # BirdNET subfolder (optional)
│       ├── Location-Site_config_20240601_080000.BirdNET.selection.table.txt
│       ├── Location-Site_config_20240601_080300.BirdNET.selection.table.txt
│       └── ...
└── Location-Site_config_20240715/          # Another deployment
    ├── Location-Site_config_20240715_080000.WAV
    └── ...
```

### File Naming Convention

- **WAV files**: `Location-Site_config_YYYYMMDD_HHMMSS.WAV`
- **BirdNET files**: `Location-Site_config_YYYYMMDD_HHMMSS.BirdNET.selection.table.txt`
- **Timestamps**: UTC time in 24-hour format

### Audio Specifications

- **Sample rate**: 48 kHz (recommended)
- **Channels**: Mono (single channel)
- **Format**: WAV

---

## Individual Index Analyses

### Step 1: Run Each Index Analysis

You'll run each of these four functions separately:

#### 1. ACI Analysis

```r
source("run_aci_analysis.R")

aci_results <- run_aci_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/aci_analysis",
  timezone = "America/Los_Angeles",
  plot_types = "all"
)
```

**Parameters to customize:**
- `wl = 512` - Window length
- `ovlp = 50` - Overlap percentage  
- `flim = c(0, 12)` - Frequency limits (kHz)

#### 2. BI Analysis

```r
source("run_bi_analysis.R")

bi_results <- run_bi_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/bi_analysis",
  timezone = "America/Los_Angeles",
  plot_types = "all"
)
```

**Parameters to customize:**
- `min_freq = 2000` - Minimum frequency (Hz)
- `max_freq = 8000` - Maximum frequency (Hz)
- `fft_w = 512` - FFT window size

#### 3. ADI Analysis

```r
source("run_adi_analysis.R")

adi_results <- run_adi_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/adi_analysis",
  timezone = "America/Los_Angeles",
  plot_types = "all"
)
```

**Parameters to customize:**
- `max_freq = 10000` - Maximum frequency (Hz)
- `db_threshold = -50` - Threshold for occupied bands (dB)
- `freq_step = 1000` - Frequency band size (Hz)

#### 4. H (Entropy) Analysis

```r
source("run_h_analysis.R")

h_results <- run_h_analysis(
  audio_path = "/path/to/recordings",
  output_path = "/path/to/h_analysis",
  timezone = "America/Los_Angeles",
  plot_types = "all"
)
```

**Parameters to customize:**
- `wl = 512` - Window length
- `envt = "hil"` - Envelope type ("hil" or "abs")
- `msmooth = c(512, 90)` - Temporal smoothing
- `ksmooth = c(3, 3)` - Spectral smoothing

### Outputs from Each Analysis

Each function creates:
- **`*_raw_results.csv`** - Raw index values for each file
- **`*_summary_by_hour.csv`** - Mean values by hour of day
- **`*_summary_by_date.csv`** - Mean values by date
- **`*_summary_by_site.csv`** - Mean values by site
- **`*_summary_by_deployment.csv`** - Mean values by deployment
- **`*_analysis_log.txt`** - Error/warning log
- **Multiple PNG visualizations** based on `plot_types` selected

---

## Comparison Analysis

### Step 2: Compare All Indices

After running individual analyses, compare them:

```r
source("compare_acoustic_indices.R")

comparison_results <- compare_acoustic_indices(
  aci_path = "/path/to/aci_analysis",
  bi_path = "/path/to/bi_analysis",
  adi_path = "/path/to/adi_analysis",
  h_path = "/path/to/h_analysis",
  output_path = "/path/to/comparison_analysis",
  plot_types = "all",
  normalize = TRUE  # Recommended for comparison
)
```

**Key Features:**
- Works with any combination of 2+ indices (set unused to `NULL`)
- Optional filtering by date range, site, or deployment
- Optional normalization to 0-1 scale
- 10 different comparison visualizations
- Publication-ready correlation matrices and summary tables

### Comparison Outputs

- **`combined_indices.csv`** - Merged dataset with all indices
- **`correlation_matrix.csv`** - Correlations between indices
- **`summary_statistics.csv`** - Descriptive stats for each index
- **`pca_summary.csv`** - PCA variance explained
- **Multiple comparison visualizations**

---

## Complete Workflow Example

### Scenario: Multi-Site Bird Monitoring Study

```r
# ==============================================================================
# STEP 1: RUN INDIVIDUAL INDEX ANALYSES
# ==============================================================================

# Source all functions
source("run_aci_analysis.R")
source("run_bi_analysis.R")
source("run_adi_analysis.R")
source("run_h_analysis.R")
source("compare_acoustic_indices.R")

# Set common parameters
base_audio_path <- "/data/bird_monitoring/recordings"
base_output_path <- "/data/bird_monitoring/analysis"
my_timezone <- "America/Los_Angeles"

# 1. ACI Analysis
aci_results <- run_aci_analysis(
  audio_path = base_audio_path,
  output_path = file.path(base_output_path, "aci"),
  timezone = my_timezone,
  wl = 512,
  ovlp = 50,
  flim = c(0, 12),
  n_cores = 8,
  plot_types = "all",
  integrate_birdnet = TRUE
)

# 2. BI Analysis
bi_results <- run_bi_analysis(
  audio_path = base_audio_path,
  output_path = file.path(base_output_path, "bi"),
  timezone = my_timezone,
  min_freq = 2000,
  max_freq = 8000,
  n_cores = 8,
  plot_types = "all",
  integrate_birdnet = TRUE
)

# 3. ADI Analysis
adi_results <- run_adi_analysis(
  audio_path = base_audio_path,
  output_path = file.path(base_output_path, "adi"),
  timezone = my_timezone,
  max_freq = 10000,
  db_threshold = -50,
  freq_step = 1000,
  n_cores = 8,
  plot_types = "all",
  integrate_birdnet = TRUE
)

# 4. H Analysis
h_results <- run_h_analysis(
  audio_path = base_audio_path,
  output_path = file.path(base_output_path, "h"),
  timezone = my_timezone,
  wl = 512,
  envt = "hil",
  n_cores = 8,
  plot_types = "all",
  integrate_birdnet = TRUE
)

# ==============================================================================
# STEP 2: COMPARE ALL INDICES
# ==============================================================================

comparison_results <- compare_acoustic_indices(
  aci_path = file.path(base_output_path, "aci"),
  bi_path = file.path(base_output_path, "bi"),
  adi_path = file.path(base_output_path, "adi"),
  h_path = file.path(base_output_path, "h"),
  output_path = file.path(base_output_path, "comparison"),
  plot_types = "all",
  normalize = TRUE,
  figure_width = 16,
  figure_height = 12,
  dpi = 300
)

# ==============================================================================
# STEP 3: ANALYZE RESULTS
# ==============================================================================

# View correlation matrix
print(comparison_results$correlation_matrix)

# Which index best predicts species richness?
library(tidyverse)

species_correlations <- comparison_results$combined_data %>%
  select(n_species, aci, bi, adi, h) %>%
  drop_na() %>%
  cor()

print(species_correlations["n_species", ])

# Rank sites by acoustic quality
site_rankings <- comparison_results$combined_data %>%
  group_by(site) %>%
  summarise(
    mean_aci = mean(aci_norm, na.rm = TRUE),
    mean_bi = mean(bi_norm, na.rm = TRUE),
    mean_adi = mean(adi_norm, na.rm = TRUE),
    mean_h = mean(h_norm, na.rm = TRUE),
    composite_score = (mean_aci + mean_bi + mean_adi + mean_h) / 4,
    n_recordings = n()
  ) %>%
  arrange(desc(composite_score))

print(site_rankings)

# Export for publication
write_csv(site_rankings, 
          file.path(base_output_path, "site_acoustic_quality_rankings.csv"))
```

---

## Files Included

### Analysis Functions (R Scripts)

1. **`run_aci_analysis.R`** - ACI analysis function
2. **`run_bi_analysis.R`** - BI analysis function
3. **`run_adi_analysis.R`** - ADI analysis function
4. **`run_h_analysis.R`** - H analysis function
5. **`compare_acoustic_indices.R`** - Comparison function

### Quick Reference Guides (Markdown)

1. **`ACI_Analysis_Quick_Reference.md`** - ACI guide
2. **`BI_Analysis_Quick_Reference.md`** - BI guide
3. **`ADI_Analysis_Quick_Reference.md`** - ADI guide
4. **`H_Analysis_Quick_Reference.md`** - H guide
5. **`Comparison_Quick_Reference.md`** - Comparison guide
6. **`Complete_Workflow_Guide.md`** - This document

---

## Visualization Summary

### Individual Index Visualizations (9 plots each)

Each index analysis can generate:
1. Daily pattern (hour of day)
2. Temporal trend (over dates)
3. Heatmap - daily (hour × date)
4. Heatmap - seasonal (hour × month)
5. Boxplot - by site
6. Boxplot - by time period (dawn/day/dusk/night)
7. Ridge plot - by month
8. Species correlation (vs BirdNET richness)
9. Distribution histogram

**Plus for H analysis:**
10. Entropy components (Ht vs Hf)

### Comparison Visualizations (10 plots)

1. Correlation matrix heatmap
2. Pairwise scatter plots
3. Temporal overlay (all indices by hour)
4. Temporal trends (all indices over time)
5. Temporal heatmaps (side-by-side)
6. Site comparison boxplots
7. Site radar plot
8. Species correlation (all indices)
9. PCA biplot
10. Index distributions

**Total potential visualizations: 46 plots**

---

## Best Practices

### 1. Parameter Selection

**Use defaults unless you have specific reasons to change:**
- Defaults are based on published best practices for bird studies
- Document any parameter changes in your methods

### 2. Processing Strategy

**For large datasets:**
```r
# Process in batches if you have memory constraints
# Use fewer cores if system becomes unstable
n_cores = 4  # Instead of 8
```

### 3. Quality Control

**Always check:**
- Log files for errors
- Summary statistics for unusual values
- Visualizations for unexpected patterns
- Missing files log (if generated)

### 4. Validation

**Validate indices with:**
- Traditional point count surveys
- BirdNET species richness (included automatically)
- Habitat quality assessments
- Weather/environmental data

### 5. Publication

**Report:**
- All parameter values used
- Sample sizes (n files, n sites, date ranges)
- Correlation matrix between indices
- Validation with species data
- Any data filtering or exclusions

---

## Troubleshooting

### Common Issues

**Issue**: "Package X is required but not installed"
**Solution**: `install.packages("X")`

**Issue**: Memory errors during parallel processing
**Solution**: Reduce `n_cores` parameter

**Issue**: File mismatch warnings in comparison
**Solution**: Check that all analyses were run on same audio files

**Issue**: No BirdNET data integrated
**Solution**: Verify BirdNET files exist and contain "birdnetLocal" in folder name

**Issue**: Very low/high index values
**Solution**: Check audio quality, gain settings, and frequency ranges

---

## Citation and References

### This Analysis Suite

If you use these functions, please cite the original index papers:

**ACI:**
Pieretti, N., et al. (2011). A new methodology to infer the singing activity of an avian community: The Acoustic Complexity Index (ACI). *Ecological Indicators*, 11(3), 868-873.

**BI:**
Boelman, N. T., et al. (2007). Multi-trophic invasion resistance in Hawaii: bioacoustics, field surveys, and airborne remote sensing. *Ecological Applications*, 17(8), 2137-2144.

**ADI:**
Villanueva-Rivera, L. J., et al. (2011). A primer of acoustic analysis for landscape ecologists. *Landscape Ecology*, 26(9), 1233-1246.

**H:**
Sueur, J., et al. (2008). Rapid acoustic survey for biodiversity appraisal. *PLoS ONE*, 3(12), e4065.

### R Packages

**seewave:**
Sueur, J., et al. (2008). Seewave, a free modular tool for sound analysis and synthesis. *Bioacoustics*, 18(2), 213-226.

**soundecology:**
Villanueva-Rivera, L. J., & Pijanowski, B. C. (2018). soundecology: Soundscape Ecology. R package.

---

## Support and Contact

For questions about these functions:
1. Check the Quick Reference guides for each index
2. Review the complete usage examples in each function file
3. Examine the log files for specific error messages

---

## Summary

You now have a **complete, production-ready acoustic analysis pipeline**:

✅ Four major acoustic biodiversity indices  
✅ Parallel processing for large datasets  
✅ Automatic BirdNET integration  
✅ Publication-quality visualizations  
✅ Comprehensive comparison analyses  
✅ Export-ready tables and figures  

**Happy analyzing! 🎵🐦📊**

---

*Document Version: 1.0*  
*Last Updated: February 2026*
