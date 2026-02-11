---
  title: "SD Wetland Dashboard"
format: 
  dashboard:
  logo: images/wildlifeCommunity_ai.jpg
nav-buttons:
  - icon: github
href: https://github.com/shannonrankin/SECO-Pilot/rails-dashboard.qmd
- icon: website
href: https://shannonrankin.github.io/SECO-Pilot
---
  
  ```{r dashPrep}
#| echo: false
#| include: false
library(tidyverse)
library(here)
library(stringr)
library(knitr)
library(kableExtra)
library(bslib)
library(fontawesome)
library(DT)
library(Distance)
library(leaflet)
library(lubridate)
library(ggplot2)
here()

source(here::here("_code/_common.R"))

birdnet_all <- readRDS(here("data/birdnet_rails.rds"))

birdnet_rails <- birdnet_all %>%
  dplyr::filter(common_name == "Ridgway's Rail")

birdnet_rails_sub <- birdnet_rails %>%
  filter(location == c('SPP', 'TJE'))

deployments <- readRDS(here("data/deployments.rds")) %>%
  mutate(hours_total = as.numeric(record_end_utc - record_start_utc, units = "hours"))

DDTable <- readRDS(here("data", "deployTable_DD.rds"))

```

# Deployments
## Row - Value boxes {height="15%"}

```{r vBox_dataSize}
#| echo: false
data_size_tb <- deployments %>%
  summarise(total_bytes = sum(file_size_bytes, na.rm = TRUE)) %>%
  mutate(total_tb = total_bytes / (1024^4)) %>%  # Convert bytes to terabytes
  pull(total_tb) %>%
  round(2)  # Round to 2 decimal places

value_box(
  value = data_size_tb,
  title = "Data Size (TB)",
  showcase = bsicons::bs_icon("database"),
  theme = "purple"
)

num_files <- deployments %>%
  summarise(total_files = sum(num_files, na.rm = TRUE)) %>%
  pull(total_files)

value_box(
  value = num_files,
  title = "Total # of Files",
  showcase = bsicons::bs_icon("file-earmark-music"),
  theme = "purple"
)

num_species <- birdnet_all %>%
  distinct(common_name) %>%
  summarise(total_species = n()) %>%
  pull(total_species)

value_box(
  value = num_species,
  title = "# Species",
  showcase = bsicons::bs_icon("feather"),
  theme = "purple"
)

```

## Recording Dates, Sites
## Row {.tabset}
### Timeline

::: {style="background-color: #e6f2ff; padding: 20px; border-radius: 8px;"}
::: columns
::: {.column width="67%"}
::: {style="background-color: white; padding: 10px; border-radius: 8px;"}
```{r recordingSite}
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: "100%"
# Ensure date columns are in Date format
deployments <- deployments %>%
  mutate(
    record_start_utc = with_tz(record_start_utc, "America/Los_Angeles"),  # Convert to LA time
    record_end_utc = with_tz(record_end_utc, "America/Los_Angeles")
  )
# Remove recording start/end dates with NAs
deployments_map <- deployments %>%
  filter(!is.na(record_start_utc), !is.na(record_end_utc))
# Create a unique identifier for each interval per site
deployments_map <- deployments_map %>%
  group_by(site) %>%
  mutate(interval_id = row_number()) %>%
  ungroup()
# Create the Gantt chart
ggplot(deployments_map, aes(x = record_start_utc, xend = record_end_utc, y = site, yend = site, color = site)) +
  geom_segment(linewidth = 8, alpha = 0.7) +
  labs(
    title = "Site Deployment Timeline",
    x = "Date",
    y = "Site",
    color = "Site"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
```
:::
  :::
  
  ::: {.column width="33%"}
::: {style="border: 1px solid #ccc; border-radius: 8px; padding: 15px; background-color: white; height: 100%;"}
Initial recordings in May included recorders deployed at Tijuana Estuary (TJE-01) and two sites at the Sweetwater Proving Pens (SPP-1N, SPP-3S). Secondary deployments were made at SPP-1N and TJE-01. A detection distance test was made at locations around the Sweetwater Proving Pens, DD01-DD10.

Redo: Rename DD01, DD01, Add SGR, recolor or gradient according to config_type.
:::
  :::
  :::
  :::
  
  
  ### Map
  
  ::: {.row}

::: {style="background-color: #e6f2ff; padding: 20px; border-radius: 8px;"}
::: columns
::: {.column width="67%"}
::: {style="background-color: white; padding: 10px; border-radius: 8px;"}

``` {r siteMap}
#| warning: false
#| fig-width: 8
#| fig-height: 5

goodCols <- c("deployment_id","location", "site", "config_type", "deploy_date", "latitude", "longitude", "retrieve_date")

deployTable <- as_tibble(readRDS(here("data/deployments.rds"))) %>%
  select(all_of(goodCols)) %>%
  filter(location %in% c("SPP-DD", "SPP", "TJE", "SGR"))

leaflet(data = deployTable, padding = 20) %>%
  #addProviderTiles(providers$Esri.WorldImagery) %>%
  addTiles() %>%
  addCircleMarkers(~longitude, ~latitude, popup = ~as.character(deployment_id))

```

:::
  :::
  
  ::: {.column width="33%"}
::: {style="border: 1px solid #ccc; border-radius: 8px; padding: 15px; background-color: white; height: 100%;"}
Say Something Here 
:::
  :::
  :::
  :::
  
  ### Deployment Table
  
  ::: {.row}

::: {style="background-color: #e6f2ff; padding: 20px; border-radius: 8px;"}
::: columns
::: {.column width="67%"}
::: {style="background-color: white; padding: 10px; border-radius: 8px;"}

```{r dataTable}
#| echo: false
#| fig-width: 8
#| fig-height: 5
deployTable |>
  DT::datatable(
    caption = "Deployment of Acoustic Recorders for Rail-Wetland Pilot Study",
    filter = "top" 
  )|>
  DT::formatRound(6:7, digits = 4)

```
::: {style="background-color: #e6f2ff; padding: 20px; border-radius: 8px;"}
::: columns
::: {.column width="67%"}
::: {style="background-color: white; padding: 10px; border-radius: 8px;"}

### Recording Specifications

::: {.row}

::: {style="background-color: #e6f2ff; padding: 20px; border-radius: 8px;"}
::: columns
::: {.column width="67%"}
::: {style="background-color: white; padding: 10px; border-radius: 8px;"}

```{r railRecorderSpectable}
#| echo: false
#| fig-width: 8
#| fig-height: 5
configTable <- readRDS(here("data", "configDetails.rds"))[1:3,] %>% 
  select(config_id, config_type, sampling_rate, sleep_duration, record_duration, filter_type, filter_low, filter_high ) %>%
  rename("Recording Type" = config_id,
         "Recording Schedule" = config_type,
         "Sampling Rate (kHz)" = sampling_rate,
         "Record Off (s)" = sleep_duration,
         "Record On (s)" = record_duration,
         "Filter Type" = filter_type,
         "High Pass Filter (kHz)" = filter_low,
         "Low Pass Filter (kHz)" = filter_high
  )

kable(configTable, caption = "Initial Recording Specifications", digits = 1) %>% 
  kable_classic()%>%
  #add_header_above(c(" " = 1, "Distance to Site" = 2)) %>%
  row_spec(0, bold = TRUE) %>%
  kable_styling(bootstrap_options = c("condensed", "striped"), full_width = F, position = "center")

```

::: {style="background-color: #e6f2ff; padding: 20px; border-radius: 8px;"}
::: columns
::: {.column width="67%"}
::: {style="background-color: white; padding: 10px; border-radius: 8px;"}


# Rails
## Row - Value boxes {height="30%"}

```{r vBox_rails}
#| echo: false

# Total # Rail Detections
numRails <- nrow(birdnet_rails_sub)

value_box(
  value = numRails,
  title = "Total # Rail Detections",
  showcase = bsicons::bs_icon("hash"),
  theme = "purple"
)

# Calls per Hour (median, range)
totalHr <- deployments %>%
  summarise(total_hours = sum(hours_total, na.rm = TRUE)) %>%
  pull(total_hours)
callHour <- numRails/totalHr 
callHour <- round(callHour, 1)

value_box(
  value = callHour,
  title = "# Rail Calls per Hour",
  showcase = bsicons::bs_icon("hourglass-bottom"),
  theme = "purple"
)

# Percentage of recordings with Rail Calls
railRec <- n_distinct(birdnet_rails$begin_path)
allRec <- n_distinct(birdnet_all$begin_path)
percentRail <- railRec/allRec
percentRail_pct_str <- paste0(round(percentRail * 100, 1), "%")

value_box(
  value = percentRail_pct_str,
  title = "% Recordings with Rail Calls",
  showcase = bsicons::bs_icon("percent"),
  theme = "purple"
)

```

## Summary Rail Detections


```{r railsPrep}
#| echo: false
#| warning: false

myData_rails <- loadDetectionData(x=birdnet_rails, source = 'csv', detectionType='detection', wide=FALSE, tz='Etc/GMT-7', columnMap=list(UTC='utc', species='common_name', Latitude='latitude', Longitude='longitude'), extraCols= c('site', 'location'))

myData_rails_sub <- loadDetectionData(x=birdnet_rails_sub, source = 'csv', detectionType='detection', wide=FALSE, tz='Etc/GMT-7', columnMap=list(UTC='utc', species='common_name', Latitude='latitude', Longitude='longitude'), extraCols= c('site', 'location'))
```

## Row {.tabset}


```{r RailBoxplot}
#| echo: false
#| warning: false
#| title: "Daily Detection"
#| results: asis

library(htmltools)
library(knitr)
DetBox <- plotDetectionBoxplot(x=myData_rails_sub, group='species', facet='location', bin='hour/day', combineYears=FALSE, title = "Detection of Ridgeway's Rails (hours/day) at the Sweetwater Proving Pens and Tijuana Estuary")

two_col_card(
  left_content = DetBox,
  title = "Detection BoxPlot",
  text = "This shows my Detections in a Boxplot"
)
```


```{r RailPolarPlot}
#| echo: false
#| warning: false
#| title: "Diurnal Detections"

plotPolarDetections(x=myData_rails, group='species', facet='species', bin='detection/hour', quantity='count')
```



# Biodiversity
## Row - Value boxes {height="30%"}

```{r vBox_biodiversity}
#| echo: false

# Total # Species
num_species <- birdnet_all %>%
  distinct(common_name) %>%
  summarise(total_species = n()) %>%
  pull(total_species)

value_box(
  value = num_species,
  title = "# Species",
  showcase = bsicons::bs_icon("feather"),
  theme = "purple"
)

# Total # Detections
numDetections <- nrow(birdnet_all)

value_box(
  value = numDetections,
  title = "Total # Detections",
  showcase = bsicons::bs_icon("hash"),
  theme = "purple"
)

# Percentage of recordings with Detections
allRec <- n_distinct(birdnet_all$begin_path)
num_files <- deployments %>%
  summarise(total_files = sum(num_files, na.rm = TRUE)) %>%
  pull(total_files)

percentDetect <- allRec/num_files
percentDetect_pct_str <- paste0(round(percentDetect * 100, 1), "%")

value_box(
  value = percentDetect_pct_str,
  title = "% Recordings with Detections",
  showcase = bsicons::bs_icon("percent"),
  theme = "purple"
)

```


## Row {.tabset}

```{r biodiversityPrep}
priority <- birdnet_all %>%
  filter(location == "TJE") %>%
  count(common_name, sort = TRUE) %>% 
  slice_head(n = 10) %>%
  pull(common_name) %>% 
  union("Ridgway's Rail") 

birdnet_top_TJE <- birdnet_all %>%
  filter(location == "TJE") %>%
  filter(common_name %in% priority)

myData_priority_TJE <- loadDetectionData(x=birdnet_top_TJE, source='csv', detectionType='detection', wide=FALSE,  tz='UTC', columnMap=list(UTC='utc', species='common_name', Latitude='latitude', Longitude='longitude'))
```

```{r bioBoxplot}
#| echo: false
#| warning: false
#| title: "TJ Top 10 Species"


plotDetectionBoxplot(x=myData_priority_TJE,  group='species', facet = 'species', bin='hour/day', combineYears = FALSE, title = "Time Series BoxPlot showing the top 10 species detected at Tijuana Estuary")

```

```{r bioAcScene}
#| echo: false
#| warning: false
#| title: "TJ Acoustic Scene"


plotAcousticScene(x=myData_priority_TJE, combineYears=FALSE) #Note: this would only be good for TJ

```