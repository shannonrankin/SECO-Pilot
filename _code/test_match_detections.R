source <- readRDS(here("data", "birdnet_LFRR.rds"))%>%
  filter(Location == "SPP") %>%
  filter(Site == c("DD01", "DD02"))%>%
  mutate(
    # Extract 15 characters before ".WAV"
    file_datetime_chr = str_sub(Begin.Path, -19, -5),
    # Convert to datetime
    file_datetime = as.POSIXct(file_datetime_chr, format = "%Y%m%d_%H%M%S", tz = "UTC")
  ) 

# Function
match_detections <- function(df, focal_site, alt_site, id_colname) {
  
  # error log container
  errors <- list()
  
  # prepare tibble with ID column
  df[[id_colname]] <- NA_integer_
  next_id <- 1
  
  focal <- df %>% filter(Site == focal_site)
  alt   <- df %>% filter(Site == alt_site)
  
  for (i in seq_len(nrow(focal))) {
    this_row <- focal[i, ]
    fdtime   <- this_row$file_datetime
    
    # candidates on other site
    candidates <- alt %>% filter(file_datetime == fdtime)
    
    if (nrow(candidates) == 0) {
      errors <- append(errors, paste0("No Matching Detection for ", focal_site, " on ", fdtime))
      next
    }
    else if (nrow(candidates) == 1) {
      match_row <- candidates
    }
    else {
      # choose nearest by Begin.Time..s.
      match_row <- candidates %>%
        slice_min(abs(Begin.Time..s. - this_row$Begin.Time..s.), with_ties = FALSE)
    }
    
    # Assign detection ID
    match_idx <- which(df$Begin.Path %in% match_row$Begin.Path)
    focal_idx <- which(df$Begin.Path %in% this_row$Begin.Path)
    
    df[[id_colname]][c(focal_idx, match_idx)] <- next_id
    next_id <- next_id + 1
  }
  
  list(data = df, errors = errors)
}


###################################
# Run
#Find Source Calls
source <- readRDS(here("data", "birdnet_LFRR.rds"))%>%
  filter(Location == "SPP") %>%
  filter(Site == c("DD01", "DD02"))%>%
  mutate(
    # Extract 15 characters before ".WAV"
    file_datetime_chr = str_sub(Begin.Path, -19, -5),
    # Convert to datetime
    file_datetime = as.POSIXct(file_datetime_chr, format = "%Y%m%d_%H%M%S", tz = "UTC")
  )

#match_detections function -- move to _common.r
# First pass: DD01 → DD02
res1 <- match_detections(source, "DD01", "DD02", "detection_ID_1")
source <- res1$data
errors1 <- res1$errors

# Second pass: DD02 → DD01
res2 <- match_detections(source, "DD02", "DD01", "detection_ID_2")
source <- res2$data
errors2 <- res2$errors

source <- source %>%
  mutate(
    detection_ID_2 = ifelse(
      !is.na(detection_ID_2) & !is.na(detection_ID_1) & detection_ID_1 != detection_ID_2,
      "multipleMatches", detection_ID_2
    )
  )
conflicts <- source %>%
  filter(detection_ID_2 == "multipleMatches") %>%
  mutate(log_msg = paste0("Conflict: multiple matches for ", Site, " on ", file_datetime)) %>%
  pull(log_msg)

source <- source %>%
  mutate(match_ID = detection_ID_1 == detection_ID_2)

all_errors <- c(errors1, errors2, conflicts) %>% unlist() %>% as.character()

# If no errors, avoid writing empty file crash
if (length(all_errors) == 0) {
  all_errors <- "No errors detected."
}

writeLines(all_errors, "LFRR_sourceMatchErrors.csv")




