## Code for processing kaleidoscope bat analysis

# Merge detections in folder and add columns for (1) Location and (2) Site


## Classification ID
# If < 5 clicks, then NoID

# If >5 but <20, if MatchRatio > 95%, then ID = Auto ID, else NoID

# If >20 clicks, if MatchRatio > 85%, then ID = Auto ID, else NoID

# Check if any species are NOT found in SoCal and flag them for review
