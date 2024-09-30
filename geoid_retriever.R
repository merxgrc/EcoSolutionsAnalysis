# Load required packages
library(dplyr)
library(httr)
library(jsonlite)

# Load the shared dams dataset
shared_dams_path <- "C:/Users/Marcos Garcia/Desktop/DataJam Project Klamath river/EcoSolutionsAnalysis/Data/shared_dams_dataset.csv"
shared_dams <- read.csv(shared_dams_path)

# Remove rows with invalid (NA) coordinates
valid_dams <- shared_dams %>%
  filter(!is.na(Latitude) & !is.na(Longitude))

# Assign a unique ID to each dam entry for easier matching later
valid_dams <- valid_dams %>%
  mutate(Unique_ID = row_number())

# Define the function to get geoid from latitude and longitude
get_geoid_updated <- function(lat, lon) {
  if (is.na(lat) || is.na(lon) || lat < -90 || lat > 90 || lon < -180 || lon > 180) {
    cat("Invalid Latitude/Longitude:", lat, lon, "\n")
    return(NA)
  }
  
  # Make the API call to the Census geocoder
  api_url <- paste0("https://geocoding.geo.census.gov/geocoder/geographies/coordinates?x=", lon, "&y=", lat, "&benchmark=Public_AR_Current&vintage=Current_Current&format=json")
  response <- GET(api_url)
  
  # Process the response
  if (status_code(response) == 200) {
    result <- content(response, as = "parsed")
    
    # Extract the GEOID from Census Tracts, if available
    if (!is.null(result$result$geographies$`Census Tracts`) && length(result$result$geographies$`Census Tracts`) > 0) {
      return(result$result$geographies$`Census Tracts`[[1]]$GEOID)
    } else if (!is.null(result$result$geographies$Counties) && length(result$result$geographies$Counties) > 0) {
      # Fall back to County GEOID if Census Tract is unavailable
      return(result$result$geographies$Counties[[1]]$GEOID)
    } else {
      cat("No suitable geographies data found for:", lat, lon, "\n")
      return(NA)
    }
  } else {
    cat("Request failed for:", lat, lon, "with status code:", status_code(response), "\n")
    return(NA)
  }
}

# Create a new column to store geoid results
valid_dams$Geoid <- NA

# Apply the function to get geoid for each valid dam with batch processing
batch_size <- 100  # Adjust the batch size if needed
start_index <- 1  # Start from the first row, adjust if resuming from a failure

# Loop through the dataset in batches
for (i in seq(start_index, nrow(valid_dams), by = batch_size)) {
  end_index <- min(i + batch_size - 1, nrow(valid_dams))
  for (j in i:end_index) {
    if (is.na(valid_dams$Geoid[j])) {
      valid_dams$Geoid[j] <- get_geoid_updated(valid_dams$Latitude[j], valid_dams$Longitude[j])
      Sys.sleep(0.1)  # Pause between requests to avoid hitting API limits
    }
  }
  
  # Save the intermediate results to a CSV file in the Data folder
  output_path <- "C:/Users/Marcos Garcia/Desktop/DataJam Project Klamath river/EcoSolutionsAnalysis/Data/shared_dams_with_geoid_intermediate.csv"
  write.csv(valid_dams, output_path, row.names = FALSE)
  cat("Saved batch results to:", output_path, "\n")
}

# Save the final dataset to a new CSV file in the Data folder
final_output_path <- "C:/Users/Marcos Garcia/Desktop/DataJam Project Klamath river/EcoSolutionsAnalysis/Data/shared_dams_with_geoid_final.csv"
write.csv(valid_dams, final_output_path, row.names = FALSE)
cat("Final dataset with geoid saved to:", final_output_path, "\n")

# Load the final dataset and view it in RStudio
shared_dams_with_geoid <- read.csv(final_output_path)
View(shared_dams_with_geoid)



