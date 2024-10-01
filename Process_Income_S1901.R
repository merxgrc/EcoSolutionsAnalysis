# R script to process income data for shared_dams file using GEO_ID
library(dplyr)
library(ggplot2)

#shared_dams_with_geoid_path <- "C:/Users/Marcos Garcia/Desktop/DataJam Project Klamath river/EcoSolutionsAnalysis/Data/shared_dams_with_geoid_final.csv"
shared_dams_with_geoid_path <- "~/Desktop/DataJam_Project/EcoSolutionsAnalysis/Data/shared_dams_with_geoid_final.csv"
shared_dams_with_geoid <- read.csv(shared_dams_with_geoid_path)

#S1901_data_path <- "C:/Users/Marcos Garcia/Desktop/DataJam Project Klamath river/EcoSolutionsAnalysis/Data/ACSST5Y2022.S1901-Data.csv"
S1901_data_path <- "~/Desktop/DataJam_Project/EcoSolutionsAnalysis/Data/ACSST5Y2022.S1901-Data.csv"
S1901_data <- read.csv(S1901_data_path)

shared_dams_with_geoid$Geoid <- as.character(shared_dams_with_geoid$Geoid)

#truncate the S1901 format's extra leading numbers
S1901_data$GEO_ID <- substr(S1901_data$GEO_ID, nchar(S1901_data$GEO_ID) - 9, nchar(S1901_data$GEO_ID))

#add new columns
S1901_data <- S1901_data %>%
  mutate(unique_dam_id = NA)

S1901_data <- S1901_data %>%
  mutate(removed = NA)

# Match the geo_ids and append the dam number and removal status
geoid_column <- "Geoid"
for (geo_id in shared_dams_with_geoid[[geoid_column]]) {
  matching_rows <- shared_dams_with_geoid[shared_dams_with_geoid[[geoid_column]] == geo_id, ]
  unique_dam_id <- matching_rows$Unique_ID[1]  # Get the unique ID from the first matching row
  removed <- matching_rows$Removed[1]  # Get the removed value from the first matching row
  
  for (i in 1:nrow(S1901_data)) {
    if (S1901_data$GEO_ID[i] == geo_id) {
      S1901_data$unique_dam_id[i] <- unique_dam_id
      S1901_data$removed[i] <- removed
    }
  }
}

#clean the data entries that are not matched to our geo_ids
S1901_data_cleaned <- S1901_data[!is.na(S1901_data$unique_dam_id),]

# Define column indices (replace 25, 26 with actual indices)
column_indices <- c(25, 26)

# Check for valid column indices
if (any(column_indices < 1 | column_indices > ncol(S1901_data_cleaned))) {
  stop("Error: Invalid column indices provided!")
}

# Select desired parameters by index
desired_parameters <- colnames(S1901_data_cleaned)[column_indices]

# Create the new dataset
selected_data <- S1901_data_cleaned[, c(desired_parameters, "removed", "unique_dam_id")]

# Print or use the selected_data for further analysis
print(head(selected_data))  # Print the first few rows

#plot median 
# Check for NA values
num_na <- sum(is.na(selected_data$S1901_C01_012E))
if (num_na > 0) {
  warning("NA values found in S1901_C01_012E. Consider handling them.")
}

# Convert to numeric if necessary
selected_data$S1901_C01_012E <- as.numeric(selected_data$S1901_C01_012E)

# Remove rows with NA values (adjust as needed)
selected_data <- selected_data %>%
  na.omit(cols = "S1901_C01_012E")

class(selected_data$S1901_C01_012E)

ggplot(selected_data, aes(x = removed, y = S1901_C01_012E)) +
  geom_bar(stat = "summary", fun = "median") +
  scale_x_discrete(labels = c("0" = "Not Removed", "1" = "Removed"), breaks = c(0, 1)) +
  scale_y_continuous(breaks = seq(min(selected_data$S1901_C01_012E), max(selected_data$S1901_C01_012E), length.out = 5)) +
  labs(title = "Median Income by Removal Status",
       x = "Removal Status",
       y = "Median Income")
