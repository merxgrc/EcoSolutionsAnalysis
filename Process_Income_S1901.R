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

# Extract the first row and convert it to a vector
first_row <- head(S1901_data, 1)
first_row_vector <- as.vector(first_row)

# Search for the phrase "estimate" in the column names to not use the error columns
matching_indices <- which(grepl("Estimate", first_row_vector))






# vvvvvv unfinished processing work for plot, stuck here


# Function to process data points for matching indices
process_values <- function(df, matching_indices) {
  # Iterate through each row of the data frame
  for (i in 1:nrow(df)) {
    selected_row <- df[i, ]
    
    # Print row number
    cat("Row ", i, ": ")
    
    # Iterate through matching indices
    for (index in matching_indices) {
      # Extract parameter value using index
      value <- selected_row[[index]]
      
      # Print parameter value
      cat(paste0(colnames(df)[index], ": ", value), sep = " ")
    }
    
    # Print newline after each row
    cat("\n")
  }
}

process_values(S1901_data_cleaned, matching_indices)




# Function to process data points for matching indices
process_values <- function(df, matching_indices) {
  # Iterate through each row of the data frame
  for (i in 1:nrow(df)) {
    selected_row <- df[i, ]
    
    # ... (rest of the code remains the same) ...
    
    # Create a data frame for this row's data
    row_data <- data.frame(
      parameter = colnames(df)[matching_indices],
      value = selected_row[matching_indices]
    )
    
    # Convert values to numeric
    row_data$value <- as.numeric(row_data$value)
    
    # Plot the data (replace 'plot type' with your desired plot)
    ggplot(row_data, aes(x = parameter, y = value)) + 
      geom_point(aes(color = df$removed[i])) +  # Color points based on removed status
      labs(title = paste0("Row ", i), x = "Parameter", y = "Value")
    
  }
}

process_values(S1901_data_cleaned, matching_indices)