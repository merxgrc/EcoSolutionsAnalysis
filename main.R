# R installs
# install.packages("ggplot2")
# install.packages("dplyr")

# Load required packages
library(dplyr)
library(ggplot2)

# Load and filter the removed dams dataset
removed_dams_path <- "C:/Users/Marcos Garcia/Desktop/DataJam Project Klamath river/EcoSolutionsAnalysis/Data/ARDamRemovalList_Figshare_Feb2024.csv"
Removed_Dams <- read.csv(removed_dams_path, fileEncoding = "latin1")

# Load and filter the dams dataset
dams_path <- "C:/Users/Marcos Garcia/Desktop/DataJam Project Klamath river/EcoSolutionsAnalysis/Data/dams.csv"
dams <- read.csv(dams_path, fileEncoding = "latin1")

# Clean column names by removing leading/trailing whitespace
names(Removed_Dams) <- trimws(names(Removed_Dams))
names(dams) <- trimws(names(dams))

# Filter and select relevant columns from the removed dams dataset
Removed_Dams_filtered <- Removed_Dams %>%
  filter(State == "CA") %>%
  select(Dam_Name, Longitude, Latitude, River, Dam_Height_ft, Year_Built) %>%
  mutate(Removed = 1)  # Mark as removed

# Filter and select relevant columns from the dams dataset
dams_filtered <- dams %>%
  filter(State == "California") %>%
  select(Dam_Name = Dam.Name, Longitude, Latitude, River = River.or.Stream.Name,
         Dam_Height_ft= Dam.Height..Ft., Year_Built = Year.Completed) %>%
  mutate(Removed = 0)  # Mark as not removed

# Convert Longitude to numeric for both datasets if necessary
Removed_Dams_filtered$Longitude <- as.numeric(Removed_Dams_filtered$Longitude)
dams_filtered$Longitude <- as.numeric(dams_filtered$Longitude)

# Remove rows with non-numeric characters in Dam_Height_ft and convert to integer
Removed_Dams_filtered <- Removed_Dams_filtered %>%
  filter(!grepl("[^0-9]", Dam_Height_ft)) %>%
  mutate(Dam_Height_ft = as.integer(Dam_Height_ft),
         Year_Built = as.integer(Year_Built))

# Join datasets based on common columns (Dam_Name, Longitude, Latitude, River)
merged_dams <- full_join(Removed_Dams_filtered, dams_filtered, 
                         by = c("Dam_Name", "Longitude", "Latitude", "River"))

# Filter merged_dams to keep only rows with complete data
merged_dams_filtered <- merged_dams %>%
  filter(
    !is.na(Dam_Name) &
      !is.na(Longitude) &
      !is.na(Latitude) &
      !is.na(River) &
      !is.na(Dam_Height_ft.x) &
      !is.na(Year_Built.x)
  )

# Handle the Removed columns after the join
merged_dams_filtered <- merged_dams_filtered %>%
  mutate(Removed = coalesce(Removed.x, Removed.y)) %>% # Combine Removed columns
  select(-Removed.x, -Removed.y) # Remove original Removed columns

# Save the filtered merged dataset to a CSV file
output_path <- "C:/Users/Marcos Garcia/Desktop/DataJam Project Klamath river/EcoSolutionsAnalysis/Data/merged_dams_filtered.csv"
write.csv(merged_dams_filtered, output_path, row.names = FALSE)

# Confirm the saved output
cat("Merged dataset saved to:", output_path, "\n")

# Visualize the data (Optional)
ggplot(merged_dams_filtered, aes(x = Dam_Height_ft.x, fill = factor(Removed))) +
  geom_histogram(position = "dodge", bins = 30) +
  labs(title = "Dam Height vs. Removal Status (CA)",
       x = "Dam Height (ft)",
       y = "Count of Dams",
       fill = "Removal Status") +
  theme_classic()


