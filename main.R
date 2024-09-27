# Load required packages
library(dplyr)

# Load and filter the removed dams dataset
#removed_dams_path <- "C:/Users/Marcos Garcia/Desktop/DataJam Project Klamath river/EcoSolutionsAnalysis/Data/ARDamRemovalList_Figshare_Feb2024.csv"
removed_dams_path <- "~/Desktop/DataJam_Project/EcoSolutionsAnalysis/Data/ARDamRemovalList_Figshare_Feb2024.csv"
Removed_Dams <- read.csv(removed_dams_path, fileEncoding = "latin1")

# Load and filter the dams dataset
#dams_path <- "C:/Users/Marcos Garcia/Desktop/DataJam Project Klamath river/EcoSolutionsAnalysis/Data/dams.csv"
dams_path <- "~/Desktop/DataJam_Project/EcoSolutionsAnalysis/Data/dams.csv"
dams <- read.csv(dams_path, fileEncoding = "latin1")

# Clean column names by removing leading/trailing whitespace
names(Removed_Dams) <- trimws(names(Removed_Dams))
names(dams) <- trimws(names(dams))

# Filter and select relevant columns from the removed dams dataset
Removed_Dams_filtered <- Removed_Dams %>%
  filter(State == "CA") %>%
  select(Dam_Name, Longitude, Latitude, River, Dam_Height_ft, Year_Built) %>%
  mutate(Removed = 1)  # Mark as removed

str(Removed_Dams_filtered)

str(dams)

# Filter and select relevant columns from the dams dataset
dams_filtered <- dams %>%
  filter(State == "California") %>%
  select(Dam_Name = Dam.Name, Longitude, Latitude, River = River.or.Stream.Name,
         Dam_Height_ft= Dam.Height..Ft., Year_Built = Year.Completed) %>%
  mutate(Removed = 0)  # Mark as not removed

str(dams_filtered)

# Convert Longitude to numeric for both datasets if necessary
Removed_Dams_filtered$Longitude <- as.numeric(Removed_Dams_filtered$Longitude)
dams_filtered$Longitude <- as.numeric(dams_filtered$Longitude)



# Join datasets based on common columns (Dam_Name, Longitude, Latitude, River)
merged_dams <- full_join(Removed_Dams_filtered, dams_filtered, 
                         by = c("Dam_Name", "Longitude", "Latitude", "River"))

# Check the structure of merged_dams to identify correct column names
str(merged_dams)
str(Removed_Dams_filtered)


# Create a shared dataset, does not removed NA entries
shared_dams <- merged_dams %>%
  filter(!is.na(Removed.x) | !is.na(Removed.y)) %>%  # Regular spaces
  select(Dam_Name, Longitude, Latitude, River,  # Regular spaces
         Dam_Height_ft.x, Year_Built.x, Removed.x)
     
str(shared_dams)

#original function does not work
# Create a shared dataset with only relevant columns and non-NA entries
#shared_dams <- merged_dams %>%
#  filter(!is.na(Removed.x) | !is.na(Removed.y)) %>%
#  select(Dam_Name, Longitude, Latitude, River, 
#         Dam_Height_ft = coalesce(Dam_Height_ft.x, Dam_Height_ft.y),
#         Year_Built = coalesce(Year_Built.x, Year_Built.y), 
#         Removed = coalesce(Removed.x, Removed.y))

# Display the shared dataset
print(shared_dams)

# Save the shared dataset to a CSV file
#output_path <- "C:/Users/Marcos Garcia/Desktop/DataJam Project Klamath river/EcoSolutionsAnalysis/Data/shared_dams_dataset.csv"
output_path <- "~/Desktop/DataJam_Project/EcoSolutionsAnalysis/Data/shared_dams_dataset.csv"
write.csv(shared_dams, output_path, row.names = FALSE)


