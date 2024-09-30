# R script to process income data for shared_dams file using GEO_ID

shared_dams_with_geoid <- read.csv("~/Desktop/DataJam_Project/EcoSolutionsAnalysis/Data/shared_dams_with_geoid.csv")

S1901_data <- read.csv("~/Desktop/DataJam_Project/EcoSolutionsAnalysis/Data/ACSST5Y2022.S1901-Data.csv", header=FALSE)
