#run the following command once for dplyr vvvvv
#install.packages("dplyr")
library(dplyr)


#process the dam data for only what we need
US_dams <- read.csv("~/Desktop/DataJam Project/Data/US_dams.csv")
US_dams_filtered <- select(filter(US_dams, State == "CA"), NID.ID, Dam.Name, Longitude, Latitude, River.or.Stream.Name, Structural.Height..Ft., Purposes, Year.Completed)