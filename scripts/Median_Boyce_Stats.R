# Script for finding median values for map making for article.
library(tidyverse)
library(here)

# Load EJ data
allbgs <- read.csv(here("projects/EJ/Article/data/BGs_All_Attributes.csv"))%>%
  #drop_na(Facility_km, Releases_km, Tanks_km, Capacity_km)%>%
  mutate(GIS_Tract = substr(GISJOIN,1,14),
         GIS_County = substr(GISJOIN,1,8),
         GIS_State = substr(GISJOIN,1,4),
         ID = as.character(ID))

medFacilities <- allbgs%>%
  select(GIS_County,Tanks_km)%>%
  group_by(GIS_County)%>%
  mutate(Med_Tanks_km = median(Tanks_km, na.rm = TRUE))%>%
  ungroup()%>%
  select(GIS_County,Med_Tanks_km)%>%
  distinct()

write.csv(medFacilities, here("projects/EJ/Article/data/Boyce/Med_Tanks_km_CNTY.csv"))

median(allbgs$Tanks_km)

# Import County level Boyce Statistics
allCounties <- read.csv(here("projects/EJ/Article/data/Boyce/All_Boyce_Counties_Both.csv"))


# Median Boyce Value for Tanks vs. Minority
median(allCounties$Boyce_Med_Tanks_km_MINORPCT, na.rm = TRUE)

# Median Boyce Value for Tanks vs. Low-Income
median(allCounties$Boyce_Med_Tanks_km_LOWINCPCT, na.rm = TRUE)

# Median Boyce Value for Releases vs. Minority
median(allCounties$Boyce_Med_Releases_km_MINORPCT, na.rm = TRUE)

# Median Boyce Value for Releases vs. Low-Income
median(allCounties$Boyce_Avg_Releases_km_LOWINCPCT, na.rm = TRUE)
