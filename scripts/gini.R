library(tidyverse)
library(here)
library(sf)
library(ineq)


# Load binned datasets
allbgs <- read.csv(here("projects/EJ/Article/data/BGs_All_Attributes.csv"))%>%
  drop_na(Bin_Tile_MN,Bin_Tile_LI,Facility_km, Releases_km, Tanks_km, Capacity_km)

gini <- Gini(allbgs$Facility_km, corr = TRUE, na.rm = TRUE)


# Alternate gini
altGini <- DescTools::Gini(allbgs$Facility_km, n = allbgs$Pct_Minority, na.rm = TRUE)




#gini coefficient by state

#gini coefficent by county



# EJ index by area
# Facilitis per km2 * (Minority percentile of block group - minority percentile of U.S. (50)) * block group population


