library(tidyverse)
library(here)

# Compare Counties

cntyBoyce <- read.csv(here("projects/EJ/Article/data/Boyce/All_Boyce_Counties.csv"))

cntyEJScreen <- read.csv("projects/EJ/Article/data/EJScreen/Counties_All_EJ_Metrics.csv")


combine <- cntyBoyce%>%
  left_join(cntyEJScreen, by = "GIS_County")%>%
  filter(Facility_km_Avg_MINORPCT_Avg_EJ < 10000000 & Boyce_Med_Facility_km_MINORPCT < 10000000 )

ggplot(combine)+
  geom_point(aes(x = Facility_km_Avg_MINORPCT_Avg_EJ, y = Boyce_Med_Facility_km_MINORPCT))+
  xlim(0,50)+
  ylim(0,200)

lmc <- lm(combine$Facility_km_Avg_MINORPCT_Avg_EJ~combine$Boyce_Med_Facility_km_MINORPCT)


# Compare Tracts
tractBoyce <- read.csv(here("projects/EJ/Article/data/Boyce/All_Boyce_Tracts.csv"))
tractEJScreen <- read.csv("projects/EJ/Article/data/EJScreen/Tracts_All_EJ_Metrics.csv")

tractCombo <- tractBoyce%>%
  left_join(tractEJScreen, by = "GIS_Tract")%>%
  filter(Facility_km_Avg_MINORPCT_Avg_EJ < 10000000 & Boyce_Med_Facility_km_MINORPCT < 10000000 )

ggplot(tractCombo)+
  geom_point(aes(x = Facility_km_Avg_MINORPCT_Avg_EJ, y = Boyce_Med_Facility_km_MINORPCT))+
  xlim(0,100)+
  ylim(0,500)

lmt <- lm(tractCombo$Facility_km_Avg_MINORPCT_Avg_EJ~tractCombo$Boyce_Med_Facility_km_MINORPCT)
