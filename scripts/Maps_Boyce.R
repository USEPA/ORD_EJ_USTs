library(mapsf)
library(tidyverse)
library(vroom)
library(here)
library(USAboundaries)

cnty <- vroom(here("projects/EJ/Article/data/Boyce/All_Boyce_Counties.csv"))

# Download Counties
cntySf <- USAboundaries::us_counties()%>%
  mutate(GIS_County = paste0("G",substr(geoid,1,2),"0",substr(geoid,3,5),"0"))%>%
  st_transform(5070)%>%
    left_join(cnty)
# Make a map

# Break geographies for insets
conus <- cntySf%>%
  filter(!state_name %in% c("Puerto Rico","Alaska","Hawaii"))
alaska <- cntySf%>%
  filter(state_name == "Alaska")%>%
  st_transform(3467)
hawaii <- cntySf%>%
  filter(state_name == "Hawaii")%>%
  st_transform("+proj=aea +lat_1=8 +lat_2=18 +lat_0=13 +lon_0=-157 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
pr <- cntySf%>%
  filter(state_name == "Puerto Rico")


# Make a map
mf_export(x = conus, filename = here("projects/EJ/Article/figures/Boyce/Test.png"), width = 1200,
          theme = "default", expandBB = c(.3,0,0,.5)) 
mf_shadow(conus, col = "grey10",cex = .1, add = TRUE)
mf_map(conus, var = "Boyce_Med_Facility_km_MINORPCT", type = "choro",
       pal = "Dark Mint", 
       breaks = c(0,.9,1.1,1.5,3,10), 
       #nbreaks = 6, 
       leg_title = "Boyce Ratio", 
       #leg_val_rnd = -2, 
       add = FALSE,
       leg_pos = "left")
mf_inset_on(x = pr, fig = c(.78,.9,.2,0.3))
# display the target municipality
mf_map(pr, var = "Boyce_Med_Facility_km_MINORPCT", type = "choro",
       pal = "Dark Mint", 
       breaks = c(0,.85,.95,1.5,3,10), 
       #nbreaks = 6, 
       leg_title = "Boyce Ratio", 
       #leg_val_rnd = -2, 
       add = FALSE,leg_pos = NA)
#mf_title("Puerto Rico", pos = "left", tab = TRUE, cex = .2, line = 1, inner = TRUE)

# close the inset
mf_inset_off()
mf_inset_on(x = pr, fig = c(.02,.24,.05,0.28))
mf_map(alaska, var = "Boyce_Med_Facility_km_MINORPCT", type = "choro",
       pal = "Dark Mint", 
       breaks = c(0,.85,.95,1.5,3,10), 
       #nbreaks = 6, 
       leg_title = "Boyce Ratio", 
       #leg_val_rnd = -2, 
       add = FALSE,leg_pos = NA)

mf_inset_off()
# Hawaii
mf_inset_on(x = pr, fig = c(.25,.38,.03,0.23))
mf_map(hawaii, var = "Boyce_Med_Facility_km_MINORPCT", type = "choro",
       pal = "Dark Mint", 
       breaks = c(0,.85,.95,1.5,3,10), 
       #nbreaks = 6, 
       leg_title = "Boyce Ratio", 
       #leg_val_rnd = -2, 
       add = FALSE,
       leg_pos = NA)

mf_inset_off()
mf_title("Facilities to Minorities Boyce Ratio")

dev.off()
