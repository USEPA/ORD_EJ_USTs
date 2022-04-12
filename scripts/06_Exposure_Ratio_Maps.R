library(mapsf)
library(sf)
library(tidyverse)
library(vroom)
library(here)
library(USAboundaries)

fips <- tigris::fips_codes%>%
  mutate(STCO = paste0(state_code,"_",county_code))

# Download state boundaries for mapping
sb <- us_states()%>%
  st_cast("MULTILINESTRING")

# Load Expoure ratios for Census tracts.
tracts <- st_read(here("data/Created/Spatial/Exposure_Ratios.gpkg"), layer = "ER_Tracts")%>%
  mutate(STCO = paste0(substr(GISJOIN,2,3),"_",substr(GISJOIN,5,7)))%>%
  left_join(fips)

# Figure out projection for each state
utm <- st_read(here("data/UTM_Zones.shp"))

# Load crs info
utmCrs <- rgdal::make_EPSG()%>%
  filter(substr(note,1,22) == "NAD83(HARN) / UTM zone")%>%
  separate(note, into = c("A","B","C","D","E"), sep = " ")%>%
  mutate(ZONE = as.numeric(substr(E,1,nchar(E)-1)))%>%
  select(code,ZONE)

# join crs numbers to states
statesUTM <- us_states()%>%
  st_transform(3857)%>%
  st_centroid()%>%
  st_intersection(utm)%>%
  select(statefp,state_name,ZONE)%>%
  left_join(utmCrs)

# Iterate through every state and every exposure ratio to make maps
# for tracts
for (st in unique(tracts$state_name)) {
  # Determine projection
  prj <- statesUTM%>%
    filter(state_name == st)
  
  stateFilt <- tracts%>%
    filter(state_name == st)%>%
    st_transform(crs = prj$code)
  # Iterate through ratios
  for(n in 3:30){
    
    # Generate labels
    parse <- str_split(colnames(stateFilt)[n], "_", simplify = TRUE)
    
    if(parse[3]=="Facility"){
      ustLabel <- "Facilities"
    } else {
      ustLabel <- parse[3]
    }
    
    if(parse[5] == "MINORPCT"){
      ejLabel <- "Minority"
    } else if(parse[5] == "LESSHSPCT"){
      ejLabel <-  "Less than High School"
    } else if(parse[5] == "UNDER5PCT"){
      ejLabel <-  "Under 5"
    } else if(parse[5] == "VULEOPCT"){
      ejLabel <-  "Minority & Low-Income"
    } else if(parse[5] == "LOWINCPCT"){
      ejLabel <- "Low-Income"
    } else if(parse[5] == "LINGISOPCT"){
      ejLabel <- "Linguistically Isolated"
    } else if(parse[5] == "OVER64PCT"){
      ejLabel <- "Over 64"
    }
    
    
    # Make a map
    dir.create(paste0(here("figures/state"),"/",st,"/Exposure_Ratios"))
    mf_export(x = stateFilt, filename = paste0(here("figures/state"),"/",st,"/Exposure_Ratios/",st,"_ER_Tracts_",
                                               ustLabel,"_",ejLabel,".png"), width = 1200,
              theme = "default", expandBB = c(.3,1,0,.5)) 
    mf_theme("dark",bg = "#1f3e6e", fg = "#808080")
    mf_shadow(stateFilt, col = "grey10",cex = .1, add = TRUE)
    mf_map(stateFilt, var = colnames(stateFilt)[n], type = "choro",
           pal = "Dark Mint", 
           breaks = c(0,.9,1.1,1.5,3,10),
           lwd = 0.1,
           #nbreaks = 6, 
           leg_title = "Exposure Ratio",
           leg_title_cex = 2,
           leg_val_cex = 1.2, 
           add = FALSE,
           leg_pos = "left")
    
    mf_title(paste0(st," ",ustLabel, " / km2 : Percent ",ejLabel," Exposure Ratio"),
             cex = 2)
    mf_arrow(pos="topright")
    mf_credits(txt = paste0("Projection: NAD83 (HARN) UTM Zone: ",prj$ZONE,"N, ",
                               "Generated: ", Sys.Date(),
                               ", Data Sources: EJScreen (2020) & USTFinder"),
               pos = "bottomleft", cex = 1.3)
    
    dev.off()
  }
  print(paste0("Completed ", st, " at: ", Sys.time()))
}

# Load Exposure ratios for Counties.
counties <- st_read(here("data/Created/Spatial/Exposure_Ratios.gpkg"), layer = "ER_Counties")%>%
  mutate(STCO = paste0(substr(GISJOIN,2,3),"_",substr(GISJOIN,5,7)))%>%
  left_join(fips)

erCols <- data.frame(Name = colnames(counties))%>%
  filter(substr(Name,7,9)=="Med")

# Iterate through every state and every exposure ratio to make maps
# for counties
for (st in unique(counties$state_name)) {
  # Determine projection
  prj <- statesUTM%>%
    filter(state_name == st)
  
  stateFilt <- counties%>%
    filter(state_name == st)%>%
    st_transform(crs = prj$code)
  # Iterate through ratios
  for(var in unique(erCols$Name)){
    
    # Generate labels
    parse <- str_split(var, "_", simplify = TRUE)
    
    if(parse[3]=="Facility"){
      ustLabel <- "Facilities"
    } else {
      ustLabel <- parse[3]
    }
    
    if(parse[5] == "MINORPCT"){
      ejLabel <- "Minority"
    } else if(parse[5] == "LESSHSPCT"){
      ejLabel <-  "Less than High School"
    } else if(parse[5] == "UNDER5PCT"){
      ejLabel <-  "Under 5"
    } else if(parse[5] == "VULEOPCT"){
      ejLabel <-  "Minority & Low-Income"
    } else if(parse[5] == "LOWINCPCT"){
      ejLabel <- "Low-Income"
    } else if(parse[5] == "LINGISOPCT"){
      ejLabel <- "Linguistically Isolated"
    } else if(parse[5] == "OVER64PCT"){
      ejLabel <- "Over 64"
    }
    
    
    # Make a map
    dir.create(paste0(here("figures/state"),"/",st,"/Exposure_Ratios"))
    mf_export(x = stateFilt, filename = paste0(here("figures/state"),"/",st,"/Exposure_Ratios/",st,"_ER_Counties_",
                                               ustLabel,"_",ejLabel,".png"), width = 1200,
              theme = "default", expandBB = c(.3,1,0,.5)) 
    mf_theme("dark",bg = "#1f3e6e", fg = "#808080")
    mf_map(stateFilt, var = var, type = "choro",
           pal = "Dark Mint", 
           breaks = c(0,.9,1.1,1.5,3,10),
           lwd = 0.1,
           #nbreaks = 6, 
           leg_title = "Exposure Ratio",
           leg_title_cex = 2,
           leg_val_cex = 1.2, 
           add = FALSE,
           leg_pos = "left")
    
    mf_title(paste0(st," ",ustLabel, " / km2 : Percent ",ejLabel," Exposure Ratio (Counties)"),
             cex = 2)
    mf_arrow(pos="topright")
    mf_credits(txt = paste0("Projection: NAD83 (HARN) UTM Zone: ",prj$ZONE,"N, ",
                            "Generated: ", Sys.Date(),
                            ", Data Sources: EJScreen (2020) & USTFinder"),
               pos = "bottomleft", cex = 1.3)
    
    dev.off()
  }
  print(paste0("Completed ", st, " at: ", Sys.time()))
}




# National Maps

# Break geographies for insets
conus <- counties%>%
  filter(!state_name %in% c("Puerto Rico","Alaska","Hawaii"))
alaska <- counties%>%
  filter(state_name == "Alaska")%>%
  st_transform(3467)
hawaii <- counties%>%
  filter(state_name == "Hawaii")%>%
  st_transform("+proj=aea +lat_1=8 +lat_2=18 +lat_0=13 +lon_0=-157 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
pr <- counties%>%
  filter(state_name == "Puerto Rico")

for(var in unique(erCols$Name)){
  
  # Generate labels
  parse <- str_split(var, "_", simplify = TRUE)
  
  if(parse[3]=="Facility"){
    ustLabel <- "Facilities"
  } else {
    ustLabel <- parse[3]
  }
  
  if(parse[5] == "MINORPCT"){
    ejLabel <- "Minority"
  } else if(parse[5] == "LESSHSPCT"){
    ejLabel <-  "Less than High School"
  } else if(parse[5] == "UNDER5PCT"){
    ejLabel <-  "Under 5"
  } else if(parse[5] == "VULEOPCT"){
    ejLabel <-  "Minority & Low-Income"
  } else if(parse[5] == "LOWINCPCT"){
    ejLabel <- "Low-Income"
  } else if(parse[5] == "LINGISOPCT"){
    ejLabel <- "Linguistically Isolated"
  } else if(parse[5] == "OVER64PCT"){
    ejLabel <- "Over 64"
  }
  
  # Make a map
  mf_export(x = conus,
            filename = paste0(here("figures/national/Exposure_Ratios"),"/National_ER_",ustLabel,"_",ejLabel,".png"),
            width = 1200,
            theme = "default", expandBB = c(.3,0,0,.5)) 
  mf_theme("dark",bg = "#1f3e6e", fg = "#808080")
  mf_map(conus, var = var, type = "choro",
         pal = "Dark Mint", 
         breaks = c(0,.9,1.1,1.5,3,10), 
         #nbreaks = 6, 
         leg_title = "Boyce Ratio", 
         #leg_val_rnd = -2, 
         add = FALSE,
         leg_pos = "left")
  mf_inset_on(x = pr, fig = c(.78,.9,.2,0.3))
  # display the target municipality
  mf_map(pr, var = var, type = "choro",
         pal = "Dark Mint", 
         breaks = c(0,.85,.95,1.5,3,10), 
         #nbreaks = 6, 
         leg_title = "Exposure Ratio", 
         #leg_val_rnd = -2, 
         add = FALSE,leg_pos = NA)
  #mf_title("Puerto Rico", pos = "left", tab = TRUE, cex = .2, line = 1, inner = TRUE)
  
  # close the inset
  mf_inset_off()
  mf_inset_on(x = pr, fig = c(.02,.24,.05,0.28))
  mf_map(alaska, var = var, type = "choro",
         pal = "Dark Mint", 
         breaks = c(0,.85,.95,1.5,3,10), 
         #nbreaks = 6, 
         leg_title = "Exposure Ratio", 
         #leg_val_rnd = -2, 
         add = FALSE,leg_pos = NA)
  
  mf_inset_off()
  # Hawaii
  mf_inset_on(x = pr, fig = c(.25,.38,.03,0.23))
  mf_map(hawaii, var = var, type = "choro",
         pal = "Dark Mint", 
         breaks = c(0,.85,.95,1.5,3,10), 
         #nbreaks = 6, 
         leg_title = "Exposure Ratio", 
         #leg_val_rnd = -2, 
         add = FALSE,
         leg_pos = NA)
  
  mf_inset_off()
  mf_title(paste0(ustLabel, " / km2 : Percent ",ejLabel," Exposure Ratio (Counties)"),
           cex = 2)
  
  dev.off()
  
}



# Make National Maps