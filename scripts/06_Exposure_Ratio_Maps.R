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
cs <- sb%>%
  filter(!state_name %in% c("Puerto Rico","Alaska","Hawaii"))%>%
  st_transform(5070)

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
            theme = "dark", expandBB = c(.3,.3,0.1,.1))
  #mf_shadow(cs, add = TRUE,col = "grey50")
  # Hawaii
  mf_inset_on(x = pr, fig = c(.2,.6,.02,.35)) #c(X1,X2,Y1,Y2)
  mf_map(hawaii, var = var, type = "choro",
         pal = "Dark Mint", 
         breaks = c(0,.85,.95,1.5,3,10), 
         #nbreaks = 6, 
         leg_title = "Exposure Ratio", 
         #leg_val_rnd = -2, 
         add = FALSE,
         leg_pos = NA)
  
  mf_inset_off()
  mf_map(conus, var = var, type = "choro",
         pal = "Dark Mint", 
         breaks = c(0,.9,1.1,1.5,3,10),
         leg_title = "Exposure Ratio", 
         add = TRUE,
         leg_pos = "left")
  mf_map(x = cs,
         type = "base",
         col = "black",
         add = TRUE,
         lwd = 2,
         )
  
  mf_inset_on(x = pr, fig = c(.78,.9,.1,0.2))
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
  mf_inset_on(x = pr, fig = c(.02,.3,.05,0.35)) #c(X1,X2,Y1,Y2)
  mf_map(alaska, var = var, type = "choro",
         pal = "Dark Mint", 
         breaks = c(0,.85,.95,1.5,3,10), 
         #nbreaks = 6, 
         leg_title = "Exposure Ratio", 
         #leg_val_rnd = -2, 
         add = FALSE,leg_pos = NA)
  
  #fig = c(.25,.38,.03,0.23)
  mf_inset_off()
  
  mf_title(paste0(ustLabel, " / km2 : Percent ",ejLabel," Exposure Ratio (Counties)"),
           cex = 2)
  mf_credits(txt = paste0("Generated on: ",Sys.Date(),", Data Sources: EJScreen (2020) & USTFinder"),
             pos = "bottomright")
  
  dev.off()
  
}

# Maps for MSAs (tracts)
stFips <- fips%>%
  select(state,state_code,state_name)%>%
  distinct()

msa <- st_read(here("data/Created/Spatial/Exposure_Ratios.gpkg"), layer = "ER_MSAs")%>%
  mutate(state = substr(NAME, nchar(NAME)-1,nchar(NAME)))%>%
  left_join(stFips)%>%
  separate(NAME, into = c("Metro","State_FIPS"), sep = ",")%>%
  mutate(Metro = str_replace_all(Metro,"/","-"))

# Determine which tracts are in which MSA
tractMSA <- tracts%>%
  st_centroid()%>%
  st_intersection(msa)%>%
  st_drop_geometry()%>%
  select(GISJOIN,GISJOIN.1)

colnames(tractMSA) <- c("Tract_ID","MSA_ID")

# Filter out maps we already created


# Create folders
for (st in unique(msa$state_name)) {
  dir.create(paste0(here("figures/MSA"),"/",st))
}

for (n in 1:nrow(msa)) {
  metro <- msa[n,]
  
  prj <- statesUTM%>%
    filter(statefp == metro$state_code)
  
  metroUTM <- metro%>%
    st_transform(prj$code)%>%
    st_cast("MULTILINESTRING")
  
  metroDf <- metroUTM%>%
    st_drop_geometry()
  
  # Get tracts for the MSA
  tractIDs <- tractMSA%>%
    filter(MSA_ID == metroUTM$GISJOIN[1])
  tractSub <- tracts%>%
    filter(GISJOIN %in% tractIDs$Tract_ID)%>%
    st_transform(prj$code)
  
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
    
    # Coordinates for annotation
    bb <- st_bbox(metroUTM)
    xcoord <- as.numeric(((bb[3]-bb[1])/2)+bb[1])
    ycoord <- as.numeric(bb[2]-1000)
    
    # Make a map
    mf_export(x = metroUTM,
              filename = paste0(here("figures/MSA"),"/",metro$state_name,"/",metro$Metro,"_ER_",ustLabel,"_",ejLabel,".png"),
              width = 1200,
              theme = "dark", expandBB = c(.3,.2,0,.1))
    
    mf_map(tractSub, var = var, type = "choro",
           pal = "Dark Mint", 
           breaks = c(0,.9,1.1,1.5,3,10),
           leg_title = "Exposure Ratio", 
           add = TRUE,
           leg_pos = "left")
    mf_map(x = metroUTM,
           type = "base",
           col = "black",
           add = TRUE,
           lwd = 2,
    )
    
    mf_annotation(x = c(xcoord,ycoord),
                  txt = paste0("MSA Exposure Ratio: ",metroDf[,var],"\n",
                               "Population: ",metroDf$ALUBE001),
                  col_arrow = NA,
                  cex = 2,
                  col_txt = "white",
                  pos = "bottomright",
                  halo = TRUE,
                  bg = "black")
    
    mf_title(paste0(metro$Metro,":\n",ustLabel, " / km2 : Percent ",ejLabel," Exposure Ratio (Tracts)"),
             line = 2,
             cex = 2)
    mf_credits(txt = paste0("Projection: NAD83 (HARN) UTM Zone ",prj$ZONE,"N, ","Generated on: ",Sys.Date(),", Data Sources: EJScreen (2020) & USTFinder"),
               pos = "bottomleft")
    
    dev.off()
    
  }
  print(paste0("Completed ", round(100*(n/nrow(msa)),2),"% at: ", Sys.time()))
}
