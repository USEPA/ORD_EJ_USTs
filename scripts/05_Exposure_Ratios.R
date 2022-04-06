# This script runs the horizontal minority/white Exposure calculation
# https://www.ineteconomics.org/uploads/papers/WP12-Boyce-et-al.pdf

library(tidyverse)
#library(sf)
library(vroom)
library(here)

## Loop for all EJScreen Metrics

# Load EJ data
allbgs <- vroom(here("data/Created/BlkGrps_Bins.tsv"))%>%
  mutate(GIS_Tract = substr(GISJOIN,1,14),
         GIS_County = substr(GISJOIN,1,8),
         GIS_State = substr(GISJOIN,1,4),
         ID = as.character(ID))

# Calculate Boyce Ratio for Tracts
allTracts <- allbgs%>%
  select(GIS_Tract)%>%
  distinct()

for (n in 39:42) {
  ei <- allbgs%>%
    select(GIS_Tract,colnames(allbgs)[n],Population,MINORPCT, LOWINCPCT, LESSHSPCT, LINGISOPCT, UNDER5PCT, OVER64PCT, VULEOPCT)
  for (i in 4:10) {
    boyce <- ei%>%
      select(GIS_Tract, colnames(ei)[2],Population,colnames(ei)[i])%>%
      mutate(NW_Left = .[[2]] * Population * .[[4]],
             NW_Right = Population * (1-.[[4]]),
             W_Left = .[[2]] * Population * (1-.[[4]]),
             W_Right = Population * .[[4]])%>%
      group_by(GIS_Tract)%>%
      mutate(Boyce_Med = round((median(NW_Left, na.rm = TRUE)/median(NW_Right, na.rm = TRUE))/(median(W_Left, na.rm = TRUE)/median(W_Right, na.rm = TRUE)),4))%>%
      #Boyce_Avg = (mean(NW_Left, na.rm = TRUE)/mean(NW_Right, na.rm = TRUE))/(mean(W_Left, na.rm = TRUE)/mean(W_Right, na.rm = TRUE)))%>%
      ungroup()%>%
      select(GIS_Tract,Boyce_Med
             #,Boyce_Avg
      )%>%
      distinct()
    
    colnames(boyce) <- c("GIS_Tract",paste0("Boyce_Med_",colnames(ei)[2],"_",colnames(ei)[i])
                         #,paste0("Boyce_Avg_",colnames(ei)[2],"_",colnames(ei)[i])
    )
    
    allTracts <- allTracts%>%
      left_join(boyce)
    
    print(paste0("Completed ",colnames(ei)[2]," with ",colnames(ei)[i], " at: ",Sys.time()))
  }
}
# Save results
write.csv(allTracts, here("data/Created/Exposure_Ratios_Tracts.csv"))


# Calculate Boyce Ratio for Counties
allCounties <- allbgs%>%
  select(GIS_County)%>%
  distinct()

for (n in 32:34) {
  ei <- allbgs%>%
    select(GIS_County,colnames(allbgs)[n],Population,MINORPCT, LOWINCPCT, LESSHSPCT, LINGISOPCT, UNDER5PCT, OVER64PCT, VULEOPCT)
  for (i in 4:10) {
    boyce <- ei%>%
      select(GIS_County, colnames(ei)[2],Population,colnames(ei)[i])%>%
      mutate(NW_Left = .[[2]] * Population * .[[4]],
             NW_Right = Population * (1-.[[4]]),
             W_Left = .[[2]] * Population * (1-.[[4]]),
             W_Right = Population * .[[4]])%>%
      group_by(GIS_County)%>%
      mutate(Boyce_Med = round((median(NW_Left, na.rm = TRUE)/median(NW_Right, na.rm = TRUE))/(median(W_Left, na.rm = TRUE)/median(W_Right, na.rm = TRUE)),4),
             #Boyce_Med = ifelse(Boyce_Med >= 10,10,Boyce_Med),
      Boyce_Avg = (mean(NW_Left, na.rm = TRUE)/mean(NW_Right, na.rm = TRUE))/(mean(W_Left, na.rm = TRUE)/mean(W_Right, na.rm = TRUE)))%>%
      ungroup()%>%
      select(GIS_County,Boyce_Med
             ,Boyce_Avg
      )%>%
      distinct()
    
    colnames(boyce) <- c("GIS_County",paste0("Boyce_Med_",colnames(ei)[2],"_",colnames(ei)[i])
                         ,paste0("Boyce_Avg_",colnames(ei)[2],"_",colnames(ei)[i])
    )
    
    allCounties <- allCounties%>%
      left_join(boyce)
    
    print(paste0("Completed ",colnames(ei)[2]," with ",colnames(ei)[i], " at: ",Sys.time()))
  }
}
allCounties[is.na(allCounties)] <- 1
write.csv(allCounties, here("projects/EJ/Article/data/Boyce/All_Boyce_Counties_Both.csv"))

# Calculate Boyce Ratio for States
allStates <- allbgs%>%
  select(GIS_State)%>%
  distinct()

for (n in 32:34) {
  ei <- allbgs%>%
    select(GIS_State,colnames(allbgs)[n],Population,MINORPCT, LOWINCPCT, LESSHSPCT, LINGISOPCT, UNDER5PCT, OVER64PCT, VULEOPCT)
  for (i in 4:10) {
    boyce <- ei%>%
      select(GIS_State, colnames(ei)[2],Population,colnames(ei)[i])%>%
      mutate(NW_Left = .[[2]] * Population * .[[4]],
             NW_Right = Population * (1-.[[4]]),
             W_Left = .[[2]] * Population * (1-.[[4]]),
             W_Right = Population * .[[4]])%>%
      group_by(GIS_State)%>%
      mutate(Boyce_Med = round((median(NW_Left, na.rm = TRUE)/median(NW_Right, na.rm = TRUE))/(median(W_Left, na.rm = TRUE)/median(W_Right, na.rm = TRUE))),4)%>%
      #Boyce_Avg = (mean(NW_Left, na.rm = TRUE)/mean(NW_Right, na.rm = TRUE))/(mean(W_Left, na.rm = TRUE)/mean(W_Right, na.rm = TRUE)))%>%
      ungroup()%>%
      select(GIS_State,Boyce_Med
             #,Boyce_Avg
      )%>%
      distinct()
    
    colnames(boyce) <- c("GIS_State",paste0("Boyce_Med_",colnames(ei)[2],"_",colnames(ei)[i])
                         #,paste0("Boyce_Avg_",colnames(ei)[2],"_",colnames(ei)[i])
    )
    
    allStates <- allStates%>%
      left_join(boyce)
    
    print(paste0("Completed ",colnames(ei)[2]," with ",colnames(ei)[i], " at: ",Sys.time()))
  }
}
allStates[is.na(allStates)] <- 1
write.csv(allStates, here("projects/EJ/Article/data/Boyce/All_Boyce_States.csv"))



# MSAs - These are done a bit differently as we need to run an intersection on block groups first
msa <- st_read("D:/data/nhgis/Boundaries.gdb", layer = "US_cbsa_2019")%>%
  mutate(Area_Type = substr(NAMELSAD,nchar(NAMELSAD)-9,nchar(NAMELSAD)))%>%
  select(GISJOIN, NAME, ALUBE001, Area_Type)%>%
  st_transform(5070)

micro <- msa%>%
  filter(Area_Type == "Micro Area")

metro <- msa%>%
  filter(Area_Type == "Metro Area")


# Import the spatial block groups and join attributes
sf <- st_read("D:/data/EJ/EJSCREEN_2020_USPR.gdb", layer = "EJSCREEN_Full")%>%
  select(ID)%>%
  left_join(allbgs)%>%
  st_transform(5070)

intersect <- st_intersection(st_make_valid(sf), st_make_valid(metro))
colnames(intersect)[39] <- "GIS_MSA"
st_write(intersect, here("projects/EJ/Article/data/MSA_Intersect.shp"))

# Calculate Boyce Ratio for States
allMSAs <- intersect%>%
  st_drop_geometry()%>%
  select(GIS_MSA)%>%
  distinct()

msaClean <- intersect%>%
  st_drop_geometry()%>%
  distinct()

for (n in 32:34) {
  ei <- msaClean%>%
    select(GIS_MSA,colnames(msaClean)[n],Population,MINORPCT, LOWINCPCT, LESSHSPCT, LINGISOPCT, UNDER5PCT, OVER64PCT, VULEOPCT)
  for (i in 4:10) {
    boyce <- ei%>%
      select(GIS_MSA, colnames(ei)[2],Population,colnames(ei)[i])%>%
      mutate(NW_Left = .[[2]] * Population * .[[4]],
             NW_Right = Population * (1-.[[4]]),
             W_Left = .[[2]] * Population * (1-.[[4]]),
             W_Right = Population * .[[4]])%>%
      group_by(GIS_MSA)%>%
      mutate(Boyce_Med = round((median(NW_Left, na.rm = TRUE)/median(NW_Right, na.rm = TRUE))/(median(W_Left, na.rm = TRUE)/median(W_Right, na.rm = TRUE)),4))%>%
      #Boyce_Avg = round((mean(NW_Left, na.rm = TRUE)/mean(NW_Right, na.rm = TRUE))/(mean(W_Left, na.rm = TRUE)/mean(W_Right, na.rm = TRUE))),4)%>%
      ungroup()%>%
      select(GIS_MSA,Boyce_Med
             #,Boyce_Avg
      )%>%
      distinct()
    
    colnames(boyce) <- c("GIS_MSA",paste0("Boyce_Med_",colnames(ei)[2],"_",colnames(ei)[i])
                         #,paste0("Boyce_Avg_",colnames(ei)[2],"_",colnames(ei)[i])
    )
    
    allMSAs <- allMSAs%>%
      left_join(boyce)
    
    print(paste0("Completed ",colnames(ei)[2]," with ",colnames(ei)[i], " at: ",Sys.time()))
  }
}

# Replace NA values with 1 (Perfect equity)
allMSAs[is.na(allMSAs)] <- 1

write.csv(allMSAs, here("projects/EJ/Article/data/Boyce/All_Boyce_MSAs.csv"))


