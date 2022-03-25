# This script runs the horizontal minority/white Exposure calculation
# https://www.ineteconomics.org/uploads/papers/WP12-Boyce-et-al.pdf

library(tidyverse)
library(sf)
library(here)

## Loop for all EJScreen Metrics

# Load EJ data
ej <- st_read("D:/data/EJ/AM.gdb", layer = "EJ")%>%
  mutate(GISJOIN = paste0("G",substr(ID,1,2),"0",substr(ID,3,5),"0",substr(ID,6,12)))%>%
  select(GISJOIN, MINORPCT, LOWINCPCT, LESSHSPCT, LINGISOPCT, UNDER5PCT, OVER64PCT,VULEOPCT, VULSVI6PCT)


# Load binned datasets
allbgs <- read.csv(here("projects/EJ/Article/data/BGs_All_Attributes.csv"))%>%
  left_join(ej)

df <- allbgs%>%
  mutate(GIS_Tract = substr(GISJOIN,1,14),
         GIS_County = substr(GISJOIN,1,8),
         GIS_State = substr(GISJOIN,1,4))

# Calculate Boyce Ratio for Tracts
allTracts <- df%>%
  select(GIS_Tract)%>%
  distinct()

for (n in 17:20) {
  ei <- df%>%
    select(GIS_Tract,colnames(allbgs)[n],Population,MINORPCT, LOWINCPCT, LESSHSPCT, LINGISOPCT, UNDER5PCT, OVER64PCT, VULEOPCT, VULSVI6PCT)
  for (i in 4:11) {
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
allTracts[is.na(allTracts)] <- 1
write.csv(allTracts, here("projects/EJ/Article/data/Boyce/All_Boyce_Tracts.csv"))


# Calculate Boyce Ratio for Counties
allCounties <- df%>%
  select(GIS_County)%>%
  distinct()

for (n in 17:20) {
  ei <- df%>%
    select(GIS_County,colnames(allbgs)[n],Population,MINORPCT, LOWINCPCT, LESSHSPCT, LINGISOPCT, UNDER5PCT, OVER64PCT, VULEOPCT, VULSVI6PCT)
  for (i in 4:11) {
    boyce <- ei%>%
      select(GIS_County, colnames(ei)[2],Population,colnames(ei)[i])%>%
      mutate(NW_Left = .[[2]] * Population * .[[4]],
             NW_Right = Population * (1-.[[4]]),
             W_Left = .[[2]] * Population * (1-.[[4]]),
             W_Right = Population * .[[4]])%>%
      group_by(GIS_County)%>%
      mutate(Boyce_Med = round((median(NW_Left, na.rm = TRUE)/median(NW_Right, na.rm = TRUE))/(median(W_Left, na.rm = TRUE)/median(W_Right, na.rm = TRUE)),4))%>%
             #Boyce_Avg = (mean(NW_Left, na.rm = TRUE)/mean(NW_Right, na.rm = TRUE))/(mean(W_Left, na.rm = TRUE)/mean(W_Right, na.rm = TRUE)))%>%
      ungroup()%>%
      select(GIS_County,Boyce_Med
             #,Boyce_Avg
             )%>%
      distinct()
    
    colnames(boyce) <- c("GIS_County",paste0("Boyce_Med_",colnames(ei)[2],"_",colnames(ei)[i])
                         #,paste0("Boyce_Avg_",colnames(ei)[2],"_",colnames(ei)[i])
                         )
    
    allCounties <- allCounties%>%
      left_join(boyce)
    
    print(paste0("Completed ",colnames(ei)[2]," with ",colnames(ei)[i], " at: ",Sys.time()))
  }
}
allCounties[is.na(allCounties)] <- 1
write.csv(allCounties, here("projects/EJ/Article/data/Boyce/All_Boyce_Counties.csv"))

# Calculate Boyce Ratio for States
allStates <- df%>%
  select(GIS_State)%>%
  distinct()

for (n in 17:20) {
  ei <- df%>%
    select(GIS_State,colnames(allbgs)[n],Population,MINORPCT, LOWINCPCT, LESSHSPCT, LINGISOPCT, UNDER5PCT, OVER64PCT, VULEOPCT, VULSVI6PCT)
  for (i in 4:11) {
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

attributes <- read.csv(here("projects/EJ/Article/data/BGs_All_Attributes.csv"))

blkGrps <- ej%>%
  left_join(attributes)%>%
  st_transform(5070)

intersect <- st_intersection(st_make_valid(blkGrps), st_make_valid(metro))
colnames(intersect)[29] <- "GIS_MSA"
st_write(intersect, here("projects/EJ/Article/data/MSA_Intersect.shp"))

# Calculate Boyce Ratio for States
allMSAs <- intersect%>%
  st_drop_geometry()%>%
  select(GIS_MSA)%>%
  distinct()

msaClean <- intersect%>%
  st_drop_geometry()%>%
  distinct()

for (n in 25:28) {
  ei <- msaClean%>%
    select(GIS_MSA,colnames(msaClean)[n],Population,MINORPCT, LOWINCPCT, LESSHSPCT, LINGISOPCT, UNDER5PCT, OVER64PCT, VULEOPCT, VULSVI6PCT)
  for (i in 4:11) {
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


############
# OLD CODE #
############


# 
# # Load binned datasets
# allbgs <- read.csv(here("projects/EJ/Article/data/BGs_All_Attributes.csv"))
# 
# 
# # We need to conduct the boyce analysis at multiple levels
# # Census Tract, County, State, National
# # Create columns for tract and county
# 
# df <- allbgs%>%
#   mutate(GIS_Tract = substr(GISJOIN,1,14),
#          GIS_County = substr(GISJOIN,1,8),
#          GIS_State = substr(GISJOIN,1,4))
# 
# 
# tracts <- df%>%
#   select(GIS_Tract, STATE)%>%
#   distinct()
# 
# # Boyce minority/white using the median & average
# 
# # Tract
# 
# ## Facilities
# btractFac <- df%>%
#   mutate(NW_Left = Facility_km * Population * (Pct_Minority/100),
#          NW_Right = Population * (1-(Pct_Minority/100)),
#          W_Left = Facility_km * Population * (1-(Pct_Minority/100)),
#          W_Right = Population * (Pct_Minority/100))%>%
#   group_by(GIS_Tract)%>%
#   mutate(Boyce_Med_Fac = (median(NW_Left, na.rm = TRUE)/median(NW_Right, na.rm = TRUE))/(median(W_Left, na.rm = TRUE)/median(W_Right, na.rm = TRUE)),
#          Boyce_Avg_Fac = (mean(NW_Left, na.rm = TRUE)/mean(NW_Right, na.rm = TRUE))/(mean(W_Left, na.rm = TRUE)/mean(W_Right, na.rm = TRUE)))%>%
#   ungroup()%>%
#   select(GIS_Tract,Boyce_Med_Fac,Boyce_Avg_Fac)%>%
#   distinct()
# 
# ggplot(btractFac)+
#   geom_point(aes(x = Boyce_Med_Fac, y = Boyce_Avg_Fac))
# 
# ## Releases
# btractRel <- df%>%
#   mutate(NW_Left = Releases_km * Population * (Pct_Minority/100),
#          NW_Right = Population * (1-(Pct_Minority/100)),
#          W_Left = Releases_km * Population * (1-(Pct_Minority/100)),
#          W_Right = Population * (Pct_Minority/100))%>%
#   group_by(GIS_Tract)%>%
#   mutate(Boyce_Med_Rel = (median(NW_Left, na.rm = TRUE)/median(NW_Right, na.rm = TRUE))/(median(W_Left, na.rm = TRUE)/median(W_Right, na.rm = TRUE)),
#          Boyce_Avg_Rel = (mean(NW_Left, na.rm = TRUE)/mean(NW_Right, na.rm = TRUE))/(mean(W_Left, na.rm = TRUE)/mean(W_Right, na.rm = TRUE)))%>%
#   ungroup()%>%
#   select(GIS_Tract,Boyce_Med_Rel,Boyce_Avg_Rel)%>%
#   distinct()
# 
# ## Tanks
# btractTnk <- df%>%
#   mutate(NW_Left = Tanks_km * Population * (Pct_Minority/100),
#          NW_Right = Population * (1-(Pct_Minority/100)),
#          W_Left = Tanks_km * Population * (1-(Pct_Minority/100)),
#          W_Right = Population * (Pct_Minority/100))%>%
#   group_by(GIS_Tract)%>%
#   mutate(Boyce_Med_Tnk = (median(NW_Left, na.rm = TRUE)/median(NW_Right, na.rm = TRUE))/(median(W_Left, na.rm = TRUE)/median(W_Right, na.rm = TRUE)),
#          Boyce_Avg_Tnk = (mean(NW_Left, na.rm = TRUE)/mean(NW_Right, na.rm = TRUE))/(mean(W_Left, na.rm = TRUE)/mean(W_Right, na.rm = TRUE)))%>%
#   ungroup()%>%
#   select(GIS_Tract,Boyce_Med_Tnk,Boyce_Avg_Tnk)%>%
#   distinct()
# 
# # Capacity
# btractCap <- df%>%
#   mutate(NW_Left = Capacity_km * Population * (Pct_Minority/100),
#          NW_Right = Population * (1-(Pct_Minority/100)),
#          W_Left = Capacity_km * Population * (1-(Pct_Minority/100)),
#          W_Right = Population * (Pct_Minority/100))%>%
#   group_by(GIS_Tract)%>%
#   mutate(Boyce_Med_Cap = (median(NW_Left, na.rm = TRUE)/median(NW_Right, na.rm = TRUE))/(median(W_Left, na.rm = TRUE)/median(W_Right, na.rm = TRUE)),
#          Boyce_Avg_Cap = (mean(NW_Left, na.rm = TRUE)/mean(NW_Right, na.rm = TRUE))/(mean(W_Left, na.rm = TRUE)/mean(W_Right, na.rm = TRUE)))%>%
#   ungroup()%>%
#   select(GIS_Tract,Boyce_Med_Cap,Boyce_Avg_Cap)%>%
#   distinct()
# 
# tractOut <- btractFac%>%
#   left_join(btractRel)%>%
#   left_join(btractTnk)%>%
#   left_join(btractCap)
# 
# write.csv(tractOut, here("projects/EJ/Article/data/Boyce/Tracts_All_Boyce.csv"))
# 
# 
# # Counties
# ## Facilities
# bCountyFac <- df%>%
#   mutate(NW_Left = Facility_km * Population * (Pct_Minority/100),
#          NW_Right = Population * (1-(Pct_Minority/100)),
#          W_Left = Facility_km * Population * (1-(Pct_Minority/100)),
#          W_Right = Population * (Pct_Minority/100))%>%
#   group_by(GIS_County)%>%
#   mutate(Boyce_Med_Fac = (median(NW_Left, na.rm = TRUE)/median(NW_Right, na.rm = TRUE))/(median(W_Left, na.rm = TRUE)/median(W_Right, na.rm = TRUE)),
#          Boyce_Avg_Fac = (mean(NW_Left, na.rm = TRUE)/mean(NW_Right, na.rm = TRUE))/(mean(W_Left, na.rm = TRUE)/mean(W_Right, na.rm = TRUE)))%>%
#   ungroup()%>%
#   select(GIS_County,Boyce_Med_Fac,Boyce_Avg_Fac)%>%
#   distinct()
# 
# ggplot(bCountyFac)+
#   geom_point(aes(x = Boyce_Med_Fac, y = Boyce_Avg_Fac))
# 
# ## Releases
# bCountyRel <- df%>%
#   mutate(NW_Left = Releases_km * Population * (Pct_Minority/100),
#          NW_Right = Population * (1-(Pct_Minority/100)),
#          W_Left = Releases_km * Population * (1-(Pct_Minority/100)),
#          W_Right = Population * (Pct_Minority/100))%>%
#   group_by(GIS_County)%>%
#   mutate(Boyce_Med_Rel = (median(NW_Left, na.rm = TRUE)/median(NW_Right, na.rm = TRUE))/(median(W_Left, na.rm = TRUE)/median(W_Right, na.rm = TRUE)),
#          Boyce_Avg_Rel = (mean(NW_Left, na.rm = TRUE)/mean(NW_Right, na.rm = TRUE))/(mean(W_Left, na.rm = TRUE)/mean(W_Right, na.rm = TRUE)))%>%
#   ungroup()%>%
#   select(GIS_County,Boyce_Med_Rel,Boyce_Avg_Rel)%>%
#   distinct()
# 
# ## Tanks
# bCountyTnk <- df%>%
#   mutate(NW_Left = Tanks_km * Population * (Pct_Minority/100),
#          NW_Right = Population * (1-(Pct_Minority/100)),
#          W_Left = Tanks_km * Population * (1-(Pct_Minority/100)),
#          W_Right = Population * (Pct_Minority/100))%>%
#   group_by(GIS_County)%>%
#   mutate(Boyce_Med_Tnk = (median(NW_Left, na.rm = TRUE)/median(NW_Right, na.rm = TRUE))/(median(W_Left, na.rm = TRUE)/median(W_Right, na.rm = TRUE)),
#          Boyce_Avg_Tnk = (mean(NW_Left, na.rm = TRUE)/mean(NW_Right, na.rm = TRUE))/(mean(W_Left, na.rm = TRUE)/mean(W_Right, na.rm = TRUE)))%>%
#   ungroup()%>%
#   select(GIS_County,Boyce_Med_Tnk,Boyce_Avg_Tnk)%>%
#   distinct()
# 
# # Capacity
# bCountyCap <- df%>%
#   mutate(NW_Left = Capacity_km * Population * (Pct_Minority/100),
#          NW_Right = Population * (1-(Pct_Minority/100)),
#          W_Left = Capacity_km * Population * (1-(Pct_Minority/100)),
#          W_Right = Population * (Pct_Minority/100))%>%
#   group_by(GIS_County)%>%
#   mutate(Boyce_Med_Cap = (median(NW_Left, na.rm = TRUE)/median(NW_Right, na.rm = TRUE))/(median(W_Left, na.rm = TRUE)/median(W_Right, na.rm = TRUE)),
#          Boyce_Avg_Cap = (mean(NW_Left, na.rm = TRUE)/mean(NW_Right, na.rm = TRUE))/(mean(W_Left, na.rm = TRUE)/mean(W_Right, na.rm = TRUE)))%>%
#   ungroup()%>%
#   select(GIS_County,Boyce_Med_Cap,Boyce_Avg_Cap)%>%
#   distinct()
# 
# CountyOut <- bCountyFac%>%
#   left_join(bCountyRel)%>%
#   left_join(bCountyTnk)%>%
#   left_join(bCountyCap)
# 
# write.csv(CountyOut, here("projects/EJ/Article/data/Boyce/Counties_All_Boyce.csv"))
# 
# # States
# ## Facilities
# bStateFac <- df%>%
#   mutate(NW_Left = Facility_km * Population * (Pct_Minority/100),
#          NW_Right = Population * (1-(Pct_Minority/100)),
#          W_Left = Facility_km * Population * (1-(Pct_Minority/100)),
#          W_Right = Population * (Pct_Minority/100))%>%
#   group_by(GIS_State)%>%
#   mutate(Boyce_Med_Fac = (median(NW_Left, na.rm = TRUE)/median(NW_Right, na.rm = TRUE))/(median(W_Left, na.rm = TRUE)/median(W_Right, na.rm = TRUE)),
#          Boyce_Avg_Fac = (mean(NW_Left, na.rm = TRUE)/mean(NW_Right, na.rm = TRUE))/(mean(W_Left, na.rm = TRUE)/mean(W_Right, na.rm = TRUE)))%>%
#   ungroup()%>%
#   select(GIS_State,Boyce_Med_Fac,Boyce_Avg_Fac)%>%
#   distinct()
# 
# ggplot(bStateFac)+
#   geom_point(aes(x = Boyce_Med_Fac, y = Boyce_Avg_Fac))
# 
# ## Releases
# bStateRel <- df%>%
#   mutate(NW_Left = Releases_km * Population * (Pct_Minority/100),
#          NW_Right = Population * (1-(Pct_Minority/100)),
#          W_Left = Releases_km * Population * (1-(Pct_Minority/100)),
#          W_Right = Population * (Pct_Minority/100))%>%
#   group_by(GIS_State)%>%
#   mutate(Boyce_Med_Rel = (median(NW_Left, na.rm = TRUE)/median(NW_Right, na.rm = TRUE))/(median(W_Left, na.rm = TRUE)/median(W_Right, na.rm = TRUE)),
#          Boyce_Avg_Rel = (mean(NW_Left, na.rm = TRUE)/mean(NW_Right, na.rm = TRUE))/(mean(W_Left, na.rm = TRUE)/mean(W_Right, na.rm = TRUE)))%>%
#   ungroup()%>%
#   select(GIS_State,Boyce_Med_Rel,Boyce_Avg_Rel)%>%
#   distinct()
# 
# ## Tanks
# bStateTnk <- df%>%
#   mutate(NW_Left = Tanks_km * Population * (Pct_Minority/100),
#          NW_Right = Population * (1-(Pct_Minority/100)),
#          W_Left = Tanks_km * Population * (1-(Pct_Minority/100)),
#          W_Right = Population * (Pct_Minority/100))%>%
#   group_by(GIS_State)%>%
#   mutate(Boyce_Med_Tnk = (median(NW_Left, na.rm = TRUE)/median(NW_Right, na.rm = TRUE))/(median(W_Left, na.rm = TRUE)/median(W_Right, na.rm = TRUE)),
#          Boyce_Avg_Tnk = (mean(NW_Left, na.rm = TRUE)/mean(NW_Right, na.rm = TRUE))/(mean(W_Left, na.rm = TRUE)/mean(W_Right, na.rm = TRUE)))%>%
#   ungroup()%>%
#   select(GIS_State,Boyce_Med_Tnk,Boyce_Avg_Tnk)%>%
#   distinct()
# 
# # Capacity
# bStateCap <- df%>%
#   mutate(NW_Left = Capacity_km * Population * (Pct_Minority/100),
#          NW_Right = Population * (1-(Pct_Minority/100)),
#          W_Left = Capacity_km * Population * (1-(Pct_Minority/100)),
#          W_Right = Population * (Pct_Minority/100))%>%
#   group_by(GIS_State)%>%
#   mutate(Boyce_Med_Cap = (median(NW_Left, na.rm = TRUE)/median(NW_Right, na.rm = TRUE))/(median(W_Left, na.rm = TRUE)/median(W_Right, na.rm = TRUE)),
#          Boyce_Avg_Cap = (mean(NW_Left, na.rm = TRUE)/mean(NW_Right, na.rm = TRUE))/(mean(W_Left, na.rm = TRUE)/mean(W_Right, na.rm = TRUE)))%>%
#   ungroup()%>%
#   select(GIS_State,Boyce_Med_Cap,Boyce_Avg_Cap)%>%
#   distinct()
# 
# StateOut <- bStateFac%>%
#   left_join(bStateRel)%>%
#   left_join(bStateTnk)%>%
#   left_join(bStateCap)
# 
# write.csv(StateOut, here("projects/EJ/Article/data/Boyce/States_All_Boyce.csv"))





