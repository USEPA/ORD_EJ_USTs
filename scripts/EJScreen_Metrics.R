library(tidyverse)
library(sf)
library(here)

# Load EJ data
ej <- st_read("D:/data/EJ/AM.gdb", layer = "EJ")%>%
  mutate(GISJOIN = paste0("G",substr(ID,1,2),"0",substr(ID,3,5),"0",substr(ID,6,12)))%>%
  select(GISJOIN, MINORPCT, LOWINCPCT, LESSHSPCT, LINGISOPCT, UNDER5PCT, OVER64PCT,VULEOPCT, VULSVI6PCT)
  

# Load binned datasets
allbgs <- read.csv(here("projects/EJ/Article/data/BGs_All_Attributes.csv"))%>%
  left_join(ej)

# Calculate EJ Index --- EJ = EI (DIbg - DIus) * POPbg

## Calculate U.S. Averages
pop <- allbgs%>%
  filter(Population > 0)


# Calculate EJ Metrics
allData <- allbgs

for (n in 17:20) {
  ei <- allbgs%>%
    select(GISJOIN,colnames(allbgs)[n],Population,MINORPCT, LOWINCPCT, LESSHSPCT, LINGISOPCT, UNDER5PCT, OVER64PCT, VULEOPCT, VULSVI6PCT)
  for (i in 4:11) {
    sub <- ei%>%
      select(GISJOIN, colnames(ei)[2],Population,colnames(ei)[i])
    
    eji <- sub%>%
      mutate(EJ = (.[[2]] * (.[[4]] - mean(pop[,i+17],na.rm = TRUE))))
    colnames(eji)[5] <- paste0(colnames(sub)[2],"_",colnames(sub)[4],"_EJ")
    
    ejSelect <- eji[,c(1,5)]
    
    allData <- allData%>%
      left_join(ejSelect)
  }
}

allData <- allData%>%
  select(!Shape)

write.csv(allData, here("projects/EJ/Article/data/EJScreen/BGs_All_EJ_Metrics.csv"))
#allbgsOut <- read.csv( here("projects/EJ/Article/data/EJScreen/BGs_All_EJ_Metrics.csv"))

# Calculate EJ metrics for tracts
tracts <- pop%>%
  mutate(GIS_Tract = substr(GISJOIN,1,14))%>%
  group_by(GIS_Tract)%>%
  mutate(Pop_sum = sum(Population),
         Facility_km_Avg = mean(Facility_km, na.rm = TRUE),
         Releases_km_Avg = mean(Releases_km, na.rm = TRUE),
         Tanks_km_Avg = mean(Tanks_km, na.rm = TRUE),
         Capacity_km_Avg = mean(Capacity_km, na.rm = TRUE),
         MINORPCT_Avg = mean(MINORPCT, na.rm = TRUE),
         LOWINCPCT_Avg = mean(LOWINCPCT, na.rm = TRUE),
         LESSHSPCT_Avg = mean(LESSHSPCT, na.rm = TRUE),
         LINGISOPCT_Avg = mean(LINGISOPCT, na.rm = TRUE),
         UNDER5PCT_Avg = mean(UNDER5PCT, na.rm = TRUE),
         OVER64PCT_Avg = mean(OVER64PCT, na.rm = TRUE),
         VULEOPCT_Avg = mean(VULEOPCT, na.rm = TRUE),
         VULSVI6PCT_Avg = mean(VULSVI6PCT, na.rm = TRUE))%>%
  ungroup()%>%
  select(GIS_Tract,Pop_sum,Facility_km_Avg,Releases_km_Avg,Tanks_km_Avg,Capacity_km_Avg,MINORPCT_Avg,LOWINCPCT_Avg,LESSHSPCT_Avg,
         LINGISOPCT_Avg,UNDER5PCT_Avg,OVER64PCT_Avg,VULEOPCT_Avg,VULSVI6PCT_Avg)%>%
  distinct()

# Calculate EJ Metrics

for (n in 3:6) {
  ei <- tracts%>%
    select(GIS_Tract,colnames(tracts)[n],Pop_sum,MINORPCT_Avg, LOWINCPCT_Avg, LESSHSPCT_Avg,
           LINGISOPCT_Avg, UNDER5PCT_Avg, OVER64PCT_Avg, VULEOPCT_Avg, VULSVI6PCT_Avg)
  for (i in 4:11) {
    sub <- ei%>%
      select(GIS_Tract, colnames(ei)[2],Pop_sum,colnames(ei)[i])
    
    eji <- sub%>%
      mutate(EJ = (.[[2]] * (.[[4]] - mean(pop[,i+17],na.rm = TRUE))))%>%
      select(EJ)
    
    colnames(eji)[1] <- paste0(colnames(sub)[2],"_",colnames(sub)[4],"_EJ")
    
    tracts <- cbind(tracts,eji)
  }
}

write.csv(tracts, here("projects/EJ/Article/data/EJScreen/Tracts_All_EJ_Metrics.csv"))

# Calculate EJ metrics for Counties
counties <- pop%>%
  mutate(GIS_County = substr(GISJOIN,1,8))%>%
  group_by(GIS_County)%>%
  mutate(Pop_sum = sum(Population),
         Facility_km_Avg = mean(Facility_km, na.rm = TRUE),
         Releases_km_Avg = mean(Releases_km, na.rm = TRUE),
         Tanks_km_Avg = mean(Tanks_km, na.rm = TRUE),
         Capacity_km_Avg = mean(Capacity_km, na.rm = TRUE),
         MINORPCT_Avg = mean(MINORPCT, na.rm = TRUE),
         LOWINCPCT_Avg = mean(LOWINCPCT, na.rm = TRUE),
         LESSHSPCT_Avg = mean(LESSHSPCT, na.rm = TRUE),
         LINGISOPCT_Avg = mean(LINGISOPCT, na.rm = TRUE),
         UNDER5PCT_Avg = mean(UNDER5PCT, na.rm = TRUE),
         OVER64PCT_Avg = mean(OVER64PCT, na.rm = TRUE),
         VULEOPCT_Avg = mean(VULEOPCT, na.rm = TRUE),
         VULSVI6PCT_Avg = mean(VULSVI6PCT, na.rm = TRUE))%>%
  ungroup()%>%
  select(GIS_County,Pop_sum,Facility_km_Avg,Releases_km_Avg,Tanks_km_Avg,Capacity_km_Avg,MINORPCT_Avg,LOWINCPCT_Avg,LESSHSPCT_Avg,
         LINGISOPCT_Avg,UNDER5PCT_Avg,OVER64PCT_Avg,VULEOPCT_Avg,VULSVI6PCT_Avg)%>%
  distinct()

# Calculate EJ Metrics

for (n in 3:6) {
  ei <- counties%>%
    select(GIS_County,colnames(counties)[n],Pop_sum,MINORPCT_Avg, LOWINCPCT_Avg, LESSHSPCT_Avg,
           LINGISOPCT_Avg, UNDER5PCT_Avg, OVER64PCT_Avg, VULEOPCT_Avg, VULSVI6PCT_Avg)
  for (i in 4:11) {
    sub <- ei%>%
      select(GIS_County, colnames(ei)[2],Pop_sum,colnames(ei)[i])
    
    eji <- sub%>%
      mutate(EJ = (.[[2]] * (.[[4]] - mean(pop[,i+17],na.rm = TRUE))))%>%
      select(EJ)
    
    colnames(eji)[1] <- paste0(colnames(sub)[2],"_",colnames(sub)[4],"_EJ")
    
    counties <- cbind(counties,eji)
  }
}

write.csv(counties, here("projects/EJ/Article/data/EJScreen/Counties_All_EJ_Metrics.csv"))

