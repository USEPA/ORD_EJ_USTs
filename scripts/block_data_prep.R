library(tidyverse)
library(sf)
library(here)

# Import block polygons


# Import block demographics


# Import Facilities
facilities <- st_read("D:/data/USTs/Public.gdb", layer = "Facilities")%>%
  filter(Facility_Status == "Open UST(s)")%>%
  filter(Address_Match_Type %in% c("","StreetInt","PointAddress","StreetAddress","Subaddress"))%>%
  st_transform(5070)

# Import Tanks
tanks <- st_read("D:/data/USTs/Public.gdb", layer = "USTs")%>%
  filter(Facility_ID %in% facilities$Facility_ID & Tank_Status %in% c("Open", "Temporarily","Temporarily out of Service"))


# Import Releases
releases <- st_read("D:/data/USTs/Public.gdb", layer = "Releases")%>%
  filter(Address_Match_Type %in% c("","PointAddress","StreetInt","StreetAddress","Subaddress"))%>%
  st_transform(5070)

# Import NHGIS Census Data
## For variable names, refer to the NHGIS codebook that came with the census data you downloaded.
df <- vroom("D:/data/nhgis/tables/Block_Groups/nhgis0274_ds244_20195_2019_blck_grp.csv")%>%
  select(GISJOIN,STATE,COUNTY, ALUBE001, ALUCE001,ALUCE002,ALUCE003,ALUCE004,ALUCE005,ALUCE006,ALUCE007,ALUCE008,ALUCE009,ALUCE010,
         ALWVE001,ALWVE002,ALWVE003,ALWVE004,ALWVE005,ALWVE006,ALWVE007,ALWVE008)%>%
  mutate(Pct_Low_Income = 100*((ALWVE002+ALWVE003+ALWVE004+ALWVE005+ALWVE006+ALWVE007)/ALWVE001),
         Pct_Minority = (1-(ALUCE002/ALUCE001))*100)%>%
  select(GISJOIN,STATE,COUNTY,ALUBE001,Pct_Low_Income,Pct_Minority)


# Import block groups
bgs <- st_read("D:/data/nhgis/boundaries/Block_Groups/US_blck_grp_2019.shp")%>%
  select(GISJOIN,ALAND,AWATER)%>%
  left_join(df)
colnames(bgs)[6] <- "Population" # Rename population code

# Make 1,500 ft. buffers around block groups
bgbuf <- st_buffer(bgs, 457.2)%>% # 457.2 meters = 1,500 ft.
  st_transform(5070)

# Save buffers
st_write(bgbuf, here("projects/EJ/Article/data/BG_bufs_1500.shp"), append = FALSE) #save the buffers as a shapefile
bgbuf <- st_read(here("projects/EJ/Article/data/BG_bufs_1500.shp"))%>%
  st_transform(5070)



# Intersect Facilities with block groups
facInt <- st_intersection(facilities,bgbuf)

# Intersect Releases with block groups
relInt <- st_intersection(releases,bgbuf)


# Tally the number of facilities, tanks and releases for each block group
facTally <- facInt%>%   # Tally Facilities by block group
  st_drop_geometry()%>%
  select(GISJOIN)%>%
  group_by(GISJOIN)%>%
  tally()
colnames(facTally) <- c("GISJOIN","N_Facilities")

## Tally releases by block group
relTally <- relInt%>% 
  st_drop_geometry()%>%
  select(GISJOIN)%>%
  group_by(GISJOIN)%>%
  tally()
colnames(relTally) <- c("GISJOIN","N_Releases")

# Tally number of tanks and capacity by facility
tankTally <- tanks%>%
  group_by(Facility_ID)%>%
  tally()
colnames(tankTally) <- c("Facility_ID","N_Tanks")

# Sum the capacity of tanks by facility
capacitySum <- tanks%>%
  group_by(Facility_ID)%>%
  mutate(Capacity = sum(Capacity, na.rm = TRUE))%>%
  ungroup()%>%
  select(Facility_ID, Capacity)%>%
  distinct()

# Determine median tank age by Facility
tankAge <- tanks%>%
  mutate(installYear <- lubridate::year(Installation_Date),
         Age = as.numeric(lubridate::year(lubridate::mdy("01/01/2021")) - installYear))%>%
  group_by(Facility_ID)%>%
  mutate(Med_Age = median(Age, na.rm = TRUE))%>%
  ungroup()%>%
  select(Facility_ID,Med_Age)%>%
  distinct()

# Tank Age by block group
bgTankAge <- facInt%>%
  st_drop_geometry()%>%
  select(GISJOIN,Facility_ID)%>%
  left_join(tankAge)%>%
  group_by(GISJOIN)%>%
  mutate(Med_Age = median(Med_Age, na.rm = TRUE))%>%
  ungroup()%>%
  select(GISJOIN, Med_Age)%>%
  distinct()

# Join tank and capacity numbers to block group IDs
tank2bg <- facInt%>%
  st_drop_geometry()%>%
  select(GISJOIN,Facility_ID)%>%
  left_join(tankTally)%>%
  left_join(capacitySum)%>%
  group_by(GISJOIN)%>%
  mutate(N_Tanks = sum(N_Tanks, na.rm = TRUE),
         Capacity = sum(Capacity, na.rm = TRUE))%>%
  ungroup()%>%
  select(GISJOIN,N_Tanks,Capacity)%>%
  distinct()



# Join the tally numbers to the block groups dataset
bgTally <- bgs%>%
  left_join(facTally)%>%
  left_join(relTally)%>%
  left_join(tank2bg)%>%
  left_join(bgTankAge)%>%
  mutate(N_Facilities = replace_na(N_Facilities,0), # For block groups that have no facilities or releases, set NA values to zero
         N_Releases = replace_na(N_Releases,0),
         N_Tanks = replace_na(N_Tanks,0),
         Capacity = ifelse(N_Facilities == 0, 0, # Here we set capacity to zero if there are no facilities, but we keep NA values
                           ifelse(Capacity > 0, Capacity,NA))) # because some tanks have unknown capacity amounts (N = 32,951)

##########
# STATS ##
##########

## Bin the percent minority and low-income
bins <- bgTally%>%
  st_drop_geometry()%>%
  mutate(Bin_Pct_MN = ifelse(Pct_Minority < 10,"0-10",
                             ifelse(Pct_Minority < 20, "10-20",
                                    ifelse(Pct_Minority < 30, "20-30",
                                           ifelse(Pct_Minority < 40, "30-40",
                                                  ifelse(Pct_Minority < 50, "40-50",
                                                         ifelse(Pct_Minority < 60, "50-60",
                                                                ifelse(Pct_Minority < 70, "60-70",
                                                                       ifelse(Pct_Minority < 80, "70-80",
                                                                              ifelse(Pct_Minority < 90, "80-90",
                                                                                     ifelse(Pct_Minority <= 100,"90-100",NA)))))))))),
         Bin_Pct_LI = ifelse(Pct_Low_Income < 10,"0-10",
                             ifelse(Pct_Low_Income < 20, "10-20",
                                    ifelse(Pct_Low_Income < 30, "20-30",
                                           ifelse(Pct_Low_Income < 40, "30-40",
                                                  ifelse(Pct_Low_Income < 50, "40-50",
                                                         ifelse(Pct_Low_Income < 60, "50-60",
                                                                ifelse(Pct_Low_Income < 70, "60-70",
                                                                       ifelse(Pct_Low_Income < 80, "70-80",
                                                                              ifelse(Pct_Low_Income < 90, "80-90",
                                                                                     ifelse(Pct_Low_Income <= 100,"90-100",NA)))))))))))
# Plot of percent minority
binsNARM <- bins%>%
  drop_na(Pct_Minority)
ggplot(binsNARM)+
  geom_histogram(aes(x = Bin_Pct_MN), stat = "count")+
  labs( title = "U.S. Percent Minority by Block Group", x = "Percent Minority", y = "Count of Block Groups")+
  ylim(0,80000)+
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))


# Separate minority and low income into binned percentiles.
MN_Pctiles <- quantile(bgTally$Pct_Minority, probs = seq(0.1,.9,.1), na.rm=TRUE)
LI_Pctiles <- quantile(bgTally$Pct_Low_Income, probs = seq(0.1,.9,.1), na.rm = TRUE)

binsPctile <- bgTally%>%
  st_drop_geometry()%>%
  mutate(Bin_Tile_MN = ifelse(Pct_Minority < as.numeric(MN_Pctiles[1]),"0-10",
                              ifelse(Pct_Minority < as.numeric(MN_Pctiles[2]), "10-20",
                                     ifelse(Pct_Minority < as.numeric(MN_Pctiles[3]), "20-30",
                                            ifelse(Pct_Minority < as.numeric(MN_Pctiles[4]), "30-40",
                                                   ifelse(Pct_Minority < as.numeric(MN_Pctiles[5]), "40-50",
                                                          ifelse(Pct_Minority < as.numeric(MN_Pctiles[6]), "50-60",
                                                                 ifelse(Pct_Minority < as.numeric(MN_Pctiles[7]), "60-70",
                                                                        ifelse(Pct_Minority < as.numeric(MN_Pctiles[8]), "70-80",
                                                                               ifelse(Pct_Minority < as.numeric(MN_Pctiles[9]), "80-90",
                                                                                      ifelse(Pct_Minority >= as.numeric(MN_Pctiles[9]),"90-100",NA)))))))))),
         Bin_Tile_LI = ifelse(Pct_Low_Income < as.numeric(LI_Pctiles[1]),"0-10",
                              ifelse(Pct_Low_Income < as.numeric(LI_Pctiles[2]), "10-20",
                                     ifelse(Pct_Low_Income < as.numeric(LI_Pctiles[3]), "20-30",
                                            ifelse(Pct_Low_Income < as.numeric(LI_Pctiles[4]), "30-40",
                                                   ifelse(Pct_Low_Income < as.numeric(LI_Pctiles[5]), "40-50",
                                                          ifelse(Pct_Low_Income < as.numeric(LI_Pctiles[6]), "50-60",
                                                                 ifelse(Pct_Low_Income < as.numeric(LI_Pctiles[7]), "60-70",
                                                                        ifelse(Pct_Low_Income < as.numeric(LI_Pctiles[8]), "70-80",
                                                                               ifelse(Pct_Low_Income < as.numeric(LI_Pctiles[9]), "80-90",
                                                                                      ifelse(Pct_Low_Income >= as.numeric(LI_Pctiles[9]),"90-100",NA)))))))))),
         Facility_km = N_Facilities / (ALAND/1000000),
         Releases_km = N_Releases / (ALAND/1000000),
         Tanks_km = N_Tanks / (ALAND/1000000),
         Capacity_km = Capacity / (ALAND/1000000))


# Save block group file with all attributes
#write.csv(binsPctile, here("projects/EJ/Article/data/BGs_All_Attributes.csv"))
binsPctile <- read.csv(here("projects/EJ/Article/data/BGs_All_Attributes.csv"))

# Plot of percentile minority
binsPctileNARM <- binsPctile%>%
  drop_na(Pct_Minority)
ggplot(binsPctileNARM)+
  geom_histogram(aes(x = Bin_Tile_MN), stat = "count")+
  labs( title = "U.S. Percentile Minority by Block Group", x = "Percentile Minority", y = "Count of Block Groups")+
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE), limits = c(0,80000))

# CREATE LABELS FOR PLOTS
ylabels <- data.frame(Var = colnames(binsPctile)[16:19],
                      yLabel = c("Number of Facilities","Number of Releases","Number of Tanks","Capacity"))
xlabels <- data.frame(Var = colnames(binsPctile)[14:15],
                      xLabel = c("Percentile Minority","Percentile Low Income"))

# Create empty data frame to write to
stats_out <- data.frame()

for(n in 15:16){
  xVar <- colnames(binsPctile)[n]
  xVals <- binsPctile[,n]
  for (i in 17:20) {
    yVar <- colnames(binsPctile)[i]
    yVals <- binsPctile[,i]
    
    newDf <- data.frame(xVal = xVals, yVal = yVals)%>%
      drop_na()
    
    stats <- newDf%>%
      group_by(xVal)%>%
      mutate(First_Quart = quantile(yVal,.25, na.rm = TRUE),
             Median = median(yVal, na.rm = TRUE),
             Third_Quart = quantile(yVal,.75, na.rm = TRUE),
             IQR = Third_Quart - First_Quart)%>%
      ungroup()%>%
      as.data.frame()%>%
      mutate(X_Var = substr(xVar,4,nchar(xVar)),
             Y_Var = yVar)%>%
      select(xVal,X_Var,Y_Var,First_Quart,Median,Third_Quart,IQR)%>%
      distinct()
    colnames(stats) <- c("Bin","X_Var","Y_Var","Q1","Median","Q3","IQR")
    
    stats_out <- rbind(stats_out,stats)
    
    xlab <- xlabels$xLabel[n-14]
    ylab <- ylabels$yLabel[i-16]
    
    
    print(paste0("Plotting ", yVar,"_by_",xVar," --- ",Sys.time()))
    
    ## USING PLOTLY 
    p <- plot_ly(newDf)%>%
      add_boxplot(x = ~xVal, y = ~yVal, marker = list(opacity=0), line = list(color = 'black'), fillcolor = '#a3a3a3')%>%
      layout(title = list(text = paste0(ylab," by<br>",xlab), font = list(family = "sans serif", size = 24, color = 'black'), y = 0.9),
             yaxis = list(title = paste0(ylab,"  [km2]") ,range = c(0, (max(stats$IQR)*1.5)+max(stats$Q3)+1),
                          font = list(family = "sans serif", size = 18, color = 'black')),
             xaxis = list(title = "Percentile Bin"),
             font = list(family = "sans serif", size = 18, color = 'black'))
    
    orca(p, file = paste0("projects/EJ/Article/figures/national","/",yVar,"_by_",xVar,".png"))
    
  }
}

write.csv(stats_out, here("projects/EJ/Article/figures/national/National_Density_EJ_Stats.csv"))

# Low Income
ggplot(binsPctile)+
  geom_boxplot(aes(x = Bin_Tile_MN, y = ))


#############################################
## Run Analysis for every individual state ##
#############################################

for (state in unique(bgTally$STATE)) {
  stateFilt <- bgTally%>%
    filter(STATE == state)
  # Separate minority and low income into binned percentiles.
  MN_Pctiles <- quantile(stateFilt$Pct_Minority, probs = seq(0.1,.9,.1), na.rm=TRUE)
  LI_Pctiles <- quantile(stateFilt$Pct_Low_Income, probs = seq(0.1,.9,.1), na.rm = TRUE)
  
  dir.create( paste0(here("projects/EJ/Article/data/state_stats"),"/",state), showWarnings = FALSE) # Create folder if it doesn't exist
  write.csv(as.data.frame(MN_Pctiles), paste0(here("projects/EJ/Article/data/state_stats"),"/",state,"/",state,"_Minority_Percentiles.csv")) # save minority percentiles
  write.csv(as.data.frame(LI_Pctiles), paste0(here("projects/EJ/Article/data/state_stats"),"/",state,"/",state,"_Low_Income_Percentiles.csv")) # save low income percentiles
  
  statePctile <- stateFilt%>%
    st_drop_geometry()%>%
    mutate(Bin_Tile_MN = ifelse(Pct_Minority < as.numeric(MN_Pctiles[1]),"0-10",
                                ifelse(Pct_Minority < as.numeric(MN_Pctiles[2]), "10-20",
                                       ifelse(Pct_Minority < as.numeric(MN_Pctiles[3]), "20-30",
                                              ifelse(Pct_Minority < as.numeric(MN_Pctiles[4]), "30-40",
                                                     ifelse(Pct_Minority < as.numeric(MN_Pctiles[5]), "40-50",
                                                            ifelse(Pct_Minority < as.numeric(MN_Pctiles[6]), "50-60",
                                                                   ifelse(Pct_Minority < as.numeric(MN_Pctiles[7]), "60-70",
                                                                          ifelse(Pct_Minority < as.numeric(MN_Pctiles[8]), "70-80",
                                                                                 ifelse(Pct_Minority < as.numeric(MN_Pctiles[9]), "80-90",
                                                                                        ifelse(Pct_Minority >= as.numeric(MN_Pctiles[9]),"90-100",NA)))))))))),
           Bin_Tile_LI = ifelse(Pct_Low_Income < as.numeric(LI_Pctiles[1]),"0-10",
                                ifelse(Pct_Low_Income < as.numeric(LI_Pctiles[2]), "10-20",
                                       ifelse(Pct_Low_Income < as.numeric(LI_Pctiles[3]), "20-30",
                                              ifelse(Pct_Low_Income < as.numeric(LI_Pctiles[4]), "30-40",
                                                     ifelse(Pct_Low_Income < as.numeric(LI_Pctiles[5]), "40-50",
                                                            ifelse(Pct_Low_Income < as.numeric(LI_Pctiles[6]), "50-60",
                                                                   ifelse(Pct_Low_Income < as.numeric(LI_Pctiles[7]), "60-70",
                                                                          ifelse(Pct_Low_Income < as.numeric(LI_Pctiles[8]), "70-80",
                                                                                 ifelse(Pct_Low_Income < as.numeric(LI_Pctiles[9]), "80-90",
                                                                                        ifelse(Pct_Low_Income >= as.numeric(LI_Pctiles[9]),"90-100",NA)))))))))),
           Facility_km = N_Facilities / (ALAND/1000000),
           Releases_km = N_Releases / (ALAND/1000000),
           Tanks_km = N_Tanks / (ALAND/1000000),
           Capacity_km = Capacity / (ALAND/1000000))
  
  # CREATE LABELS FOR PLOTS
  ylabels <- data.frame(Var = colnames(binsPctile)[16:19],
                        yLabel = c("Number of Facilities","Number of Releases","Number of Tanks","Capacity"))
  xlabels <- data.frame(Var = colnames(binsPctile)[14:15],
                        xLabel = c("Percentile Minority","Percentile Low Income"))
  
  # Create empty data frame to write to
  stats_out <- data.frame()
  
  for(n in 14:15){
    xVar <- colnames(binsPctile)[n]
    xVals <- binsPctile[,n]
    for (i in 16:19) {
      yVar <- colnames(binsPctile)[i]
      yVals <- binsPctile[,i]
      
      newDf <- data.frame(xVal = xVals, yVal = yVals)%>%
        drop_na()
      
      stats <- newDf%>%
        group_by(xVal)%>%
        mutate(First_Quart = quantile(yVal,.25, na.rm = TRUE),
               Median = median(yVal, na.rm = TRUE),
               Third_Quart = quantile(yVal,.75, na.rm = TRUE),
               IQR = Third_Quart - First_Quart)%>%
        ungroup()%>%
        as.data.frame()%>%
        mutate(X_Var = substr(xVar,4,nchar(xVar)),
               Y_Var = yVar)%>%
        select(xVal,X_Var,Y_Var,First_Quart,Median,Third_Quart,IQR)%>%
        distinct()
      colnames(stats) <- c("Bin","X_Var","Y_Var","Q1","Median","Q3","IQR")
      
      stats_out <- rbind(stats_out,stats)
      
      xlab <- xlabels$xLabel[n-13]
      ylab <- ylabels$yLabel[i-15]
      
      
      print(paste0("Plotting ",state," -- ", yVar,"_by_",xVar," --- ",Sys.time()))
      
      ## USING PLOTLY INSTEAD
      p <- plot_ly(newDf)%>%
        add_boxplot(x = ~xVal, y = ~yVal, marker = list(opacity=0), line = list(color = 'black'), fillcolor = '#a3a3a3')%>%
        layout(title = list(text = paste0(state," ",ylab," by<br>",xlab), font = list(family = "sans serif", size = 24, color = 'black'), y = 0.9),
               yaxis = list(title = paste0(ylab,"  [km2]") ,range = c(0, (max(stats$IQR)*1.5)+max(stats$Q3)+1),
                            font = list(family = "sans serif", size = 18, color = 'black')),
               xaxis = list(title = "Percentile Bin"),
               font = list(family = "sans serif", size = 18, color = 'black'))
      
      dir.create( paste0(here("projects/EJ/Article/figures/state",state)), showWarnings = FALSE) # Create folder if it doesn't exist
      orca(p, file = paste0("projects/EJ/Article/figures/state","/",state,"/",state,"_",yVar,"_by_",xVar,".png"))
      
    }
  }
  write.csv(stats_out,paste0(here("projects/EJ/Article/data/state_stats"),"/",state,"/",state,"_EJ_Stats.csv"))
}