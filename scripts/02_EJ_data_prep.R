library(tidyverse)
library(sf)
library(plotly)
library(here)
library(vroom)
library(scales)


# Import UST Facility data (625,755 out of 742,782 with filter)
facilities <- vroom("D:/data/USTs/USTFINDER/All_Facilities.csv")%>%
  filter(is.na(Address_Match_Type) | Address_Match_Type %in% c("","Rooftop","StreetInt",
                                                               "PointAddress","StreetAddress","Subaddress"))

# Import tanks installed in or after 1970 but keep NA values since we can't
# be sure of those install dates
tanks <- vroom("D:/data/USTs/USTFINDER/All_Tanks.csv")%>%
  filter(Installation_Date >= lubridate::mdy("01-01-1970") | is.na(Installation_Date) | as.character(Installation_Date) == "<NA>")


# Import releases reported in or after 1970 but keep NA values,
# also remove uncertain locations (483,446 out of 531,753 with filter).
releases <- vroom("D:/data/USTs/USTFINDER/All_Releases.csv")%>%
  filter(is.na(Address_Match_Type) | Address_Match_Type %in% c("","Rooftop","StreetInt",
                                                               "PointAddress","StreetAddress","Subaddress"))%>%
  filter(Reported_Date >= lubridate::mdy("01-01-1970") | is.na(Reported_Date) | as.character(Reported_Date) == "<NA>")

# Import EJScreen Demographic Data with block group boundaries
sf <- st_read(here("data/EJScreen/EJSCREEN_2020_USPR.gdb"), layer = "EJSCREEN_Full")%>%
mutate(GISJOIN = paste0("G",substr(ID,1,2),"0",substr(ID,3,5),"0",substr(ID,6,12)))%>% # Create GISJOIN ID in case we want to use NHGIS data later
  select(GISJOIN,ID,STATE_NAME,ACSTOTPOP,MINORPCT,LOWINCPCT,LESSHSPCT,LINGISOPCT,UNDER5PCT,OVER64PCT,VULEOPCT,AREALAND,
         P_MINORPCT,P_LWINCPCT,P_LESHSPCT,P_LNGISPCT,P_UNDR5PCT,P_OVR64PCT,P_VULEOPCT,)%>% # Trim to needed columns
  filter(AREALAND > 0 & ACSTOTPOP > 0)%>%
  st_transform(5070) # Project to Albers equal area conic projection

colnames(sf)[4] <- "Population" # Rename population code

# Make 1,500 ft. buffers around block groups
bgbuf <- st_buffer(sf, 457.2)%>% # 457.2 meters = 1,500 ft.
  st_transform(5070)

# Save buffers
st_write(bgbuf, here("data/Created/Spatial/Buffers.gpkg"), layer = "BlockGroups_1500ft", append = FALSE) #save the buffers as a shapefile

# Import buffers if you have previously run this script to save time
#bgbuf <- st_read(here("data/Created/Spatial/Buffers.gpkg"), layer = "BlockGroups_1500ft")

# Intersect Facilities with block groups
facSf <- facilities%>%
  drop_na(Latitude,Longitude)%>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326)%>%
  st_transform(5070)

facInt <- st_intersection(facSf,bgbuf)

# Intersect Releases with block groups
relSf <- releases%>%
  drop_na(Latitude,Longitude)%>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326)%>%
  st_transform(5070)
             
relInt <- st_intersection(relSf,bgbuf)


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
bgTally <- sf%>%
  st_drop_geometry()%>%
  left_join(facTally)%>%
  left_join(relTally)%>%
  left_join(tank2bg)%>%
  left_join(bgTankAge)%>%
  mutate(N_Facilities = replace_na(N_Facilities,0), # For block groups that have no facilities or releases, set NA values to zero
         N_Releases = replace_na(N_Releases,0),
         N_Tanks = replace_na(N_Tanks,0),
         Capacity = ifelse(N_Facilities == 0, 0, # Here we set capacity to zero if there are no facilities, but we keep NA values
                           ifelse(Capacity > 0, Capacity,NA))) # because some tanks have unknown capacity amounts (N = 32,951)

############
# BINNING ##
############

# Import National Percentile Data for EJScreen 2020
nEJ <- bgTally%>%
  mutate(N_P_MINORPCT_BIN = ifelse(as.numeric(P_MINORPCT) <= 10,"0-10",
                                   ifelse(as.numeric(P_MINORPCT) <=20,"10-20",
                                          ifelse(as.numeric(P_MINORPCT) <= 30,"20-30",
                                                 ifelse(as.numeric(P_MINORPCT)<=40,"30-40",
                                                        ifelse(as.numeric(P_MINORPCT)<=50,"40-50",
                                                               ifelse(as.numeric(P_MINORPCT)<=60,"50-60",
                                                                      ifelse(as.numeric(P_MINORPCT)<=70,"60-70",
                                                                             ifelse(as.numeric(P_MINORPCT)<=80,"70-80",
                                                                                    ifelse(as.numeric(P_MINORPCT)<=90,"80-90",
                                                                                           ifelse(as.numeric(P_MINORPCT)<=100,"90-100",NA)))))))))),
         N_P_LWINCPCT_BIN = ifelse(as.numeric(P_LWINCPCT) <= 10,"0-10",
                                   ifelse(as.numeric(P_LWINCPCT) <=20,"10-20",
                                          ifelse(as.numeric(P_LWINCPCT) <= 30,"20-30",
                                                 ifelse(as.numeric(P_LWINCPCT)<=40,"30-40",
                                                        ifelse(as.numeric(P_LWINCPCT)<=50,"40-50",
                                                               ifelse(as.numeric(P_LWINCPCT)<=60,"50-60",
                                                                      ifelse(as.numeric(P_LWINCPCT)<=70,"60-70",
                                                                             ifelse(as.numeric(P_LWINCPCT)<=80,"70-80",
                                                                                    ifelse(as.numeric(P_LWINCPCT)<=90,"80-90",
                                                                                           ifelse(as.numeric(P_LWINCPCT)<=100,"90-100",NA)))))))))),
         N_P_LESHSPCT_BIN = ifelse(as.numeric(P_LESHSPCT) <= 10,"0-10",
                                  ifelse(as.numeric(P_LESHSPCT) <=20,"10-20",
                                         ifelse(as.numeric(P_LESHSPCT) <= 30,"20-30",
                                                ifelse(as.numeric(P_LESHSPCT)<=40,"30-40",
                                                       ifelse(as.numeric(P_LESHSPCT)<=50,"40-50",
                                                              ifelse(as.numeric(P_LESHSPCT)<=60,"50-60",
                                                                     ifelse(as.numeric(P_LESHSPCT)<=70,"60-70",
                                                                            ifelse(as.numeric(P_LESHSPCT)<=80,"70-80",
                                                                                   ifelse(as.numeric(P_LESHSPCT)<=90,"80-90",
                                                                                          ifelse(as.numeric(P_LESHSPCT)<=100,"90-100",NA)))))))))),
         N_P_LNGISPCT_BIN = ifelse(as.numeric(P_LNGISPCT) <= 10,"0-10",
                                    ifelse(as.numeric(P_LNGISPCT) <=20,"10-20",
                                           ifelse(as.numeric(P_LNGISPCT) <= 30,"20-30",
                                                  ifelse(as.numeric(P_LNGISPCT)<=40,"30-40",
                                                         ifelse(as.numeric(P_LNGISPCT)<=50,"40-50",
                                                                ifelse(as.numeric(P_LNGISPCT)<=60,"50-60",
                                                                       ifelse(as.numeric(P_LNGISPCT)<=70,"60-70",
                                                                              ifelse(as.numeric(P_LNGISPCT)<=80,"70-80",
                                                                                     ifelse(as.numeric(P_LNGISPCT)<=90,"80-90",
                                                                                            ifelse(as.numeric(P_LNGISPCT)<=100,"90-100",NA)))))))))),
         N_P_UNDR5PCT_BIN = ifelse(as.numeric(P_UNDR5PCT) <= 10,"0-10",
                                    ifelse(as.numeric(P_UNDR5PCT) <=20,"10-20",
                                           ifelse(as.numeric(P_UNDR5PCT) <= 30,"20-30",
                                                  ifelse(as.numeric(P_UNDR5PCT)<=40,"30-40",
                                                         ifelse(as.numeric(P_UNDR5PCT)<=50,"40-50",
                                                                ifelse(as.numeric(P_UNDR5PCT)<=60,"50-60",
                                                                       ifelse(as.numeric(P_UNDR5PCT)<=70,"60-70",
                                                                              ifelse(as.numeric(P_UNDR5PCT)<=80,"70-80",
                                                                                     ifelse(as.numeric(P_UNDR5PCT)<=90,"80-90",
                                                                                            ifelse(as.numeric(P_UNDR5PCT)<=100,"90-100",NA)))))))))),
         N_P_OVR64PCT_BIN = ifelse(as.numeric(P_OVR64PCT) <= 10,"0-10",
                                    ifelse(as.numeric(P_OVR64PCT) <=20,"10-20",
                                           ifelse(as.numeric(P_OVR64PCT) <= 30,"20-30",
                                                  ifelse(as.numeric(P_OVR64PCT)<=40,"30-40",
                                                         ifelse(as.numeric(P_OVR64PCT)<=50,"40-50",
                                                                ifelse(as.numeric(P_OVR64PCT)<=60,"50-60",
                                                                       ifelse(as.numeric(P_OVR64PCT)<=70,"60-70",
                                                                              ifelse(as.numeric(P_OVR64PCT)<=80,"70-80",
                                                                                     ifelse(as.numeric(P_OVR64PCT)<=90,"80-90",
                                                                                            ifelse(as.numeric(P_OVR64PCT)<=100,"90-100",NA)))))))))),
         N_P_VULEOPCT_BIN = ifelse(as.numeric(P_VULEOPCT) <= 10,"0-10",
                                    ifelse(as.numeric(P_VULEOPCT) <=20,"10-20",
                                           ifelse(as.numeric(P_VULEOPCT) <= 30,"20-30",
                                                  ifelse(as.numeric(P_VULEOPCT)<=40,"30-40",
                                                         ifelse(as.numeric(P_VULEOPCT)<=50,"40-50",
                                                                ifelse(as.numeric(P_VULEOPCT)<=60,"50-60",
                                                                       ifelse(as.numeric(P_VULEOPCT)<=70,"60-70",
                                                                              ifelse(as.numeric(P_VULEOPCT)<=80,"70-80",
                                                                                     ifelse(as.numeric(P_VULEOPCT)<=90,"80-90",
                                                                                            ifelse(as.numeric(P_VULEOPCT)<=100,"90-100",NA)))))))))))%>%
  select(GISJOIN,N_P_MINORPCT_BIN,N_P_LWINCPCT_BIN,N_P_LESHSPCT_BIN,N_P_LNGISPCT_BIN,N_P_UNDR5PCT_BIN,N_P_OVR64PCT_BIN,N_P_VULEOPCT_BIN)
  
sEJ <- vroom(here("data/EJScreen/EJSCREEN_2020_StatePctile.csv"))%>%
  mutate(GISJOIN = paste0("G",substr(ID,1,2),"0",substr(ID,3,5),"0",substr(ID,6,12)),
         S_P_MINORPCT_BIN = ifelse(as.numeric(P_MINORPCT) <= 10,"0-10",
                                   ifelse(as.numeric(P_MINORPCT) <=20,"10-20",
                                          ifelse(as.numeric(P_MINORPCT) <= 30,"20-30",
                                                 ifelse(as.numeric(P_MINORPCT)<=40,"30-40",
                                                        ifelse(as.numeric(P_MINORPCT)<=50,"40-50",
                                                               ifelse(as.numeric(P_MINORPCT)<=60,"50-60",
                                                                      ifelse(as.numeric(P_MINORPCT)<=70,"60-70",
                                                                             ifelse(as.numeric(P_MINORPCT)<=80,"70-80",
                                                                                    ifelse(as.numeric(P_MINORPCT)<=90,"80-90",
                                                                                           ifelse(as.numeric(P_MINORPCT)<=100,"90-100",NA)))))))))),
         S_P_LWINCPCT_BIN = ifelse(as.numeric(P_LWINCPCT) <= 10,"0-10",
                                   ifelse(as.numeric(P_LWINCPCT) <=20,"10-20",
                                          ifelse(as.numeric(P_LWINCPCT) <= 30,"20-30",
                                                 ifelse(as.numeric(P_LWINCPCT)<=40,"30-40",
                                                        ifelse(as.numeric(P_LWINCPCT)<=50,"40-50",
                                                               ifelse(as.numeric(P_LWINCPCT)<=60,"50-60",
                                                                      ifelse(as.numeric(P_LWINCPCT)<=70,"60-70",
                                                                             ifelse(as.numeric(P_LWINCPCT)<=80,"70-80",
                                                                                    ifelse(as.numeric(P_LWINCPCT)<=90,"80-90",
                                                                                           ifelse(as.numeric(P_LWINCPCT)<=100,"90-100",NA)))))))))),
         S_P_LESHSPCT_BIN = ifelse(as.numeric(P_LESHSPCT) <= 10,"0-10",
                                   ifelse(as.numeric(P_LESHSPCT) <=20,"10-20",
                                          ifelse(as.numeric(P_LESHSPCT) <= 30,"20-30",
                                                 ifelse(as.numeric(P_LESHSPCT)<=40,"30-40",
                                                        ifelse(as.numeric(P_LESHSPCT)<=50,"40-50",
                                                               ifelse(as.numeric(P_LESHSPCT)<=60,"50-60",
                                                                      ifelse(as.numeric(P_LESHSPCT)<=70,"60-70",
                                                                             ifelse(as.numeric(P_LESHSPCT)<=80,"70-80",
                                                                                    ifelse(as.numeric(P_LESHSPCT)<=90,"80-90",
                                                                                           ifelse(as.numeric(P_LESHSPCT)<=100,"90-100",NA)))))))))),
         S_P_LNGISPCT_BIN = ifelse(as.numeric(P_LNGISPCT) <= 10,"0-10",
                                   ifelse(as.numeric(P_LNGISPCT) <=20,"10-20",
                                          ifelse(as.numeric(P_LNGISPCT) <= 30,"20-30",
                                                 ifelse(as.numeric(P_LNGISPCT)<=40,"30-40",
                                                        ifelse(as.numeric(P_LNGISPCT)<=50,"40-50",
                                                               ifelse(as.numeric(P_LNGISPCT)<=60,"50-60",
                                                                      ifelse(as.numeric(P_LNGISPCT)<=70,"60-70",
                                                                             ifelse(as.numeric(P_LNGISPCT)<=80,"70-80",
                                                                                    ifelse(as.numeric(P_LNGISPCT)<=90,"80-90",
                                                                                           ifelse(as.numeric(P_LNGISPCT)<=100,"90-100",NA)))))))))),
         S_P_UNDR5PCT_BIN = ifelse(as.numeric(P_UNDR5PCT) <= 10,"0-10",
                                   ifelse(as.numeric(P_UNDR5PCT) <=20,"10-20",
                                          ifelse(as.numeric(P_UNDR5PCT) <= 30,"20-30",
                                                 ifelse(as.numeric(P_UNDR5PCT)<=40,"30-40",
                                                        ifelse(as.numeric(P_UNDR5PCT)<=50,"40-50",
                                                               ifelse(as.numeric(P_UNDR5PCT)<=60,"50-60",
                                                                      ifelse(as.numeric(P_UNDR5PCT)<=70,"60-70",
                                                                             ifelse(as.numeric(P_UNDR5PCT)<=80,"70-80",
                                                                                    ifelse(as.numeric(P_UNDR5PCT)<=90,"80-90",
                                                                                           ifelse(as.numeric(P_UNDR5PCT)<=100,"90-100",NA)))))))))),
         S_P_OVR64PCT_BIN = ifelse(as.numeric(P_OVR64PCT) <= 10,"0-10",
                                   ifelse(as.numeric(P_OVR64PCT) <=20,"10-20",
                                          ifelse(as.numeric(P_OVR64PCT) <= 30,"20-30",
                                                 ifelse(as.numeric(P_OVR64PCT)<=40,"30-40",
                                                        ifelse(as.numeric(P_OVR64PCT)<=50,"40-50",
                                                               ifelse(as.numeric(P_OVR64PCT)<=60,"50-60",
                                                                      ifelse(as.numeric(P_OVR64PCT)<=70,"60-70",
                                                                             ifelse(as.numeric(P_OVR64PCT)<=80,"70-80",
                                                                                    ifelse(as.numeric(P_OVR64PCT)<=90,"80-90",
                                                                                           ifelse(as.numeric(P_OVR64PCT)<=100,"90-100",NA)))))))))),
         S_P_VULEOPCT_BIN = ifelse(as.numeric(P_VULEOPCT) <= 10,"0-10",
                                   ifelse(as.numeric(P_VULEOPCT) <=20,"10-20",
                                          ifelse(as.numeric(P_VULEOPCT) <= 30,"20-30",
                                                 ifelse(as.numeric(P_VULEOPCT)<=40,"30-40",
                                                        ifelse(as.numeric(P_VULEOPCT)<=50,"40-50",
                                                               ifelse(as.numeric(P_VULEOPCT)<=60,"50-60",
                                                                      ifelse(as.numeric(P_VULEOPCT)<=70,"60-70",
                                                                             ifelse(as.numeric(P_VULEOPCT)<=80,"70-80",
                                                                                    ifelse(as.numeric(P_VULEOPCT)<=90,"80-90",
                                                                                           ifelse(as.numeric(P_VULEOPCT)<=100,"90-100",NA)))))))))))%>%
  select(GISJOIN,S_P_MINORPCT_BIN,S_P_LWINCPCT_BIN,S_P_LESHSPCT_BIN,S_P_LNGISPCT_BIN,S_P_UNDR5PCT_BIN,S_P_OVR64PCT_BIN,S_P_VULEOPCT_BIN)

# Join all data together
bins <- bgTally%>%
  left_join(nEJ)%>%
  left_join(sEJ)%>%
  mutate(Facility_km = N_Facilities / (AREALAND/1000000),
                  Releases_km = N_Releases / (AREALAND/1000000),
                  Tanks_km = N_Tanks / (AREALAND/1000000),
                  Capacity_km = Capacity / (AREALAND/1000000))

# Save block group file with all attributes
vroom_write(bins,here("data/Created/BlkGrps_Bins.tsv"), delim = "\t")
#bins <- vroom(here("data/Created/BlkGrps_Bins.tsv"))


# CREATE LABELS FOR PLOTS
xlabels <- data.frame(Var = colnames(bins)[25:38],
                      xLabel = c("Percentile Minority (National)","Percentile Low Income (National)", "Perecntile Less Than High School (National)",
                                 "Percentile Linguistically Isolated (National)","Percentile Under 5 (National)","Percentile Over 64 (National)",
                                 "Percentile Minority & Low-Income (National)","Percentile Minority (State)","Percentile Low Income (State)",
                                 "Perecntile Less Than High School (State)",
                                 "Percentile Linguistically Isolated (State)","Percentile Under 5 (State)","Percentile Over 64 (State)",
                                 "Percentile Minority & Low-Income (State)"))

ylabels <- data.frame(Var = colnames(bins)[39:42],
                      yLabel = c("Number of Facilities","Number of Releases","Number of Tanks","Capacity (Gallons)"))

# Create empty data frame to write to
stats_out <- data.frame()

for(n in 25:32){
  xVar <- colnames(bins)[n] #Select X Variable (Percentiles)
  #xVals <- bins[,n] # Extract values from selected x variable
  for (i in 39:42) {
    yVar <- colnames(bins)[i] # Select Y variable (UST Variables)
    #yVals <- bins[,i] # Extract values for UST variables
    newDf <- data.frame("xVal" = bins[,n], "yVal" = bins[,i])%>% # Create a new data frame with x and y values
      drop_na()
    colnames(newDf) <- c("xVal","yVal")
    
    stats <- newDf%>%
      group_by(xVal)%>% # Group by the bin and compute stats
      mutate(First_Quart = quantile(yVal,.25, na.rm = TRUE),
             Median = median(yVal, na.rm = TRUE),
             Third_Quart = quantile(yVal,.75, na.rm = TRUE),
             IQR = Third_Quart - First_Quart)%>%
      ungroup()%>%
      mutate(X_Var = substr(xVar,1,nchar(xVar)-4),
             Y_Var = yVar)%>%
      select(xVal,X_Var,Y_Var,First_Quart,Median,Third_Quart,IQR)%>%
      distinct()
    colnames(stats) <- c("Bin","X_Var","Y_Var","Q1","Median","Q3","IQR")
    
    stats_out <- rbind(stats_out,stats)
    
    # Provide variable names for the output dataset
    xlab <- xlabels$xLabel[n-24]
    ylab <- ylabels$yLabel[i-38]
    
    print(paste0("Plotting ", yVar,"_by_",xVar," --- ",Sys.time()))
    
    ## Using Plotly, create output plots for every combination of X and Y variables
    
    max3q <- newDf%>%
      group_by(xVal)%>%
      mutate(TQ = quantile(yVal,.75))

    # p1 <- ggplot(newDf)+
    #   geom_boxplot(aes(x = xVal, y = yVal),outlier.shape = NA,coef = 0)+
    #   coord_cartesian(ylim=c(0, max(max3q$TQ)))+
    #   labs(title = paste0(ylab," by\n",xlab), y = paste0(ylab," [km2]"), x = "Percentile Bin")+
    #   scale_y_continuous(labels = comma)+
    #   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    # 
    # ggsave(filename = paste0("projects/EJ/Article/figures/temp","/",yVar,"_by_",xVar,".png") ,plot = p1, width = 4, units = "in", dpi = 600)
    # 
     p <- plot_ly(newDf)%>%
       add_boxplot(x = ~xVal, y = ~yVal, marker = list(opacity=0), line = list(color = 'black'), fillcolor = '#a3a3a3')%>%
       layout(title = list(text = paste0(ylab," by<br>",xlab), font = list(family = "sans serif", size = 24, color = 'black'), y = 0.9),
              yaxis = list(title = paste0(ylab,"  [km<sup>2</sup>]") ,range = c(0, (max(stats$IQR)*1.5)+max(stats$Q3)+1),
                           font = list(family = "sans serif", size = 18, color = 'black')),
              xaxis = list(title = "Percentile Bin"),
              font = list(family = "sans serif", size = 18, color = 'black'))

     save_image(p, file = paste0(here("figures/national/Bin_Box_Plots"),"/",yVar,"_by_",xVar,".png"))
    
  }
}

write.csv(stats_out, here("data/Created/National_Density_EJ_Stats.csv"))


#############################################
## Run Analysis for every individual state ##
#############################################

states <- unique(bins%>%
  filter(!STATE_NAME == "Puerto Rico")%>%
    select(STATE_NAME))
states <- states$STATE_NAME

for (state in states) {
  stateFilt <- bins%>%
    filter(STATE_NAME == state)
  
  # CREATE LABELS FOR PLOTS
  xlabels <- data.frame(Var = colnames(bins)[25:38],
                        xLabel = c("Percentile Minority (National)","Percentile Low Income (National)", "Perecntile Less Than High School (National)",
                                   "Percentile Linguistically Isolated (National)","Percentile Under 5 (National)","Percentile Over 64 (National)",
                                   "Percentile Minority & Low-Income (National)","Percentile Minority (State)","Percentile Low Income (State)",
                                   "Perecntile Less Than High School (State)",
                                   "Percentile Linguistically Isolated (State)","Percentile Under 5 (State)","Percentile Over 64 (State)",
                                   "Percentile Minority & Low-Income (State)"))
  
  ylabels <- data.frame(Var = colnames(bins)[39:42],
                        yLabel = c("Number of Facilities","Number of Releases","Number of Tanks","Capacity - Gallons"))
  
    # Create empty data frame to write to
  stats_out <- data.frame()
  
  for(n in 25:38){
    xVar <- colnames(stateFilt)[n] #Select X Variable (Percentiles)
    xVals <- stateFilt[,n] # Extract values from selected x variable
    for (i in 39:42) {
      yVar <- colnames(stateFilt)[i] # Select Y variable (UST Variables)
      yVals <- stateFilt[,i] # Extract values for UST variables
      newDf <- data.frame(xVal = xVals, yVal = yVals)%>% # Create a new data frame with x and y values
        drop_na()
      colnames(newDf) <- c("xVal","yVal")
      stats <- newDf%>%
        group_by(xVal)%>% # Group by the bin and compute stats
        mutate(First_Quart = quantile(yVal,.25, na.rm = TRUE),
               Median = median(yVal, na.rm = TRUE),
               Third_Quart = quantile(yVal,.75, na.rm = TRUE),
               IQR = Third_Quart - First_Quart)%>%
        ungroup()%>%
        as.data.frame()%>%
        mutate(X_Var = substr(xVar,1,nchar(xVar)-4),
               Y_Var = yVar)%>%
        select(xVal,X_Var,Y_Var,First_Quart,Median,Third_Quart,IQR)%>%
        distinct()
      colnames(stats) <- c("Bin","X_Var","Y_Var","Q1","Median","Q3","IQR")
      
      stats$State <- state
      
      stats_out <- rbind(stats_out,stats)
      
      # Provide variable names for the output dataset
      xlab <- xlabels$xLabel[n-24]
      ylab <- ylabels$yLabel[i-38]
      
      print(paste0("Plotting ",state,": ", yVar,"_by_",xVar," --- ",Sys.time()))
      
      
      ## Using Plotly, create output plots for every combination of X and Y variables
      
      p <- plot_ly(newDf)%>%
        add_boxplot(x = ~xVal, y = ~yVal, marker = list(opacity=0), line = list(color = 'black'), fillcolor = '#a3a3a3')%>%
        layout(title = list(text = paste0(state," ",ylab," by<br>",xlab), font = list(family = "sans serif", size = 24, color = 'black'), y = 0.9),
               yaxis = list(title = paste0(ylab,"  [km<sup>2</sup>]") ,range = c(0, (max(stats$IQR)*1.5)+max(stats$Q3)+1),
                            font = list(family = "sans serif", size = 18, color = 'black')),
               xaxis = list(title = "Percentile Bin"),
               font = list(family = "sans serif", size = 18, color = 'black'))

      dir.create( paste0(here("figures/state",state)), showWarnings = FALSE) # Create folder if it doesn't exist
      save_image(p, file = paste0(here("figures/state"),"/",state,"/",state,"_",yVar,"_by_",xVar,".png"))
      
    }
  }
  
  dir.create(paste0(here("data/Created/state_stats"),"/",state), showWarnings = TRUE) # Create folder if it doesn't exist
  write.csv(stats_out,paste0(here("data/Created/state_stats"),"/",state,"/",state,"_EJ_Bin_Stats.csv"))
  
  print(paste0("Completed ",state," --- ",Sys.time()))
}

# Run for Puerto Rico
stateFilt <- bins%>%
  filter(STATE_NAME == "Puerto Rico")

# CREATE LABELS FOR PLOTS
xlabels <- data.frame(Var = colnames(bins)[32:38],
                      xLabel = c("Percentile Minority (State)",
                                 "Percentile Low Income (State)",
                                 "Perecntile Less Than High School (State)",
                                 "Percentile Linguistically Isolated (State)",
                                 "Percentile Under 5 (State)",
                                 "Percentile Over 64 (State)",
                                 "Percentile Minority & Low-Income (State)"))

ylabels <- data.frame(Var = colnames(bins)[39:42],
                      yLabel = c("Number of Facilities","Number of Releases","Number of Tanks","Capacity - Gallons"))

# Create empty data frame to write to
stats_out <- data.frame()

for(n in 32:38){
  xVar <- colnames(stateFilt)[n] #Select X Variable (Percentiles)
  xVals <- stateFilt[,n] # Extract values from selected x variable
  for (i in 39:42) {
    yVar <- colnames(stateFilt)[i] # Select Y variable (UST Variables)
    yVals <- stateFilt[,i] # Extract values for UST variables
    newDf <- data.frame(xVal = xVals, yVal = yVals)%>% # Create a new data frame with x and y values
      drop_na()
    colnames(newDf) <- c("xVal","yVal")
    stats <- newDf%>%
      group_by(xVal)%>% # Group by the bin and compute stats
      mutate(First_Quart = quantile(yVal,.25, na.rm = TRUE),
             Median = median(yVal, na.rm = TRUE),
             Third_Quart = quantile(yVal,.75, na.rm = TRUE),
             IQR = Third_Quart - First_Quart)%>%
      ungroup()%>%
      as.data.frame()%>%
      mutate(X_Var = substr(xVar,1,nchar(xVar)-4),
             Y_Var = yVar)%>%
      select(xVal,X_Var,Y_Var,First_Quart,Median,Third_Quart,IQR)%>%
      distinct()
    colnames(stats) <- c("Bin","X_Var","Y_Var","Q1","Median","Q3","IQR")
    
    stats$State <- state
    
    stats_out <- rbind(stats_out,stats)
    
    # Provide variable names for the output dataset
    xlab <- xlabels$xLabel[n-31]
    ylab <- ylabels$yLabel[i-38]
    
    print(paste0("Plotting ",state,": ", yVar,"_by_",xVar," --- ",Sys.time()))
    
    
    ## Using Plotly, create output plots for every combination of X and Y variables
    
    p <- plot_ly(newDf)%>%
      add_boxplot(x = ~xVal, y = ~yVal, marker = list(opacity=0), line = list(color = 'black'), fillcolor = '#a3a3a3')%>%
      layout(title = list(text = paste0(state," ",ylab," by<br>",xlab), font = list(family = "sans serif", size = 24, color = 'black'), y = 0.9),
             yaxis = list(title = paste0(ylab,"  [km<sup>2</sup>]") ,range = c(0, (max(stats$IQR)*1.5)+max(stats$Q3)+1),
                          font = list(family = "sans serif", size = 18, color = 'black')),
             xaxis = list(title = "Percentile Bin"),
             font = list(family = "sans serif", size = 18, color = 'black'))
    
    dir.create( paste0(here("figures/state",state)), showWarnings = FALSE) # Create folder if it doesn't exist
    save_image(p, file = paste0(here("figures/state"),"/",state,"/",state,"_",yVar,"_by_",xVar,".png"))
    
  }
}

dir.create(paste0(here("data/Created/state_stats"),"/",state), showWarnings = TRUE) # Create folder if it doesn't exist
write.csv(stats_out,paste0(here("data/Created/state_stats"),"/",state,"/",state,"_EJ_Bin_Stats.csv"))

print(paste0("Completed ",state," --- ",Sys.time()))

