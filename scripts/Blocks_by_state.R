library(tidyverse)
library(sf)
library(vroom)
library(rstatix)



# Import block demographics
## For variable names, refer to the NHGIS codebook that came with the census data you downloaded.
df <- vroom("/work/TANKSDB/github/UST_groundwater_vulnerability_model/data/NHGIS/Tables/Blocks/nhgis0275_ds172_2010_block.csv")%>%
  select(GISJOIN,STATE,COUNTY,H7V001,H7X001,H7X002)%>% # Import Population, and white alone
  mutate(Pct_Minority = (1-(H7X002/H7X001))*100)%>%
  select(GISJOIN,STATE,COUNTY,H7V001,Pct_Minority)%>%
  filter(H7V001 > 0)

# Import Facilities
facilities <- st_read("/work/TANKSDB/github/UST_groundwater_vulnerability_model/data/USTs/Public.gdb", layer = "Facilities")%>%
  filter(Facility_Status == "Open UST(s)")%>%
  filter(Address_Match_Type %in% c("","StreetInt","PointAddress","StreetAddress","Subaddress"))%>%
  st_transform(5070)

# Import Tanks
tanks <- st_read("/work/TANKSDB/github/UST_groundwater_vulnerability_model/data/USTs/Public.gdb", layer = "USTs")%>%
  filter(Facility_ID %in% facilities$Facility_ID & Tank_Status %in% c("Open", "Temporarily","Temporarily out of Service"))


# Import Releases
releases <- st_read("/work/TANKSDB/github/UST_groundwater_vulnerability_model/data/USTs/Public.gdb", layer = "Releases")%>%
  filter(Address_Match_Type %in% c("","PointAddress","StreetInt","StreetAddress","Subaddress"))%>%
  st_transform(5070)



# Import block polygons state by state and iterate the analysis
layers <- st_layers("/work/TANKSDB/github/UST_groundwater_vulnerability_model/data/NHGIS/Blocks.gdb")

for (n in 1:length(layers$name)) {
  state <- layers$name[n]
  print(paste0("Beginning ",substr(state,1,2)," at: ",Sys.time()))
  
  blks <- st_read("/work/TANKSDB/github/UST_groundwater_vulnerability_model/data/NHGIS/Blocks.gdb", layer = state)%>%
    select(GISJOIN,ALAND10,AWATER10)%>%
    left_join(df)
  colnames(blks)[6] <- "Population" # Rename population code
  
  print(paste0("Creating buffers for ",substr(state,1,2)," at: ",Sys.time()))
  # Make 1,500 ft. buffers around blocks
  bbuf <- st_buffer(blks, 457.2)%>% # 457.2 meters = 1,500 ft.
    st_transform(5070)
  
  # Save buffers
  st_write(bbuf, dsn = paste0("/work/TANKSDB/github/UST_groundwater_vulnerability_model/projects/EJ/Article/data/blocks/Blk_bufs_1500_",substr(state,1,2),".shp")) #save the buffers as a shapefile
  
  print(paste0("Calculating intersections for ",substr(state,1,2)," at: ",Sys.time()))
  # Intersect Facilities with blocks
  facInt <- st_intersection(facilities,bbuf)
  
  # Intersect Releases with blocks
  relInt <- st_intersection(releases,bbuf)
  
  print(paste0("Tabulating the data for ",substr(state,1,2)," at: ",Sys.time()))
  
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
  blkTankAge <- facInt%>%
    st_drop_geometry()%>%
    select(GISJOIN,Facility_ID)%>%
    left_join(tankAge)%>%
    group_by(GISJOIN)%>%
    mutate(Med_Age = median(Med_Age, na.rm = TRUE))%>%
    ungroup()%>%
    select(GISJOIN, Med_Age)%>%
    distinct()
  
  # Join tank and capacity numbers to block group IDs
  tank2blk <- facInt%>%
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
  

  # Join the tally numbers to the blocks dataset
  blkTally <- blks%>%
    left_join(facTally)%>%
    left_join(relTally)%>%
    left_join(tank2blk)%>%
    left_join(blkTankAge)%>%
    mutate(N_Facilities = replace_na(N_Facilities,0), # For blocks that have no facilities or releases, set NA values to zero
           N_Releases = replace_na(N_Releases,0),
           N_Tanks = replace_na(N_Tanks,0),
           Capacity = ifelse(N_Facilities == 0, 0, # Here we set capacity to zero if there are no facilities, but we keep NA values
                             ifelse(Capacity > 0, Capacity,NA))) # because some tanks have unknown capacity amounts (N = 32,951)
  
  ##########
  # STATS ##
  ##########
  
  print(paste0("Calculating Bins and Densities for ",substr(state,1,2)," at: ",Sys.time()))
  
  # Separate minority and low income into binned percentiles.
  MN_Pctiles <- quantile(blkTally$Pct_Minority, probs = seq(0.1,.9,.1), na.rm=TRUE)
  
  binsPctile <- blkTally%>%
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
           Facility_km = N_Facilities / (ALAND10/1000000),
           Releases_km = N_Releases / (ALAND10/1000000),
           Tanks_km = N_Tanks / (ALAND10/1000000),
           Capacity_km = Capacity / (ALAND10/1000000))
  
  
  # Save block group file with all attributes
  write.csv(binsPctile, paste0("/work/TANKSDB/github/UST_groundwater_vulnerability_model/projects/EJ/Article/data/By_State/Census_Blocks/",substr(state,1,2),"_blks_All_Attributes.csv"))
  
  # CREATE LABELS FOR PLOTS
  xlabels <- data.frame(Var = colnames(binsPctile)[13],
                        xLabel = c("Percentile Minority"))
  ylabels <- data.frame(Var = colnames(binsPctile)[14:17],
                        yLabel = c("Number of Facilities","Number of Releases","Number of Tanks","Capacity"))
  
  # Create empty data frame to write to
  stats_out <- data.frame()
  
  for(n in 13){
    xVar <- colnames(binsPctile)[n]
    xVals <- binsPctile[,n]
    for (i in 14:17) {
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
      stats$State <- substr(state,1,2)
      
      stats_out <- rbind(stats_out,stats)
      
#      xlab <- xlabels$xLabel[n-12]
#      ylab <- ylabels$yLabel[i-16]
      
      
#      print(paste0("Plotting ", yVar,"_by_",xVar," --- ",Sys.time()))
      
      ## USING PLOTLY 
#      p <- plot_ly(newDf)%>%
#        add_boxplot(x = ~xVal, y = ~yVal, marker = list(opacity=0), line = list(color = 'black'), fillcolor = '#a3a3a3')%>%
#        layout(title = list(text = paste0(ylab," by<br>",xlab), font = list(family = "sans serif", size = 24, color = 'black'), y = 0.9),
#               yaxis = list(title = paste0(ylab,"  [km2]") ,range = c(0, (max(stats$IQR)*1.5)+max(stats$Q3)+1),
#                            font = list(family = "sans serif", size = 18, color = 'black')),
#               xaxis = list(title = "Percentile Bin"),
#               font = list(family = "sans serif", size = 18, color = 'black'))
      
#     orca(p, file = paste0("projects/EJ/Article/figures/national","/",yVar,"_by_",xVar,".png"))
      
    }
  }
  
  write.csv(stats_out, paste0("/work/TANKSDB/github/UST_groundwater_vulnerability_model/projects/EJ/Article/data/By_State/Census_Blocks/",substr(state,1,2),"_Density_EJ_Stats.csv"))
  
  print(paste0("Beginning Pairwise Wilcox for ",substr(state,1,2)," at: ",Sys.time()))
  ####################
  # Pairwise Wilcox ##
  ####################
  
  PW <- data.frame()
  
  for (n in 13) {
    
    for (i in 14:17) {
      sub <- binsPctile[,c(n,i)]%>%
        arrange(.[[1]])
      colnames(sub) <- c("Bin","Density")
      pw <- sub%>%
        wilcox_test(Density ~ Bin, p.adjust.method = "bonferroni")%>%
        as.data.frame()
      pw.effect <- sub%>%
        wilcox_effsize(Density ~ Bin)%>%
        as.data.frame()%>%
        select(effsize,magnitude)
      newData <- cbind(pw,pw.effect)%>%
        mutate("X_Var" = colnames(binsPctile)[n],
               "Y_Var" = colnames(binsPctile)[i])
      
      PW <- rbind(PW,newData)
      
      print(paste0("Completed: ",substr(state,1,2)," ",colnames(binsPctile)[n]," with ", colnames(binsPctile)[i]," --- ",Sys.time()))
    }
  }
  
  write.csv(PW, paste0("/work/TANKSDB/github/UST_groundwater_vulnerability_model/projects/EJ/Article/data/By_State/Census_Blocks/",substr(state,1,2),"_Pairwise_Wilcox.csv"))
  
}
