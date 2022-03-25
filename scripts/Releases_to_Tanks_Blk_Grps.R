# By year, what is the ratio of releases to tanks
# Do it at the block group level

# The average number of tanks


# For every year, the number of active tanks / releases that year

# We want a dataset with  row for every block group and every year(start in 1989)
# Number of active tanks, and number of confirmed releases that year

library(tidyverse)
library(sf)
library(plotly)
library(here)
library(vroom)


facilities <- vroom("D:/data/USTs/USTFINDER/All_Facilities.csv")%>%
  filter(is.na(Address_Match_Type) | Address_Match_Type %in% c("","Rooftop","StreetInt",
                                                               "PointAddress","StreetAddress","Subaddress"))%>%
  select(Facility_ID,Latitude,Longitude)

tanks <- vroom("D:/data/USTs/USTFINDER/All_Tanks.csv")%>%
  filter(Installation_Date >= lubridate::mdy("01-01-1970") | is.na(Installation_Date) | as.character(Installation_Date) == "<NA>")%>%
  mutate(Install_Year = lubridate::year(Installation_Date),
         Removal_Year = lubridate::year(Removal_Date),
         Removal_Year = ifelse(Tank_Status == "Currently in Use", 2022,
                               ifelse(Tank_Status == "Temporarily Out of Use", 2022,
                                      ifelse(Tank_Status == "Open", 2022,
                                             ifelse(Tank_Status == "Temporarily", 2022,Removal_Year)))))%>%
  left_join(facilities)



releases <- vroom("D:/data/USTs/USTFINDER/All_Releases.csv")%>%
  filter(is.na(Address_Match_Type) | Address_Match_Type %in% c("","Rooftop","StreetInt",
                                                               "PointAddress","StreetAddress","Subaddress"))%>%
  filter(Reported_Date >= lubridate::mdy("01-01-1970") | is.na(Reported_Date) | as.character(Reported_Date) == "<NA>")%>%
  select(LUST_ID,Latitude,Longitude,Reported_Date)%>%
  mutate(Year = lubridate::year(Reported_Date))


blkGrps <- st_read("D:/data/EJ/EJSCREEN_2020_USPR.gdb", layer = "EJSCREEN_Full")%>%
  mutate(GISJOIN = paste0("G",substr(ID,1,2),"0",substr(ID,3,5),"0",substr(ID,6,12)))%>% # Create GISJOIN ID in case we want to use NHGIS data later
  select(GISJOIN,ID,STATE_NAME,ACSTOTPOP,MINORPCT,LOWINCPCT,P_MINORPCT,P_LWINCPCT,AREALAND)%>% # Trim to needed columns
  filter(AREALAND > 0 & ACSTOTPOP > 0)%>%
  st_transform(5070) # Project to Albers equal area conic projection

# Loop through every year to find active tanks to releases ratio by block group
relOut <- data.frame()
tnkOut <- data.frame()

for (n in 1989:2018) {
  # Filter releases
  rel <- releases%>%
    filter(Year == n)%>%
    drop_na(Latitude,Longitude)
  
  # Spatial Join Releases
  relSf <- st_as_sf(rel, coords = c("Longitude","Latitude"), crs = 4326)%>%
    st_transform(5070) # Project to Albers equal area conic projection
  
  relJoin <- st_intersection(relSf, blkGrps)%>%
    st_drop_geometry()%>%
    select(GISJOIN,ACSTOTPOP,MINORPCT,LOWINCPCT)%>%
    mutate(Year = n)
  
  # Count releases by county
  relCnt <- as.data.frame(table(relJoin$GISJOIN))
  colnames(relCnt) <- c("GISJOIN","Releases")
  relCnt$Year <- n
  
  rout <- relJoin%>%
    distinct()%>%
    left_join(relCnt)
  
  relOut <- rbind(relOut,rout)
  
  # Filter Tanks by year
  tnk <- tanks%>%
    filter(Install_Year <= n & Removal_Year >= n)%>%
    drop_na(Latitude,Longitude)
  
  # Spatial Join Tanks
  tnkSf <- st_as_sf(tnk, coords = c("Longitude","Latitude"), crs = 4326)%>%
    st_transform(5070) # Project to Albers equal area conic projection
  
  tnkJoin <- st_intersection(tnkSf, blkGrps)%>%
    st_drop_geometry()%>%
    select(GISJOIN,ACSTOTPOP,MINORPCT,LOWINCPCT)
  
  # Count tanks by county
  tnkCnt <- as.data.frame(table(tnkJoin$GISJOIN))
  colnames(tnkCnt) <- c("GISJOIN","Tanks")
  tnkCnt$Year <- n
  
  tnkOut <- rbind(tnkOut,tnkCnt)
  
  print(paste0("Completed ",n," at: ", Sys.time()))
  
  
}

demo <- blkGrps%>%
  st_drop_geometry()

# Combine datasets
df <- tnkOut%>%
  left_join(relOut)%>%
  mutate(Tanks = replace_na(Tanks,0),
         Releases= replace_na(Releases,0))%>%
  select(GISJOIN,Year,Tanks,Releases)%>%
  left_join(demo)


library(plotly)

lm <- lm(df$Releases/df$Tanks ~ df$Year)

df%>%
  group_by(GISJOIN)%>%
  plot_ly()%>%
  add_lines( x = ~Year, y = ~Releases/Tanks, color = ~Pct_Minority,
             hovertext = ~paste(name," County (",state_abbr,") - ",Year,"<br>",
                                "Tanks: ",Tanks,"<br>",
                                "Releases: ",Releases))

write.csv(df,here("projects/EJ/Article/data/BlkGrps_Tanks_to_Releases.csv"))
df <- read.csv(here("projects/EJ/Article/data/BlkGrps_Tanks_to_Releases.csv"))
# Plot minority vs release / tanks ratio
sub <- df%>%
  filter(Tanks>0 & Releases>0)

fit <- lm(Releases/Tanks ~ P_MINORPCT, data = sub)

plot_ly(sub)%>%
  add_markers(x = ~Pct_Minority, y = ~Releases/Tanks,
              hovertext = ~paste(name," County (",state_abbr,") - ",Year,"<br>",
                                 "Tanks: ",Tanks,"<br>",
                                 "Releases: ",Releases))%>%
  add_lines(x = ~Pct_Minority, y = fitted(fit))


ggplot(sub)+
  geom_point(aes(x = P_MINORPCT, y = Releases/Tanks, color = Year), alpha = .01)+
  geom_smooth(aes(x = P_MINORPCT , y = Releases/Tanks), color = 'orange', method = 'lm', linetype = "longdash")+
  ylim(0,.15)+
  labs(x = "Percentile Minority", y = "Releases / Tanks", title = "Reported Releases to Active Tanks (Block Group)")+
  annotate("text", x = 75, y = 2.6, label =paste("R2 = ",round(summary(fit)$r.squared,5)))


# Year for x-axis
ggplot(sub)+
  geom_point(aes(x = Year, y = Releases/Tanks), alpha = .01)+
  labs(x = "Year", y = "Releases / Tanks", title = "Reported Releases to Active Tanks (Block Group)")+
  ylim(0,5)
  #annotate("text", x = 75, y = 2.6, label =paste("R2 = ",round(summary(fit)$r.squared,5)))

# Average by year
avg <- sub%>%
  group_by(GISJOIN)%>%
  mutate(Avg_Releases = mean(Releases, na.rm=TRUE),
         Avg_Tanks = mean(Tanks, na.rm = TRUE))%>%
  select(GISJOIN,Avg_Releases,Avg_Tanks,P_MINORPCT,P_LWINCPCT,STATE_NAME)%>%
  distinct()%>%
  mutate(Min_Bin = ifelse(P_MINORPCT < 10,"0-10",
                          ifelse(P_MINORPCT < 20, "10-20",
                                 ifelse(P_MINORPCT < 30, "20-30",
                                        ifelse(P_MINORPCT < 40, "30-40",
                                               ifelse(P_MINORPCT < 50, "40-50",
                                                      ifelse(P_MINORPCT < 60, "50-60",
                                                             ifelse(P_MINORPCT < 70, "60-70",
                                                                    ifelse(P_MINORPCT < 80, "70-80",
                                                                           ifelse(P_MINORPCT < 90, "80-90","90-100"))))))))),
         LI_Bin = ifelse(P_LWINCPCT < 10,"0-10",
                          ifelse(P_LWINCPCT < 20, "10-20",
                                 ifelse(P_LWINCPCT < 30, "20-30",
                                        ifelse(P_LWINCPCT < 40, "30-40",
                                               ifelse(P_LWINCPCT < 50, "40-50",
                                                      ifelse(P_LWINCPCT < 60, "50-60",
                                                             ifelse(P_LWINCPCT < 70, "60-70",
                                                                    ifelse(P_LWINCPCT < 80, "70-80",
                                                                           ifelse(P_LWINCPCT < 90, "80-90","90-100"))))))))))

ggplot(avg)+
  geom_point(aes(x = P_LWINCPCT, y = Avg_Releases/Avg_Tanks), alpha = .05)+
  geom_smooth(aes(x = P_LWINCPCT , y = Avg_Releases/Avg_Tanks), color = 'green', method = 'lm', linetype = "longdash", lwd = 2)+
  ylim(0,1.2)+
  labs(x = "Percentile Low-Income", y = "Avg Releases / Tanks", title = "Reported Releases to Active Tanks (Block Group)")+
  annotate("text", x = 75, y = 2.6, label =paste("R2 = ",round(summary(fit)$r.squared,5)))

write.csv(avg,here("projects/EJ/Tanks_2_Releases_Avg_Blk_Grps.csv"))
# Box and whiskers
## Minority
p1 <- ggplot(avg)+
  geom_boxplot(aes(x = Min_Bin, y = Avg_Releases/Avg_Tanks, group = Min_Bin), alpha = .05)+
  #geom_smooth(aes(x = P_MINORPCT , y = Avg_Releases/Avg_Tanks), color = 'green', method = 'lm', linetype = "longdash", lwd = 2)+
  ylim(0,1.2)+
  labs(x = "Percentile Minority", y = "Avg Releases / Tanks", title = "Reported Releases to Active Tanks (Block Group)")
#annotate("text", x = 75, y = 2.6, label =paste("R2 = ",round(summary(fit)$r.squared,5)))

ggsave(plot = p1,here("projects/EJ/Article/figures/Releases_to_tanks_Minority.png"),device = "png", height = 4, units = "in")

## Low Income
p2 <- ggplot(avg)+
  geom_boxplot(aes(x = LI_Bin, y = Avg_Releases/Avg_Tanks, group = LI_Bin), alpha = .05)+
  #geom_smooth(aes(x = P_MINORPCT , y = Avg_Releases/Avg_Tanks), color = 'green', method = 'lm', linetype = "longdash", lwd = 2)+
  ylim(0,1.2)+
  labs(x = "Percentile Low-Income", y = "Avg Releases / Tanks", title = "Reported Releases to Active Tanks (Block Group)")

ggsave(plot = p2,here("projects/EJ/Article/figures/Releases_to_tanks_Low_Income.png"),device = "png", height = 4, units = "in")

# Loop through by state
for(state in unique(avg$STATE_NAME)){
  stateFilt <- avg%>%
    filter(STATE_NAME == state)
  
  # Fit regression
  lm <- lm(Avg_Releases/Avg_Tanks ~ P_MINORPCT, data = stateFilt)
  
  p <- ggplot(stateFilt)+
    geom_point(aes(x = P_MINORPCT, y = Avg_Releases/Avg_Tanks), alpha = 1)+
    geom_smooth(aes(x = P_MINORPCT , y = Avg_Releases/Avg_Tanks), color = 'red', method = 'lm', linetype = "longdash")+
    ylim(0,1.1)+
    labs(x = "Percentile Minority", y = "Avg Releases / Tanks", title = paste0(state," - Reported Releases to Active Tanks (Block Groups)"))+
    annotate("text", x = 75, y = .75, color = 'red', label =paste("R2 = ",round(summary(lm)$r.squared,5)))
  
  ggsave(plot = p, paste0("D:/data/temp/Minority_",state,".png"),device = "png", height = 4, units = "in")
  
  print(paste0("Completed ",state," at: ",Sys.time()))
}


















