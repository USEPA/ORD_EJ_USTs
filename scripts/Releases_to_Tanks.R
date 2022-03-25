# By year, what is the ratio of releases to tanks
# Do it at the block group level

# The average number of tanks


# For every year, the number of active tanks / releases that year

# We want a dataset with  row for every block group and every year(start in 1989)
# Number of active tanks, and number of confirmed releases that year


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

counties <- USAboundaries::us_counties()

# Loop through every year to find active tanks to releases ratio by block group
relOut <- data.frame()
tnkOut <- data.frame()

for (n in 1989:2018) {
  # Filter releases
  rel <- releases%>%
    filter(Year == n)%>%
    drop_na(Latitude,Longitude)
  
  # Spatial Join Releases
  relSf <- st_as_sf(rel, coords = c("Longitude","Latitude"), crs = 4326)
  
  relJoin <- st_intersection(relSf, counties)%>%
    st_drop_geometry()%>%
    select(LUST_ID,geoid,state_name)%>%
    mutate(Year = n)
  
  # Count releases by county
  relCnt <- as.data.frame(table(relJoin$geoid))
  colnames(relCnt) <- c("geoid","Releases")
  relCnt$Year <- n
  
  relOut <- rbind(relOut,relCnt)
  
  # Filter Tanks by year
  tnk <- tanks%>%
    filter(Install_Year <= n & Removal_Year >= n)%>%
    drop_na(Latitude,Longitude)
  
  # Spatial Join Tanks
  tnkSf <- st_as_sf(tnk, coords = c("Longitude","Latitude"), crs = 4326)
  
  tnkJoin <- st_intersection(tnkSf, counties)%>%
    st_drop_geometry()%>%
    select(geoid)
  
  # Count tanks by county
  tnkCnt <- as.data.frame(table(tnkJoin$geoid))
  colnames(tnkCnt) <- c("geoid","Tanks")
  tnkCnt$Year <- n
  
  tnkOut <- rbind(tnkOut,tnkCnt)
  
  print(paste0("Completed ",n," at: ", Sys.time()))
  
  
}

# Add in some label data
labels <- counties%>%
  st_drop_geometry()%>%
  select(geoid,name,state_abbr)

# Add Demographics
mn <- read.csv("D:/data/nhgis/tables/Counties/nhgis0283_ds239_20185_2018_county.csv")%>%
  select(GISJOIN,AJWNE001,AJWNE002)

# Combine datasets
df <- tnkOut%>%
  left_join(relOut)%>%
  mutate(Tanks = replace_na(Tanks,0),
         Releases= replace_na(Releases,0))%>%
  left_join(labels)%>%
  mutate(GISJOIN = paste0("G",substr(geoid,1,2),"0",substr(geoid,3,5),"0"))%>%
  select(-geoid)%>%
  left_join(mn)

colnames(df) <- c("Tanks","Year","Releases","name","state_abbr","GISJOIN","Population","White")

df$Pct_Minority <- 100*(1 - (df$White/ df$Population))

library(plotly)

lm <- lm(df$Releases/df$Tanks ~ df$Year)

df%>%
  group_by(GISJOIN)%>%
  plot_ly()%>%
  add_lines( x = ~Year, y = ~Releases/Tanks, color = ~Pct_Minority,
             hovertext = ~paste(name," County (",state_abbr,") - ",Year,"<br>",
                                "Tanks: ",Tanks,"<br>",
                                "Releases: ",Releases))

write.csv(df,here("projects/EJ/Article/data/County_Tanks_to_Releases.csv"))
library(tidyverse)
library(plotly)
library(here)

df <- read.csv(here("projects/EJ/Article/data/County_Tanks_to_Releases.csv"))
# Plot minority vs release / tanks ratio
sub <- df%>%
  filter(Tanks>0 & Releases>0)

fit <- lm(Releases/Tanks ~ Pct_Minority, data = sub)

plot_ly(sub)%>%
  add_markers(x = ~Pct_Minority, y = ~Releases/Tanks,
              hovertext = ~paste(name," County (",state_abbr,") - ",Year,"<br>",
                                 "Tanks: ",Tanks,"<br>",
                                 "Releases: ",Releases))%>%
  add_lines(x = ~Pct_Minority, y = fitted(fit))


ggplot(sub)+
  geom_point(aes(x = Pct_Minority, y = Releases/Tanks, color = Year), alpha = .1)+
  geom_smooth(aes(x = Pct_Minority, y = Releases/Tanks), color = 'orange', method = 'lm', linetype = "longdash")+
  ylim(0,5)+
  labs(x = "% Minority", y = "Releases / Tanks", title = "Reported Releases to Active Tanks (County)")+
  annotate("text", x = 75, y = 3, label =paste("R2 = ",round(summary(fit)$r.squared,5)))
