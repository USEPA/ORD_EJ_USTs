library(tidyverse)
library(sf)
library(here)

tanks <- st_read("D:/data/USTs/Public.gdb", layer = "USTs")

facilities <- st_read("D:/data/USTs/Public.gdb", layer = "Facilities")
actFacilities <- facilities%>%
  filter(Facility_Status == "Open UST(s)")
  

releases <- st_read("D:/data/USTs/Public.gdb", layer = "Releases")


# How Current is the data?
lastDate <- tanks%>%
  group_by(State)%>%
  mutate(MaxDate = max(Installation_Date, na.rm = TRUE))%>%
  ungroup()%>%
  select(State,MaxDate)%>%
  distinct()

#### About 2018

# Number of Tanks:
table(tanks$Tank_Status)

# Number of Facilities
table(facilities$Facility_Status)

# Number of Releases


# Tally of release date counts by state

## Releases by state
stateRel <- releases%>%
  st_drop_geometry()%>%
  group_by(State)%>%
  tally()

colnames(stateRel) <- c("State","Releases")

## Reported Date NA by state
stateNAdate <- releases%>%
  st_drop_geometry()%>%
  filter(is.na(Reported_Date))%>%
  group_by(State)%>%
  tally()

colnames(stateNAdate) <- c("State","NA_Dates")
## Reported date by state
statedate <- releases%>%
  st_drop_geometry()%>%
  filter(!is.na(Reported_Date))%>%
  group_by(State)%>%
  tally()
colnames(statedate) <- c("State","Known_Dates")
## Join and calculate
releaseDates <- stateRel%>%
  left_join(stateNAdate)%>%
  left_join(statedate)%>%
  mutate(Pct_Known = (Known_Dates / Releases)*100)

## Known releases for 30 states with complete date data
filt30 <- releaseDates%>%
  filter(Pct_Known > 99)

release30 <- releases%>%
  st_drop_geometry()%>%
  filter(State %in% filt30$State)%>%
  mutate(Year = lubridate::year(Reported_Date))

ggplot(release30)+
  geom_histogram(aes(x = Year))

releasetab <- as.data.frame(table(release30$Year))

releasetab%>%
  filter(Var1 > 1999)
  pull(Freq)
  
# All release dates
years <- releases%>%
  st_drop_geometry()%>%
  mutate(Year = lubridate::year(Reported_Date))

## Since 1990
releases1990 <- years%>%
  filter(Year > 1990)

## Locational accuracy of facilities
table(actFacilities$Address_Match_Type)

accurate <- actFacilities%>%
  st_drop_geometry()%>%
  filter(Address_Match_Type %in% c("","PointAddress","StreetAddress","StreetAddressExt","Subaddress"))
187338/195976

## Locational accuracy of Releases
releaseAcc <- 
  releases%>%
  st_drop_geometry()
table(releaseAcc$Address_Match_Type)

releaseAccurate <- releaseAcc%>%
  filter(Address_Match_Type %in% c("","PointAddress","StreetAddress","StreetAddressExt","Subaddress"))
nrow(releaseAccurate) / nrow(releases)


# Tanks at active / accurate facilities.
tanksAcc <- tanks%>%
  filter(Facility_ID %in% accurate$Facility_ID & Tank_Status %in% c("Open", "Temporarily","Temporarily out of Service"))
