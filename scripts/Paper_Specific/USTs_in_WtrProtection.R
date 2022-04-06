# This code provides the percentages given in the paper which describe USTs
# and LUSTs which are located within surface water protection areas
# and wellhead protection areas.
library(tidyverse)
library(vroom)
library(here)

facility_files <- list.files(here("data/UST_Finder/"), pattern = "facilities",
                             full.names = TRUE)
facilities <- vroom(facility_files)%>%
  filter(is.na(Address_Match_Type) | Address_Match_Type %in% c("","Rooftop","StreetInt",
                                                               "PointAddress","StreetAddress","Subaddress"))

tanks_files <- list.files(here("data/UST_Finder/"), pattern = "tanks",
                          full.names = TRUE)
tanks <- vroom(tanks_files)%>%
  filter(Installation_Date >= lubridate::mdy("01-01-1970") | is.na(Installation_Date) | as.character(Installation_Date) == "<NA>")
  
release_files <- list.files(here("data/UST_Finder/"), pattern = "releases",
                            full.names = TRUE)
releases <- vroom(release_files)%>%
  filter(is.na(Address_Match_Type) | Address_Match_Type %in% c("","Rooftop","StreetInt",
                                                               "PointAddress","StreetAddress","Subaddress"))%>%
  filter(Reported_Date >= lubridate::mdy("01-01-1970") | is.na(Reported_Date) | as.character(Reported_Date) == "<NA>")

# Percent of all releases in surface water protection area

swpaReleases <- releases%>%
  filter(Within_SPA == "Yes")

round(100*(nrow(swpaReleases)/nrow(releases)),3)

# Percent of all releases in well head protection area
whpaReleases <- releases%>%
  filter(Within_WHPA == "Yes")

round(100*(nrow(whpaReleases)/nrow(releases)),3)

# Percent of active tanks in groundwater or surface water protection area

## Active tanks
activeTanks <- tanks%>%
  filter(Tank_Status %in% c("Open","Temporarily","Temporarily out of Service"))
## facilities within SWPA or WHPA
fac_in_wtrPro <- facilities%>%
  filter(Within_SPA == "Yes" | Within_WHPA == "Yes")

## Active tanks within SPWA or WHPA
act_Tanks_in_wtrPro <- activeTanks%>%
  filter(Facility_ID %in% fac_in_wtrPro$Facility_ID)

## PErcent of tanks within WHPA or SWPA
round(100*(nrow(act_Tanks_in_wtrPro)/nrow(activeTanks)),3)

## Percent of open releases in whpa or swpa
openReleases <- releases%>%
  filter(Status == "Open")

## Active releases within WHPA or SWPA
releases_in_WtrPro <- openReleases%>%
  filter(Within_SPA == "Yes" | Within_WHPA == "Yes")

## Percent
round(100*(nrow(releases_in_WtrPro)/nrow(openReleases)),3)


# Wells within 1,500 ft of an active LUST
sum(openReleases$DomesticWells_within_1500ft, na.rm = TRUE)
