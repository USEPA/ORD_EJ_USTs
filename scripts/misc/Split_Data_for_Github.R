# This is the original script used to split USTFinder data so that it could
# be uploaded to Github and comply with the 100 MB upload limit.


library(tidyverse)
library(sf)
library(vroom)
library(here)

facilitiessf <- st_read("D:/data/USTs/UST_Public.gdb", layer = "Facilities")%>%
  filter(is.na(Address_Match_Type) | Address_Match_Type %in% c("","Rooftop","StreetInt",
                                                               "PointAddress","StreetAddress","Subaddress"))


facilities <- vroom("D:/data/USTs/USTFINDER/All_Facilities.csv")%>%
  filter(is.na(Address_Match_Type) | Address_Match_Type %in% c("","Rooftop","StreetInt",
                                                               "PointAddress","StreetAddress","Subaddress"))

nomatch <- facilities%>%
  filter(!Facility_ID %in% facilitiessf$Facility_ID)


facilities_1 <- facilities[1:round(nrow(facilities)/2),]
facilities_2 <- facilities[nrow(facilities_1)+1:nrow(facilities),]


vroom_write(facilities_1, here("data/UST_Finder/facilities_1.txt"), delim = "\t")
vroom_write(facilities_2, here("data/UST_Finder/facilities_2.txt"), delim = "\t")
