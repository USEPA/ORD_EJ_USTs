# This is the original script used to split USTFinder data so that it could
# be uploaded to Github and comply with the 100 MB upload limit.

library(tidyverse)
library(sf)
library(vroom)
library(here)

# Facilities
facilities <- st_read("D:/data/USTs/USTFINDER/Finals.gdb", layer = "Facilities")%>%
  st_drop_geometry()

facEnd1 <- round(nrow(facilities)/2)
facStart2 <- facEnd1+1
facLastRow <- nrow(facilities)

# Split in half
facilities_1 <- facilities[1:facEnd1,]
facilities_2 <- facilities[facStart2:facLastRow,]

# Write facilities in halves
vroom_write(facilities_1, here("data/UST_Finder/facilities_1.txt"), delim = "\t")
vroom_write(facilities_2, here("data/UST_Finder/facilities_2.txt"), delim = "\t")

# Tanks
tanks <- st_read("D:/data/USTs/USTFINDER/Finals.gdb", layer = "USTs")

tanksEnd1 <- round(nrow(tanks)/2)
tanksStart2 <- tanksEnd1+1

# Split in half
tanks_1 <- tanks[1:tanksEnd1,]
tanks_2 <- tanks[tanksStart2:nrow(tanks),]

# Write tanks in halves
vroom_write(tanks_1, here("data/UST_Finder/tanks_1.txt"), delim = "\t")
vroom_write(tanks_2, here("data/UST_Finder/tanks_2.txt"), delim = "\t")

# LUSTs
releases <- st_read("D:/data/USTs/USTFINDER/Finals.gdb", layer = "Releases")%>%
  st_drop_geometry()

releasesEnd1 <- round(nrow(releases)/2)
releasesStart2 <- releasesEnd1 + 1

# Split in half
releases_1 <- releases[1:releasesEnd1,]
releases_2 <- releases[releasesStart2: nrow(releases),]

# Write releases in halves
vroom_write(releases_1, here("data/UST_Finder/releases_1.txt"), delim = "\t")
vroom_write(releases_2, here("data/UST_Finder/releases_2.txt"), delim = "\t")
