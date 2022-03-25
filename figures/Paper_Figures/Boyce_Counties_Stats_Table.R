library(tidyverse)
library(gt)
library(here)

df <- read.csv(here("projects/EJ/Article/data/Boyce/All_Boyce_Counties_Both.csv"))

sub <- data.frame("Demographic" = c("Minority","Low-Income","Minority","Low-Income"),
                  "Exposure" = c("Tanks","Tanks","Releases","Releases"),
                  Min = c(min(df$Boyce_Med_Tanks_km_MINORPCT, na.rm = TRUE),min(df$Boyce_Med_Tanks_km_LOWINCPCT, na.rm = TRUE),min(df$Boyce_Med_Releases_km_MINORPCT, na.rm = TRUE),min(df$Boyce_Med_Releases_km_LOWINCPCT, na.rm = TRUE)),
                  Median = c(median(df$Boyce_Med_Tanks_km_MINORPCT, na.rm = TRUE),median(df$Boyce_Med_Tanks_km_LOWINCPCT, na.rm = TRUE),median(df$Boyce_Med_Releases_km_MINORPCT, na.rm = TRUE),median(df$Boyce_Med_Releases_km_LOWINCPCT, na.rm = TRUE)),
                  Max = c(max(df$Boyce_Med_Tanks_km_MINORPCT, na.rm = TRUE),max(df$Boyce_Med_Tanks_km_LOWINCPCT, na.rm = TRUE),max(df$Boyce_Med_Releases_km_MINORPCT, na.rm = TRUE),max(df$Boyce_Med_Releases_km_LOWINCPCT, na.rm = TRUE)))
sub$Median <- round(sub$Median,3)

gt(sub)


filt <- df%>%
  filter(is.infinite(Boyce_Med_Tanks_km_MINORPCT))
