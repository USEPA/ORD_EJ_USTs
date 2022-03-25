library(tidyverse)
library(here)
library(vroom)
library(gt)

nmk <- vroom(here("projects/EJ/Article/data/national/National_Mann_Kendall.csv"))%>%
  filter(substr(X_Var,1,1)=="N")%>%
  mutate(xLab = ifelse(X_Var == "N_P_MINORPCT","Minority",
                       ifelse(X_Var == "N_P_LWINCPCT", "Low-Income",
                              ifelse(X_Var == "N_P_LESHSPCT","Less Than High-School",
                                     ifelse(X_Var == "N_P_LNGISPCT","Linguistically Isolated",
                                            ifelse(X_Var == "N_P_UNDR5PCT","Under 5",
                                                   ifelse(X_Var == "N_P_OVR64PCT","Over 64",
                                                          ifelse(X_Var == "N_P_VULEOPCT","Minority & Low-Income",NA))))))),
         yLab = ifelse(Y_Var == "Facility_km","Facilities",
                       ifelse(Y_Var == "Releases_km","Releases",
                              ifelse(Y_Var == "Tanks_km","Tanks",
                                     ifelse(Y_Var == "Capacity_km","Capacity",NA)))))

table1 <- nmk%>%
  select(xLab,yLab,p_val,z_val,s,Sens_Slope)%>%
  mutate(p_val = round(p_val,5),
         z_val = round(z_val,3),
         Sens_Slope = round(Sens_Slope,2))
colnames(table1) <- c("Demographic Indicator (Percentile)","UST Indicator","P-Value",
                      "Z-Value","Score","Sens Slope")
tbl1 <- gt(table1)
tbl1
gtsave(tbl1,here("projects/EJ/Article/figures/Paper_Figures/MK_Table.png"))

# Make smaller table including only Tanks / Releases / Minority / Low-Income
sub <- nmk%>%
  filter(Y_Var =="Releases_km" | Y_Var == "Tanks_km")%>%
  filter(X_Var == "N_P_MINORPCT" | X_Var == "N_P_LWINCPCT")%>%
  arrange(desc(Y_Var))

table2 <- sub%>%
  select(yLab,xLab,p_val,z_val,s,Sens_Slope)%>%
  mutate(p_val = round(p_val,5),
         z_val = round(z_val,3),
         Sens_Slope = round(Sens_Slope,2))
colnames(table2) <- c("UST Indicator","Demographic Indicator (Percentile)","P-Value",
                      "Z-Value","Score","Sens Slope")
tbl2 <- gt(table2)
tbl2
gtsave(tbl2,here("projects/EJ/Article/figures/Paper_Figures/MK_Table_Tanks_Releases.png"))
