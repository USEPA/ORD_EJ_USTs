library(tidyverse)
library(sf)
#library(vroom)


sf <- st_read("D:/data/EJ/EJSCREEN_2020_USPR.gdb", layer = "EJSCREEN_Full")%>%
  mutate(GISJOIN = paste0("G",substr(ID,1,2),"0",substr(ID,3,5),"0",substr(ID,6,12)))%>% # Create GISJOIN ID in case we want to use NHGIS data later
  select(GISJOIN,P_LDPNT_D2,P_DSLPM_D2,P_CANCR_D2,P_RESP_D2,P_PTRAF_D2,P_PWDIS_D2,
         P_PNPL_D2,P_PRMP_D2,P_PTSDF_D2,P_OZONE_D2,P_PM25_D2,ACSTOTPOP,AREALAND)%>% # Trim to needed columns
  filter(AREALAND > 0 & ACSTOTPOP > 0)%>%
  st_transform(5070) # Project to Albers equal area conic projection

df <- sf%>%
  st_drop_geometry()

counts80 <- df%>%
  mutate(PM25_80 = ifelse(P_PM25_D2 >= 80,1,0),
         Ozone_80 = ifelse(P_OZONE_D2 >=80,1,0),
         TSDF_80 = ifelse(P_PTSDF_D2 >=80,1,0),
         RMP_80 = ifelse(P_PRMP_D2 >=80,1,0),
         NPL_80 = ifelse(P_PNPL_D2 >= 80,1,0),
         MjrQ_80 = ifelse(P_PWDIS_D2 >= 80,1,0),
         Trfc_80 = ifelse(P_PTRAF_D2 >= 80,1,0),
         AirTox_80 = ifelse(P_RESP_D2 >=80,1,0),
         Ar_Cncr_80 = ifelse(P_CANCR_D2 >=80,1,0),
         Dsl_PM_80 = ifelse(P_DSLPM_D2 >= 80,1,0),
         Led_Pnt_80 = ifelse(P_LDPNT_D2 >= 80,1,0))%>%
  mutate(Total80 = select(., PM25_80:Led_Pnt_80) %>% rowSums(na.rm = TRUE),
         Total80 = replace_na(Total80,0))%>%
  select(GISJOIN,Total80)


# Import Block Group Data from script 01
blkGrps <- read.csv(here("projects/EJ/Article/data/BGs_All_Attributes_fix.csv"))%>%
  left_join(counts80)%>%
  select(GISJOIN,Tanks_km,Releases_km,Total80)

statsOut <- data.frame()
for (n in 2:3) {
  
  y_var <- colnames(blkGrps[n])
  
  sel <- blkGrps%>%
    select(GISJOIN,colnames(blkGrps[n]),Total80)
  
  colnames(sel)[2] <- 'VAR'
  
  stats <- sel%>%
    group_by(Total80)%>% # Group by the bin and compute stats
    mutate(First_Quart = quantile(VAR,.25, na.rm = TRUE),
           Median = median(VAR, na.rm = TRUE),
           Third_Quart = quantile(VAR,.75, na.rm = TRUE),
           IQR = Third_Quart - First_Quart,
           Y_Var = y_var,
           X_Var = "Total80")%>%
    ungroup()%>%
    select(Total80,X_Var,Y_Var,First_Quart,Median,Third_Quart,IQR)%>%
    distinct()
  
  statsOut <- rbind(statsOut,stats)
}

write.csv(statsOut, here("projects/EJ/Article/data/EJ_Index_Stats.csv"))

# Run mann kendall on everything

MK1 <- data.frame()

for (y in 2:3) {
  sub <- blkGrps%>%
    select(GISJOIN,colnames(blkGrps)[y],Total80)%>%
    arrange(Total80)
  colnames(sub)[2] <- "VAR"
  ts <- ts(sub$VAR, start=0, end=11)
  mk <- mk.test(ts)
  ss <- sens.slope(ts)
  newrow <- data.frame("X_Var" = "Total80", "Y_Var" = y,
                       "p_val" = mk$p.value,
                       "z_val" = as.numeric(mk$statistic),
                       "s" = as.numeric(mk$estimates[1]),
                       "vars" = as.numeric(mk$estimates[2]),
                       "tau" = as.numeric(mk$estimates[3]),
                       "Sens_Slope" = as.numeric(ss$estimates))
  MK1 <- rbind(MK1,newrow)
}

ggplot(counts80)+
  geom_histogram(aes(x = Total80),binwidth = 1, color = "black")+
  xlim(0,11)+
  ylim(0,20000)

# Plot Tanks
ggplot(blkGrps)+
  geom_boxplot(aes(x = Total80, y = Tanks_km, group = Total80))

library(plotly)
plot_ly(blkGrps)%>%
  add_boxplot(x = ~Total80, y = ~Tanks_km)

# Plot Releases
plot_ly(blkGrps)%>%
  add_boxplot(x = ~Total80, y = ~Releases_km)

# Try binning Diesel Particulate Matter and Traffic
## Traffic
trf_Bin <- df%>%
  mutate(Bin = ifelse(as.numeric(P_PTRAF_D2) <= 10,"0-10",
                                   ifelse(as.numeric(P_PTRAF_D2) <=20,"10-20",
                                          ifelse(as.numeric(P_PTRAF_D2) <= 30,"20-30",
                                                 ifelse(as.numeric(P_PTRAF_D2)<=40,"30-40",
                                                        ifelse(as.numeric(P_PTRAF_D2)<=50,"40-50",
                                                               ifelse(as.numeric(P_PTRAF_D2)<=60,"50-60",
                                                                      ifelse(as.numeric(P_PTRAF_D2)<=70,"60-70",
                                                                             ifelse(as.numeric(P_PTRAF_D2)<=80,"70-80",
                                                                                    ifelse(as.numeric(P_PTRAF_D2)<=90,"80-90",
                                                                                           ifelse(as.numeric(P_PTRAF_D2)<=100,"90-100",NA)))))))))))

tBlkGrps <- blkGrps%>%
  left_join(trf_Bin)
MK2 <- data.frame()

for (y in 2:3) {
  sub <- tBlkGrps%>%
    select(GISJOIN,colnames(tBlkGrps)[y],Bin)%>%
    arrange(Bin)
  colnames(sub)[2] <- "VAR"
  ts <- ts(sub$VAR, start=0, end=11)
  mk <- mk.test(ts)
  ss <- sens.slope(ts)
  newrow <- data.frame("X_Var" = "Total80", "Y_Var" = colnames(tBlkGrps)[y],
                       "p_val" = mk$p.value,
                       "z_val" = as.numeric(mk$statistic),
                       "s" = as.numeric(mk$estimates[1]),
                       "vars" = as.numeric(mk$estimates[2]),
                       "tau" = as.numeric(mk$estimates[3]),
                       "Sens_Slope" = as.numeric(ss$estimates))
  MK2 <- rbind(MK2,newrow)
}


# Plot traffic vs Tanks
plot_ly(tBlkGrps)%>%
  add_boxplot(x = ~Bin, y = ~Tanks_km)%>%
  layout(xaxis = list(title = "EJScreen Traffic Percentile"),
         yaxis = list(title = "Tanks / km<sup>2</sup>"))

# Plot releases vs Tanks
plot_ly(tBlkGrps)%>%
  add_boxplot(x = ~Bin, y = ~Releases_km)%>%
  layout(xaxis = list(title = "EJScreen Traffic Percentile"),
         yaxis = list(title = "Releases / km<sup>2</sup>"))


dsl_Bin <- df%>%
  mutate(Bin = ifelse(as.numeric(P_DSLPM_D2) <= 10,"0-10",
                      ifelse(as.numeric(P_DSLPM_D2) <=20,"10-20",
                             ifelse(as.numeric(P_DSLPM_D2) <= 30,"20-30",
                                    ifelse(as.numeric(P_DSLPM_D2)<=40,"30-40",
                                           ifelse(as.numeric(P_DSLPM_D2)<=50,"40-50",
                                                  ifelse(as.numeric(P_DSLPM_D2)<=60,"50-60",
                                                         ifelse(as.numeric(P_DSLPM_D2)<=70,"60-70",
                                                                ifelse(as.numeric(P_DSLPM_D2)<=80,"70-80",
                                                                       ifelse(as.numeric(P_DSLPM_D2)<=90,"80-90",
                                                                              ifelse(as.numeric(P_DSLPM_D2)<=100,"90-100",NA)))))))))))

dBlkGrps <- blkGrps%>%
  left_join(dsl_Bin)
MK3 <- data.frame()

for (y in 2:3) {
  sub <- dBlkGrps%>%
    select(GISJOIN,colnames(dBlkGrps)[y],Bin)%>%
    arrange(Bin)
  colnames(sub)[2] <- "VAR"
  ts <- ts(sub$VAR, start=0, end=11)
  mk <- mk.test(ts)
  ss <- sens.slope(ts)
  newrow <- data.frame("X_Var" = "Total80", "Y_Var" = colnames(dBlkGrps)[y],
                       "p_val" = mk$p.value,
                       "z_val" = as.numeric(mk$statistic),
                       "s" = as.numeric(mk$estimates[1]),
                       "vars" = as.numeric(mk$estimates[2]),
                       "tau" = as.numeric(mk$estimates[3]),
                       "Sens_Slope" = as.numeric(ss$estimates))
  MK3 <- rbind(MK3,newrow)
}


# Plot traffic vs Tanks
plot_ly(dBlkGrps)%>%
  add_boxplot(x = ~Bin, y = ~Tanks_km)%>%
  layout(xaxis = list(title = "EJScreen Diesel Percentile"),
         yaxis = list(title = "Tanks / km<sup>2</sup>"))

# Plot traffic vs Releases
plot_ly(dBlkGrps)%>%
  add_boxplot(x = ~Bin, y = ~Releases_km)%>%
  layout(xaxis = list(title = "EJScreen Diesel Percentile"),
         yaxis = list(title = "Releases / km<sup>2</sup>"))
