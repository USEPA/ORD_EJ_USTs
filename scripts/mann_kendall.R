library(tidyverse)
library(trend)

#########################
## National Statistics ##
#########################

# us <- read.csv("/work/TANKSDB/github/UST_groundwater_vulnerability_model/projects/EJ/Article/figures/national/National_Density_EJ_Stats.csv")
# 
# ##########################
# # National Mann-Kendall ##
# ##########################
# 
# 
# # Iterate through variable combinations and output tests to new data frame
# 
# ## USING MEDIANS
# 
# usMK <- data.frame()
# 
# for (x in unique(us$X_Var)) {
#   for (y in unique(us$Y_Var)) {
#     sub <- us%>%
#       filter(X_Var == x & Y_Var == y)%>%
#       arrange(Bin)
#     ts <- ts(sub$Median, start=1, end=10, frequency=1)
#     mk <- mk.test(ts)
#     ss <- sens.slope(ts)
#     newrow <- data.frame("X_Var" = x, "Y_Var" = y,
#                          "p_val" = mk$p.value,
#                          "z_val" = as.numeric(mk$statistic),
#                          "s" = as.numeric(mk$estimates[1]),
#                          "vars" = as.numeric(mk$estimates[2]),
#                          "tau" = as.numeric(mk$estimates[3]),
#                          "Sens_Slope" = as.numeric(ss$estimates))
#     usMK <- rbind(usMK,newrow)
#   }
# }
# 
# # Save the outputs
# write.csv(usMK, "/work/TANKSDB/github/UST_groundwater_vulnerability_model/projects/EJ/Article/data/US_Medians_Mann_Kendall.csv")

## USING ENTIRE DATASET
allbgs <- read.csv("/work/TANKSDB/github/UST_groundwater_vulnerability_model/projects/EJ/Article/data/BGs_All_Attributes.csv")%>%
  drop_na(Bin_Tile_MN,Bin_Tile_LI,Facility_km, Releases_km, Tanks_km, Capacity_km)

usMK2 <- data.frame()

for (n in 15:16) {
  
  freq <- round(as.numeric(table(allbgs[,n])[1]))
  
  for (i in 17:20) {
    print(paste0("Starting ",colnames(allbgs)[n]," x ",colnames(allbgs)[i]," --- ",Sys.time()))
    sub <- allbgs[,c(n,i)]%>%
      arrange(.[[1]])
    print(paste0("Creating Time Series ---",Sys.time()))
    ts <- ts(sub[,2], start=1, end=10, frequency=freq)
    print(paste0("Running Mann-Kendall ---", Sys.time()))
    mk <- mk.test(ts)
    print(paste0("Calculating sens slope ---", Sys.time()))
    ss <- sens.slope(ts)
    print(paste0("Writing Results ---",Sys.time()))
    newrow <- data.frame("X_Var" = colnames(allbgs)[n], "Y_Var" = colnames(allbgs)[i],
                         "p_val" = mk$p.value,
                         "z_val" = as.numeric(mk$statistic),
                         "s" = as.numeric(mk$estimates[1]),
                         "vars" = as.numeric(mk$estimates[2]),
                         "tau" = as.numeric(mk$estimates[3]),
                         "Sens_Slope" = as.numeric(ss$estimates))
    usMK2 <- rbind(usMK2,newrow)
    
    write.csv(usMK2, "/work/TANKSDB/github/UST_groundwater_vulnerability_model/projects/EJ/Article/data/US_All_Mann_Kendall.csv")
    
    print(paste0("Completed: ",colnames(allbgs)[n]," with ", colnames(allbgs)[i]," --- ",Sys.time()))
  }
}

# Save the outputs
write.csv(usMK2, "/work/TANKSDB/github/UST_groundwater_vulnerability_model/projects/EJ/Article/data/US_All_Mann_Kendall.csv")