library(foreach)
library(doParallel)
library(vroom)
library(tidyverse)




library(plotly)
library(trend)
library(rstatix)
library(aod)
library(here)
library(colorspace)

# In this script we use parallel processing to speed up wilcox computation times.

####################
# Pairwise Wilcox ##
####################

## National ##
allbgs <- vroom(here("projects/EJ/Article/data/BGs_All_Attributes_fix.csv"))%>%
  select(GISJOIN:Tanks_km)

allbgs <- allbgs[1:1000,]
 
n_cores <- detectCores() - 1  
cl <- makeCluster(n_cores)  
registerDoParallel(cl)  

N_Wilcox <-foreach(n=18:31, .combine='rbind', .packages = c("tidyverse","trend","rstatix","aod")) %:%
  foreach(i=32:34, .combine='rbind', .packages = c("tidyverse","trend","rstatix","aod")) %dopar% {
    usPW <- data.frame()
        sub <- allbgs[,c(n,i)]%>%
          arrange(.[[1]])
        colnames(sub) <- c("Bin","Density")
        pw <- sub%>%
          wilcox_test(Density ~ Bin, p.adjust.method = "bonferroni")%>%
          as.data.frame()
        pw.effect <- sub%>%
          wilcox_effsize(Density ~ Bin)%>%
          as.data.frame()%>%
          select(effsize,magnitude)
        newData <- cbind(pw,pw.effect)%>%
          mutate("X_Var" = colnames(allbgs)[n],
                 "Y_Var" = colnames(allbgs)[i],
                 x_lab = ifelse(X_Var == "N_P_LESHSPCT_BIN","Less Than High School",
                                ifelse(X_Var == "S_P_LESHSPCT_BIN","Less Than High School",
                                       ifelse(X_Var == "N_P_LNGISPCT_BIN","Linguistically Isolated",
                                              ifelse(X_Var == "S_P_LNGISPCT_BIN","Linguistically Isolated",
                                                     ifelse(X_Var == "N_P_LWINCPCT_BIN","Low Income",
                                                            ifelse(X_Var == "S_P_LWINCPCT_BIN","Low Income",
                                                                   ifelse(X_Var == "N_P_MINORPCT_BIN","Minority",
                                                                          ifelse(X_Var == "S_P_MINORPCT_BIN","Minority",
                                                                                 ifelse(X_Var == "N_P_OVR64PCT_BIN","Over 64",
                                                                                        ifelse(X_Var == "S_P_OVR64PCT_BIN","Over 64",
                                                                                               ifelse(X_Var == "N_P_UNDR5PCT_BIN","Under 5",
                                                                                                      ifelse(X_Var == "S_P_UNDR5PCT_BIN","Under 5",
                                                                                                             ifelse(X_Var == "N_P_VULEOPCT_BIN","Minority & Low Income",
                                                                                                                    ifelse(X_Var == "S_P_VULEOPCT_BIN","Minority & Low Income",NA)))))))))))))),
                 y_lab = ifelse(Y_Var == "Facility_km","Facilities",
                                ifelse(Y_Var == "Releases_km", "Releases",
                                       ifelse(Y_Var == "Tanks_km", "Tanks",
                                              ifelse(Y_Var == "Capacity_km","Capacity",NA)))),
                 group = paste0(x_lab," vs. ",y_lab),
                 EJ_Cat = substr(X_Var,1,1))
        
        
        usPW <- rbind(usPW,newData)
        
        #print(paste0("Completed: ",colnames(allbgs)[n]," with ", colnames(allbgs)[i]," --- ",Sys.time()))
  }

write.csv(N_Wilcox, here("projects/EJ/Article/data/national/US_Pairwise_Wilcox.csv"))
#usPW <- read.csv(here("projects/EJ/Article/data/national/US_Pairwise_Wilcox.csv"))

# Create Wilcox Plots
usPW <- usPW%>%
  filter(!Y_Var =="Capacity_km")

p <- ggplot(N_Wilcox)+
  geom_tile(aes(x = group1, y = group2, fill = effsize))+
  geom_text(aes(x = group1, y = group2, label = p.adj.signif),color = "black", size = 4)+
  scale_fill_continuous_divergingx(palette = "RdYlGn",rev = TRUE, mid = 0.1, l3 = 10, p3 = 2, p4 = .1)+
  labs(title = "Pairwise Wilcox Effect Size Between Bin Pairs",
       y = "Percentile Bin", x = "Percentile Bin")+
  facet_wrap(~group, ncol = 3)+
  theme(strip.text.x = element_text(size=12),
        strip.background = element_rect(colour="black", fill="#bdbdbd"),
        legend.position="bottom")+
  guides(fill= guide_colorbar(barwidth=15))

p
ggsave(filename = here("projects/EJ/Article/figures/national/US_Pairwise_Wilcox.png"), plot = p, width = 8.5, height = 10, units = "in",dpi = 600)

## State by State ##

  # Testing parallel structure for state by state
S_Wilcox <-foreach(state = unique(allbgs$STATE_NAME), .combine='rbind', .packages = c("tidyverse","trend","rstatix","aod")) %:%
  foreach(n=18:31, .combine='rbind') %:%
  foreach(i=32:34, .combine='rbind') %dopar% {
    stateFilt <- allbgs%>%
      filter(STATE_NAME == state)
    
    stPW <- data.frame() # Create empty data frame for each state
    
    sub <- stateFilt[,c(n,i)]%>%
      arrange(.[[1]])
    colnames(sub) <- c("Bin","Density")
    
    # Remove bins with all zeroes
    nonZero <- sub%>%
      group_by(Bin)%>%
      mutate(max = max(Density))%>%
      ungroup()%>%
      select(Bin, max)%>%
      distinct()%>%
      filter(!max == 0)
    
    sub <- sub%>%
      filter(Bin %in% nonZero$Bin)
    
    pw <- sub%>%
      wilcox_test(Density ~ Bin, p.adjust.method = "bonferroni")%>%
      as.data.frame()
    pw.effect <- sub%>%
      wilcox_effsize(Density ~ Bin)%>%
      as.data.frame()%>%
      select(effsize,magnitude)
    newData <- cbind(pw,pw.effect)%>%
      mutate("X_Var" = colnames(allbgs)[n],
             "Y_Var" = colnames(allbgs)[i],
             State = state,
             x_lab = ifelse(X_Var == "Bin_Tile_LESSHSPCT","Less Than High School",
                            ifelse(X_Var == "Bin_Tile_LINGISOPCT","Linguistically Isolated",
                                   ifelse(X_Var == "Bin_Tile_LOWINCPCT","Low Income",
                                          ifelse(X_Var == "Bin_Tile_MINORPCT","Minority",
                                                 ifelse(X_Var == "Bin_Tile_OVER64PCT","Over 64",
                                                        ifelse(X_Var == "Bin_Tile_UNDER5PCT","Under 5",
                                                               ifelse(X_Var == "Bin_Tile_VULEOPCT","Minority & Low Income",NA))))))),
             y_lab = ifelse(Y_Var == "Facility_km","Facilities",
                            ifelse(Y_Var == "Releases_km", "Releases",
                                   ifelse(Y_Var == "Tanks_km", "Tanks",
                                          ifelse(Y_Var == "Capacity_km","Capacity",NA)))),
             group = paste0(x_lab," vs. ",y_lab))
    
    stPW <- bind_rows(stPW,newData)
    
}


# Write the stats when a state finishes
write.csv(stPW, paste0(here("projects/EJ/Article/data/state_stats_Pairwise_Wilcox_All.csv"))
  
for (state in unique(stPW$State)) {
  
  sub <- stPW%>%
    filter(State == state)
  
  # Create a plot for each state
  p <- ggplot(sub)+
    geom_tile(aes(x = group1, y = group2, fill = effsize))+
    geom_text(aes(x = group1, y = group2, label = p.adj.signif),color = "black", size = 4)+
    scale_fill_continuous_divergingx(palette = "RdYlGn",rev = TRUE, mid = 0.1, l3 = 10, p3 = 2, p4 = .1)+
    labs(title = "Pairwise Wilcox Effect Size Between Bin Pairs",
         y = "Percentile Bin", x = "Percentile Bin")+
    facet_wrap(~group, nrow = 7)+
    theme(strip.text.x = element_text(size=12),
          strip.background = element_rect(colour="black", fill="#bdbdbd"),
          legend.position="bottom")+
    guides(fill= guide_colorbar(barwidth=15))
  
  p
  ggsave(filename = paste0(here("projects/EJ/Article/figures/state"),"/",state,"/",state,"_Pairwise_Wilcox.png"), plot = p, width = 14, height = 18, units = "in",dpi = 600)
  print(paste0("Completed plot: ",state," - ",Sys.time()))
}
