library(tidyverse)
library(plotly)
library(trend)
library(rstatix)
library(aod)
library(here)
library(colorspace)
library(vroom)

####################
# Pairwise Wilcox ##
####################

## National ##
allbgs <- vroom(here("projects/EJ/Article/data/BGs_All_Attributes_fix.csv"))%>%
  drop_na(Facility_km, Releases_km, Tanks_km)%>%
  select(GISJOIN:Tanks_km)

usPW <- data.frame()

for (n in 18:31) { # Iterate through demographic indicators
  for (i in 32:34) { # Iterate through UST measures
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
    
    print(paste0("Completed: ",colnames(allbgs)[n]," with ", colnames(allbgs)[i]," --- ",Sys.time()))
  }
}

write.csv(usPW, here("projects/EJ/Article/data/national/US_Pairwise_Wilcox.csv"))
#usPW <- read.csv(here("projects/EJ/Article/data/national/US_Pairwise_Wilcox.csv"))

# Create Wilcox Plots
usPW <- usPW%>%
  filter(!Y_Var =="Capacity_km")

# National Percentiles
usPWN <- usPW%>%
  filter(EJ_Cat == "N")

p <- ggplot(usPWN)+
  geom_tile(aes(x = group1, y = group2, fill = effsize))+
  geom_text(aes(x = group1, y = group2, label = p.adj.signif),color = "black", size = 4)+
  scale_fill_continuous_divergingx(palette = "RdYlGn",rev = TRUE, mid = 0.1, l3 = 10, p3 = 2, p4 = .1)+
  labs(title = "Pairwise Wilcox Effect Size Between Bin Pairs (National Percentiles)",
       y = "Percentile Bin", x = "Percentile Bin")+
  facet_wrap(~group, ncol = 4)+
  theme(strip.text.x = element_text(size=12),
        strip.background = element_rect(colour="black", fill="#bdbdbd"),
        legend.position="bottom")+
  guides(fill= guide_colorbar(barwidth=15))

p
ggsave(filename = here("projects/EJ/Article/figures/national/US_Pairwise_Wilcox_Nat_Pctiles.png"), plot = p, width = 14, height = 18, units = "in",dpi = 600)

# State Percentiles
usPWS <- usPW%>%
  filter(EJ_Cat == "S")

p2 <- ggplot(usPWS)+
  geom_tile(aes(x = group1, y = group2, fill = effsize))+
  geom_text(aes(x = group1, y = group2, label = p.adj.signif),color = "black", size = 4)+
  scale_fill_continuous_divergingx(palette = "RdYlGn",rev = TRUE, mid = 0.1, l3 = 10, p3 = 2, p4 = .1)+
  labs(title = "Pairwise Wilcox Effect Size Between Bin Pairs (State Percentiles)",
       y = "Percentile Bin", x = "Percentile Bin")+
  facet_wrap(~group, nrow = 7)+
  theme(strip.text.x = element_text(size=12),
        strip.background = element_rect(colour="black", fill="#bdbdbd"),
        legend.position="bottom")+
  guides(fill= guide_colorbar(barwidth=15))

p2
ggsave(filename = here("projects/EJ/Article/figures/national/US_Pairwise_Wilcox_ST_Pctiles.png"), plot = p2, width = 14, height = 18, units = "in",dpi = 600)


## State by State ##


for(state in unique(allbgs$STATE_NAME)){
  stateFilt <- allbgs%>%
    filter(STATE_NAME == state)
  
  stPW <- data.frame() # Create empty data frame for each state
  
  for (n in 25:31) { # Iterate through demographic indicators
    for (i in 32:35) { # Iterate through UST measures
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
      
      stPW <- bind_rows(stPW,newData)
      
      print(paste0("Completed stats: ",state," - ",colnames(allbgs)[n]," with ", colnames(allbgs)[i]," --- ",Sys.time()))
    }
  }
  
  # Write the stats when a state finishes
  write.csv(stPW, paste0(here("projects/EJ/Article/data/state_stats"),"/",state,"/",state,"_Pairwise_Wilcox.csv"))
  
  # Create a plot for each state
  
  # Using National Percentiles
  
  # stPWN <- stPW%>%
  #   filter(EJ_Cat == "N")
  # 
  # p3 <- ggplot(stPWN)+
  #   geom_tile(aes(x = group1, y = group2, fill = effsize))+
  #   geom_text(aes(x = group1, y = group2, label = p.adj.signif),color = "black", size = 4)+
  #   scale_fill_continuous_divergingx(palette = "RdYlGn",rev = TRUE, mid = 0.1, l3 = 10, p3 = 2, p4 = .1)+
  #   labs(title = "Pairwise Wilcox Effect Size Between Bin Pairs (National Percentiles)",
  #        y = "Percentile Bin", x = "Percentile Bin")+
  #   facet_wrap(~group, nrow = 7)+
  #   theme(strip.text.x = element_text(size=12),
  #         strip.background = element_rect(colour="black", fill="#bdbdbd"),
  #         legend.position="bottom")+
  #   guides(fill= guide_colorbar(barwidth=15))
  # 
  # #p3
  # ggsave(filename = paste0(here("projects/EJ/Article/figures/state"),"/",state,"/",state,"_Pairwise_Wilcox_Nat_Pctiles.png"), plot = p3, width = 14, height = 18, units = "in",dpi = 600)
  # print(paste0("Completed plot: ",state," - ",Sys.time()))
  
  # Using State Percentiles
  
  stPWS <- stPW%>%
    filter(EJ_Cat == "S")
  
  p4 <- ggplot(stPWS)+
    geom_tile(aes(x = group1, y = group2, fill = effsize))+
    geom_text(aes(x = group1, y = group2, label = p.adj.signif),color = "black", size = 4)+
    scale_fill_continuous_divergingx(palette = "RdYlGn",rev = TRUE, mid = 0.1, l3 = 10, p3 = 2, p4 = .1)+
    labs(title = "Pairwise Wilcox Effect Size Between Bin Pairs (State Percentiles)",
         y = "Percentile Bin", x = "Percentile Bin")+
    facet_wrap(~group, nrow = 7)+
    theme(strip.text.x = element_text(size=12),
          strip.background = element_rect(colour="black", fill="#bdbdbd"),
          legend.position="bottom")+
    guides(fill= guide_colorbar(barwidth=15))
  
  p4
  ggsave(filename = paste0(here("projects/EJ/Article/figures/state"),"/",state,"/",state,"_Pairwise_Wilcox_ST_Pctiles.png"), plot = p4, width = 14, height = 18, units = "in",dpi = 600)
  print(paste0("Completed plot: ",state," - ",Sys.time()))
}



# State Percentiles