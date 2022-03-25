library(tidyverse)
library(gt)
library(plotly)
library(trend)
library(rstatix)
library(aod)
library(here)
library(colorspace)


#########################
## National Statistics ##
#########################

us <- read.csv(here("projects/EJ/Article/figures/national/National_Density_EJ_Stats.csv"))

# Calculate Ratio between upper and lower median bu variable pair

usratios <- data.frame()

for (x in unique(us$X_Var)){
  for (y in unique(us$Y_Var)){
    lowbin <- us%>%
      filter(X_Var == x & Y_Var == y & Bin == "0-10")
    highbin <- us%>%
      filter(X_Var == x & Y_Var == y & Bin == "90-100")
    newrow <- data.frame("X_Var" = x, "Y_Var" = y, "Ratio" = round(highbin$Median / lowbin$Median,1))
    usratios <- rbind(usratios,newrow)
  }
}

# Rename variables to plot
usratios <- usratios%>%
  mutate(X_Var = ifelse(X_Var == "_Tile_MN","Percent Minority",
                        ifelse(X_Var == "_Tile_LI","Percent Low Income", NA)),
         Y_Var = ifelse(Y_Var == "Facility_km", "Facilities",
                        ifelse(Y_Var == "Releases_km","Releases",
                               ifelse(Y_Var == "Tanks_km", "Tanks",
                                      ifelse(Y_Var == "Capacity_km", "Capacity",NA)))))

## Heatmap
ggplot(usratios, aes(x = X_Var, y = Y_Var, fill= Ratio)) + 
  geom_tile() +
  geom_text(aes(label=Ratio),hjust=0, vjust=0)+
  scale_fill_gradient(low="yellow", high="red")+
  labs(title = "Ratios of Highest Bin to Lowest Bin Medians", x = "Demographic Indicator", y = "UST  Variable")

# GT table
uswide <- usratios%>%
  pivot_wider(names_from = X_Var, values_from = Ratio)
colnames(uswide)[1] <- "Indicator"
gt(uswide)%>%
  tab_header(title = "U.S. EJ Ratios",
             subtitle = "Highest Bin Median Divided by Lowest Bin Median")



##########################
# National Mann-Kendall ##
##########################


# Iterate through variable combinations and output tests to new data frame

## USING MEDIANS

usMK <- data.frame()

for (x in unique(us$X_Var)) {
  for (y in unique(us$Y_Var)) {
    sub <- us%>%
      filter(X_Var == x & Y_Var == y)%>%
      arrange(Bin)
    ts <- ts(sub$Median, start=1, end=10, frequency=1)
    mk <- mk.test(ts)
    ss <- sens.slope(ts)
    newrow <- data.frame("X_Var" = x, "Y_Var" = y,
                         "p_val" = mk$p.value,
                         "z_val" = as.numeric(mk$statistic),
                         "s" = as.numeric(mk$estimates[1]),
                         "vars" = as.numeric(mk$estimates[2]),
                         "tau" = as.numeric(mk$estimates[3]),
                         "Sens_Slope" = as.numeric(ss$estimates))
    usMK <- rbind(usMK,newrow)
  }
}

# Save National Mann-Kendall
write.csv(usMK, here("projects/EJ/Article/data/national/National_Mann_Kendall.csv"))

################################
# State by state Mann-Kendall ##
################################

# Import all state stats and run the mann-kendall
stfiles <- list.files(here("projects/EJ/Article/data/state_stats/"), pattern = "EJ_Stats.csv", recursive = TRUE, full.names = TRUE)

stEJ <- data.frame()

for (n in 1:length(stfiles)) { # Combine all states into one dataset
  df <- read.csv(stfiles[n])
  state <- str_split(stfiles[n], "/")[[1]][9]
  df$State <- state
  stEJ <- rbind(stEJ,df)
}

## Run MK iterator
statesMK <- data.frame()

for (state in unique(stEJ$State)) {
  df <- stEJ%>%
    filter(State == state)
  for (x in unique(df$X_Var)) {
    for (y in unique(df$Y_Var)) {
      sub <- df%>%
        filter(X_Var == x & Y_Var == y)%>%
        arrange(Bin)
      ts <- ts(sub$Median, start=1, end=10, frequency=1)
      mk <- mk.test(ts)
      ss <- sens.slope(ts)
      newrow <- data.frame("State" = state,"X_Var" = x, "Y_Var" = y,
                           "p_val" = mk$p.value,
                           "z_val" = as.numeric(mk$statistic),
                           "s" = as.numeric(mk$estimates[1]),
                           "vars" = as.numeric(mk$estimates[2]),
                           "tau" = as.numeric(mk$estimates[3]),
                           "Sens_Slope" = as.numeric(ss$estimates))
      statesMK <- rbind(statesMK,newrow)
    }
  }
  print(paste0("Completed ",state, " --- ",Sys.time()))
}

## Save Mann-Kendall results
write.csv(statesMK, here("projects/EJ/Article/data/state_stats/All_States_Mann_Kendall.csv"))



# Reference for MK:
# https://www.statmethods.net/advstats/timeseries.html (making ts data)
# https://cran.r-project.org/web/packages/trend/trend.pdf (trend package)
# https://cran.r-project.org/web/packages/trend/vignettes/trend.pdf (MK test)


###################
# Kruskal-Wallis ##
###################

# National
allbgs <- read.csv(here("projects/EJ/Article/data/BGs_All_Attributes.csv"))%>%
  drop_na(Bin_Tile_MN,Bin_Tile_LI,Facility_km, Releases_km, Tanks_km, Capacity_km)

usKW <- data.frame()

for (n in 15:16) {
  
  for (i in 17:20) {
    sub <- allbgs[,c(n,i)]%>%
      arrange(.[[1]])
    colnames(sub) <- c("Bin","Density")
    kw <- sub%>%
      kruskal_test(Density ~ Bin)
    kw.effect <- sub%>%
      kruskal_effsize(Density ~ Bin)
    newrow <- data.frame("X_Var" = colnames(allbgs)[n], "Y_Var" = colnames(allbgs)[i],
                         "p_val" = as.numeric(kw$p),
                         "chi-squared" = as.numeric(kw$statistic),
                         "effect" = as.numeric(kw.effect$effsize),
                         "magnitude" = as.character(kw.effect$magnitude))
    usKW <- rbind(usKW,newrow)
    
    print(paste0("Completed: ",colnames(allbgs)[n]," with ", colnames(allbgs)[i]," --- ",Sys.time()))
  }
}
write.csv(statesMK, here("projects/EJ/Article/data/national/US_Kruskal_Wallis.csv"))


####################
# Pairwise Wilcox ##
####################

usPW <- data.frame()

for (n in 15:16) {
  
  for (i in 17:20) {
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
             "Y_Var" = colnames(allbgs)[i])

    usPW <- rbind(usPW,newData)
    
    print(paste0("Completed: ",colnames(allbgs)[n]," with ", colnames(allbgs)[i]," --- ",Sys.time()))
  }
}


#write.csv(usPW, here("projects/EJ/Article/data/national/US_Pairwise_Wilcox.csv"))
usPW <- read.csv(here("projects/EJ/Article/data/national/US_Pairwise_Wilcox.csv"))


# Heatmaps for pairwise wilcox (National)

## Create Labels
usPW <- usPW%>%
  mutate(x_lab = ifelse(X_Var == "Bin_Tile_MN","Minority","Low Income"),
         y_lab = ifelse(Y_Var == "Facility_km","Facilities",
                        ifelse(Y_Var == "Releases_km", "Releases",
                               ifelse(Y_Var == "Tanks_km", "Tanks",
                                      ifelse(Y_Var == "Capacity_km","Capacity",NA)))),
         group = paste0(x_lab," vs. ",y_lab))

for (n in unique(usPW$X_Var)) {
  for (i in unique(usPW$Y_Var)) {
    sub <- usPW%>%
      filter(X_Var == n & Y_Var == i)
    p <- ggplot(sub)+
      geom_tile(aes(x = group1, y = group2, fill = effsize))+
      geom_text(aes(x = group1, y = group2, label = p.adj.signif))+
      scale_fill_gradient2(name = "Effect",
                           low = "green",
                           mid = "yellow",
                           high = "red",
                           midpoint = .1,
                           na.value = "grey50"
      )+
      labs(title = paste0("Effect Size Between Bin Pairs ",sub$x_lab[1]," vs. ",sub$y_lab[1], " Density)"), x = paste0(sub$x_lab[1], " Percentile Bin"), y = paste0(sub$y_lab[1],"Low-Income Percentile Bin"))
    
    ggsave(filename = paste0(here("projects/EJ/Article/figures/national"),"/",n,"_",i,"_heat.png"), plot = p)
    # State 
  }
}


#pal <- RColorBrewer::brewer.pal(3,"RdYlGn")

#p <- ggplot(usPW)+
#  geom_tile(aes(x = group1, y = group2, fill = effsize))+
#  geom_text(aes(x = group1, y = group2, label = p.adj.signif),color = "black", size = 4)+
#  scale_fill_continuous_divergingx(palette = "RdYlGn",rev = TRUE, mid = 0.1, l3 = 10, p3 = 2, p4 = .1) +



p <- ggplot(usPW)+
  geom_tile(aes(x = group1, y = group2, fill = effsize))+
  geom_text(aes(x = group1, y = group2, label = p.adj.signif))+
  scale_fill_gradient2(name = "Effect",
                       low = "green",
                       mid = "yellow",
                       high = "red",
                       midpoint = .1,
                       na.value = "grey50")+

  labs(title = "Pairwise Wilcox Effect Size Between Bin Pairs",
       y = "Percentile Bin", x = "Percentile Bin")+
  facet_wrap(~group, nrow = 4)+
  theme(strip.text.x = element_text(size=12),
        strip.background = element_rect(colour="black", fill="#bdbdbd"),
        legend.position="bottom")+
  guides(fill= guide_colorbar(barwidth=15))

p
ggsave(filename = here("projects/EJ/Article/figures/national/US_heat.png"), plot = p, width = 7.5, height = 9, units = "in",dpi = 600)


### Scratch code

scale_fill_gradient2(name = "Effect",
                     low = "#05f5a9",
                     mid = "#fcba03",
                     high = "red",
                     midpoint = .1,
                     na.value = "grey50")+


## mann-kendall USING ENTIRE DATASET
allbgs <- read.csv(here("projects/EJ/Article/data/BGs_All_Attributes.csv"))%>%
  drop_na(Bin_Tile_MN,Bin_Tile_LI,Facility_km, Releases_km, Tanks_km, Capacity_km)

usMK2 <- data.frame()

for (n in 15:16) {
  
  freq <- round(as.numeric(table(allbgs[,n])[1]))
  
  for (i in 17:20) {
    sub <- allbgs[,c(n,i)]%>%
      arrange(.[[1]])
    ts <- ts(sub[,2], start=1, end=10, frequency=freq)
    mk <- mk.test(ts)
    ss <- sens.slope(ts)
    newrow <- data.frame("X_Var" = colnames(allbgs)[n], "Y_Var" = colnames(allbgs)[i],
                         "p_val" = mk$p.value,
                         "z_val" = as.numeric(mk$statistic),
                         "s" = as.numeric(mk$estimates[1]),
                         "vars" = as.numeric(mk$estimates[2]),
                         "tau" = as.numeric(mk$estimates[3]),
                         "Sens_Slope" = as.numeric(ss$estimates))
    usMK2 <- rbind(usMK2,newrow)
    
    print(paste0("Completed: ",colnames(allbgs)[n]," with ", colnames(allbgs)[i]," --- ",Sys.time()))
  }
}

# Pairwise Kruskal-Wallis
pwc <-sub%>%
  wilcox_test(Density ~ Bin, p.adjust.method = "bonferroni")

ggplot(sub)+
  geom_boxplot(aes(x = Bin_Tile_MN, y = Facility_km))+
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(kw, detailed = TRUE)
  )