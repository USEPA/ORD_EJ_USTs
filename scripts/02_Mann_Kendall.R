library(tidyverse)
library(plotly)
library(trend)
library(rstatix)
library(aod)
library(here)



##########################
# National Mann-Kendall ##
##########################

us <- read.csv(here("projects/EJ/Article/figures/national/National_Density_EJ_Stats.csv"))

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
write.csv(usMK, here("projects/EJ/Article/data/national/National_Mann_Kendall1.csv"))

################################
# State by state Mann-Kendall ##
################################

# Import all state stats and run the mann-kendall
stfiles <- list.files(here("projects/EJ/Article/data/state_stats/"), pattern = "EJ_Bin_Stats.csv", recursive = TRUE, full.names = TRUE)

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