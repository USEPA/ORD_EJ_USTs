library(tidyverse)
library(plotly)
library(trend)
library(aod)
library(here)
library(vroom)

##########################
# National Mann-Kendall ##
##########################

us <- vroom(here("data/Created/National_Density_EJ_Stats.csv"))

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
write.csv(usMK, here("data/Created/National_Mann_Kendall.csv"))

################################
# State by state Mann-Kendall ##
################################

# Import all state stats and run the mann-kendall
stFiles <- list.files(here("data/Created/state_stats/"),
                      pattern = "EJ_Bin_Stats.csv",
                      recursive = TRUE,
                      full.names = TRUE)
stEJ <- vroom(stFiles)

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
write.csv(statesMK, here("data/Created/All_States_Mann_Kendall.csv"))


# Reference for MK:
# https://www.statmethods.net/advstats/timeseries.html (making ts data)
# https://cran.r-project.org/web/packages/trend/trend.pdf (trend package)
# https://cran.r-project.org/web/packages/trend/vignettes/trend.pdf (MK test)