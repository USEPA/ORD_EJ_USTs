# This script will check to make sure you have all of the necessary packages
# installed.

# All packages needed
packages <- c("tidyverse","plotly","sf","here","vroom","scales","trend",
              "rstatix","aod","colorspace","foreach","doParallel","reticulate",
              "mapsf","USAboundaries","tigris")

# Create empty data frame to write to
pkgCheck <- data.frame()

# Loop through packages to check install status
for(n in 1:length(packages)){
  pkg <- packages[n]
  path <- system.file(package = pkg)
  
  newRow <- data.frame("Package" = pkg,
                       "Installed" = ifelse(path == "","NO","YES"))
  
  pkgCheck <- rbind(pkgCheck,newRow)
}

# Filter out packages already installed
need2install <- subset(pkgCheck, Installed == "NO")

# Install needed packages
install.packages(need2install$Package)

reticulate::install_miniconda()
reticulate::conda_install('r-reticulate', 'python-kaleido')
reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
reticulate::use_miniconda('r-reticulate')
